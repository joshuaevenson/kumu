//  kumu.c

#include "kumu.h"
#include <stdio.h>

// ------------------------------------------------------------
// Macros
// ------------------------------------------------------------
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)ku_alloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(vm, type, ptr, old) ku_alloc(vm, ptr, sizeof(type) * (old), 0)
#define KALLOC(vm, type, count) \
    (type*)ku_alloc(vm, NULL, 0, sizeof(type) * (count))
#define KALLOC_OBJ(vm, type, objtype) \
    (type*)ku_obj_alloc(vm, sizeof(type), objtype)
#define FREE(vm, type, ptr) \
  ku_alloc(vm, ptr, sizeof(type), 0)

#define READ_SHORT(vm) \
(frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define BYTE_READ(vm) (*(frame->ip++))
#define CONST_READ(vm) (frame->closure->func->chunk.constants.values[BYTE_READ(vm)])
#define READ_STRING(vm) AS_STR(CONST_READ(vm))

#define BIN_OP(v, vt, op) \
  do { \
  if (!IS_NUM(ku_peek_stack(v,0)) || !IS_NUM(ku_peek_stack(v,1))) { \
    ku_err(v, "numbers expected"); \
    return KVM_ERR_RUNTIME; \
  } \
  double b = AS_NUM(ku_pop(v)); \
  double a = AS_NUM(ku_pop(v)); \
  ku_push(v, vt(a op b)); \
} while (false)


static void ku_printf(kuvm *vm, const char *fmt, ...) {
  va_list args;

  if (vm->flags & KVM_F_QUIET) {
    return;
  }
  
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}


// ------------------------------------------------------------
// Values
// ------------------------------------------------------------
static kuobj* ku_obj_alloc(kuvm* vm, size_t size, kuobjtype type) {
  kuobj* obj = (kuobj*)ku_alloc(vm, NULL, 0, size);
  obj->type = type;
  obj->next = (struct kuobj*)vm->objects;
  vm->objects = obj;
  return obj;
}

void ku_obj_free(kuvm* vm, kuobj* obj) {
  switch (obj->type) {
    case OBJ_FUNC: {
      kufunc *fn = (kufunc*)obj;
      ku_chunk_free(vm, &fn->chunk);
      FREE(vm, kufunc, obj);
      break;
    }
      
    case OBJ_CFUNC:
      FREE(vm, kucfunc, obj);
      break;
     
    case OBJ_CLOSURE:
      FREE(vm, kuclosure, obj);
      break;
      
  case OBJ_STR: {
    kustr* str = (kustr*)obj;
    ARRAY_FREE(vm, char, str->chars, str->len + 1);
    FREE(vm, kustr, obj);
    break;
  }
  }
}

// FNV-1a hashing function
static uint32_t ku_str_hash(const char* key, int len) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < len; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

static kustr* ku_str_alloc(kuvm* vm, char* chars, int len, uint32_t hash) {
  kustr* str = KALLOC_OBJ(vm, kustr, OBJ_STR);
  str->len = len;
  str->chars = chars;
  str->hash = hash;
  ku_map_set(vm, &vm->strings, str, NIL_VAL);
  return str;
}

bool ku_obj_istype(kuval v, kuobjtype ot) {
  return IS_OBJ(v) && AS_OBJ(v)->type == ot;
}

kustr* ku_str_copy(kuvm* vm, const char* chars, int len) {
  uint32_t hash = ku_str_hash(chars, len);

  kustr* interned = ku_map_find_str(vm, &vm->strings, chars, len, hash);
  if (interned != NULL) {
    return interned;
  }

  char* buff = KALLOC(vm, char, len + 1);
  memcpy(buff, chars, len);
  buff[len] = '\0';
  return ku_str_alloc(vm, buff, len, hash);
}


bool ku_val_eq(kuval v1, kuval v2) {
  if (v1.type != v2.type) {
    return false;
  }
  switch (v1.type) {
    case VAL_NIL: return true;
    case VAL_BOOL: return v1.as.bval == v2.as.bval;
    case VAL_NUM: return v1.as.dval == v2.as.dval;
    case VAL_OBJ: return AS_OBJ(v1) == AS_OBJ(v2);
    default: break;
  }
  return false;
}

static kustr* ku_str_take(kuvm* vm, char* buff, int len) {
  uint32_t hash = ku_str_hash(buff, len);
  kustr* interned = ku_map_find_str(vm, &vm->strings, buff, len, hash);
  if (interned != NULL) {
    ARRAY_FREE(vm, char, buff, len + 1);
    return interned;
  }

  return ku_str_alloc(vm, buff, len, hash);
}


static void ku_str_cat(kuvm* vm) {
  kustr *b = AS_STR(ku_pop(vm));
  kustr* a = AS_STR(ku_pop(vm));
  int len = a->len + b->len;
  char* buff = KALLOC(vm, char, len + 1);
  memcpy(buff, a->chars, a->len);
  memcpy(buff + a->len, b->chars, b->len);
  buff[len] = '\0';
  kustr* res = ku_str_take(vm, buff, len);
  ku_push(vm, OBJ_VAL(res));
}

// ------------------------------------------------------------
// Map / hash table
// ------------------------------------------------------------
void ku_map_init(kuvm* vm, kumap* map) {
  map->count = 0;
  map->capacity = 0;
  map->entries = NULL;
}

void ku_map_free(kuvm* vm, kumap* map) {
  ARRAY_FREE(vm, kuentry, map->entries, map->capacity);
  ku_map_init(vm, map);
}

#define MAP_MAX_LOAD 0.75

static kuentry* ku_map_find(kuvm* vm, kuentry* entries, int capacity, kustr* key) {
  uint32_t index = key->hash % capacity;
  kuentry* tombstone = NULL;

  for (;;) {
    kuentry* e = &entries[index];

    if (e->key == NULL) {
      if (IS_NIL(e->value)) { 
        // empty entry and have a tombstone ~> return the tombstone
        // otherwise return this entry. 
        // this allows reusing tombstone slots for added efficiency
        return tombstone != NULL ? tombstone : e;
      }
      else {
        // a tombstone has NULL key and BOOL(true) value, remember 
        // the first tombstone we found so we can reused it
        if (tombstone == NULL) {
          tombstone = e;
        }
      }
    } else if (e->key == key) {
      return e;
    }
    
    index = (index + 1) % capacity;
  }
}

static void ku_map_adjust(kuvm* vm, kumap* map, int capacity) {
  kuentry* entries = KALLOC(vm, kuentry, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  map->count = 0;
  for (int i = 0; i < map->capacity; i++) {
    kuentry* src = &map->entries[i];
    if (src->key == NULL) {
      continue;
    }

    kuentry* dest = ku_map_find(vm, entries, capacity, src->key);
    dest->key = src->key;
    dest->value = src->value;
    map->count++;
  }

  ARRAY_FREE(vm, kuentry, map->entries, map->capacity);
  map->entries = entries;
  map->capacity = capacity;
}

bool ku_map_set(kuvm* vm, kumap* map, kustr* key, kuval value) {
  if (map->count + 1 > map->capacity * MAP_MAX_LOAD) {
    int capacity = CAPACITY_GROW(map->capacity);
    ku_map_adjust(vm, map, capacity);
  }

  kuentry* e = ku_map_find(vm, map->entries, map->capacity, key);
  bool isnew = e->key == NULL;
  // we don't increase the count if we use a tombstone slot
  if (isnew && IS_NIL(e->value)) {
    map->count++;
  }
  e->key = key;
  e->value = value;
  return isnew;
}

void ku_map_copy(kuvm* vm, kumap* from, kumap* to) {
  for (int i = 0; i < from->capacity; i++) {
    kuentry* e = &from->entries[i];
    if (e->key != NULL) {
      ku_map_set(vm, to, e->key, e->value);
    }
  }
}

bool ku_map_get(kuvm* vm, kumap* map, kustr* key, kuval* value) {
  if (map->count == 0) {
    return false;
  }

  kuentry* e = ku_map_find(vm, map->entries, map->capacity, key);
  if (e->key == NULL) {
    return false;
  }

  *value = e->value;
  return true;
}

bool ku_map_del(kuvm* vm, kumap* map, kustr* key) {
  if (map->count == 0) {
    return false;
  }

  kuentry* e = ku_map_find(vm, map->entries, map->capacity, key);
  if (e->key == NULL) {
    return false;
  }
  e->key = NULL;
  e->value = BOOL_VAL(true);
  return true;
}

kustr* ku_map_find_str(kuvm* vm, kumap* map, const char* chars, int len, uint32_t hash) {
  if (map->count == 0) {
    return NULL;
  }

  uint32_t index = hash % map->capacity;
  for (;;) {
    kuentry* e = &map->entries[index];
    if (e->key == NULL) {
      if (IS_NIL(e->value)) {
        return NULL;    // empty non-tombstone
      }
    }
    else if (e->key->len == len && e->key->hash == hash &&
      memcmp(e->key->chars, chars, len) == 0) {
      return e->key;
    }
    index = (index + 1) % map->capacity;
  }
}

// ------------------------------------------------------------
// Scanner
// ------------------------------------------------------------
static char ku_lex_advance(kuvm *vm) {
  vm->scanner.curr++;
  return vm->scanner.curr[-1];
}


void ku_lex_init(kuvm *vm, const char *source) {
  vm->scanner.start = source;
  vm->scanner.curr = source;
  vm->scanner.line = 1;
}

static bool ku_lex_is_end(kuvm *vm) {
  return (*(vm->scanner.curr) == '\0');
}

static bool ku_isdigit(char c) {
  return (c >= '0' && c <= '9');
}

static bool ku_isalpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
         (c == '_');
}

static char ku_lex_peeknext(kuvm *vm) {
  if (ku_lex_is_end(vm)) return '\0';
  return vm->scanner.curr[1];
}

static char ku_lex_peek(kuvm *vm) {
  return *vm->scanner.curr;
}

static void ku_lex_skip_space(kuvm *vm) {
  while (true) {
    char c = ku_lex_peek(vm);
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        ku_lex_advance(vm);
        break;
      case '\n':
        vm->scanner.line++;
        ku_lex_advance(vm);
        break;
      case '/':
        if (ku_lex_peeknext(vm) == '/') {
          while (ku_lex_peek(vm) != '\n' && !ku_lex_is_end(vm))
            ku_lex_advance(vm);
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

static kutok ku_lex_make(kuvm *vm, kutoktype type) {
  kutok token;
  token.type = type;
  token.start = vm->scanner.start;
  token.len = (int) (vm->scanner.curr - vm->scanner.start);
  token.line = vm->scanner.line;
  return token;
}

static kutok ku_lex_err(kuvm *vm, const char *msg) {
  kutok token;
  token.type = TOK_ERR;
  token.start = msg;
  token.len = (int)strlen(msg);
  token.line = vm->scanner.line;
  return token;
}

static bool ku_lex_match(kuvm *vm, char expected) {
  if (ku_lex_is_end(vm)) return false;
  if (*vm->scanner.curr != expected) return false;
  vm->scanner.curr++;
  return true;
}

static kutok ku_lex_number(kuvm *vm) {
  while(ku_isdigit(ku_lex_peek(vm))) ku_lex_advance(vm);
  
  if (ku_lex_peek(vm) == '.' && ku_isdigit(ku_lex_peeknext(vm))) {
    ku_lex_advance(vm);
  }
  while(ku_isdigit(ku_lex_peek(vm))) ku_lex_advance(vm);
  return ku_lex_make(vm, TOK_NUM);
}

static kutok ku_lex_string(kuvm *vm) {
  while(ku_lex_peek(vm) != '"' && !ku_lex_is_end(vm)) {
    if (ku_lex_peek(vm) == '\n') vm->scanner.line++;
    ku_lex_advance(vm);
  }
  if (ku_lex_is_end(vm)) return ku_lex_err(vm, "unterminated string");
  ku_lex_advance(vm);
  return ku_lex_make(vm, TOK_STR);
}

static kutoktype ku_lex_keyword(kuvm *vm, int start, int len,
                        const char *rest, kutoktype type) {
  if (vm->scanner.curr - vm->scanner.start == start + len &&
      memcmp(vm->scanner.start + start, rest, len) == 0) {
    return type;
  }
  return TOK_IDENT;
}

static kutoktype ku_lex_identity_type(kuvm *vm) {
  switch(vm->scanner.start[0]) {
    case 'a': return ku_lex_keyword(vm, 1,2,"nd", TOK_AND);
    case 'c': return ku_lex_keyword(vm, 1,4,"lass", TOK_CLASS);
    case 'e': return ku_lex_keyword(vm, 1,3,"lse", TOK_ELSE);
    case 'f':
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch (vm->scanner.start[1]) {
          case 'a': return ku_lex_keyword(vm, 2, 3, "lse", TOK_FALSE);
          case 'o': return ku_lex_keyword(vm, 2, 1, "r", TOK_FOR);
          case 'u': return ku_lex_keyword(vm, 2, 1, "n", TOK_FUN);
        }
      }
    case 'i': return ku_lex_keyword(vm, 1,1,"f", TOK_IF);
    case 'n': return ku_lex_keyword(vm, 1,2,"il", TOK_NIL);
    case 'o': return ku_lex_keyword(vm, 1,1,"r", TOK_OR);
    case 'p': return ku_lex_keyword(vm, 1,4,"rint", TOK_PRINT);
    case 'r': return ku_lex_keyword(vm, 1,5,"eturn", TOK_RETURN);
    case 's': return ku_lex_keyword(vm, 1,4,"uper", TOK_SUPER);
    case 't':
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch(vm->scanner.start[1]) {
          case 'h': return ku_lex_keyword(vm, 2, 2, "is", TOK_THIS);
          case 'r': return ku_lex_keyword(vm, 2, 2, "ue", TOK_TRUE);
        }
      }
    case 'v': return ku_lex_keyword(vm, 1,2,"ar", TOK_VAR);
    case 'w': return ku_lex_keyword(vm, 1,4,"hile", TOK_WHILE);
  }
  return TOK_IDENT;
}

static kutok ku_lex_identifier(kuvm *vm) {
  while (ku_isalpha(ku_lex_peek(vm)) || ku_isdigit(ku_lex_peek(vm))) {
    ku_lex_advance(vm);
  }
  return ku_lex_make(vm, ku_lex_identity_type(vm));
}

kutok ku_lex_scan(kuvm *vm) {
  ku_lex_skip_space(vm);
  vm->scanner.start = vm->scanner.curr;
  
  if (ku_lex_is_end(vm)) {
    return ku_lex_make(vm, TOK_EOF);
  }
  
  char c = ku_lex_advance(vm);
  if (ku_isalpha(c)) return ku_lex_identifier(vm);
  if (ku_isdigit(c)) return ku_lex_number(vm);
  switch (c) {
    case '(': return ku_lex_make(vm, TOK_LPAR);
    case ')': return ku_lex_make(vm, TOK_RPAR);
    case '{': return ku_lex_make(vm, TOK_LBRACE);
    case '}': return ku_lex_make(vm, TOK_RBRACE);
    case ';': return ku_lex_make(vm, TOK_SEMI);
    case ',': return ku_lex_make(vm, TOK_COMMA);
    case '.': return ku_lex_make(vm, TOK_DOT);
    case '+': return ku_lex_make(vm, TOK_PLUS);
    case '-': return ku_lex_make(vm, TOK_MINUS);
    case '*': return ku_lex_make(vm, TOK_STAR);
    case '/':
      return ku_lex_make(vm, TOK_SLASH);
    case '!':
      return ku_lex_make(vm, ku_lex_match(vm, '=') ? TOK_NE : TOK_BANG);
    case '=':
      return ku_lex_make(vm, ku_lex_match(vm, '=') ? TOK_EQEQ : TOK_EQ);
    case '<':
      return ku_lex_make(vm, ku_lex_match(vm, '=') ? TOK_LE : TOK_LT);
    case '>':
      return ku_lex_make(vm, ku_lex_match(vm, '=') ? TOK_GE : TOK_GT);
    case '"':
      return ku_lex_string(vm);
  }
  return ku_lex_err(vm, "unexpected character");
}

void ku_lex_print_all(kuvm *vm) {
  int line = -1;
  
  while (true) {
    kutok token = ku_lex_scan(vm);
    if (token.line != line) {
      ku_printf(vm, "%4d ", token.line);
    } else {
      ku_printf(vm, "  |  ");
    }
    ku_printf(vm, "%2d '%.*s'\n", token.type, token.len, token.start);
    
    if (token.type == TOK_EOF) {
      break;
    }
  }
}

// ------------------------------------------------------------
// Parser
// ------------------------------------------------------------
static void ku_parse_err_at(kuvm *vm, kutok *tok, const char *msg) {
  char out[1024];
  char buff[1024];

  if (vm->parser.panic) return;
  vm->parser.panic = true;
  
  sprintf(out, "[line %d] error", tok->line);
  
  if (tok->type == TOK_EOF) {
    sprintf(buff, "%s at end ", out);
  } else if (tok->type == TOK_ERR) {
    // nothing
  } else {
    sprintf(buff, "%s at '%.*s'", out, tok->len, tok->start);
  }
  
  strcat(buff, msg);
  strcat(buff, "\n");
  vm->parser.err = true;
}

static void ku_parse_err(kuvm *vm, const char *msg) {
  ku_parse_err_at(vm, &vm->parser.curr, msg);
}

static void ku_parse_advance(kuvm *vm) {
  vm->parser.prev = vm->parser.curr;
  
  while (true) {
    vm->parser.curr = ku_lex_scan(vm);
    if (vm->parser.curr.type != TOK_ERR) break;
    ku_parse_err(vm, vm->parser.curr.start);
  }
}

static void ku_parse_consume(kuvm *vm, kutoktype type, const char *msg) {
  if (vm->parser.curr.type == type) {
    ku_parse_advance(vm);
    return;
  }
  ku_parse_err(vm, msg);
}

kuchunk *ku_chunk(kuvm *vm) {
  return &vm->compiler->function->chunk;
}

static void ku_parse_emit_byte(kuvm *vm, uint8_t byte) {
  ku_chunk_write(vm, ku_chunk(vm), byte, vm->parser.prev.line);
}

static void ku_emit_ret(kuvm *vm) {
  ku_parse_emit_byte(vm, OP_NIL);
  ku_parse_emit_byte(vm, OP_RET);
}

static kufunc *ku_parse_end(kuvm *vm) {
  ku_emit_ret(vm);
  kufunc *fn = vm->compiler->function;
  // @todo: supposed to be vm->compiler = vm->compiler->enclosing
  //        but this crashes if we have a runtime error after
  //        a successful parse since the root compiler is gone
  vm->compiler = vm->compiler->enclosing ? vm->compiler->enclosing : vm->compiler;
  return fn;
}

static void ku_parse_emit_bytes(kuvm *vm, uint8_t b1, uint8_t b2) {
  ku_parse_emit_byte(vm, b1);
  ku_parse_emit_byte(vm, b2);
}


static uint8_t ku_parse_make_const(kuvm *vm, kuval val) {
  int cons = ku_chunk_add_const(vm, ku_chunk(vm), val);
  if (cons > UINT8_MAX) {
    ku_parse_err(vm, "out of constant space");
    return 0;
  }
  return (uint8_t)cons;
}
static void ku_parse_emit_const(kuvm *vm, kuval val) {
  ku_parse_emit_bytes(vm, OP_CONST, ku_parse_make_const(vm, val));
}

typedef enum {
  P_NONE,
  P_ASSIGN,     // =
  P_OR,         // or
  P_AND,        // and
  P_EQ,         // == and !=
  P_COMP,       // < > <= >=
  P_TERM,       // + -
  P_FACTOR,     // * /
  P_UNARY,      // ! -
  P_CALL,       // . ()
  P_PRIMARY
} kup_precedence;

typedef void (*ku_parse_func)(kuvm *, bool lhs);

typedef struct {
  ku_parse_func prefix;
  ku_parse_func infix;
  kup_precedence precedence;
} ku_parse_rule;

static ku_parse_rule *ku_parse_get_rule(kuvm *vm, kutoktype optype);

static bool ku_parse_checktype(kuvm* vm, kutoktype type) {
  return vm->parser.curr.type == type;
}

static bool ku_parse_match(kuvm* vm, kutoktype type) {
  if (!ku_parse_checktype(vm, type)) {
    return false;
  }
  ku_parse_advance(vm);
  return true;
}

static void ku_parse_process(kuvm *vm, kup_precedence prec) {
  ku_parse_advance(vm);
  ku_parse_func prefix = ku_parse_get_rule(vm, vm->parser.prev.type)->prefix;
  if (prefix == NULL) {
    ku_parse_err(vm, "expected expression");
    return;
  }

  bool lhs = prec <= P_ASSIGN;
  prefix(vm, lhs);
  
  while (prec <= ku_parse_get_rule(vm, vm->parser.curr.type)->precedence) {
    ku_parse_advance(vm);
    ku_parse_func infix = ku_parse_get_rule(vm, vm->parser.prev.type)->infix;
    infix(vm, lhs);
  }

  if (lhs && ku_parse_match(vm, TOK_EQ)) {
    ku_parse_err(vm, "invalid assignment target");
  }
}


static void ku_parse_literal(kuvm *vm, bool lhs) {
  switch (vm->parser.prev.type) {
    case TOK_FALSE: ku_parse_emit_byte(vm, OP_FALSE); break;
    case TOK_TRUE: ku_parse_emit_byte(vm, OP_TRUE); break;
    case TOK_NIL: ku_parse_emit_byte(vm, OP_NIL); break;
    default: return; // unreachable
  }
}

static void ku_parse_string(kuvm* vm, bool lhs) {
  ku_parse_emit_const(vm, OBJ_VAL(ku_str_copy(vm, 
            vm->parser.prev.start + 1,
            vm->parser.prev.len - 2)));
}

static void ku_parse_number(kuvm *vm, bool lhs) {
  double val = strtod(vm->parser.prev.start, NULL);
  ku_parse_emit_const(vm, NUM_VAL(val));
}

static void ku_parse_expression(kuvm *vm) {
  ku_parse_process(vm, P_ASSIGN);
}

static uint8_t ku_arglist(kuvm *vm) {
  uint8_t argc = 0;
  if (!ku_parse_checktype(vm, TOK_RPAR)) {
    do {
      ku_parse_expression(vm);
      if (argc > vm->max_params) {
        ku_parse_err(vm, "too many parameters");
      }
      argc++;
    } while (ku_parse_match(vm, TOK_COMMA));
  }
  
  ku_parse_consume(vm, TOK_RPAR, "')' expected");
  return argc;
}

static void ku_call(kuvm *vm, bool lhs) {
  uint8_t argc = ku_arglist(vm);
  ku_parse_emit_bytes(vm, OP_CALL, argc);
}

static void ku_parse_grouping(kuvm *vm, bool lhs) {
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_RPAR, "')' expected");
}

static void ku_parse_unary(kuvm *vm, bool lhs) {
  kutoktype optype = vm->parser.prev.type;
  
  ku_parse_process(vm, P_UNARY);
  
  switch(optype) {
    case TOK_MINUS: ku_parse_emit_byte(vm, OP_NEG); break;
    case TOK_BANG: ku_parse_emit_byte(vm, OP_NOT); break;
    default: return;
  }
}

static void ku_parse_expression_statement(kuvm* vm) {
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_SEMI, "; expected");
  ku_parse_emit_byte(vm, OP_POP);
}

static void ku_parse_print_statement(kuvm* vm) {
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_SEMI, "; expected");
  ku_parse_emit_byte(vm, OP_PRINT);
}

static void ku_return(kuvm *vm) {
  if (vm->compiler->type == FUNC_MAIN) {
    ku_parse_err(vm, "can't return from top-level");
  }
  
  if (ku_parse_match(vm, TOK_SEMI)) {
    ku_emit_ret(vm);
  } else {
    ku_parse_expression(vm);
    ku_parse_consume(vm, TOK_SEMI, "';' expected");
    ku_parse_emit_byte(vm, OP_RET);
  }
}

static void ku_parse_statement(kuvm* vm) {
  if (ku_parse_match(vm, TOK_PRINT)) {
    ku_parse_print_statement(vm);
  } else if (ku_parse_match(vm, TOK_IF)) {
    ku_ifstatement(vm);
  } else if (ku_parse_match(vm, TOK_RETURN)) {
    ku_return(vm);
  } else if (ku_parse_match(vm, TOK_WHILE)) {
    ku_whilestatement(vm);
  } else if (ku_parse_match(vm, TOK_FOR)) {
    ku_forstatement(vm);
  } else if (ku_parse_match(vm, TOK_LBRACE)) {
    ku_beginscope(vm);
    ku_block(vm);
    ku_endscope(vm);
  } else {
    ku_parse_expression_statement(vm);
  }
}

static void ku_parse_skip(kuvm* vm) {
  vm->parser.panic = false;

  while (vm->parser.curr.type != TOK_EOF) {
    if (vm->parser.prev.type == TOK_SEMI) {
      return;
    }

    switch (vm->parser.curr.type) {
    case TOK_CLASS:
    case TOK_FUN:
    case TOK_VAR:
    case TOK_FOR:
    case TOK_IF:
    case TOK_WHILE:
    case TOK_PRINT:
    case TOK_RETURN:
      return;
    default:
      ;
    }

    ku_parse_advance(vm);
  }
}

static uint8_t ku_parse_identifier_const(kuvm* vm, kutok* name) {
  return ku_parse_make_const(vm, OBJ_VAL(ku_str_copy(vm, name->start, name->len)));
}

static uint8_t ku_parse_var(kuvm* vm, const char* msg) {
  ku_parse_consume(vm, TOK_IDENT, msg);
  ku_vardecl(vm);
  if (vm->compiler->depth > 0) {
    return 0;
  }
  return ku_parse_identifier_const(vm, &vm->parser.prev);
}

static void ku_parse_var_def(kuvm* vm, uint8_t index) {
  if (vm->compiler->depth > 0) {
    ku_markinit(vm);
    return;
  }
  ku_parse_emit_bytes(vm, OP_DEF_GLOBAL, index);
}

static void ku_parse_var_decl(kuvm* vm) {
  uint8_t g = ku_parse_var(vm, "name expected");
  if (ku_parse_match(vm, TOK_EQ)) {
    ku_parse_expression(vm);
  }
  else {
    ku_parse_emit_byte(vm, OP_NIL);
  }

  ku_parse_consume(vm, TOK_SEMI, "; expected");
  ku_parse_var_def(vm, g);
}

static void ku_function(kuvm *vm, kufunctype type) {
  kucompiler compiler;
  ku_compiler_init(vm, &compiler, type);
  ku_beginscope(vm);
  ku_parse_consume(vm, TOK_LPAR, "'(' expected after function name");
  if (!ku_parse_checktype(vm, TOK_RPAR)) {
    do {
      vm->compiler->function->arity++;
      if (vm->compiler->function->arity > vm->max_params) {
        ku_parse_err(vm, "too many params");
      }
      
      uint8_t constant = ku_parse_var(vm, "expected parameter name");
      ku_parse_var_def(vm, constant);
    } while(ku_parse_match(vm, TOK_COMMA));
  }
  ku_parse_consume(vm, TOK_RPAR, "')' expected after parameters");
  ku_parse_consume(vm, TOK_LBRACE, "'{' expected before function body");
  ku_block(vm);
  kufunc *fn = ku_parse_end(vm);
  ku_parse_emit_bytes(vm, OP_CLOSURE, ku_parse_make_const(vm, OBJ_VAL(fn)));
//  ku_parse_emit_bytes(vm, OP_CONST, ku_parse_make_const(vm, OBJ_VAL(fn)));
}

static void ku_func_decl(kuvm *vm) {
  uint8_t global = ku_parse_var(vm, "function name expected");
  ku_markinit(vm);
  ku_function(vm, FUNC_STD);
  ku_parse_var_def(vm, global);
}

static void ku_parse_declaration(kuvm* vm) {
  if (ku_parse_match(vm, TOK_FUN)) {
    ku_func_decl(vm);
  } else if (ku_parse_match(vm, TOK_VAR)) {
    ku_parse_var_decl(vm);
  }
  else {
    ku_parse_statement(vm);
  }
  if (vm->parser.panic) {
    ku_parse_skip(vm);
  }
}

static void ku_named_var(kuvm* vm, kutok name, bool lhs) {
  int arg = ku_resolvelocal(vm, &name);
  uint8_t set, get;
  
  if (arg != -1) {
    get = OP_GET_LOCAL;
    set = OP_SET_LOCAL;
  } else {
    arg = ku_parse_identifier_const(vm, &name);
    get = OP_GET_GLOBAL;
    set = OP_SET_GLOBAL;
  }
  if (lhs && ku_parse_match(vm, TOK_EQ)) {
    ku_parse_expression(vm);
    ku_parse_emit_bytes(vm, set, (uint8_t)arg);
  }
  else {
    ku_parse_emit_bytes(vm, get, (uint8_t)arg);
  }
}

static void ku_parse_variable(kuvm* vm, bool lhs) {
  ku_named_var(vm, vm->parser.prev, lhs);
}

static void ku_parse_binary(kuvm *vm, bool lhs) {
  kutoktype optype = vm->parser.prev.type;
  ku_parse_rule *rule = ku_parse_get_rule(vm, optype);
  ku_parse_process(vm, (kup_precedence)(rule->precedence + 1));
  
  switch (optype) {
    case TOK_PLUS: ku_parse_emit_byte(vm, OP_ADD); break;
    case TOK_MINUS: ku_parse_emit_byte(vm, OP_SUB); break;
    case TOK_STAR: ku_parse_emit_byte(vm, OP_MUL); break;
    case TOK_SLASH: ku_parse_emit_byte(vm, OP_DIV); break;
    case TOK_NE: ku_parse_emit_bytes(vm, OP_EQ, OP_NOT); break;
    case TOK_EQEQ: ku_parse_emit_byte(vm, OP_EQ); break;
    case TOK_GT: ku_parse_emit_byte(vm, OP_GT); break;
    case TOK_GE: ku_parse_emit_bytes(vm, OP_LT, OP_NOT); break;
    case TOK_LT: ku_parse_emit_byte(vm, OP_LT); break;
    case TOK_LE: ku_parse_emit_bytes(vm, OP_GT, OP_NOT); break;
    default: return;
  }
}

ku_parse_rule rules[] = {
  [TOK_LPAR] =      { ku_parse_grouping,   ku_call,     P_CALL },
  [TOK_RPAR] =      { NULL,        NULL,     P_NONE },
  [TOK_LBRACE] =    { NULL,        NULL,     P_NONE },
  [TOK_RBRACE] =    { NULL,        NULL,     P_NONE },
  [TOK_COMMA] =     { NULL,        NULL,     P_NONE },
  [TOK_DOT] =       { NULL,        NULL,     P_NONE },
  [TOK_MINUS] =     { ku_parse_unary,      ku_parse_binary,  P_TERM },
  [TOK_PLUS] =      { NULL,        ku_parse_binary,  P_TERM },
  [TOK_SEMI] =      { NULL,        NULL,     P_NONE },
  [TOK_SLASH] =     { NULL,        ku_parse_binary,  P_FACTOR },
  [TOK_STAR] =      { NULL,        ku_parse_binary,  P_FACTOR },
  [TOK_BANG] =      { ku_parse_unary,      NULL,     P_NONE },
  [TOK_NE] =        { NULL,        ku_parse_binary,  P_EQ },
  [TOK_EQ] =        { NULL,        NULL,     P_NONE },
  [TOK_EQEQ] =      { NULL,        ku_parse_binary,  P_EQ },
  [TOK_GT] =        { NULL,        ku_parse_binary,  P_COMP },
  [TOK_GE] =        { NULL,        ku_parse_binary,  P_COMP },
  [TOK_LT] =        { NULL,        ku_parse_binary,  P_COMP },
  [TOK_LE] =        { NULL,        ku_parse_binary,  P_COMP },
  [TOK_IDENT] =     { ku_parse_variable,        NULL,     P_NONE },
  [TOK_STR] =       { ku_parse_string,     NULL,     P_NONE },
  [TOK_NUM] =       { ku_parse_number,     NULL,     P_NONE },
  [TOK_AND] =       { NULL,        ku_parse_and,     P_AND },
  [TOK_CLASS] =     { NULL,        NULL,     P_NONE },
  [TOK_ELSE] =      { NULL,        NULL,     P_NONE },
  [TOK_FALSE] =     { ku_parse_literal,    NULL,     P_NONE },
  [TOK_FOR] =       { NULL,        NULL,     P_NONE },
  [TOK_FUN] =       { NULL,        NULL,     P_NONE },
  [TOK_IF] =        { NULL,        NULL,     P_NONE },
  [TOK_NIL] =       { ku_parse_literal,    NULL,     P_NONE },
  [TOK_OR] =        { NULL,        ku_parse_or,     P_OR },
  [TOK_PRINT] =     { NULL,        NULL,     P_NONE },
  [TOK_SUPER] =     { NULL,        NULL,     P_NONE },
  [TOK_THIS] =      { NULL,        NULL,     P_NONE },
  [TOK_TRUE] =      { ku_parse_literal,    NULL,     P_NONE },
  [TOK_VAR] =       { NULL,        NULL,     P_NONE },
  [TOK_WHILE] =     { NULL,        NULL,     P_NONE },
  [TOK_ERR] =       { NULL,        NULL,     P_NONE },
  [TOK_EOF] =       { NULL,        NULL,     P_NONE },
};

static ku_parse_rule *ku_parse_get_rule(kuvm *vm, kutoktype optype) {
  return &rules[optype];
}

// ------------------------------------------------------------
// Virtual machine
// ------------------------------------------------------------
void ku_reset_stack(kuvm *vm) {
  vm->sp = vm->stack;
  vm->framecount = 0;
}

void ku_push(kuvm *vm, kuval val) {
  *(vm->sp) = val;
  vm->sp++;
}

kuval ku_pop(kuvm *vm) {
  vm->sp--;
  return *(vm->sp);
}

static bool ku_is_falsy(kuval v) {
  return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v));
}

kuval ku_peek_stack(kuvm *vm, int distance) {
  return vm->sp[-1 - distance];
}

kuvm *ku_new(void) {
  kuvm *vm = malloc(sizeof(kuvm));
  if (!vm) {
    return NULL;
  }
  vm->allocated = sizeof(kuvm);
  vm->max_params = 255;
  vm->flags = 0;
  if (!vm) {
    return NULL;
  }

  vm->freed = 0;
  vm->stop = false;
  vm->objects = NULL;
  vm->compiler = NULL;
  ku_map_init(vm, &vm->strings);
  ku_map_init(vm, &vm->globals);
  ku_reset_stack(vm);
  return vm;
}

void ku_print_mem(kuvm *vm) {
  ku_printf(vm, "allocated: %ld, freed: %ld, delta: %ld\n",
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
}

static void ku_free_objects(kuvm* vm) {
  kuobj* obj = vm->objects;
  while (obj != NULL) {
    kuobj* next = (kuobj*)obj->next;
    ku_obj_free(vm, obj);
    obj = next;
  }
}

void ku_free(kuvm *vm) {
  ku_free_objects(vm);
  ku_map_free(vm, &vm->strings);
  ku_map_free(vm, &vm->globals);
  vm->freed += sizeof(kuvm);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

void ku_print_stack(kuvm *vm) {
  ku_printf(vm, " [");
  for (kuval* vp = vm->stack; vp < vm->sp; vp++) {
    ku_print_val(vm, *vp);
    if (vp < vm->sp - 1) {
      ku_printf(vm, ",");
    }
  }
  ku_printf(vm, "]");
  ku_printf(vm, "\n");
}


static void ku_err(kuvm *vm, const char *fmt, ...) {
  va_list args;
  char out[1024];

  va_start(args, fmt);
  vsprintf(out, fmt, args);
  va_end(args);
  
  ku_printf(vm, "error %s\n", out);
  for (int f = vm->framecount - 1; f >= 0; f--) {
    kuframe *frame = &vm->frames[f];
    kufunc *fn = frame->closure->func;
    size_t inst = frame->ip - fn->chunk.code - 1;
    ku_printf(vm, "[line %d] in ", fn->chunk.lines[inst]);
    if (fn->name == NULL) {
      ku_printf(vm, "__main__\n");
    } else {
      ku_printf(vm, "%s()\n", fn->name->chars);
    }
  }
  
  ku_reset_stack(vm);
}

static bool ku_docall(kuvm *vm, kuclosure *cl, int argc) {
  if (argc != cl->func->arity) {
    ku_err(vm, "%d expected got %d", cl->func->arity, argc);
    return false;
  }
  
  if (vm->framecount == FRAMES_MAX) { // @todo: make vm field
    ku_err(vm, "stack overflow");
    return false;
  }
  
  kuframe *frame = &vm->frames[vm->framecount++];
  frame->closure = cl;
  frame->ip = cl->func->chunk.code;
  frame->bp = vm->sp - argc - 1;
  return true;
}

static bool ku_callvalue(kuvm *vm, kuval callee, int argc) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_CFUNC: {
        cfunc cf = AS_CFUNC(callee);
        kuval res = cf(vm, argc, vm->sp - argc);
        vm->sp -= argc + 1;
        ku_push(vm, res);
        return true;
      }
      case OBJ_CLOSURE:
        return ku_docall(vm, AS_CLOSURE(callee), argc);
      case OBJ_FUNC: // not allowed anymore
      default:
        break;
    }
  }
  ku_err(vm, "not callable");
  return false;
}

kures ku_run(kuvm *vm) {
  kuframe *frame = &vm->frames[vm->framecount - 1];
  
  kures res = KVM_CONT;
  while (res == KVM_CONT) {
    uint8_t op;

    if (vm->flags & KVM_F_TRACE) {
     ku_print_op(vm, ku_chunk(vm), (int) (frame->ip - ku_chunk(vm)->code));
    }


    switch(op = BYTE_READ(vm)) {
      case OP_NOP:
        break;
      case OP_CALL: {
        int argc = BYTE_READ(vm);
        if (!ku_callvalue(vm, ku_peek_stack(vm, argc), argc)) {
          return KVM_ERR_RUNTIME;
        }
        frame = &vm->frames[vm->framecount - 1];
        break;
      }
      case OP_CLOSURE: {
        kufunc *fn = AS_FUNC(CONST_READ(vm));
        kuclosure *cl = ku_closure_new(vm, fn);
        ku_push(vm, OBJ_VAL(cl));
        break;
      }
      case OP_NIL:
        ku_push(vm, NIL_VAL);
        break;
      case OP_TRUE:
        ku_push(vm, BOOL_VAL(true));
        break;
      case OP_FALSE:
        ku_push(vm, BOOL_VAL(false));
        break;
      case OP_EQ: {
        kuval b = ku_pop(vm);
        kuval a = ku_pop(vm);
        ku_push(vm, BOOL_VAL(ku_val_eq(a, b)));
        break;
      }
      case OP_RET: {
        kuval v = ku_pop(vm);
        vm->framecount--;
        if (vm->framecount == 0) {
          ku_pop(vm);
          res = KVM_OK;
          break;
        }
        vm->sp = frame->bp;
        ku_push(vm, v);
        frame = &vm->frames[vm->framecount - 1];
        break;
      }
      case OP_CONST: {
        kuval con = CONST_READ(vm);
        ku_push(vm, con);
        break;
      }
      case OP_NEG: {
        if (! IS_NUM(ku_peek_stack(vm, 0))) {
          ku_err(vm, "number expected" );
          return KVM_ERR_RUNTIME;
        }
        kuval v = ku_pop(vm);
        double dv = AS_NUM(v);
        kuval nv = NUM_VAL(-dv);
        ku_push(vm, nv);
        break;
      }
      case OP_PRINT: {
        ku_print_val(vm, ku_pop(vm));
        ku_printf(vm, "\n");
        break;
      }
      
      case OP_POP: 
        ku_pop(vm);
        break;

      case OP_GET_GLOBAL: {
        kustr* name = READ_STRING(vm);
        kuval value;

        if (!ku_map_get(vm, &vm->globals, name, &value)) {
          ku_err(vm, "undefined variable %s", name->chars);
          return KVM_ERR_RUNTIME;
        }
        ku_push(vm, value);
        break;
      }

      case OP_DEF_GLOBAL: {
        kustr* name = READ_STRING(vm);
        ku_map_set(vm, &vm->globals, name, ku_peek_stack(vm, 0));
        ku_pop(vm);
        break;
      }

      case OP_SET_GLOBAL: {
        kustr* name = READ_STRING(vm);
        if (ku_map_set(vm, &vm->globals, name, ku_peek_stack(vm, 0))) {
          ku_map_del(vm, &vm->globals, name);
          ku_err(vm, "undefined variable %s", name->chars);
          return KVM_ERR_RUNTIME;
        }
        break;
      }

      case OP_GET_LOCAL: {
        uint8_t slot = BYTE_READ(vm);
        ku_push(vm, frame->bp[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = BYTE_READ(vm);
        frame->bp[slot] = ku_peek_stack(vm, 0);
        break;
      }
        
      case OP_ADD: {
        if (IS_STR(ku_peek_stack(vm, 0)) && IS_STR(ku_peek_stack(vm, 1))) {
          ku_str_cat(vm);
        }
        else if (IS_NUM(ku_peek_stack(vm, 0)) && IS_NUM(ku_peek_stack(vm, 1))) {
          double a = AS_NUM(ku_pop(vm));
          double b = AS_NUM(ku_pop(vm));
          ku_push(vm, NUM_VAL(a + b));
        }
        else {
          ku_err(vm, "numbers expected");
          return KVM_ERR_RUNTIME;
        }
        break;
      }
      case OP_SUB: BIN_OP(vm,NUM_VAL, -); break;
      case OP_MUL: BIN_OP(vm,NUM_VAL, *); break;
      case OP_DIV: BIN_OP(vm,NUM_VAL, /); break;
      case OP_GT: BIN_OP(vm, BOOL_VAL, >); break;
      case OP_LT: BIN_OP(vm,BOOL_VAL, <); break;
      case OP_NOT:
        ku_push(vm, BOOL_VAL(ku_is_falsy(ku_pop(vm))));
        break;
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT(vm);
        if (ku_is_falsy(ku_peek_stack(vm, 0))) {
          frame->ip += offset;
        }
        break;
      }
        
      case OP_JUMP: {
        uint16_t offset = READ_SHORT(vm);
        frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT(vm);
        frame->ip -= offset;
        break;
      }
    }
    if (vm->flags & KVM_F_TRACE && vm->flags & KVM_F_STACK) {
     ku_print_stack(vm);
    } else {
      if (vm->flags & KVM_F_TRACE) {
        ku_printf(vm, "\n");
      }
    }

  }
  return KVM_OK;
}

static kufunc *ku_compile(kuvm *vm, char *source) {
  kucompiler compiler;
  ku_compiler_init(vm, &compiler, FUNC_MAIN);
  ku_lex_init(vm, source);
  vm->parser.err = false;
  vm->parser.panic = false;
  ku_parse_advance(vm);
  while (!ku_parse_match(vm, TOK_EOF)) {
    ku_parse_declaration(vm);
  }
  kufunc *fn = ku_parse_end(vm);
  
  if (vm->flags & KVM_F_DISASM) {
    ku_print_chunk(vm, ku_chunk(vm), "code");
  }

  return vm->parser.err ? NULL : fn;
}

kures ku_exec(kuvm *vm, char *source) {
  kufunc *fn = ku_compile(vm, source);
  if (fn == NULL) {
    return KVM_ERR_SYNTAX;
  }

  if (vm->flags & KVM_F_LIST) {
    ku_print_chunk(vm, &fn->chunk, fn->name ? fn->name->chars : "__main__");
  }
    
  if (vm->flags & KVM_F_NOEXEC) {
    return KVM_OK;
  }
  
  ku_push(vm, OBJ_VAL(fn));
  kuclosure *closure = ku_closure_new(vm, fn);
  ku_pop(vm);
  ku_push(vm, OBJ_VAL(closure));
  ku_docall(vm, closure, 0);
  return ku_run(vm);
}

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *ku_alloc(kuvm *vm, void *ptr, size_t oldsize, size_t nsize) {
  
  if (vm->flags & KVM_F_TRACEMEM) {
    ku_printf(vm, "malloc %d -> %d\n", (int)oldsize, (int)nsize);
  }
  
  vm->allocated += nsize;
  vm->freed += oldsize;
  
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  return realloc(ptr, nsize);
}

// ------------------------------------------------------------
// Chunks
// ------------------------------------------------------------
void ku_chunk_init(kuvm *vm, kuchunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  ku_arr_init(vm, &chunk->constants);
}

void ku_chunk_write(kuvm *vm, kuchunk *chunk, uint8_t byte, int line) {
  if (chunk->capacity < chunk->count + 1) {
    int cap = chunk->capacity;
    chunk->capacity = CAPACITY_GROW(cap);
    chunk->code = ARRAY_GROW(vm, uint8_t, chunk->code, cap, chunk->capacity);
    chunk->lines = ARRAY_GROW(vm, int, chunk->lines, cap, chunk->capacity);
    assert(chunk->code != NULL);
  }
  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  chunk->count++;
}

void ku_chunk_free(kuvm *vm, kuchunk *chunk) {
  ARRAY_FREE(vm, uint8_t, chunk->code, chunk->capacity);
  ARRAY_FREE(vm, int, chunk->lines, chunk->capacity);
  ARRAY_FREE(vm, kuval, chunk->constants.values, chunk->constants.capacity);
}

int ku_chunk_add_const(kuvm *vm, kuchunk *chunk, kuval value) {
  ku_arr_write(vm, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
static void ku_print_obj(kuvm* vm, kuval val) {
  switch (OBJ_TYPE(val)) {
    case OBJ_FUNC:
      ku_print_func(vm, AS_FUNC(val));
      break;
    case OBJ_CFUNC:
      ku_printf(vm, "<cfunc>");
      break;
    case OBJ_CLOSURE:
      ku_print_func(vm, AS_CLOSURE(val)->func);
      break;
  case OBJ_STR:
    ku_printf(vm, "%s", AS_CSTR(val));
    break;
  }
}

void ku_print_val(kuvm *vm, kuval value) {
  switch (value.type) {
    case VAL_BOOL:
    ku_printf(vm, "%s", (value.as.bval) ? "true": "false");
      break;
    case VAL_NIL:
      ku_printf(vm, "nil");
      break;;
    case VAL_NUM:
      ku_printf(vm, "%g", value.as.dval);
      break;
    case VAL_OBJ:
      ku_print_obj(vm, value);
      break;
  }
}

void ku_arr_init(kuvm* vm, kuarr *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void ku_arr_write(kuvm* vm, kuarr *array, kuval value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = CAPACITY_GROW(old);
    array->values = ARRAY_GROW(vm, kuval, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void ku_print_chunk(kuvm *vm, kuchunk *chunk, const char * name) {
  ku_printf(vm, "== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; ) {
    offset = ku_print_op(vm, chunk, offset);
    ku_printf(vm, "\n");
  }
}

static int ku_print_simple_op(kuvm *vm, const char *name, int offset) {
  ku_printf(vm, "%-16s", name);
  return offset + 1;
}

static int ku_print_const(kuvm *vm, const char *name, kuchunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  ku_printf(vm, "%-16s %4d '", name, con);
  ku_print_val(vm, chunk->constants.values[con]);
  ku_printf(vm, "'");
  return offset+2;
}

int ku_print_op(kuvm *vm, kuchunk *chunk, int offset) {
  ku_printf(vm, "%04d ", offset);

  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset-1]) {
    ku_printf(vm, "   | ");
  } else {
    ku_printf(vm, "%4d ", chunk->lines[offset]);
  }
  uint8_t op = chunk->code[offset];
  switch (op) {
    case OP_NOP: return ku_print_simple_op(vm, "OP_NOP", offset);
    case OP_RET: return ku_print_simple_op(vm, "OP_RET", offset);
    case OP_NEG: return ku_print_simple_op(vm, "OP_NEG", offset);
    case OP_ADD: return ku_print_simple_op(vm, "OP_ADD", offset);
    case OP_SUB: return ku_print_simple_op(vm, "OP_SUB", offset);
    case OP_MUL: return ku_print_simple_op(vm, "OP_MUL", offset);
    case OP_DIV: return ku_print_simple_op(vm, "OP_DIV", offset);
    case OP_NIL: return ku_print_simple_op(vm, "OP_NIL", offset);
    case OP_TRUE: return ku_print_simple_op(vm, "OP_TRUE", offset);
    case OP_FALSE: return ku_print_simple_op(vm, "OP_FALSE", offset);
    case OP_GT: return ku_print_simple_op(vm, "OP_GT", offset);
    case OP_LT: return ku_print_simple_op(vm, "OP_LT", offset);
    case OP_EQ: return ku_print_simple_op(vm, "OP_EQ", offset);
    case OP_PRINT: return ku_print_simple_op(vm, "OP_PRINT", offset);
    case OP_POP: return ku_print_simple_op(vm, "OP_POP", offset);
    case OP_CONST:
      return ku_print_const(vm, "OP_CONST", chunk, offset);
    case OP_DEF_GLOBAL:
      return ku_print_const(vm, "OP_DEF_GLOBAL", chunk, offset);
    case OP_GET_GLOBAL:
      return ku_print_const(vm, "OP_GET_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL:
      return ku_print_const(vm, "OP_SET_GLOBAL", chunk, offset);
    case OP_GET_LOCAL:
      return ku_print_byte_op(vm, "OP_GET_LOCAL", chunk, offset);
    case OP_SET_LOCAL:
      return ku_print_byte_op(vm, "OP_SET_LOCAL", chunk, offset);
    case OP_JUMP:
      return ku_print_jump_op(vm, "OP_JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE:
      return ku_print_jump_op(vm, "OP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_LOOP:
      return ku_print_jump_op(vm, "OP_LOOP", -1, chunk, offset);
    case OP_CALL:
      return ku_print_byte_op(vm, "OP_CALL", chunk, offset);
    case OP_CLOSURE: {
      offset++;
      uint8_t con = chunk->code[offset++];
      ku_printf(vm, "%-16s %4d", "OP_CLOSURE", con);
      ku_print_val(vm, chunk->constants.values[con]);
      ku_printf(vm, "\n");
      return offset;
    }
    default:
      ku_printf(vm, "Unknown opcode %d\n", op);
      return offset + 1;
  }
#undef OP_DEF1
}


// ------------------------------------------------------------
// Locals
// ------------------------------------------------------------
void ku_compiler_init(kuvm *vm, kucompiler *compiler, kufunctype type) {
  compiler->function = NULL; // for GC
  compiler->enclosing = vm->compiler;
  compiler->count = 0;
  compiler->depth = 0;
  compiler->type = type;
  compiler->function = ku_func_new(vm);
  
  vm->compiler = compiler;
  if (type != FUNC_MAIN) {
    compiler->function->name = ku_str_copy(vm, vm->parser.prev.start, vm->parser.prev.len);
  }
  kulocal *local = &vm->compiler->locals[vm->compiler->count++];
  local->depth = 0;
  local->name.start = "";
  local->name.len = 0;
}

void ku_block(kuvm *vm) {
  while (!ku_parse_checktype(vm, TOK_RBRACE) && !ku_parse_checktype(vm, TOK_EOF)) {
    ku_parse_declaration(vm);
  }
  ku_parse_consume(vm, TOK_RBRACE, "'}' expected");
}

void ku_beginscope(kuvm *vm) {
  vm->compiler->depth++;
}

void ku_endscope(kuvm *vm) {
  vm->compiler->depth--;
  
  while (vm->compiler->count > 0 &&
         vm->compiler->locals[vm->compiler->count - 1].depth >
         vm->compiler->depth) {
    ku_parse_emit_byte(vm, OP_POP);
    vm->compiler->count--;
    }
}

void ku_vardecl(kuvm *vm) {
  if (vm->compiler->depth == 0) {
    return;
  }
  kutok *name = &vm->parser.prev;
  for (int i = vm->compiler->count - 1; i >= 0; i--) {
    kulocal *local = &vm->compiler->locals[i];
    if (local->depth != -1 && local->depth < vm->compiler->depth) {
      break;
    }
    
    if (ku_identeq(vm, name, &local->name)) {
      ku_parse_err(vm, "local already defined");
    }
  }
  ku_addlocal(vm, *name);
}

void ku_addlocal(kuvm *vm, kutok name) {
  if (vm->compiler->count == MAX_LOCALS) {
    ku_parse_err(vm, "too many locals");
    return;
  }
  
  kulocal *local = &vm->compiler->locals[vm->compiler->count++];
  local->name = name;
  local->depth = -1;
}

bool ku_identeq(kuvm *vm, kutok *a, kutok *b) {
  if (a->len != b->len) {
    return false;
  }
  
  return memcmp(a->start, b->start, a->len) == 0;
}

int ku_resolvelocal(kuvm *vm, kutok *name) {
  for (int i = vm->compiler->count - 1; i >= 0; i--) {
    kulocal *local = &vm->compiler->locals[i];
    if (ku_identeq(vm, name, &local->name)) {
      if (local->depth == -1) {
        ku_parse_err(vm, "own initialization disallowed");
      }
      return i;
    }
  }
  return -1;
}

void ku_markinit(kuvm *vm) {
  // functions can reference themselves unlike global
  // variables which can't use their own value in their
  // initialization
  if (vm->compiler->depth == 0) return;
  vm->compiler->locals[vm->compiler->count - 1].depth = vm->compiler->depth;
}

int ku_print_byte_op(kuvm *vm, const char *name, kuchunk *chunk, int offset) {
  uint8_t slot = chunk->code[offset + 1];
  ku_printf(vm, "%-16s %4d", name, slot);
  return offset + 2;
}

// ------------------------------------------------------------
// Branching
// ------------------------------------------------------------
int ku_print_jump_op(kuvm *vm, const char *name, int sign, kuchunk *chunk,
int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  ku_printf(vm, "%-16s %4d -> %d", name, offset,
            offset + 3 + sign * jump);
  return offset + 3;
}

void ku_ifstatement(kuvm *vm) {
  ku_parse_consume(vm, TOK_LPAR, "'(' expected after 'if'");
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_RPAR, "'R' expected after condition");
  int then_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  ku_parse_emit_byte(vm, OP_POP);
  ku_parse_statement(vm);
  int else_jump = ku_emitjump(vm, OP_JUMP);
  ku_patchjump(vm, then_jump);
  ku_parse_emit_byte(vm, OP_POP);
  if (ku_parse_match(vm, TOK_ELSE)) {
    ku_parse_statement(vm);
  }
  ku_patchjump(vm, else_jump);
}

int ku_emitjump(kuvm *vm, k_op op) {
  ku_parse_emit_byte(vm, op);
  ku_parse_emit_byte(vm, 0xff);
  ku_parse_emit_byte(vm, 0xff);
  return ku_chunk(vm)->count - 2;
}

void ku_patchjump(kuvm *vm, int offset) {
  int jump = ku_chunk(vm)->count - offset - 2;
  
  if (jump > UINT16_MAX) {
    ku_parse_err(vm, "too much code to jump over");
  }
  
  ku_chunk(vm)->code[offset] = (jump >> 8) & 0xff;
  ku_chunk(vm)->code[offset + 1] = jump & 0xff;
}

void ku_emitloop(kuvm *vm, int start) {
  ku_parse_emit_byte(vm, OP_LOOP);
  int offset = ku_chunk(vm)->count - start + 2;
  if (offset > UINT16_MAX) {
    ku_parse_err(vm, "loop body too large");
  }
  ku_parse_emit_byte(vm, (offset >> 8) & 0xff);
  ku_parse_emit_byte(vm, offset  & 0xff);
}

void ku_whilestatement(kuvm *vm) {
  int loop_start = ku_chunk(vm)->count;
  ku_parse_consume(vm, TOK_LPAR, "'(' expected after 'while'");
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_RPAR, "')' expected after 'while'");
  int jump_exit = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  ku_parse_emit_byte(vm, OP_POP);
  ku_parse_statement(vm);
  ku_emitloop(vm, loop_start);
  ku_patchjump(vm, jump_exit);
  ku_parse_emit_byte(vm, OP_POP);
}

void ku_forstatement(kuvm *vm) {
  ku_beginscope(vm);
  ku_parse_consume(vm, TOK_LPAR, "'(' expected after 'for'");
  if (ku_parse_match(vm, TOK_SEMI)) {
    // no init
  } else if (ku_parse_match(vm, TOK_VAR)) {
    ku_parse_var_decl(vm);
  } else {
    ku_parse_expression_statement(vm);
  }
  int loop_start = ku_chunk(vm)->count;
  int exit_jump = -1;
  
  if (!ku_parse_match(vm, TOK_SEMI)) {
    ku_parse_expression(vm);
    ku_parse_consume(vm, TOK_SEMI, "';' expected");
    exit_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
    ku_parse_emit_byte(vm, OP_POP);
  }
  
  if (!ku_parse_match(vm, TOK_RPAR)) {
    int body_jump = ku_emitjump(vm, OP_JUMP);
    int inc_start = ku_chunk(vm)->count;
    ku_parse_expression(vm);
    ku_parse_emit_byte(vm, OP_POP);
    ku_parse_consume(vm, TOK_RPAR, "')' expected");
    ku_emitloop(vm, loop_start);
    loop_start = inc_start;
    ku_patchjump(vm, body_jump);
  }
  
  ku_parse_statement(vm);
  ku_emitloop(vm, loop_start);
  
  if (exit_jump != -1) {
    ku_patchjump(vm, exit_jump);
    ku_parse_emit_byte(vm, OP_POP);
  }
  ku_endscope(vm);
}

void ku_parse_and(kuvm *vm, bool lhs) {
  int end_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  ku_parse_emit_byte(vm, OP_POP);
  ku_parse_process(vm, P_AND);
  ku_patchjump(vm, end_jump);
}

void ku_parse_or(kuvm *vm, bool lhs) {
  int else_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  int end_jump = ku_emitjump(vm, OP_JUMP);
  ku_patchjump(vm, else_jump);
  ku_parse_emit_byte(vm, OP_POP);
  ku_parse_process(vm, P_OR);
  ku_patchjump(vm, end_jump);
}

// ------------------------------------------------------------
// Functions
// ------------------------------------------------------------
kufunc *ku_func_new(kuvm *vm) {
  kufunc *fn = (kufunc*)KALLOC_OBJ(vm, kufunc, OBJ_FUNC);
  fn->arity = 0;
  fn->name = NULL;
  ku_chunk_init(vm, &fn->chunk);
  return fn;
}

void ku_print_func(kuvm *vm, kufunc *fn) {
  ku_printf(vm, "<fn %s>", fn->name ? fn->name->chars : "__main__");
}

// ------------------------------------------------------------
// C Functions
// ------------------------------------------------------------
kucfunc *ku_cfunc_new(kuvm *vm, cfunc f) {
  kucfunc *cf = KALLOC_OBJ(vm, kucfunc, OBJ_CFUNC);
  cf->fn = f;
  return cf;
}

void ku_cfunc_def(kuvm *vm, const char *name, cfunc f) {
  int len = (int)strlen(name);
  kustr *sname = ku_str_copy(vm, name, len);
  ku_push(vm, OBJ_VAL(sname));
  ku_push(vm, OBJ_VAL(ku_cfunc_new(vm, f)));
  ku_map_set(vm, &vm->globals, AS_STR(vm->stack[0]), vm->stack[1]);
  ku_pop(vm);
  ku_pop(vm);
}

#include <time.h>
static kuval ku_clock(kuvm *vm, int argc, kuval *argv) {
  return NUM_VAL((double)clock() / CLOCKS_PER_SEC);
}

static kuval ku_print(kuvm *vm, int argc, kuval *argv) {
  for (int a = 0; a < argc; a++) {
    kuval v = argv[a];
    ku_print_val(vm, v);
  }
  return NIL_VAL;
}

void ku_reglibs(kuvm *vm) {
  ku_cfunc_def(vm, "clock", ku_clock);
  ku_cfunc_def(vm, "printf", ku_print);
}

// ------------------------------------------------------------
// Closures
// ------------------------------------------------------------
kuclosure *ku_closure_new(kuvm *vm, kufunc *f) {
  kuclosure *cl = KALLOC_OBJ(vm, kuclosure, OBJ_CLOSURE);
  cl->func = f;
  return cl;
}
