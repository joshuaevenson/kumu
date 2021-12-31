//  kumu.c

#include "kumu.h"
#include <stdio.h>

// ********************** macros **********************
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)ku_alloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(vm, type, ptr, old) ku_alloc(vm, ptr, sizeof(type) * (old), 0)
#define KALLOC(vm, type, count) \
    (type*)ku_alloc(vm, NULL, 0, sizeof(type) * (count))
#define KALLOC_OBJ(vm, type, objtype) \
    (type*)ku_objalloc(vm, sizeof(type), objtype)
#define FREE(vm, type, ptr) \
  ku_alloc(vm, ptr, sizeof(type), 0)

#define READ_SHORT(vm) \
(frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define BYTE_READ(vm) (*(frame->ip++))
#define CONST_READ(vm) (frame->closure->func->chunk.constants.values[BYTE_READ(vm)])
#define READ_STRING(vm) AS_STR(CONST_READ(vm))

#define BIN_OP(v, vt, op) \
  do { \
  if (!IS_NUM(ku_peek(v,0)) || !IS_NUM(ku_peek(v,1))) { \
    ku_err(v, "numbers expected"); \
    return KVM_ERR_RUNTIME; \
  } \
  double b = AS_NUM(ku_pop(v)); \
  double a = AS_NUM(ku_pop(v)); \
  ku_push(v, vt(a op b)); \
} while (false)

// ********************** forwards **********************
static void ku_err(kuvm *vm, const char *fmt, ...);
kuval ku_peek(kuvm *vm, int distance);

static void ku_printf(kuvm *vm, const char *fmt, ...);
static void ku_printobj(kuvm* vm, kuval val);
void ku_printval(kuvm *vm, kuval value);
static void ku_printfunc(kuvm *vm, kufunc *fn);

int ku_bytedis(kuvm *vm, kuchunk *chunk, int offset);
static void ku_chunkdump(kuvm *vm, kuchunk *chunk, const char * name);

static void ku_function(kuvm *vm, kufunc_t type);
// ********************** object **********************
kuobj* ku_objalloc(kuvm* vm, size_t size, kuobj_t type) {
  kuobj* obj = (kuobj*)ku_alloc(vm, NULL, 0, size);
  obj->type = type;
  obj->marked = false;
  obj->next = (struct kuobj*)vm->objects;
  vm->objects = obj;
  if (vm->flags & KVM_F_GCLOG) {
    ku_printf(vm, "%p alloc %zu for %d\n", (void*)obj, size, type);
  }
  return obj;
}

void ku_objfree(kuvm* vm, kuobj* obj) {
  if (vm->flags & KVM_F_GCLOG) {
    ku_printf(vm, "%p free type %d\n", (void*)obj, obj->type);
  }
  switch (obj->type) {
    case OBJ_FUNC: {
      kufunc *fn = (kufunc*)obj;
      ku_chunkfree(vm, &fn->chunk);
      FREE(vm, kufunc, obj);
      break;
    }
      
    case OBJ_CCLASS: {
      kucclass *cc = (kucclass*)obj;
      if (cc->sfree) {
        cc->sfree(vm, obj);
      }
      FREE(vm, kucclass, obj);
      break;
    }

    case OBJ_CINST: {
      kunobj *i = (kunobj*)obj;
      if (i->klass->ifree) {
        i->klass->ifree(vm, obj);
      }
      break;
    }
    case OBJ_CFUNC:
      FREE(vm, kucfunc, obj);
      break;
     
    case OBJ_CLOSURE: {
      kuclosure *cl = (kuclosure*)obj;
      ARRAY_FREE(vm, kuxobj*, cl->upvals, cl->upcount);
      FREE(vm, kuclosure, obj);
      break;
    }
      
    case OBJ_CLASS: {
      kuclass *c = (kuclass*)obj;
      ku_tabfree(vm, &c->methods);
      FREE(vm, kuclass, obj);
      break;
    }
      
    case OBJ_INSTANCE: {
      kuiobj *i = (kuiobj*)obj;
      ku_tabfree(vm, &i->fields);
      FREE(vm, kuiobj, obj);
      break;
    }
      
    case OBJ_BOUND_METHOD: {
      FREE(vm, kubound, obj);
      break;
    }
      
  case OBJ_STR: {
    kustr* str = (kustr*)obj;
    ARRAY_FREE(vm, char, str->chars, str->len + 1);
    FREE(vm, kustr, obj);
    break;
    
  case OBJ_UPVAL:
    FREE(vm, kuxobj, obj);
    break;
  }
  }
}

bool ku_objis(kuval v, kuobj_t ot) {
  return IS_OBJ(v) && AS_OBJ(v)->type == ot;
}

// ********************** string **********************
// FNV-1a hashing function
static uint32_t ku_strhash(const char* key, int len) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < len; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

static kustr* ku_stralloc(kuvm* vm, char* chars, int len, uint32_t hash) {
  kustr* str = KALLOC_OBJ(vm, kustr, OBJ_STR);
  str->len = len;
  str->chars = chars;
  str->hash = hash;
  ku_push(vm, OBJ_VAL(str)); // for GC
  ku_tabset(vm, &vm->strings, str, NIL_VAL);
  ku_pop(vm);
  return str;
}


kustr* ku_strfrom(kuvm* vm, const char* chars, int len) {
  uint32_t hash = ku_strhash(chars, len);

  kustr* interned = ku_tabfindc(vm, &vm->strings, chars, len, hash);
  if (interned != NULL) {
    return interned;
  }

  char* buff = KALLOC(vm, char, len + 1);
  memcpy(buff, chars, len);
  buff[len] = '\0';
  return ku_stralloc(vm, buff, len, hash);
}

static kustr* ku_strtake(kuvm* vm, char* buff, int len) {
  uint32_t hash = ku_strhash(buff, len);
  kustr* interned = ku_tabfindc(vm, &vm->strings, buff, len, hash);
  if (interned != NULL) {
    ARRAY_FREE(vm, char, buff, len + 1);
    return interned;
  }

  return ku_stralloc(vm, buff, len, hash);
}

static void ku_strcat(kuvm* vm) {
  kustr *b = AS_STR(ku_peek(vm,0)); // for GC
  kustr* a = AS_STR(ku_peek(vm,1)); // for GC
  int len = a->len + b->len;
  char* buff = KALLOC(vm, char, len + 1);
  memcpy(buff, a->chars, a->len);
  memcpy(buff + a->len, b->chars, b->len);
  buff[len] = '\0';
  kustr* res = ku_strtake(vm, buff, len);
  ku_pop(vm);
  ku_pop(vm);
  ku_push(vm, OBJ_VAL(res));
}

// ********************** value **********************
bool ku_equal(kuval v1, kuval v2) {

#ifdef NAN_BOX
#ifdef NAN_BOX_COMPARE
  if (IS_NUM(v1) && IS_NUM(v2)) {
    return AS_NUM(v1) == AS_NUM(v2)
  }
#endif
  return v1 == v2;
#else
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
#endif
}


// ********************** hash table **********************
void ku_tabinit(kuvm* vm, kutab* map) {
  map->count = 0;
  map->capacity = 0;
  map->entries = NULL;
}

void ku_tabfree(kuvm* vm, kutab* map) {
  ARRAY_FREE(vm, kuentry, map->entries, map->capacity);
  ku_tabinit(vm, map);
}

#define MAP_MAX_LOAD 0.75

#define MOD2(a,b) ((a) & (b - 1))

static kuentry* ku_tabfinds(kuvm* vm, kuentry* entries, int capacity, kustr* key) {
  uint32_t index = MOD2(key->hash, capacity);
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
    
    index = MOD2(index + 1, capacity);
  }
}

static void ku_tabadjust(kuvm* vm, kutab* map, int capacity) {
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

    kuentry* dest = ku_tabfinds(vm, entries, capacity, src->key);
    dest->key = src->key;
    dest->value = src->value;
    map->count++;
  }

  ARRAY_FREE(vm, kuentry, map->entries, map->capacity);
  map->entries = entries;
  map->capacity = capacity;
}

bool ku_tabset(kuvm* vm, kutab* map, kustr* key, kuval value) {
  if (map->count + 1 > map->capacity * MAP_MAX_LOAD) {
    int capacity = CAPACITY_GROW(map->capacity);
    ku_tabadjust(vm, map, capacity);
  }

  kuentry* e = ku_tabfinds(vm, map->entries, map->capacity, key);
  bool isnew = e->key == NULL;
  // we don't increase the count if we use a tombstone slot
  if (isnew && IS_NIL(e->value)) {
    map->count++;
  }
  e->key = key;
  e->value = value;
  return isnew;
}

void ku_tabcopy(kuvm* vm, kutab* from, kutab* to) {
  for (int i = 0; i < from->capacity; i++) {
    kuentry* e = &from->entries[i];
    if (e->key != NULL) {
      ku_tabset(vm, to, e->key, e->value);
    }
  }
}

bool ku_tabget(kuvm* vm, kutab* map, kustr* key, kuval* value) {
  if (map->count == 0) {
    return false;
  }

  kuentry* e = ku_tabfinds(vm, map->entries, map->capacity, key);
  if (e->key == NULL) {
    return false;
  }

  *value = e->value;
  return true;
}

bool ku_tabdel(kuvm* vm, kutab* map, kustr* key) {
  if (map->count == 0) {
    return false;
  }

  kuentry* e = ku_tabfinds(vm, map->entries, map->capacity, key);
  if (e->key == NULL) {
    return false;
  }
  e->key = NULL;
  e->value = BOOL_VAL(true);
  return true;
}

kustr* ku_tabfindc(kuvm* vm, kutab* map, const char* chars, int len, uint32_t hash) {
  if (map->count == 0) {
    return NULL;
  }

  uint32_t index = MOD2(hash, map->capacity);
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
    index = MOD2(index + 1,map->capacity);
  }
}

// ********************** scanner **********************
static char ku_advance(kuvm *vm) {
  vm->scanner.curr++;
  return vm->scanner.curr[-1];
}


void ku_lexinit(kuvm *vm, const char *source) {
  vm->scanner.start = source;
  vm->scanner.curr = source;
  vm->scanner.line = 1;
}

static bool ku_lexend(kuvm *vm) {
  return (*(vm->scanner.curr) == '\0');
}

static bool ku_isdigit(char c) {
  return (c >= '0' && c <= '9');
}

static bool ku_ishexdigit(char c) {
  return (c >= '0' && c <= '9') ||
         (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}

static bool ku_isalpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
         (c == '_');
}

static char ku_lexpeeknext(kuvm *vm) {
  if (ku_lexend(vm)) return '\0';
  return vm->scanner.curr[1];
}

static char ku_lexpeek(kuvm *vm) {
  return *vm->scanner.curr;
}

static void ku_lexspace(kuvm *vm) {
  while (true) {
    char c = ku_lexpeek(vm);
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        ku_advance(vm);
        break;
      case '\n':
        vm->scanner.line++;
        ku_advance(vm);
        break;
      case '/':
        if (ku_lexpeeknext(vm) == '/') {
          while (ku_lexpeek(vm) != '\n' && !ku_lexend(vm))
            ku_advance(vm);
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

static kutok ku_tokmake(kuvm *vm, kutok_t type) {
  kutok token;
  token.type = type;
  token.start = vm->scanner.start;
  token.len = (int) (vm->scanner.curr - vm->scanner.start);
  token.line = vm->scanner.line;
  return token;
}

static kutok ku_lexerr(kuvm *vm, const char *msg) {
  kutok token;
  token.type = TOK_ERR;
  token.start = msg;
  token.len = (int)strlen(msg);
  token.line = vm->scanner.line;
  return token;
}

static bool ku_lexmatch(kuvm *vm, char expected) {
  if (ku_lexend(vm)) return false;
  if (*vm->scanner.curr != expected) return false;
  vm->scanner.curr++;
  return true;
}

static kutok ku_hexnum(kuvm *vm) {
  ku_advance(vm); // skip 'x'
  while(ku_ishexdigit(ku_lexpeek(vm))) ku_advance(vm);
  return ku_tokmake(vm, TOK_HEX);
}

static kutok ku_lexnum(kuvm *vm) {
  while(ku_isdigit(ku_lexpeek(vm))) ku_advance(vm);
  
  if (ku_lexpeek(vm) == '.' && ku_isdigit(ku_lexpeeknext(vm))) {
    ku_advance(vm);
    while(ku_isdigit(ku_lexpeek(vm))) ku_advance(vm);
  }

  if (ku_lexpeek(vm) == 'e' || ku_lexpeek(vm) == 'E') {
    ku_advance(vm);
    while(ku_isdigit(ku_lexpeek(vm))) ku_advance(vm);
  }

  return ku_tokmake(vm, TOK_NUM);
}

static kutok ku_lexstr(kuvm *vm) {
  kutok_t type = TOK_STR;
  
  while(ku_lexpeek(vm) != '"' && !ku_lexend(vm)) {
    if (ku_lexpeek(vm) == '\\') type = TOK_STRESC;
    if (ku_lexpeek(vm) == '\n') vm->scanner.line++;
    ku_advance(vm);
  }
  if (ku_lexend(vm)) return ku_lexerr(vm, "unterminated string");
  ku_advance(vm);
  return ku_tokmake(vm, type);
}

static kutok_t ku_lexkey(kuvm *vm, int start, int len,
                        const char *rest, kutok_t type) {
  if (vm->scanner.curr - vm->scanner.start == start + len &&
      memcmp(vm->scanner.start + start, rest, len) == 0) {
    return type;
  }
  return TOK_IDENT;
}

static kutok_t ku_keyword(kuvm *vm) {
  switch(vm->scanner.start[0]) {
    case 'a': return ku_lexkey(vm, 1,2,"nd", TOK_AND);
    case 'b': return ku_lexkey(vm, 1,4,"reak", TOK_BREAK);
    case 'c': {
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch(vm->scanner.start[1]) {
          case 'l': return ku_lexkey(vm, 2, 3, "ass", TOK_CLASS);
          case 'o': return ku_lexkey(vm, 2, 6, "ntinue", TOK_CONTINUE);
        }
      }
      break;
    }
    case 'e': return ku_lexkey(vm, 1,3,"lse", TOK_ELSE);
    case 'f':
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch (vm->scanner.start[1]) {
          case 'a': return ku_lexkey(vm, 2, 3, "lse", TOK_FALSE);
          case 'o': return ku_lexkey(vm, 2, 1, "r", TOK_FOR);
          case 'u': return ku_lexkey(vm, 2, 1, "n", TOK_FUN);
        }
      }
    case 'i': return ku_lexkey(vm, 1,1,"f", TOK_IF);
    case 'n': return ku_lexkey(vm, 1,2,"il", TOK_NIL);
    case 'o': return ku_lexkey(vm, 1,1,"r", TOK_OR);
    case 'r': return ku_lexkey(vm, 1,5,"eturn", TOK_RETURN);
    case 's': return ku_lexkey(vm, 1,4,"uper", TOK_SUPER);
    case 't':
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch(vm->scanner.start[1]) {
          case 'h': return ku_lexkey(vm, 2, 2, "is", TOK_THIS);
          case 'r': return ku_lexkey(vm, 2, 2, "ue", TOK_TRUE);
        }
      }
    case 'v': return ku_lexkey(vm, 1,2,"ar", TOK_VAR);
    case 'w': return ku_lexkey(vm, 1,4,"hile", TOK_WHILE);
  }
  return TOK_IDENT;
}

static kutok ku_lexid(kuvm *vm) {
  while (ku_isalpha(ku_lexpeek(vm)) || ku_isdigit(ku_lexpeek(vm))) {
    ku_advance(vm);
  }
  return ku_tokmake(vm, ku_keyword(vm));
}

kutok ku_scan(kuvm *vm) {
  ku_lexspace(vm);
  vm->scanner.start = vm->scanner.curr;
  
  if (ku_lexend(vm)) {
    return ku_tokmake(vm, TOK_EOF);
  }
  
  char c = ku_advance(vm);
  if (ku_isalpha(c)) return ku_lexid(vm);
  if (c == '0' && (ku_lexpeek(vm) == 'x' || ku_lexpeek(vm) == 'X')) {
    return ku_hexnum(vm);
  }
  else if (ku_isdigit(c)) return ku_lexnum(vm);
  switch (c) {
    case '(': return ku_tokmake(vm, TOK_LPAR);
    case ')': return ku_tokmake(vm, TOK_RPAR);
    case '{': return ku_tokmake(vm, TOK_LBRACE);
    case '}': return ku_tokmake(vm, TOK_RBRACE);
    case ';': return ku_tokmake(vm, TOK_SEMI);
    case ',': return ku_tokmake(vm, TOK_COMMA);
    case '.': return ku_tokmake(vm, TOK_DOT);
    case '+': return ku_tokmake(vm, TOK_PLUS);
    case '-': return ku_tokmake(vm, TOK_MINUS);
    case '*': return ku_tokmake(vm, TOK_STAR);
    case '/':
      return ku_tokmake(vm, TOK_SLASH);
    case '!':
      return ku_tokmake(vm, ku_lexmatch(vm, '=') ? TOK_NE : TOK_BANG);
    case '=': {
      if (ku_lexmatch(vm, '=')) {
        return ku_tokmake(vm, TOK_EQEQ);
      } else if (ku_lexmatch(vm, '>')) {
        return ku_tokmake(vm, TOK_ARROW);
      }
      return ku_tokmake(vm, TOK_EQ);
    }
    case '<':
      return ku_tokmake(vm, ku_lexmatch(vm, '=') ? TOK_LE : TOK_LT);
    case '>':
      return ku_tokmake(vm, ku_lexmatch(vm, '=') ? TOK_GE : TOK_GT);
    case '"':
      return ku_lexstr(vm);
  }
  return ku_lexerr(vm, "unexpected character");
}

void ku_lexdump(kuvm *vm) {
  int line = -1;
  
  while (true) {
    kutok token = ku_scan(vm);
    if (token.line != line) {
      ku_printf(vm, "%4d ", token.line);
      line = token.line;
    } else {
      ku_printf(vm, "  |  ");
    }
    ku_printf(vm, "%2d '%.*s'\n", token.type, token.len, token.start);
    
    if (token.type == TOK_EOF) {
      break;
    }
  }
}

// ********************** parser **********************
static void ku_perrat(kuvm *vm, kutok *tok, const char *msg) {
  if (vm->parser.panic) return;
  vm->parser.panic = true;
  
  ku_printf(vm, "[line %d] error ", tok->line);
  
  if (tok->type == TOK_EOF) {
    ku_printf(vm, " at end ");
  } else if (tok->type == TOK_ERR) {
    // nothing
  } else {
    ku_printf(vm, " at '%.*s'",  tok->len, tok->start);
  }
  
  ku_printf(vm, "%s\n", msg);
  vm->parser.err = true;
}

static void ku_perr(kuvm *vm, const char *msg) {
  ku_perrat(vm, &vm->parser.curr, msg);
}

static void ku_pnext(kuvm *vm) {
  vm->parser.prev = vm->parser.curr;
  
  while (true) {
    vm->parser.curr = ku_scan(vm);
    if (vm->parser.curr.type != TOK_ERR) break;
    ku_perr(vm, vm->parser.curr.start);
  }
}

static void ku_pconsume(kuvm *vm, kutok_t type, const char *msg) {
  if (vm->parser.curr.type == type) {
    ku_pnext(vm);
    return;
  }
  ku_perr(vm, msg);
}

kuchunk *ku_chunk(kuvm *vm) {
  return &vm->compiler->function->chunk;
}

static void ku_emitbyte(kuvm *vm, uint8_t byte) {
  ku_chunkwrite(vm, ku_chunk(vm), byte, vm->parser.prev.line);
}

static void ku_emitbytes(kuvm *vm, uint8_t b1, uint8_t b2);
static void ku_emitret(kuvm *vm, bool lambda) {
  if (lambda) {
    ku_emitbyte(vm, OP_RET);
    return;
  }
  if (vm->compiler->type == FUNC_INIT) {
    ku_emitbytes(vm, OP_GET_LOCAL, 0);
  } else {
    ku_emitbyte(vm, OP_NIL);
  }
  ku_emitbyte(vm, OP_RET);
}

static kufunc *ku_pend(kuvm *vm, bool lambda) {
  ku_emitret(vm, lambda);
  kufunc *fn = vm->compiler->function;
  vm->compiler = vm->compiler->enclosing;
  return fn;
}

static void ku_emitbytes(kuvm *vm, uint8_t b1, uint8_t b2) {
  ku_emitbyte(vm, b1);
  ku_emitbyte(vm, b2);
}


static uint8_t ku_pconst(kuvm *vm, kuval val) {
  int cons = ku_chunkconst(vm, ku_chunk(vm), val);
  if (cons > vm->max_const) {
    ku_perr(vm, "out of constant space");
    return 0;
  }
  return (uint8_t)cons;
}
static void ku_emitconst(kuvm *vm, kuval val) {
  ku_emitbytes(vm, OP_CONST, ku_pconst(vm, val));
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

typedef void (*kupfunc)(kuvm *, bool lhs);

typedef struct {
  kupfunc prefix;
  kupfunc infix;
  kup_precedence precedence;
} kuprule;

static kuprule *ku_getrule(kuvm *vm, kutok_t optype);

static bool ku_pcheck(kuvm* vm, kutok_t type) {
  return vm->parser.curr.type == type;
}

static bool ku_pmatch(kuvm* vm, kutok_t type) {
  if (!ku_pcheck(vm, type)) {
    return false;
  }
  ku_pnext(vm);
  return true;
}

static void ku_prec(kuvm *vm, kup_precedence prec) {
  ku_pnext(vm);
  kupfunc prefix = ku_getrule(vm, vm->parser.prev.type)->prefix;
  if (prefix == NULL) {
    ku_perr(vm, "expected expression");
    return;
  }

  bool lhs = prec <= P_ASSIGN;
  prefix(vm, lhs);
  
  while (prec <= ku_getrule(vm, vm->parser.curr.type)->precedence) {
    ku_pnext(vm);
    kupfunc infix = ku_getrule(vm, vm->parser.prev.type)->infix;
    infix(vm, lhs);
  }

  if (lhs && ku_pmatch(vm, TOK_EQ)) {
    ku_perr(vm, "invalid assignment target");
  }
}


static void ku_lit(kuvm *vm, bool lhs) {
  switch (vm->parser.prev.type) {
    case TOK_FALSE: ku_emitbyte(vm, OP_FALSE); break;
    case TOK_TRUE: ku_emitbyte(vm, OP_TRUE); break;
    case TOK_NIL: ku_emitbyte(vm, OP_NIL); break;
    default: return; // unreachable
  }
}

static void ku_xstring(kuvm* vm, bool lhs) {
  const char *chars = vm->parser.prev.start + 1;
  int len = vm->parser.prev.len - 2;
  
  // len(encoded) always <= len(chars);
  char *encoded = ku_alloc(vm, NULL, 0, len);
  
  int esclen = len;
  char *ep = encoded;
  for (int i = 0; i < len; i++) {
    char ch = chars[i];
    if (ch == '\\') {
      char next = (i < len-1) ? chars[i+1] : 0;
      if (next) {
        i++;
        esclen--;
        switch (next) {
          case 'n':
            *ep++ = '\n';
            break;
          case 'r':
            *ep++ = '\r';
            break;
          case 't':
            *ep++ = '\t';
            break;
        }
      }
    } else {
      *ep++ = chars[i];
    }
  }

  ku_emitconst(vm, OBJ_VAL(ku_strfrom(vm, encoded, esclen)));
  ku_alloc(vm, encoded, len, 0);
}
static void ku_string(kuvm* vm, bool lhs) {
  const char *chars = vm->parser.prev.start + 1;
  int len = vm->parser.prev.len - 2;
  ku_emitconst(vm, OBJ_VAL(ku_strfrom(vm, chars, len)));
}

static void ku_hex(kuvm *vm, bool lhs) {
  const char *start = vm->parser.prev.start + 2;  // skip 0x
  int len = vm->parser.prev.len - 2;
  
  double val = 0;
  for (int i = 0; i < len; i++) {
    char ch = start[i];
    if (ch >= '0' && ch <= '9') {
      val = val*16 + (double)(ch - '0');
    } else if (ch >= 'a' && ch <= 'f') {
      val = val*16 + (double)(ch - 'a' + 10);
    } else if (ch >= 'A' && ch <= 'F') {
      val = val*16 + (double)(ch - 'A' + 10);
    }
  }
  ku_emitconst(vm, NUM_VAL(val));
}

static void ku_number(kuvm *vm, bool lhs) {
  double val = strtod(vm->parser.prev.start, NULL);
  ku_emitconst(vm, NUM_VAL(val));
}

static void ku_funcexpr(kuvm *vm, bool lhs) {
  ku_function(vm, FUNC_STD);
}

static void ku_expr(kuvm *vm) {
  ku_prec(vm, P_ASSIGN);
}

static uint8_t ku_arglist(kuvm *vm) {
  uint8_t argc = 0;
  if (!ku_pcheck(vm, TOK_RPAR)) {
    do {
      ku_expr(vm);
      if (argc > vm->max_params) {
        ku_perr(vm, "too many parameters");
      }
      argc++;
    } while (ku_pmatch(vm, TOK_COMMA));
  }
  
  ku_pconsume(vm, TOK_RPAR, "')' expected");
  return argc;
}

static void ku_call(kuvm *vm, bool lhs) {
  uint8_t argc = ku_arglist(vm);
  ku_emitbytes(vm, OP_CALL, argc);
}

static void ku_grouping(kuvm *vm, bool lhs) {
  ku_expr(vm);
  ku_pconsume(vm, TOK_RPAR, "')' expected");
}

static void ku_unary(kuvm *vm, bool lhs) {
  kutok_t optype = vm->parser.prev.type;
  
  ku_prec(vm, P_UNARY);
  
  switch(optype) {
    case TOK_MINUS: ku_emitbyte(vm, OP_NEG); break;
    case TOK_BANG: ku_emitbyte(vm, OP_NOT); break;
    default: return;
  }
}

static void ku_exprstmt(kuvm* vm, kuloop *loop) {
  ku_expr(vm);
  ku_pconsume(vm, TOK_SEMI, "; expected");
  ku_emitbyte(vm, OP_POP);
}

static void ku_return(kuvm *vm, kuloop *loop) {
  if (vm->compiler->type == FUNC_MAIN) {
    ku_perr(vm, "can't return from top-level");
  }
  
  if (ku_pmatch(vm, TOK_SEMI)) {
    ku_emitret(vm, false);
  } else {
    
    if (vm->compiler->type == FUNC_INIT) {
      ku_err(vm, "cannot return from initializer");
    }
    ku_expr(vm);
    ku_pconsume(vm, TOK_SEMI, "';' expected");
    ku_emitbyte(vm, OP_RET);
  }
}

static void ku_break(kuvm *vm, kuloop *loop) {
  if (loop == NULL) {
    ku_perr(vm, "break must be in a loop");
  } else {
    ku_pconsume(vm, TOK_SEMI, "; expected");
    ku_emitpatch(vm, &loop->breakpatch, OP_JUMP);
  }
}

static void ku_continue(kuvm *vm, kuloop *loop) {
  if (loop == NULL) {
    ku_perr(vm, "continue must be in a loop");
  } else {
    ku_pconsume(vm, TOK_SEMI, "; expected");
    ku_emitpatch(vm, &loop->continuepatch, OP_LOOP);
  }
}

static void ku_stmt(kuvm* vm, kuloop *loop) {
  if (ku_pmatch(vm, TOK_IF)) {
    ku_ifstmt(vm, loop);
  } else if (ku_pmatch(vm, TOK_RETURN)) {
    ku_return(vm, loop);
  } else if (ku_pmatch(vm, TOK_WHILE)) {
    ku_whilestmt(vm, loop);
  } else if (ku_pmatch(vm, TOK_FOR)) {
    ku_forstmt(vm, loop);
  } else if (ku_pmatch(vm, TOK_BREAK)) {
    ku_break(vm, loop);
  } else if (ku_pmatch(vm, TOK_CONTINUE)) {
    ku_continue(vm, loop);
  } else if (ku_pmatch(vm, TOK_LBRACE)) {
    ku_beginscope(vm);
    ku_block(vm, loop);
    ku_endscope(vm);
  } else {
    ku_exprstmt(vm, loop);
  }
}

static void ku_pskip(kuvm* vm) {
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
    case TOK_RETURN:
      return;
    default:
      ;
    }

    ku_pnext(vm);
  }
}

static uint8_t ku_pidconst(kuvm* vm, kutok* name) {
  return ku_pconst(vm, OBJ_VAL(ku_strfrom(vm, name->start, name->len)));
}

static uint8_t ku_var(kuvm* vm, const char* msg) {
  ku_pconsume(vm, TOK_IDENT, msg);
  ku_declare_var(vm);
  if (vm->compiler->depth > 0) {
    return 0;
  }
  return ku_pidconst(vm, &vm->parser.prev);
}

static void ku_vardef(kuvm* vm, uint8_t index) {
  if (vm->compiler->depth > 0) {
    ku_markinit(vm);
    return;
  }
  ku_emitbytes(vm, OP_DEF_GLOBAL, index);
}

static void ku_vardecl(kuvm* vm) {
  uint8_t g = ku_var(vm, "name expected");
  if (ku_pmatch(vm, TOK_EQ)) {
    ku_expr(vm);
  }
  else {
    ku_emitbyte(vm, OP_NIL);
  }

  ku_pconsume(vm, TOK_SEMI, "; expected");
  ku_vardef(vm, g);
}

// common between functions and lambdas
static void ku_functail(kuvm *vm, kucomp *compiler, bool lambda) {
  kufunc *fn = ku_pend(vm, lambda);
  ku_emitbytes(vm, OP_CLOSURE, ku_pconst(vm, OBJ_VAL(fn)));
  for (int i = 0; i < fn->upcount; i++) {
    ku_emitbyte(vm, compiler->upvals[i].local ? 1: 0);
    ku_emitbyte(vm, compiler->upvals[i].index);
  }
}

static void ku_params(kuvm *vm) {
  do {
    vm->compiler->function->argc++;
    if (vm->compiler->function->argc > vm->max_params) {
      ku_perr(vm, "too many params");
    }
    
    uint8_t constant = ku_var(vm, "expected parameter name");
    ku_vardef(vm, constant);
  } while(ku_pmatch(vm, TOK_COMMA));
}

static void ku_lbody(kuvm *vm, kucomp *compiler) {
  if (ku_pmatch(vm, TOK_LBRACE)) {
    ku_block(vm, NULL);
    ku_functail(vm, compiler, false);
  } else {
    ku_expr(vm);
    ku_functail(vm, compiler, true);
  }
}

static void ku_lblock(kuvm *vm, bool lhs) {
  kucomp compiler;
  ku_compinit(vm, &compiler, FUNC_STD);
  ku_beginscope(vm);
  ku_params(vm);
  ku_pconsume(vm, TOK_ARROW, "'=>' expected");
  ku_lbody(vm, &compiler);
  ku_pconsume(vm, TOK_RBRACE, "'}' expected");
}

static void ku_function(kuvm *vm, kufunc_t type) {
  kucomp compiler;
  ku_compinit(vm, &compiler, type);
  ku_beginscope(vm);
  ku_pconsume(vm, TOK_LPAR, "'(' expected after function name");
  if (!ku_pcheck(vm, TOK_RPAR)) {
    ku_params(vm);
  }
  ku_pconsume(vm, TOK_RPAR, "')' expected after parameters");
  ku_pconsume(vm, TOK_LBRACE, "'{' expected before function body");
  ku_block(vm, NULL);
  ku_functail(vm, &compiler, false);
}


static void ku_funcdecl(kuvm *vm) {
  uint8_t global = ku_var(vm, "function name expected");
  ku_markinit(vm);
  ku_function(vm, FUNC_STD);
  ku_vardef(vm, global);
}

static void ku_method(kuvm *vm) {
  ku_pconsume(vm, TOK_IDENT, "method name expected");
  uint8_t name = ku_pidconst(vm, &vm->parser.prev);
  kufunc_t type = FUNC_METHOD;
  
  if (vm->parser.prev.len == 4 && memcmp(vm->parser.prev.start, "init", 4) == 0) {
    type = FUNC_INIT;
  }
  ku_function(vm, type);
  ku_emitbytes(vm, OP_METHOD, name);
}

static void ku_namedvar(kuvm* vm, kutok name, bool lhs);
static void ku_pvar(kuvm* vm, bool lhs);

static kutok ku_maketok(kuvm *vm, const char *text) {
  kutok tok;
  tok.start = text;
  tok.len = (int)strlen(text);
  return tok;
}

static void ku_classdecl(kuvm *vm) {
  ku_pconsume(vm, TOK_IDENT, "class name expected");
  kutok cname = vm->parser.prev;
  uint8_t name = ku_pidconst(vm, &vm->parser.prev);
  ku_declare_var(vm);
  ku_emitbytes(vm, OP_CLASS, name);
  ku_vardef(vm, name);
  kuclasscomp cc;
  cc.enclosing = vm->curclass;
  cc.hassuper = false;
  vm->curclass = &cc;
  
  if (ku_pmatch(vm, TOK_LT)) {
    ku_pconsume(vm, TOK_IDENT, "class name expected");
    ku_pvar(vm, false); //  [.. GET_GLOBAL <name>;]
    
    if (ku_identeq(vm, &cname, &vm->parser.prev)) {
      ku_perr(vm, "cannot inherit from self");
    }
    
    ku_beginscope(vm);
    ku_addlocal(vm, ku_maketok(vm, "super"));
    ku_vardef(vm, 0);
    ku_namedvar(vm, cname, false);
    ku_emitbyte(vm, OP_INHERIT);
    cc.hassuper = true;
  }
  
  ku_namedvar(vm, cname, false);
  ku_pconsume(vm, TOK_LBRACE, "'{' expected");
  while (!ku_pcheck(vm, TOK_RBRACE) && !ku_pcheck(vm, TOK_EOF)) {
    ku_method(vm);
  }
  ku_pconsume(vm, TOK_RBRACE, "'}' expected");
  ku_emitbyte(vm, OP_POP);
  
  if (cc.hassuper) {
    ku_endscope(vm);
  }
  vm->curclass = vm->curclass->enclosing;
}

static void ku_decl(kuvm* vm, kuloop *loop) {
  if (ku_pmatch(vm, TOK_CLASS)) {
    ku_classdecl(vm);
  } else if (ku_pmatch(vm, TOK_FUN)) {
    ku_funcdecl(vm);
  } else if (ku_pmatch(vm, TOK_VAR)) {
    ku_vardecl(vm);
  } else {
    ku_stmt(vm, loop);
  }
  if (vm->parser.panic) {
    ku_pskip(vm);
  }
}

static int ku_xvaladd(kuvm *vm, kucomp *compiler, uint8_t index, bool local) {
  int upcount = compiler->function->upcount;
  
  for (int i = 0; i < upcount; i++) {
    kuxval *uv = &compiler->upvals[i];
    if (uv->index == index && uv->local == local) {
      return i;
    }
  }
  
  if (upcount == vm->max_closures) {
    ku_err(vm, "too many closures");
    return 0;
  }
  compiler->upvals[upcount].local = local;
  compiler->upvals[upcount].index = index;
  return compiler->function->upcount++;
}

static int ku_xvalresolve(kuvm *vm, kucomp *compiler, kutok *name) {
  if (compiler->enclosing == NULL) {
    return -1;
  }
  
  int local = ku_resolvelocal(vm, compiler->enclosing, name);
  if (local != -1) {
    compiler->enclosing->locals[local].captured = true;
    return ku_xvaladd(vm, compiler, (uint8_t)local, true);
  }
  
  int upv = ku_xvalresolve(vm, compiler->enclosing, name);
  if (upv != -1) {
    return ku_xvaladd(vm, compiler, (uint8_t)upv, false);
  }
  return -1;
}

static void ku_lambda(kuvm *vm, kutok name) {
  kucomp compiler;
  ku_compinit(vm, &compiler, FUNC_STD);
  ku_beginscope(vm);
  ku_addlocal(vm, name);
  ku_markinit(vm);
  ku_lbody(vm, &compiler);
  compiler.function->argc = 1;
}


static void ku_namedvar(kuvm* vm, kutok name, bool lhs) {
  int arg = ku_resolvelocal(vm, vm->compiler, &name);
  uint8_t set, get;
  
  if (arg != -1) {
    get = OP_GET_LOCAL;
    set = OP_SET_LOCAL;
  } else if ((arg = ku_xvalresolve(vm, vm->compiler, &name)) != -1) {
    get = OP_GET_UPVAL;
    set = OP_SET_UPVAL;
  } else {
    arg = ku_pidconst(vm, &name);
    get = OP_GET_GLOBAL;
    set = OP_SET_GLOBAL;
  }
  if (lhs && ku_pmatch(vm, TOK_EQ)) {
    ku_expr(vm);
    ku_emitbytes(vm, set, (uint8_t)arg);
  } else if (ku_pmatch(vm, TOK_ARROW)) {
    ku_lambda(vm, name);
  } else {
    ku_emitbytes(vm, get, (uint8_t)arg);
  }
}

static void ku_pvar(kuvm* vm, bool lhs) {
  ku_namedvar(vm, vm->parser.prev, lhs);
}

static void ku_this(kuvm *vm, bool lhs) {
  if (vm->curclass == NULL) {
    ku_perr(vm, "cannot use this outside a class");
    return;
  }
  ku_pvar(vm, false);
}

static void ku_super(kuvm *vm, bool lhs) {
  if (vm->curclass == NULL) {
    ku_perr(vm, "'super' must be used inside a class");
  } else if (!vm->curclass->hassuper) {
    ku_perr(vm, "cannot use super in a class with no superclass");
  }
  ku_pconsume(vm, TOK_DOT, "'.' expected after super");
  ku_pconsume(vm, TOK_IDENT, "superclass method expected");
  uint8_t name = ku_pidconst(vm, &vm->parser.prev);
  ku_namedvar(vm, ku_maketok(vm, "this"), false); // [ ... GET_LOCAL <0> ]
  
  if (ku_pmatch(vm, TOK_LPAR)) {
    uint8_t argc = ku_arglist(vm);
    ku_namedvar(vm, ku_maketok(vm, "super"), false);
    ku_emitbytes(vm, OP_SUPER_INVOKE, name);
    ku_emitbyte(vm, argc);
  } else {
    ku_namedvar(vm, ku_maketok(vm, "super"), false); // [ ... GET_UPVAL <0> ]
    ku_emitbytes(vm, OP_GET_SUPER, name);
  }
}

static void ku_bin(kuvm *vm, bool lhs) {
  kutok_t optype = vm->parser.prev.type;
  kuprule *rule = ku_getrule(vm, optype);
  ku_prec(vm, (kup_precedence)(rule->precedence + 1));
  
  switch (optype) {
    case TOK_PLUS: ku_emitbyte(vm, OP_ADD); break;
    case TOK_MINUS: ku_emitbyte(vm, OP_SUB); break;
    case TOK_STAR: ku_emitbyte(vm, OP_MUL); break;
    case TOK_SLASH: ku_emitbyte(vm, OP_DIV); break;
    case TOK_NE: ku_emitbytes(vm, OP_EQ, OP_NOT); break;
    case TOK_EQEQ: ku_emitbyte(vm, OP_EQ); break;
    case TOK_GT: ku_emitbyte(vm, OP_GT); break;
    case TOK_GE: ku_emitbytes(vm, OP_LT, OP_NOT); break;
    case TOK_LT: ku_emitbyte(vm, OP_LT); break;
    case TOK_LE: ku_emitbytes(vm, OP_GT, OP_NOT); break;
    default: return;
  }
}
static void ku_dot(kuvm *vm, bool lhs) {
  ku_pconsume(vm, TOK_IDENT, "property name expected");
  uint8_t name = ku_pidconst(vm, &vm->parser.prev);
  if (lhs && ku_pmatch(vm, TOK_EQ)) {
    ku_expr(vm);
    ku_emitbytes(vm, OP_SET_PROP, name);
  } else if (ku_pmatch(vm, TOK_LPAR)) {
    uint8_t argc = ku_arglist(vm);
    ku_emitbytes(vm, OP_INVOKE, name);
    ku_emitbyte(vm, argc);
  } else {
    ku_emitbytes(vm, OP_GET_PROP, name);
  }
}

int ku_jumpdis(kuvm *vm, const char *name, int sign, kuchunk *chunk,
int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  ku_printf(vm, "%-16s %4d -> %d", name, offset,
            offset + 3 + sign * jump);
  return offset + 3;
}

void ku_ifstmt(kuvm *vm, kuloop *loop) {
  ku_pconsume(vm, TOK_LPAR, "'(' expected after 'if'");
  ku_expr(vm);
  ku_pconsume(vm, TOK_RPAR, "'R' expected after condition");
  int then_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  ku_emitbyte(vm, OP_POP);
  ku_stmt(vm, loop);
  int else_jump = ku_emitjump(vm, OP_JUMP);
  ku_patchjump(vm, then_jump);
  ku_emitbyte(vm, OP_POP);
  if (ku_pmatch(vm, TOK_ELSE)) {
    ku_stmt(vm, loop);
  }
  ku_patchjump(vm, else_jump);
}

int ku_emitjump(kuvm *vm, k_op op) {
  ku_emitbyte(vm, op);
  ku_emitbyte(vm, 0xff);
  ku_emitbyte(vm, 0xff);
  return ku_chunk(vm)->count - 2;
}

void ku_patchjump(kuvm *vm, int offset) {
  int jump = ku_chunk(vm)->count - offset - 2;
  
  if (jump > vm->max_jump) {
    ku_perr(vm, "too much code to jump over");
  }
  
  ku_chunk(vm)->code[offset] = (jump >> 8) & 0xff;
  ku_chunk(vm)->code[offset + 1] = jump & 0xff;
}

void ku_emitloop(kuvm *vm, int start) {
  ku_emitbyte(vm, OP_LOOP);
  int offset = ku_chunk(vm)->count - start + 2;
  if (offset > vm->max_body) {
    ku_perr(vm, "loop body too large");
  }
  ku_emitbyte(vm, (offset >> 8) & 0xff);
  ku_emitbyte(vm, offset  & 0xff);
}

void ku_whilestmt(kuvm *vm, kuloop *loop) {
  int loop_start = ku_chunk(vm)->count;
  ku_pconsume(vm, TOK_LPAR, "'(' expected after 'while'");
  ku_expr(vm);
  ku_pconsume(vm, TOK_RPAR, "')' expected after 'while'");
  int jump_exit = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  ku_emitbyte(vm, OP_POP);
  kuloop inner;
  ku_loopinit(vm, &inner);
  ku_stmt(vm, &inner);
  ku_emitloop(vm, loop_start);
  ku_patchjump(vm, jump_exit);
  uint16_t loop_end = ku_chunk(vm)->count;
  ku_emitbyte(vm, OP_POP);
  ku_patchall(vm, &inner.continuepatch, loop_start, true);
  ku_patchall(vm, &inner.breakpatch, loop_end, false);
}

void ku_forstmt(kuvm *vm, kuloop *loop) {
  ku_beginscope(vm);
  ku_pconsume(vm, TOK_LPAR, "'(' expected after 'for'");
  if (ku_pmatch(vm, TOK_SEMI)) {
    // no init
  } else if (ku_pmatch(vm, TOK_VAR)) {
    ku_vardecl(vm);
  } else {
    ku_exprstmt(vm, loop);
  }
  int loop_start = ku_chunk(vm)->count;
  int exit_jump = -1;
  
  if (!ku_pmatch(vm, TOK_SEMI)) {
    ku_expr(vm);
    ku_pconsume(vm, TOK_SEMI, "';' expected");
    exit_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
    ku_emitbyte(vm, OP_POP);
  }
  
  if (!ku_pmatch(vm, TOK_RPAR)) {
    int body_jump = ku_emitjump(vm, OP_JUMP);
    int inc_start = ku_chunk(vm)->count;
    ku_expr(vm);
    ku_emitbyte(vm, OP_POP);
    ku_pconsume(vm, TOK_RPAR, "')' expected");
    ku_emitloop(vm, loop_start);
    loop_start = inc_start;
    ku_patchjump(vm, body_jump);
  }
  
  kuloop inner;
  ku_loopinit(vm, &inner);
  ku_stmt(vm, &inner);
  ku_emitloop(vm, loop_start);

  if (exit_jump != -1) {
    ku_patchjump(vm, exit_jump);
    ku_emitbyte(vm, OP_POP);
  }

  uint16_t loop_end = ku_chunk(vm)->count;
  ku_patchall(vm, &inner.continuepatch, loop_start, true);
  ku_patchall(vm, &inner.breakpatch, loop_end, false);

  ku_endscope(vm);
}

void ku_and(kuvm *vm, bool lhs) {
  int end_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  ku_emitbyte(vm, OP_POP);
  ku_prec(vm, P_AND);
  ku_patchjump(vm, end_jump);
}

void ku_or(kuvm *vm, bool lhs) {
  int else_jump = ku_emitjump(vm, OP_JUMP_IF_FALSE);
  int end_jump = ku_emitjump(vm, OP_JUMP);
  ku_patchjump(vm, else_jump);
  ku_emitbyte(vm, OP_POP);
  ku_prec(vm, P_OR);
  ku_patchjump(vm, end_jump);
}
kuprule ku_rules[] = {
  [TOK_LPAR] =   { ku_grouping, ku_call,  P_CALL },
  [TOK_RPAR] =   { NULL,        NULL,     P_NONE },
  [TOK_LBRACE] = { ku_lblock,   NULL,     P_NONE },
  [TOK_RBRACE] = { NULL,        NULL,     P_NONE },
  [TOK_COMMA] =  { NULL,        NULL,     P_NONE },
  [TOK_DOT] =    { NULL,        ku_dot,   P_CALL },
  [TOK_MINUS] =  { ku_unary,    ku_bin,   P_TERM },
  [TOK_PLUS] =   { NULL,        ku_bin,   P_TERM },
  [TOK_SEMI] =   { NULL,        NULL,     P_NONE },
  [TOK_SLASH] =  { NULL,        ku_bin,   P_FACTOR },
  [TOK_STAR] =   { NULL,        ku_bin,   P_FACTOR },
  [TOK_BANG] =   { ku_unary,    NULL,     P_NONE },
  [TOK_NE] =     { NULL,        ku_bin,   P_EQ },
  [TOK_EQ] =     { NULL,        NULL,     P_NONE },
  [TOK_EQEQ] =   { NULL,        ku_bin,   P_EQ },
  [TOK_GT] =     { NULL,        ku_bin,   P_COMP },
  [TOK_GE] =     { NULL,        ku_bin,   P_COMP },
  [TOK_LT] =     { NULL,        ku_bin,   P_COMP },
  [TOK_LE] =     { NULL,        ku_bin,   P_COMP },
  [TOK_IDENT] =  { ku_pvar,     NULL,     P_NONE },
  [TOK_STR] =    { ku_string,   NULL,     P_NONE },
  [TOK_STRESC] = { ku_xstring,  NULL,     P_NONE },
  [TOK_NUM] =    { ku_number,   NULL,     P_NONE },
  [TOK_HEX] =    { ku_hex,      NULL,     P_NONE },
  [TOK_AND] =    { NULL,        ku_and,   P_AND },
  [TOK_CLASS] =  { NULL,        NULL,     P_NONE },
  [TOK_ELSE] =   { NULL,        NULL,     P_NONE },
  [TOK_FALSE] =  { ku_lit,      NULL,     P_NONE },
  [TOK_FOR] =    { NULL,        NULL,     P_NONE },
  [TOK_FUN] =    { ku_funcexpr, NULL,     P_NONE },
  [TOK_IF] =     { NULL,        NULL,     P_NONE },
  [TOK_NIL] =    { ku_lit,      NULL,     P_NONE },
  [TOK_OR] =     { NULL,        ku_or,    P_OR },
  [TOK_SUPER] =  { ku_super,    NULL,     P_NONE },
  [TOK_THIS] =   { ku_this,     NULL,     P_NONE },
  [TOK_TRUE] =   { ku_lit,      NULL,     P_NONE },
  [TOK_VAR] =    { NULL,        NULL,     P_NONE },
  [TOK_WHILE] =  { NULL,        NULL,     P_NONE },
  [TOK_ERR] =    { NULL,        NULL,     P_NONE },
  [TOK_EOF] =    { NULL,        NULL,     P_NONE },
};

static kuprule *ku_getrule(kuvm *vm, kutok_t optype) {
  return &ku_rules[optype];
}

// ********************** virtual machine **********************
void ku_reset(kuvm *vm) {
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

static bool ku_falsy(kuval v) {
  return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v));
}

kuval ku_peek(kuvm *vm, int distance) {
  return vm->sp[-1 - distance];
}

kuvm *ku_new(void) {
  kuvm *vm = malloc(sizeof(kuvm));
  if (!vm) {
    return NULL;
  }
  vm->allocated = sizeof(kuvm);
  vm->max_params = 255;
  vm->max_const = UINT8_MAX;
  vm->max_closures = UPSTACK_MAX;
  vm->max_jump = UINT16_MAX;
  vm->max_body = UINT16_MAX;
  vm->max_frames = FRAMES_MAX;
  vm->max_locals = LOCALS_MAX;
  
  vm->flags = 0;
  vm->curclass = NULL;
  vm->gcnext = 1024*1024;
  vm->gccount = 0;
  vm->gccap = 0;
  vm->gcstack = NULL;
  vm->err = false;
  vm->objects = NULL;
  vm->openupvals = NULL;
  vm->compiler = NULL;
  ku_tabinit(vm, &vm->strings);
  ku_tabinit(vm, &vm->globals);
  ku_reset(vm);
  vm->initstr = NULL; // GC can run when we do str_copy below
  vm->initstr = ku_strfrom(vm, "init", 4);

  return vm;
}

static void ku_freeobjects(kuvm* vm) {
  kuobj* obj = vm->objects;
  while (obj != NULL) {
    if (vm->flags & KVM_F_GCLOG) {
      ku_printf(vm, "%p dangling ", (void*)obj);
      ku_printval(vm, OBJ_VAL(obj));
      ku_printf(vm, "\n");
    }
    kuobj* next = (kuobj*)obj->next;
    ku_objfree(vm, obj);
    obj = next;
  }
}

void ku_free(kuvm *vm) {
  vm->initstr = NULL; // free_objects will take care of it
  ku_freeobjects(vm);
  ku_tabfree(vm, &vm->strings);
  ku_tabfree(vm, &vm->globals);
  vm->allocated -= sizeof(kuvm);
  assert(vm->allocated == 0);
  free(vm->gcstack);
  free(vm);
}

static void ku_err(kuvm *vm, const char *fmt, ...) {
  va_list args;
  char out[1024];

  vm->err = true;
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
  
  ku_reset(vm);
}

static bool ku_docall(kuvm *vm, kuclosure *cl, int argc) {
  if (argc != cl->func->argc) {
    ku_err(vm, "%d expected got %d", cl->func->argc, argc);
    return false;
  }
  
  if (vm->framecount == vm->max_frames) {
    ku_err(vm, "stack overflow");
    return false;
  }
  
  kuframe *frame = &vm->frames[vm->framecount++];
  frame->closure = cl;
  frame->ip = cl->func->chunk.code;
  frame->bp = vm->sp - argc - 1;
  return true;
}

static bool ku_callvalue(kuvm *vm, kuval callee, int argc, bool *native) {
  *native = false;
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_CCLASS: {
        kucclass *cc = AS_CCLASS(callee);
        if (cc->cons) {
          kuval res = cc->cons(vm, argc, vm->sp - argc);
          vm->sp -= argc + 1;
          ku_push(vm, res);
          *native = true;
          if (!ku_objis(res, OBJ_CINST)) {
            ku_err(vm, "invalid instance");
          } else {
            kunobj *i = AS_CINST(res);
            i->klass = cc;
          }
          return !vm->err;
        }
        break;
      }
      case OBJ_CFUNC: {
        cfunc cf = AS_CFUNC(callee);
        kuval res = cf(vm, argc, vm->sp - argc);
        vm->sp -= argc + 1;
        ku_push(vm, res);
        *native = true;
        return !vm->err;
      }
      case OBJ_CLOSURE:
        return ku_docall(vm, AS_CLOSURE(callee), argc);
      case OBJ_CLASS: {
        kuclass *c = AS_CLASS(callee);
        kuval initfn;
        vm->sp[-argc - 1] = OBJ_VAL(ku_instnew(vm, c));
        if (ku_tabget(vm, &c->methods, vm->initstr, &initfn)) {
          return ku_docall(vm, AS_CLOSURE(initfn), argc);
        } else if (argc != 0) {
          ku_err(vm, "no args expected got %d", argc);
          return false;
        }
        return true;
      }
        
      case OBJ_BOUND_METHOD: {
        kubound *bm = AS_BOUND_METHOD(callee);
        vm->sp[-argc - 1] = bm->receiver;
        return ku_docall(vm, bm->method, argc);
      }
        
      case OBJ_FUNC: // not allowed anymore
      default:
        break;
    }
  }
  ku_err(vm, "not callable");
  return false;
}

#ifdef TRACE_ENABLED
// We can't use vm->compiler at runtime
static kuchunk *ku_chunk_runtime(kuvm *vm) {
  return &vm->frames[vm->framecount-1].closure->func->chunk;
}
#endif // TRACE_ENABLED

static kuxobj *ku_capture(kuvm *vm, kuval *local) {
  kuxobj *prev = NULL;
  kuxobj *uv = vm->openupvals;
  
  while (uv != NULL && uv->location > local) {
    prev = uv;
    uv = uv->next;
  }
  
  if (uv != NULL && uv->location == local) {
    return uv;
  }
  
  kuxobj *created = ku_xobjnew(vm, local);
  
  created->next = uv;
  
  if (prev == NULL) {
    vm->openupvals = created;
  } else {
    prev->next = created;
  }
  return created;
}

static void ku_close(kuvm *vm, kuval *last) {
  while (vm->openupvals != NULL && vm->openupvals->location >= last) {
    kuxobj *uo = vm->openupvals;
    uo->closed = *uo->location;
    uo->location = &uo->closed;
    vm->openupvals = uo->next;
  }
}

static void ku_defmethod(kuvm *vm, kustr *name) {
  kuval method = ku_peek(vm, 0);
  kuclass *c = AS_CLASS(ku_peek(vm, 1));
//  ku_printf(vm, ">> class %p method %s\n", (void*)c, name->chars);
  ku_tabset(vm, &c->methods, name, method);
  ku_pop(vm);
}

static bool ku_bindmethod(kuvm *vm, kuclass *klass, kustr *name) {
  kuval method;
  if (!ku_tabget(vm, &klass->methods, name, &method)) {
    ku_err(vm, "undefined property %s", name->chars);
    return false;
  }
  kubound *bm = ku_boundnew(vm, ku_peek(vm,0), AS_CLOSURE(method));
  ku_pop(vm);
  ku_push(vm, OBJ_VAL(bm));
  return true;
}

static bool ku_classinvoke(kuvm *vm, kuclass *klass, kustr *name, int argc) {
  kuval method;
  if (!ku_tabget(vm, &klass->methods, name, &method)) {
    ku_err(vm, "undefined property '%s'", name->chars);
    return false;
  }
  return ku_docall(vm, AS_CLOSURE(method), argc);
}

static bool ku_invoke(kuvm *vm, kustr *name, int argc, bool *native) {
  kuval receiver = ku_peek(vm, argc);
  *native = false;
  if (IS_CCLASS(receiver)) {
    kucclass *cc = AS_CCLASS(receiver);
    if (cc->scall) {
      *native = true;
      kuval v = cc->scall(vm, name, argc, vm->sp - argc);
      vm->sp -= argc +1;
      ku_push(vm, v);
      return true;
    }
    return false;
  }
  if (!IS_INSTANCE(receiver)) {
    ku_err(vm, "instance expected");
    return false;
  }
  kuiobj *inst = AS_INSTANCE(receiver);
  
  kuval val;
  if (ku_tabget(vm, &inst->fields, name, &val)) {
    vm->sp[-argc - 1] = val;
    bool native;
    return ku_callvalue(vm, val, argc, &native);
  }
  
  return ku_classinvoke(vm, inst->klass, name, argc);
}

kures ku_run(kuvm *vm) {
  kuframe *frame = &vm->frames[vm->framecount - 1];
  
  vm->err = false;
  kures res = KVM_CONT;
  while (res == KVM_CONT) {
    uint8_t op;

#ifdef TRACE_ENABLED
    if (vm->flags & KVM_F_TRACE) {
      kuchunk *ck = ku_chunk_runtime(vm);
      ku_bytedis(vm, ck, (int) (frame->ip - ck->code));
    }
#endif // TRACE_ENABLED

    switch(op = BYTE_READ(vm)) {
      case OP_CALL: {
        int argc = BYTE_READ(vm);
        bool native;
        if (!ku_callvalue(vm, ku_peek(vm, argc), argc, &native)) {
          return KVM_ERR_RUNTIME;
        }
        if (!native) {
          frame = &vm->frames[vm->framecount - 1];
        }
        break;
      }
        
      case OP_CLASS: {
        ku_push(vm, OBJ_VAL(ku_classnew(vm, READ_STRING(vm))));
        break;
      }
        
      case OP_METHOD: {
        ku_defmethod(vm, READ_STRING(vm));
        break;
      }
      case OP_INVOKE: {
        bool native;
        kustr *method = READ_STRING(vm);
        int argc = BYTE_READ(vm);
        if (!ku_invoke(vm, method, argc, &native)) {
          return KVM_ERR_RUNTIME;
        }
        if (!native) {
          frame = &vm->frames[vm->framecount - 1];
        }
        break;
      }
        
      case OP_SUPER_INVOKE: {
        kustr *method = READ_STRING(vm);
        int argc = BYTE_READ(vm);
        kuclass *superclass = AS_CLASS(ku_pop(vm));
        if (!ku_classinvoke(vm, superclass, method, argc)) {
          return KVM_ERR_RUNTIME;
        }
        frame = &vm->frames[vm->framecount - 1];
        break;
      }
      case OP_INHERIT: {
        kuval sc = ku_peek(vm, 1);

        if (!IS_CLASS(sc)) {
          ku_err(vm, "superclass must be a class");
          return KVM_ERR_RUNTIME;
        }
        kuclass *subclass = AS_CLASS(ku_peek(vm, 0));
        kuclass *superclass = AS_CLASS(sc);
//        ku_printf(vm, ">> class %p inherits from %p\n", (void*)subclass, (void*)superclass);
        ku_tabcopy(vm, &superclass->methods, &subclass->methods);
        ku_pop(vm); // subclass
        break;
      }
      case OP_CLOSURE: {
        kufunc *fn = AS_FUNC(CONST_READ(vm));
        ku_push(vm, OBJ_VAL(fn));  // for GC
        kuclosure *cl = ku_closurenew(vm, fn);
        ku_pop(vm);
        ku_push(vm, OBJ_VAL(cl));
        for (int i = 0; i < cl->upcount; i++) {
          uint8_t local = BYTE_READ(vm);
          uint8_t index = BYTE_READ(vm);
          if (local) {
            cl->upvals[i] = ku_capture(vm, frame->bp + index);
          } else {
            cl->upvals[i] = frame->closure->upvals[index];
          }
        }
        break;
      }
      case OP_CLOSE_UPVAL: {
        ku_close(vm, vm->sp - 1);
        ku_pop(vm);
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
        ku_push(vm, BOOL_VAL(ku_equal(a, b)));
        break;
      }
      case OP_RET: {
        kuval v = ku_pop(vm);
        ku_close(vm, frame->bp);
        vm->framecount--;
        if (vm->framecount <= 0) {
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
        if (! IS_NUM(ku_peek(vm, 0))) {
          ku_err(vm, "number expected" );
          return KVM_ERR_RUNTIME;
        }
        kuval v = ku_pop(vm);
        double dv = AS_NUM(v);
        kuval nv = NUM_VAL(-dv);
        ku_push(vm, nv);
        break;
      }
      
      case OP_POP: 
        ku_pop(vm);
        break;

      case OP_GET_GLOBAL: {
        kustr* name = READ_STRING(vm);
        kuval value;

        if (!ku_tabget(vm, &vm->globals, name, &value)) {
          ku_err(vm, "undefined variable %s", name->chars);
          return KVM_ERR_RUNTIME;
        }
        ku_push(vm, value);
        break;
      }

      case OP_DEF_GLOBAL: {
        kustr* name = READ_STRING(vm);
        ku_tabset(vm, &vm->globals, name, ku_peek(vm, 0));
        ku_pop(vm);
        break;
      }

      case OP_SET_GLOBAL: {
        kustr* name = READ_STRING(vm);
        if (ku_tabset(vm, &vm->globals, name, ku_peek(vm, 0))) {
          ku_tabdel(vm, &vm->globals, name);
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
        frame->bp[slot] = ku_peek(vm, 0);
        break;
      }
        
      case OP_GET_UPVAL: {
        uint8_t slot = BYTE_READ(vm);
        ku_push(vm, *frame->closure->upvals[slot]->location);
        break;
      }
        
      case OP_SET_UPVAL: {
        uint8_t slot = BYTE_READ(vm);
        *frame->closure->upvals[slot]->location = ku_peek(vm, 0);
        break;
      }
        
      case OP_GET_PROP: {
        if (IS_CCLASS(ku_peek(vm, 0))) {
          kucclass *cc = AS_CCLASS(ku_peek(vm, 0));
          if (cc->sget) {
            kustr *name = READ_STRING(vm);
            kuval ret = cc->sget(vm, name);
            ku_pop(vm); // instance
            ku_push(vm, ret);
            break;
          }
        }
        
        if (IS_CINST(ku_peek(vm,0))) {
          kunobj *i = AS_CINST(ku_peek(vm, 0));
          if (i->klass->iget) {
            kustr *name = READ_STRING(vm);
            kuval ret = i->klass->iget(vm, (kuobj*)i, name);
            ku_pop(vm); // instance
            ku_push(vm, ret);
            break;
          }
        }
        if (!IS_INSTANCE(ku_peek(vm, 0))) {
          ku_err(vm, "instance expected");
          return KVM_ERR_RUNTIME;
        }
        kuiobj *i = AS_INSTANCE(ku_peek(vm, 0));
        kustr *name = READ_STRING(vm);
        kuval val;
        if (ku_tabget(vm, &i->fields, name, &val)) {
          ku_pop(vm); // pop instance
          ku_push(vm, val);
          break;
        }
        
        if (!ku_bindmethod(vm, i->klass, name)) {
          return KVM_ERR_RUNTIME;
        }
        break;
      }
        
      case OP_SET_PROP: {
        if (IS_CCLASS(ku_peek(vm, 1))) {
          kucclass *cc = AS_CCLASS(ku_peek(vm, 1));
          if (cc->sput) {
            kustr *name = READ_STRING(vm);
            kuval val = ku_pop(vm);
            ku_pop(vm); // cclass
            val = cc->sput(vm, name, val);
            ku_push(vm, val);
            break;
          }
        }

        if (!IS_INSTANCE(ku_peek(vm, 1))) {
          ku_err(vm, "instance expected");
          return KVM_ERR_RUNTIME;
        }
        kuiobj *i = AS_INSTANCE(ku_peek(vm, 1));
        ku_tabset(vm, &i->fields, READ_STRING(vm), ku_peek(vm, 0));
        kuval val = ku_pop(vm);
        ku_pop(vm); // instance
        ku_push(vm, val);
        break;
      }
        
      case OP_GET_SUPER: {
        kustr *name = READ_STRING(vm);
        kuval v = ku_pop(vm);
        kuclass *superclass = AS_CLASS(v);
        if (!ku_bindmethod(vm, superclass, name)) {
          return KVM_ERR_RUNTIME;
        }
        break;
      }
      case OP_ADD: {
        if (IS_STR(ku_peek(vm, 0)) && IS_STR(ku_peek(vm, 1))) {
          ku_strcat(vm);
        }
        else if (IS_NUM(ku_peek(vm, 0)) && IS_NUM(ku_peek(vm, 1))) {
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
        ku_push(vm, BOOL_VAL(ku_falsy(ku_pop(vm))));
        break;
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT(vm);
        if (ku_falsy(ku_peek(vm, 0))) {
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
    
#ifdef TRACE_ENABLED
    if (vm->flags & KVM_F_TRACE && vm->flags & KVM_F_STACK) {
     ku_printstack(vm);
    } else {
      if (vm->flags & KVM_F_TRACE) {
        ku_printf(vm, "\n");
      }
    }
#endif // TRACE_ENABLED
  }
  return KVM_OK;
}

kufunc *ku_compile(kuvm *vm, char *source) {
  kucomp compiler;
  ku_compinit(vm, &compiler, FUNC_MAIN);
  ku_lexinit(vm, source);
  vm->parser.err = false;
  vm->parser.panic = false;
  ku_pnext(vm);
  while (!ku_pmatch(vm, TOK_EOF)) {
    ku_decl(vm, NULL);
  }

  if (vm->flags & KVM_F_DISASM) {
    ku_chunkdump(vm, ku_chunk(vm), "code");
  }

  kufunc *fn = ku_pend(vm, false);
  

  return vm->parser.err ? NULL : fn;
}

kures ku_exec(kuvm *vm, char *source) {
  kufunc *fn = ku_compile(vm, source);
  if (fn == NULL) {
    return KVM_ERR_SYNTAX;
  }

  if (vm->flags & KVM_F_LIST) {
    ku_chunkdump(vm, &fn->chunk, fn->name ? fn->name->chars : "__main__");
  }
    
  if (vm->flags & KVM_F_NOEXEC) {
    return KVM_OK;
  }
  
  ku_push(vm, OBJ_VAL(fn));
  kuclosure *closure = ku_closurenew(vm, fn);
  ku_pop(vm);
  ku_push(vm, OBJ_VAL(closure));
  ku_docall(vm, closure, 0);
  return ku_run(vm);
}

// ********************** memory **********************
char *ku_alloc(kuvm *vm, void *ptr, size_t oldsize, size_t nsize) {
  if ((vm->flags & KVM_F_GCSTRESS) && nsize > oldsize) {
    ku_gc(vm);
  }
  
  if (vm->allocated > vm->gcnext) {
    if (vm->flags & KVM_F_GCLOG) {
      ku_printf(vm, "%zu allocated %zu next -> gc()\n",
                vm->allocated, vm->gcnext);
    }
    ku_gc(vm);
  }
  
  if (vm->flags & KVM_F_TRACEMEM) {
    ku_printf(vm, "malloc %d -> %d\n", (int)oldsize, (int)nsize);
  }
  
  vm->allocated += nsize - oldsize;
  
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  return realloc(ptr, nsize);
}

// ********************** chunk **********************
void ku_chunkinit(kuvm *vm, kuchunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  ku_arrinit(vm, &chunk->constants);
}

void ku_chunkwrite(kuvm *vm, kuchunk *chunk, uint8_t byte, int line) {
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

void ku_chunkfree(kuvm *vm, kuchunk *chunk) {
  ARRAY_FREE(vm, uint8_t, chunk->code, chunk->capacity);
  ARRAY_FREE(vm, int, chunk->lines, chunk->capacity);
  ARRAY_FREE(vm, kuval, chunk->constants.values, chunk->constants.capacity);
}

int ku_chunkconst(kuvm *vm, kuchunk *chunk, kuval value) {
  ku_push(vm, value); // for GC
  ku_arrwrite(vm, &chunk->constants, value);
  ku_pop(vm);
  return chunk->constants.count - 1;
}

// ********************** array **********************
void ku_arrinit(kuvm* vm, kuarr *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void ku_arrwrite(kuvm* vm, kuarr *array, kuval value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = CAPACITY_GROW(old);
    array->values = ARRAY_GROW(vm, kuval, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

// ********************** debugging **********************
static void ku_chunkdump(kuvm *vm, kuchunk *chunk, const char * name) {
  ku_printf(vm, "== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; ) {
    offset = ku_bytedis(vm, chunk, offset);
    ku_printf(vm, "\n");
  }
}

static int ku_opdis(kuvm *vm, const char *name, int offset) {
  ku_printf(vm, "%-16s", name);
  return offset + 1;
}

static int ku_constdis(kuvm *vm, const char *name, kuchunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  ku_printf(vm, "%-16s %4d '", name, con);
  ku_printval(vm, chunk->constants.values[con]);
  ku_printf(vm, "'");
  return offset+2;
}

static int ku_invokedis(kuvm *vm, const char *name, kuchunk *chunk, int offset) {
  uint8_t cons = chunk->code[offset + 1];
  uint8_t argc = chunk->code[offset + 2];
  ku_printf(vm, "%-16s (%d args) %4d'", name, argc, cons);
  ku_printval(vm, chunk->constants.values[cons]);
  ku_printf(vm, "'\n");
  return offset + 3;
}

int ku_bytedis(kuvm *vm, kuchunk *chunk, int offset) {
  ku_printf(vm, "%04d ", offset);

  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset-1]) {
    ku_printf(vm, "   | ");
  } else {
    ku_printf(vm, "%4d ", chunk->lines[offset]);
  }
  uint8_t op = chunk->code[offset];
  switch (op) {
    case OP_RET: return ku_opdis(vm, "OP_RET", offset);
    case OP_NEG: return ku_opdis(vm, "OP_NEG", offset);
    case OP_ADD: return ku_opdis(vm, "OP_ADD", offset);
    case OP_SUB: return ku_opdis(vm, "OP_SUB", offset);
    case OP_MUL: return ku_opdis(vm, "OP_MUL", offset);
    case OP_DIV: return ku_opdis(vm, "OP_DIV", offset);
    case OP_NIL: return ku_opdis(vm, "OP_NIL", offset);
    case OP_TRUE: return ku_opdis(vm, "OP_TRUE", offset);
    case OP_FALSE: return ku_opdis(vm, "OP_FALSE", offset);
    case OP_GT: return ku_opdis(vm, "OP_GT", offset);
    case OP_LT: return ku_opdis(vm, "OP_LT", offset);
    case OP_EQ: return ku_opdis(vm, "OP_EQ", offset);
    case OP_POP: return ku_opdis(vm, "OP_POP", offset);
    case OP_CLASS: return ku_constdis(vm, "OP_CLASS", chunk, offset);
    case OP_METHOD: return ku_constdis(vm, "OP_METHOD", chunk, offset);
    case OP_INVOKE: return ku_invokedis(vm, "OP_INVOKE", chunk, offset);
    case OP_SUPER_INVOKE: return ku_invokedis(vm, "OP_SUPER_INVOKE", chunk, offset);
    case OP_INHERIT: return ku_opdis(vm, "OP_INHERIT", offset);
    case OP_CONST:
      return ku_constdis(vm, "OP_CONST", chunk, offset);
    case OP_DEF_GLOBAL:
      return ku_constdis(vm, "OP_DEF_GLOBAL", chunk, offset);
    case OP_GET_GLOBAL:
      return ku_constdis(vm, "OP_GET_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL:
      return ku_constdis(vm, "OP_SET_GLOBAL", chunk, offset);
    case OP_GET_LOCAL:
      return ku_opslotdis(vm, "OP_GET_LOCAL", chunk, offset);
    case OP_SET_LOCAL:
      return ku_opslotdis(vm, "OP_SET_LOCAL", chunk, offset);
    case OP_GET_UPVAL:
      return ku_opslotdis(vm, "OP_GET_UPVAL", chunk, offset);
    case OP_SET_PROP:
      return ku_opslotdis(vm, "OP_SET_PROP", chunk, offset);
    case OP_GET_PROP:
      return ku_opslotdis(vm, "OP_GET_PROP", chunk, offset);
    case OP_SET_UPVAL:
      return ku_opslotdis(vm, "OP_SET_UPVAL", chunk, offset);
    case OP_GET_SUPER:
      return ku_constdis(vm, "OP_GET_SUPER", chunk, offset);
    case OP_JUMP:
      return ku_jumpdis(vm, "OP_JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE:
      return ku_jumpdis(vm, "OP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_LOOP:
      return ku_jumpdis(vm, "OP_LOOP", -1, chunk, offset);
    case OP_CALL:
      return ku_opslotdis(vm, "OP_CALL", chunk, offset);
    case OP_CLOSE_UPVAL:
      return ku_opdis(vm, "OP_CLOSE_UPVAL", offset);
    case OP_CLOSURE: {
      offset++;
      uint8_t con = chunk->code[offset++];
      ku_printf(vm, "%-16s %4d", "OP_CLOSURE", con);
      ku_printval(vm, chunk->constants.values[con]);
      ku_printf(vm, "\n");
      kufunc *fn = AS_FUNC(chunk->constants.values[con]);
      for (int j = 0; j < fn->upcount; j++) {
        int local = chunk->code[offset++];
        int index = chunk->code[offset++];
        ku_printf(vm, "%04d | %s %d\n", offset - 2, local ? "local" : "upval", index);
      }
      
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
void ku_compinit(kuvm *vm, kucomp *compiler, kufunc_t type) {
  compiler->function = NULL; // for GC
  compiler->enclosing = vm->compiler;
  compiler->count = 0;
  compiler->depth = 0;
  compiler->type = type;
  compiler->function = ku_funcnew(vm);
  
  vm->compiler = compiler;
  if (type != FUNC_MAIN) {
    compiler->function->name = ku_strfrom(vm, vm->parser.prev.start, vm->parser.prev.len);
  }
  kulocal *local = &vm->compiler->locals[vm->compiler->count++];
  local->depth = 0;
  local->captured = false;
  
  if (type != FUNC_STD) {
    local->name.start = "this";
    local->name.len = 4;
  } else {
    local->name.start = "";
    local->name.len = 0;
  }
}

void ku_block(kuvm *vm, kuloop *loop) {
  while (!ku_pcheck(vm, TOK_RBRACE) && !ku_pcheck(vm, TOK_EOF)) {
    ku_decl(vm, loop);
  }
  ku_pconsume(vm, TOK_RBRACE, "'}' expected");
}

void ku_beginscope(kuvm *vm) {
  vm->compiler->depth++;
}

void ku_endscope(kuvm *vm) {
  vm->compiler->depth--;
  
  while (vm->compiler->count > 0 &&
         vm->compiler->locals[vm->compiler->count - 1].depth >
         vm->compiler->depth) {
    
    if (vm->compiler->locals[vm->compiler->count - 1].captured) {
      ku_emitbyte(vm, OP_CLOSE_UPVAL);
    } else {
      ku_emitbyte(vm, OP_POP);
    }
    vm->compiler->count--;
    }
}

void ku_declare_var(kuvm *vm) {
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
      ku_perr(vm, "local already defined");
    }
  }
  ku_addlocal(vm, *name);
}

void ku_addlocal(kuvm *vm, kutok name) {
  if (vm->compiler->count == vm->max_locals) {
    ku_perr(vm, "too many locals");
    return;
  }
  
  kulocal *local = &vm->compiler->locals[vm->compiler->count++];
  local->name = name;
  local->depth = -1;
  local->captured = false;
}

bool ku_identeq(kuvm *vm, kutok *a, kutok *b) {
  if (a->len != b->len) {
    return false;
  }
  
  return memcmp(a->start, b->start, a->len) == 0;
}

int ku_resolvelocal(kuvm *vm, kucomp *compiler, kutok *name) {
  for (int i = compiler->count - 1; i >= 0; i--) {
    kulocal *local = &compiler->locals[i];
    if (ku_identeq(vm, name, &local->name)) {
      if (local->depth == -1) {
        ku_perr(vm, "own initialization disallowed");
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

int ku_opslotdis(kuvm *vm, const char *name, kuchunk *chunk, int offset) {
  uint8_t slot = chunk->code[offset + 1];
  ku_printf(vm, "%-16s %4d", name, slot);
  return offset + 2;
}

// ********************** function **********************
kufunc *ku_funcnew(kuvm *vm) {
  kufunc *fn = (kufunc*)KALLOC_OBJ(vm, kufunc, OBJ_FUNC);
  fn->argc = 0;
  fn->upcount = 0;
  fn->name = NULL;
  ku_chunkinit(vm, &fn->chunk);
  return fn;
}

// ********************** native functions **********************
kucfunc *ku_cfuncnew(kuvm *vm, cfunc f) {
  kucfunc *cf = KALLOC_OBJ(vm, kucfunc, OBJ_CFUNC);
  cf->fn = f;
  return cf;
}

void ku_cfuncdef(kuvm *vm, const char *name, cfunc f) {
  int len = (int)strlen(name);
  kustr *sname = ku_strfrom(vm, name, len);
  ku_push(vm, OBJ_VAL(sname));
  ku_push(vm, OBJ_VAL(ku_cfuncnew(vm, f)));
  ku_tabset(vm, &vm->globals, AS_STR(vm->stack[0]), vm->stack[1]);
  ku_pop(vm);
  ku_pop(vm);
}

// ********************** library **********************
#include <time.h>
static kuval ku_clock(kuvm *vm, int argc, kuval *argv) {
  return NUM_VAL((double)clock() / CLOCKS_PER_SEC);
}

static kuval ku_strlen(kuvm *vm, int argc, kuval *argv) {
  if (argc < 1 || !IS_STR(argv[0])) {
    ku_err(vm, "strlen string expected");
    return NIL_VAL;
  }
  return NUM_VAL(strlen(AS_STR(argv[0])->chars));
}

static kuval ku_print(kuvm *vm, int argc, kuval *argv) {
  for (int a = 0; a < argc; a++) {
    kuval v = argv[a];
    ku_printval(vm, v);
  }
  return NIL_VAL;
}

#include <math.h>

#define M2(c,s) (c->len==2 && c->chars[0]==s[0] && \
                              c->chars[1]==s[1])

#define M3(c,s) (c->len==3 && c->chars[0]==s[0] && \
                              c->chars[1]==s[1] && \
                              c->chars[2]==s[2])

kuval math_sget(kuvm *vm, kustr *p) {
  if (M2(p, "pi")) {
    return NUM_VAL(M_PI);
  }
  return NIL_VAL;
}

kuval math_scall(kuvm *vm, kustr *m, int argc, kuval *argv) {
  double x = AS_NUM(argv[0]);
  
  if (M3(m, "sin")) {
    return NUM_VAL(sin(x));
  } else if (M3(m, "cos")) {
    return NUM_VAL(cos(x));
  } else if (M3(m, "tan")) {
    return NUM_VAL(tan(x));
  }
  return NIL_VAL;
}

void ku_reglibs(kuvm *vm) {
  ku_cfuncdef(vm, "clock", ku_clock);
  ku_cfuncdef(vm, "printf", ku_print);
  ku_cfuncdef(vm, "strlen", ku_strlen);
  
  kucclass *math = ku_cclassnew(vm, "math");
  math->sget = math_sget;
  math->scall = math_scall;
  ku_cclassdef(vm, math);
}

// ********************** closure **********************
kuclosure *ku_closurenew(kuvm *vm, kufunc *f) {
  ku_push(vm, OBJ_VAL(f)); // for GC
  kuclosure *cl = KALLOC_OBJ(vm, kuclosure, OBJ_CLOSURE);
  cl->func = f;
  cl->upcount = 0;  // for GC
  cl->upvals = NULL; // for GC
  ku_push(vm, OBJ_VAL(cl));
  kuxobj **upvals = KALLOC(vm, kuxobj*, f->upcount);
  ku_pop(vm);
  ku_pop(vm);
  for (int i = 0; i < f->upcount; i++) {
    upvals[i] = NULL;
  }
  cl->upvals = upvals;
  cl->upcount = f->upcount;
  return cl;
}

// ********************** upvalue **********************
kuxobj *ku_xobjnew(kuvm *vm, kuval *slot) {
  kuxobj *uo = KALLOC_OBJ(vm, kuxobj, OBJ_UPVAL);
  uo->location = slot;
  uo->next = NULL;
  uo->closed = NIL_VAL;
  return uo;
}

// ********************** garbage collection **********************
static void ku_markval(kuvm *vm, kuval v);
static void ku_markobj(kuvm *vm, kuobj *o);
static void ku_marktable(kuvm *vm, kutab *tab);

static void ku_markarray(kuvm *vm, kuarr *array) {
  for (int i = 0; i < array->count; i++) {
    ku_markval(vm, array->values[i]);
  }
}

static void ku_traceobj(kuvm *vm, kuobj *o) {
  
  if (vm->flags & KVM_F_GCLOG) {
    ku_printf(vm, "%p trace ", (void*)o);
    ku_printval(vm, OBJ_VAL(o));
    ku_printf(vm, "\n");
  }
  
  switch (o->type) {
    case OBJ_STR:
    case OBJ_CFUNC:
      break;
      
    case OBJ_CINST: {
      kunobj *i = (kunobj*)o;
      ku_markobj(vm, (kuobj*)i->klass);
      if (i->klass->imark) {
        i->klass->imark(vm, o);
      }
      break;
    }
    case OBJ_UPVAL:
      ku_markval(vm, ((kuxobj*)o)->closed);
      break;
      
    case OBJ_FUNC: {
      kufunc *fn = (kufunc*)o;
      ku_markobj(vm, (kuobj*)fn->name);
      ku_markarray(vm, &fn->chunk.constants);
      break;
    }
      
    case OBJ_CCLASS: {
      kucclass *cc = (kucclass*)o;
      ku_markobj(vm, (kuobj*)cc->name);
      if (cc->smark) {
        cc->smark(vm, o);
      }
      break;
    }
    case OBJ_CLASS: {
      kuclass *c = (kuclass*)o;
      ku_markobj(vm, (kuobj*)c->name);
      ku_marktable(vm, &c->methods);
      break;
    }
      
    case OBJ_INSTANCE: {
      kuiobj *i = (kuiobj*)o;
      ku_markobj(vm, (kuobj*)i->klass);
      ku_marktable(vm, &i->fields);
      break;
    }
      
    case OBJ_BOUND_METHOD: {
      kubound *bm = (kubound*)o;
      ku_markval(vm, bm->receiver);
      ku_markobj(vm, (kuobj*)bm->method);
      break;
    }
    case OBJ_CLOSURE: {
      kuclosure *cl = (kuclosure*)o;
      ku_markobj(vm, (kuobj*)cl->func);
      for (int i = 0; i < cl->upcount; i++) {
        ku_markobj(vm, (kuobj*)cl->upvals[i]);
      }
      break;
    }
  }
}

static void ku_tracerefs(kuvm *vm) {
  while (vm->gccount > 0) {
    kuobj *o = vm->gcstack[--vm->gccount];
    ku_traceobj(vm, o);
  }
}

static void ku_markobj(kuvm *vm, kuobj *o) {
  if (o == NULL) {
    return;
  }
  
  if (o->marked) return;
  if (vm->flags & KVM_F_GCLOG) {
    ku_printf(vm, "%p mark ", (void*) o);
    ku_printval(vm, OBJ_VAL(o));
    ku_printf(vm, "\n");
  }
  
  o->marked = true;
  
  if (vm->gccap < vm->gccount + 1) {
    vm->gccap = CAPACITY_GROW(vm->gccap);
    vm->gcstack = (kuobj**)realloc(vm->gcstack, sizeof(kuobj*)*vm->gccap);
    
    assert (vm->gcstack != NULL);
  }
  vm->gcstack[vm->gccount++] = o;
}

static void ku_markval(kuvm *vm, kuval v) {
  if (IS_OBJ(v)) {
    ku_markobj(vm, AS_OBJ(v));
  }
}

static void ku_marktable(kuvm *vm, kutab *tab) {
  for (int i = 0; i < tab->capacity; i++) {
    kuentry *e = &tab->entries[i];
    ku_markobj(vm, (kuobj*)e->key);
    ku_markval(vm, e->value);
  }
}

static void ku_markcomproots(kuvm *vm) {
  kucomp *comp = vm->compiler;
  while (comp != NULL) {
    ku_markobj(vm, (kuobj*)comp->function);
    comp = comp->enclosing;
  }
}

static void ku_markroots(kuvm *vm) {
  for (kuval *pv = vm->stack; pv < vm->sp; pv++) {
    ku_markval(vm, *pv);
  }
  
  for (int i = 0; i < vm->framecount; i++) {
    ku_markobj(vm, (kuobj*)vm->frames[i].closure);
  }
  
  for (kuxobj *uo = vm->openupvals; uo != NULL; uo = uo->next) {
    ku_markobj(vm, (kuobj*)uo);
  }
  
  ku_marktable(vm, &vm->globals);
  ku_markcomproots(vm);
}

void ku_sweep(kuvm *vm) {
  kuobj *prev = NULL;
  kuobj *obj = vm->objects;
  
  while (obj != NULL) {
    if (obj->marked) {
      obj->marked = false;
      prev = obj;
      obj = obj->next;
    } else {
      kuobj *unreached = obj;
      obj = obj->next;
      if (prev != NULL) {
        prev->next = obj;
      } else {
        vm->objects = obj;
      }
      ku_objfree(vm, unreached);
    }
  }
}

static void ku_freeweak(kuvm *vm, kutab *table) {
  for (int i = 0; i < table->capacity; i++) {
    kuentry *e = &table->entries[i];
    if (e->key != NULL && !e->key->obj.marked) {
      ku_tabdel(vm, table, e->key);
    }
  }
}

#define GC_HEAP_GROW_FACTOR 2

void ku_gc(kuvm *vm) {
  if (vm->flags & KVM_F_GCLOG) {
    ku_printf(vm, "-- gc start\n");
  }
  
  size_t bytes = vm->allocated;
  
  ku_markroots(vm);
  ku_markobj(vm, (kuobj*)vm->initstr);
  ku_tracerefs(vm);
  ku_freeweak(vm, &vm->strings);
  ku_sweep(vm);
  
  vm->gcnext = vm->allocated * GC_HEAP_GROW_FACTOR;
  
  if (vm->flags & KVM_F_GCLOG) {
    ku_printf(vm, "-- gc end\n");
    ku_printf(vm, "collected %zu bytes (%zu -> %zu) next %zu\n",
              bytes - vm->allocated, bytes, vm->allocated, vm->gcnext);
  }
}

// ********************** class **********************
kuclass *ku_classnew(kuvm *vm, kustr *name) {
  kuclass *c = KALLOC_OBJ(vm, kuclass, OBJ_CLASS);
  c->name = name;
  ku_tabinit(vm, &c->methods);
  return c;
}

// ********************** instance **********************
kuiobj *ku_instnew(kuvm *vm, kuclass *klass) {
  kuiobj *i = KALLOC_OBJ(vm, kuiobj, OBJ_INSTANCE);
  i->klass = klass;
  ku_tabinit(vm, &i->fields);
  return i;
}


// ********************** bound method **********************
kubound *ku_boundnew(kuvm *vm, kuval receiver, kuclosure *method) {
  kubound *bm = KALLOC_OBJ(vm, kubound, OBJ_BOUND_METHOD);
  bm->receiver = receiver;
  bm->method = method;
  return bm;
}

// ********************** print **********************
static void ku_printf(kuvm *vm, const char *fmt, ...) {
  va_list args;

  if (vm->flags & KVM_F_QUIET) {
    return;
  }
  
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

void ku_printmem(kuvm *vm) {
  ku_printf(vm, "delta: %ld\n", vm->allocated);
}

static void ku_printobj(kuvm* vm, kuval val) {
  switch (OBJ_TYPE(val)) {
    case OBJ_FUNC:
      ku_printfunc(vm, AS_FUNC(val));
      break;
    case OBJ_CFUNC:
      ku_printf(vm, "<cfunc>");
      break;
    case OBJ_CINST: {
      kunobj *i = AS_CINST(val);
      ku_printf(vm, "<%s instance>", i->klass->name->chars);
      break;
    }
    case OBJ_CCLASS: {
      kucclass *cc = AS_CCLASS(val);
      ku_printf(vm, "<class %s>", cc->name->chars);
      break;
    }
    case OBJ_CLOSURE:
      ku_printfunc(vm, AS_CLOSURE(val)->func);
      break;
  case OBJ_STR:
    ku_printf(vm, "%s", AS_CSTR(val));
    break;
    case OBJ_CLASS:
      ku_printf(vm, "%s", AS_CLASS(val)->name->chars);
      break;
    case OBJ_INSTANCE:
      ku_printf(vm, "%s instance", AS_INSTANCE(val)->klass->name->chars);
      break;
    case OBJ_BOUND_METHOD:
      ku_printfunc(vm, AS_BOUND_METHOD(val)->method->func);
      break;
    case OBJ_UPVAL:
      ku_printf(vm, "upvalue");
      break;
  }
}

void ku_printval(kuvm *vm, kuval value) {

#ifdef NAN_BOX
  if (IS_BOOL(value)) {
    ku_printf(vm, AS_BOOL(value) ? "true" : "false");
  } else if (IS_NIL(value)) {
    ku_printf(vm, "nil");
  } else if (IS_NUM(value)) {
    ku_printf(vm, "%g", AS_NUM(value));
  } else if (IS_OBJ(value)) {
    ku_printobj(vm, value);
  }
#else
  switch (value.type) {
    case VAL_BOOL:
    ku_printf(vm, "%s", (value.as.bval) ? "true": "false");
      break;
    case VAL_NIL:
      ku_printf(vm, "nil");
      break;
    case VAL_NUM:
      ku_printf(vm, "%g", value.as.dval);
      break;
    case VAL_OBJ:
      ku_printobj(vm, value);
      break;
  }
#endif
}


static void ku_printfunc(kuvm *vm, kufunc *fn) {
  ku_printf(vm, "<fn %s>", fn->name ? fn->name->chars : "__main__");
}

void ku_printstack(kuvm *vm) {
  ku_printf(vm, " [");
  for (kuval* vp = vm->stack; vp < vm->sp; vp++) {
    ku_printval(vm, *vp);
    if (vp < vm->sp - 1) {
      ku_printf(vm, ",");
    }
  }
  ku_printf(vm, "]");
  ku_printf(vm, "\n");
}

// ********************** loop patch **********************
void ku_loopinit(kuvm *vm, kuloop *loop) {
  loop->breakpatch.count = 0;
  loop->continuepatch.count = 0;
}

void ku_emitpatch(kuvm *vm, kupatch *patch, uint8_t op) {
  patch->offset[patch->count++] = ku_emitjump(vm, op);
}

void ku_patchall(kuvm *vm, kupatch *patch, uint16_t to, bool rev) {
  for (int i = 0; i < patch->count; i++) {
    kuchunk *c = ku_chunk(vm);
    uint16_t delta = (rev) ? patch->offset[i] - to + 2 : to - patch->offset[i];
    c->code[patch->offset[i]] = (delta >> 8) & 0xff;
    c->code[patch->offset[i]+1] = delta & 0xff;
  }
}


// ********************** native class **********************
kucclass *ku_cclassnew(kuvm *vm, const char *name) {
  int len = (int)strlen(name);
  kustr *sname = ku_strfrom(vm, name, len);

  kucclass *cc = KALLOC_OBJ(vm, kucclass, OBJ_CCLASS);
  cc->name = sname;
  cc->cons = NULL;
  cc->scall = NULL;
  cc->sget = NULL;
  cc->sput = NULL;
  cc->sfree = NULL;
  cc->smark = NULL;
  cc->icall = NULL;
  cc->iget = NULL;
  cc->iput = NULL;
  cc->ifree = NULL;
  cc->imark = NULL;
  return cc;
}

void ku_cclassdef(kuvm *vm, kucclass *cc) {
  ku_push(vm, OBJ_VAL(cc->name));
  ku_push(vm, OBJ_VAL((kuobj*)cc));
  ku_tabset(vm, &vm->globals, AS_STR(vm->stack[0]), vm->stack[1]);
  ku_pop(vm);
  ku_pop(vm);
}
