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

static void ku_obj_free(kuvm* vm, kuobj* obj) {
  switch (obj->type) {
  case OBJ_STR: {
    kustr* str = (kustr*)obj;
    ARRAY_FREE(vm, char, str->chars, str->len + 1);
    FREE(vm, kustr, obj);
    break;
  }
  }
}

static kustr* ku_str_alloc(kuvm* vm, char* chars, int len) {
  kustr* str = KALLOC_OBJ(vm, kustr, OBJ_STR);
  str->len = len;
  str->chars = chars;
  return str;
}

bool ku_obj_istype(kuval v, kuobjtype ot) {
  return IS_OBJ(v) && AS_OBJ(v)->type == ot;
}

static kustr* ku_str_copy(kuvm* vm, const char* chars, int len) {
  char* buff = KALLOC(vm, char, len + 1);
  memcpy(buff, chars, len);
  buff[len] = '\0';
  return ku_str_alloc(vm, buff, len);
}
bool ku_val_eq(kuval v1, kuval v2) {
  if (v1.type != v2.type) {
    return false;
  }
  switch (v1.type) {
    case VAL_NIL: return true;
    case VAL_BOOL: return v1.as.bval == v2.as.bval;
    case VAL_NUM: return v1.as.dval == v2.as.dval;
    case VAL_OBJ: {
      kustr* a = AS_STR(v1);
      kustr* b = AS_STR(v2);
      return a->len == b->len &&
        memcmp(a->chars, b->chars, a->len) == 0;
    default:
      break;
    }
  }
  return false;
}

static kustr* ku_str_take(kuvm* vm, char* buff, int len) {
  return ku_str_alloc(vm, buff, len);
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
// Scanner
// ------------------------------------------------------------
static char ku_lex_advance(kuvm *vm) {
  vm->scanner.curr++;
  return vm->scanner.curr[-1];
}


static void ku_lex_init(kuvm *vm, const char *source) {
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

static kutok ku_lex_scan(kuvm *vm) {
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
      kuprintf("%4d ", token.line);
    } else {
      kuprintf("  |  ");
    }
    kuprintf("%2d '%.*s'\n", token.type, token.len, token.start);
    
    if (token.type == TOK_EOF) {
      break;
    }
  }
}


// ------------------------------------------------------------
// Parser
// ------------------------------------------------------------
static void ku_parse_err_at(kuvm *vm, kutok *tok, const char *msg) {
  if (vm->parser.panic) return;
  vm->parser.panic = true;
  
  fprintf(stderr, "[line %d] error", tok->line);
  
  if (tok->type == TOK_EOF) {
    fprintf(stderr, " at end");
  } else if (tok->type == TOK_ERR) {
    // nothing
  } else {
    fprintf(stderr, " at '%.*s'", tok->len, tok->start);
  }
  
  fprintf(stderr, ": %s\n", msg);
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

kuchunk *ku_current_chunk(kuvm *vm) {
  return vm->chunk;
}

static void ku_parse_emit_byte(kuvm *vm, uint8_t byte) {
  ku_chunk_write(vm, ku_current_chunk(vm), byte, vm->parser.prev.line);
}

static void ku_parse_end(kuvm *vm) {
  ku_parse_emit_byte(vm, OP_RET);
}

static void ku_parse_emit_bytes(kuvm *vm, uint8_t b1, uint8_t b2) {
  ku_parse_emit_byte(vm, b1);
  ku_parse_emit_byte(vm, b2);
}


static uint8_t ku_parse_make_const(kuvm *vm, kuval val) {
  int cons = ku_chunk_add_const(vm, ku_current_chunk(vm), val);
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

typedef void (*ku_parse_func)(kuvm *);

typedef struct {
  ku_parse_func prefix;
  ku_parse_func infix;
  kup_precedence precedence;
} ku_parse_rule;

static ku_parse_rule *ku_parse_get_rule(kuvm *vm, kutoktype optype);

static void ku_parse_process(kuvm *vm, kup_precedence prec) {
  ku_parse_advance(vm);
  ku_parse_func prefix = ku_parse_get_rule(vm, vm->parser.prev.type)->prefix;
  if (prefix == NULL) {
    ku_parse_err(vm, "expected expression");
    return;
  }
  prefix(vm);
  
  while (prec <= ku_parse_get_rule(vm, vm->parser.curr.type)->precedence) {
    ku_parse_advance(vm);
    ku_parse_func infix = ku_parse_get_rule(vm, vm->parser.prev.type)->infix;
    infix(vm);
  }
}


static void ku_parse_literal(kuvm *vm) {
  switch (vm->parser.prev.type) {
    case TOK_FALSE: ku_parse_emit_byte(vm, OP_FALSE); break;
    case TOK_TRUE: ku_parse_emit_byte(vm, OP_TRUE); break;
    case TOK_NIL: ku_parse_emit_byte(vm, OP_NIL); break;
    default: return; // unreachable
  }
}

static void ku_parse_string(kuvm* vm) {
  ku_parse_emit_const(vm, OBJ_VAL(ku_str_copy(vm, 
            vm->parser.prev.start + 1,
            vm->parser.prev.len - 2)));
}

static void ku_parse_number(kuvm *vm) {
  double val = strtod(vm->parser.prev.start, NULL);
  ku_parse_emit_const(vm, NUM_VAL(val));
}

static void ku_parse_expression(kuvm *vm) {
  ku_parse_process(vm, P_ASSIGN);
}

static void ku_parse_grouping(kuvm *vm) {
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_RPAR, "')' expected");
}

static void ku_parse_unary(kuvm *vm) {
  kutoktype optype = vm->parser.prev.type;
  
  ku_parse_expression(vm);
  
  switch(optype) {
    case TOK_MINUS: ku_parse_emit_byte(vm, OP_NEG); break;
    case TOK_BANG: ku_parse_emit_byte(vm, OP_NOT); break;
    default: return;
  }
}


static void ku_parse_binary(kuvm *vm) {
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
  [TOK_LPAR] =      { ku_parse_grouping,   NULL,     P_NONE },
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
  [TOK_IDENT] =     { NULL,        NULL,     P_NONE },
  [TOK_STR] =       { ku_parse_string,     NULL,     P_NONE },
  [TOK_NUM] =       { ku_parse_number,     NULL,     P_NONE },
  [TOK_AND] =       { NULL,        NULL,     P_NONE },
  [TOK_CLASS] =     { NULL,        NULL,     P_NONE },
  [TOK_ELSE] =      { NULL,        NULL,     P_NONE },
  [TOK_FALSE] =     { ku_parse_literal,    NULL,     P_NONE },
  [TOK_FOR] =       { NULL,        NULL,     P_NONE },
  [TOK_FUN] =       { NULL,        NULL,     P_NONE },
  [TOK_IF] =        { NULL,        NULL,     P_NONE },
  [TOK_NIL] =       { ku_parse_literal,    NULL,     P_NONE },
  [TOK_OR] =        { NULL,        NULL,     P_NONE },
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
  vm->flags = 0;
  if (!vm) {
    return NULL;
  }

  vm->freed = 0;
  vm->stop = false;
  vm->chunk = NULL;
  vm->objects = NULL;
  ku_reset_stack(vm);
  return vm;
}

#ifdef KVM_TRACE
void ku_print_mem(kuvm *vm) {
  printf("allocated: %d, freed: %d, delta: %d\n",
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
}
#endif

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
  vm->freed += sizeof(kuvm);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

#ifdef KVM_TRACE
void ku_print_stack(kuvm *vm) {
  printf(" [");
  for (kuval* vp = vm->stack; vp < vm->sp; vp++) {
    ku_print_val(vm, *vp);
    if (vp < vm->sp - 1) {
      printf(",");
    }
  }
  printf("]");
  printf("\n");
}
#endif

static void ku_err(kuvm *vm, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fputs("\n", stderr);
  size_t instruction = vm->ip - vm->chunk->code - 1;
  int line = vm->chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  ku_reset_stack(vm);
}

static kures ku_runloop(kuvm *vm) {
#define BYTE_READ(vm) (*(vm->ip++))
#define CONST_READ(vm) (vm->chunk->constants.values[BYTE_READ(vm)])
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

  

  kures res = KVM_CONT;
  while (res == KVM_CONT) {
    uint8_t op;
#ifdef KVM_TRACE
    if (vm->flags & KVM_F_TRACE) {
     ku_print_op(vm, vm->chunk, (int) (vm->ip - vm->chunk->code));
    }
#endif

    switch(op = BYTE_READ(vm)) {
      case OP_NOP:
        break;
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
        break;;
      }
      case OP_RET: {
        res = KVM_OK;
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
    }
#ifdef KVM_TRACE
    if (vm->flags & KVM_F_TRACE && vm->flags & KVM_F_STACK) {
     ku_print_stack(vm);
    } else {
      if (vm->flags & KVM_F_TRACE) {
        kuprintf("\n");
      }
    }
#endif
  }
  return KVM_OK;
#undef BYTE_READ
#undef CONST_READ
#undef BIN_OP
}

kures ku_run(kuvm *vm, kuchunk *chunk) {
  vm->chunk = chunk;
  vm->ip = vm->chunk->code;
  return ku_runloop(vm);
}

static kures ku_compile(kuvm *vm, char *source, kuchunk *chunk) {
  ku_lex_init(vm, source);
  vm->parser.err = false;
  vm->parser.panic = false;
  vm->chunk = chunk;
  ku_parse_advance(vm);
  ku_parse_expression(vm);
  ku_parse_consume(vm, TOK_EOF, "expected expression end");
  ku_parse_end(vm);
  
#ifdef DEBUG_PRINT_CODE
  ku_print_chunk(vm, ku_current_chunk(vm), "code");
#endif
  return (vm->parser.err ? KVM_ERR_SYNTAX : KVM_OK);
}

static kures ku_exec(kuvm *vm, char *source) {
  kuchunk chunk;
  
  ku_chunk_init(vm, &chunk);
  
  if (ku_compile(vm, source, &chunk) != KVM_OK) {
    ku_chunk_free(vm, &chunk);
    return KVM_ERR_SYNTAX;
  }
  
  vm->ip = chunk.code;
  kures res = ku_run(vm, &chunk);
  
  if (vm->flags & KVM_F_LIST) {
    ku_print_chunk(vm, ku_current_chunk(vm), "code");
  }
  ku_chunk_free(vm, &chunk);
  return res;
}

#ifdef KVM_MAIN
static char *ku_readfile(kuvm *vm, const char *path) {
  FILE * file = fopen(path , "rb");
  
  if (file == NULL) {
    return NULL;
  }
  fseek(file , 0L , SEEK_END);
  size_t fsize = ftell(file);
  rewind(file);
  char * buffer = (char *) malloc(fsize + 1);
  size_t read = fread(buffer , sizeof (char), fsize, file);
  
  if (read < fsize) {
    free(buffer);
    return NULL;
  }
  buffer [read] = '\0' ;
  fclose(file);
  return buffer ;
}

kures ku_runfile(kuvm *vm, const char *file) {
  char *source = ku_readfile(vm, file);
  
  if (source == NULL) {
    return KVM_FILE_NOTFOUND;
  }
  kures res = ku_exec(vm, source);
  free(source);
  return res;
}


static bool ku_check_flag(kuvm *vm, char *line,
                       const char *name, uint64_t flag) {
  char buff[256];
  sprintf(buff, ".%s on\n", name);
  if (strcmp(line, buff) == 0) {
    vm->flags |= flag;
    printf("%s on\n", name);
    return true;
  }
  sprintf(buff, ".%s off\n", name);
  if (strcmp(line, buff) == 0) {
    vm->flags &= ~flag;
    printf("%s off\n", name);
    return true;
  }
  return false;
}
static void ku_repl(kuvm *vm) {
  printf("kumu %d.%d\n", KVM_MAJOR, KVM_MINOR);
  char line[1024];
  
  while(true) {
    printf("$ ");
    
    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }
    
    if (strcmp(line, ".quit\n") == 0) {
      break;
    }

    if (strcmp(line, ".test\n") == 0) {
      ku_test();
      continue;
    }
    
    if (ku_check_flag(vm, line, "trace", KVM_F_TRACE)) continue;
    if (ku_check_flag(vm, line, "stack", KVM_F_STACK)) continue;
    if (ku_check_flag(vm, line, "list", KVM_F_LIST)) continue;

    if (strcmp(line, ".mem\n") == 0) {
      ku_print_mem(vm);
      continue;
    }
    
    ku_exec(vm, line);
    if (vm->sp > vm->stack) {
      kuval v = ku_pop(vm);
      ku_print_val(vm, v);
      printf("\n");
    }

  }
}
#endif

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *ku_alloc(kuvm *vm, void *ptr, size_t oldsize, size_t nsize) {
  
#ifdef MEMORY_TRACE
  printf("malloc %d -> %d\n", (int)oldsize, (int)nsize);
#endif
  
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
  case OBJ_STR:
    printf("%s", AS_CSTR(val));
    break;
  }
}

void ku_print_val(kuvm *vm, kuval value) {
  switch (value.type) {
    case VAL_BOOL:
      printf("%s", (value.as.bval) ? "true": "false");
      break;
    case VAL_NIL:
      printf("nil");
      break;;
    case VAL_NUM:
      printf("%g", value.as.dval);
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
#ifdef KVM_TRACE
void ku_print_chunk(kuvm *vm, kuchunk *chunk, const char * name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; ) {
    offset = ku_print_op(vm, chunk, offset);
    printf("\n");
  }
}

static int ku_print_simple_op(const char *name, int offset) {
  printf("%-17s", name);
  return offset + 1;
}

static int ku_print_const(kuvm *vm, const char *name, kuchunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  printf("%-6s %4d '", name, con);
  ku_print_val(vm, chunk->constants.values[con]);
  printf("'");
  return offset+2;
}

int ku_print_op(kuvm *vm, kuchunk *chunk, int offset) {
#define OP_DEF1(o) \
case o:\
return ku_print_simple_op(#o, offset);
  
  printf("%04d ", offset);

  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset-1]) {
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
  }
  uint8_t op = chunk->code[offset];
  switch (op) {
      OP_DEF1(OP_NOP)
      OP_DEF1(OP_RET)
      OP_DEF1(OP_NEG)
      OP_DEF1(OP_ADD)
      OP_DEF1(OP_SUB)
      OP_DEF1(OP_MUL)
      OP_DEF1(OP_DIV)
      OP_DEF1(OP_NIL)
      OP_DEF1(OP_TRUE)
      OP_DEF1(OP_FALSE)
      OP_DEF1(OP_GT)
      OP_DEF1(OP_LT)
      OP_DEF1(OP_EQ)
    case OP_CONST:
      return ku_print_const(vm, "OP_CONST", chunk, offset);
    default:
      printf("Unknown opcode %d\n", op);
      return offset + 1;
  }
#undef OP_DEF1
}
#endif

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KVM_MAIN
int ku_main(int argc, const char * argv[]) {
  kuvm *vm = ku_new();
  
  if (argc == 2 && strcmp(argv[1],"--test") == 0) {
    ku_test();
    return 0;
  }
  
  if (argc == 1) {
    ku_repl(vm);
  } else if (argc == 2) {
    kures res = ku_runfile(vm, argv[1]);
    if (res == KVM_ERR_RUNTIME) {
      ku_free(vm);
      exit(70);
    }
    if (res == KVM_ERR_SYNTAX)  {
      ku_free(vm);
      exit(65);
    }
    if (res == KVM_FILE_NOTFOUND) {
      fprintf(stderr, "file error '%s'\n", argv[1]);
      ku_free(vm);
      exit(74);
    }
    
  } else {
    ku_free(vm);
    fprintf(stderr, "usage kumu [file]\n");
    exit(64);
  }
  ku_free(vm);
  return 0;
}
#endif

// ------------------------------------------------------------
// TEST
// ------------------------------------------------------------
#ifdef KVM_TEST

static void ku_chunk_write_const(kuvm *vm, int cons, int line) {
  int index = ku_chunk_add_const(vm, vm->chunk, NUM_VAL(cons));
  ku_chunk_write(vm, vm->chunk, OP_CONST, line);
  ku_chunk_write(vm, vm->chunk, index, line);
}

static int ktest_pass = 0;
int ktest_fail = 0;

static void tint_eq(kuvm *vm, int v1, int v2, const char *m) {
  if (v1 == v2) {
    ktest_pass++;
    return;
  }
  ktest_fail++;
  printf("expected: %d found %d [%s]\n", v1, v2, m);
}

static void tval_eq(kuvm* vm, kuval v1, kuval v2, const char *msg) {
  if (ku_val_eq(v1, v2)) {
    ktest_pass++;
    return;
  }
  
  ktest_fail++;
  printf("expected: ");
  ku_print_val(vm, v2);
  printf(" found: ");
  ku_print_val(vm, v1);
  printf(" [%s]\n", msg);
}

static void ku_test_summary() {
  printf("tests %d passed %d failed\n", ktest_pass, ktest_fail);
}

void ku_test() {
  kuvm *vm = ku_new();
  kuchunk chunk;
  ku_chunk_init(vm, &chunk);
  vm->chunk = &chunk;
  int line = 1;
  ku_chunk_write(vm, &chunk, OP_NOP, line++);
  ku_chunk_write_const(vm, 1, line);
  ku_chunk_write_const(vm, 2, line);
  ku_chunk_write(vm, &chunk, OP_ADD, line);
  ku_chunk_write(vm, &chunk, OP_NEG, line++);
  ku_chunk_write_const(vm, 4, line);
  ku_chunk_write(vm, &chunk, OP_SUB, line++);
  ku_chunk_write_const(vm, 5, line);
  ku_chunk_write(vm, &chunk, OP_MUL, line++);
  ku_chunk_write_const(vm, 6, line);
  ku_chunk_write(vm, &chunk, OP_DIV, line++);
  ku_chunk_write(vm, &chunk, OP_RET, line);
  kures res = ku_run(vm, &chunk);
  tint_eq(vm, res, KVM_OK, "ku_run res");
  kuval v = ku_pop(vm);
  tval_eq(vm, v, NUM_VAL((-(1.0+2.0)-4.0)*5.0/6.0), "ku_run ret");
  ku_chunk_free(vm, &chunk);
  ku_free(vm);
  
  vm = ku_new();
  res = ku_exec(vm, "1+2");
  tint_eq(vm, res, KVM_OK, "ku_exec res");
  v = ku_pop(vm);
  tval_eq(vm, v, NUM_VAL(3), "ku_exec ret");
  ku_free(vm);
  
  vm = ku_new();
  ku_lex_init(vm, "12+3");
  ku_lex_print_all(vm);
  ku_free(vm);
  
  vm = ku_new();
  res = ku_exec(vm, "12+");
  tint_eq(vm, res, KVM_ERR_SYNTAX, "12+");
  ku_free(vm);
  
  vm = ku_new();
  res = ku_exec(vm, "(1+2)*3");
  tint_eq(vm, res, KVM_OK, "grouping res");
  tval_eq(vm, ku_pop(vm), NUM_VAL(9), "grouping ret");
  ku_free(vm);
  
  vm = ku_new();
  vm->flags |= KVM_F_LIST;
  res = ku_exec(vm, "(1+2)*3");
  ku_free(vm);

  vm = ku_new();
  ku_print_mem(vm);
  ku_free(vm);
  
  vm = ku_new();
  ku_lex_init(vm, "var x=30; \n  x=\"hello\";");
  ku_lex_print_all(vm);
  ku_free(vm);

  vm = ku_new();
  ku_exec(vm, "2*3");
  ku_print_stack(vm);
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "-2*3");
  tint_eq(vm, res, KVM_OK, "unary res");
  tval_eq(vm, ku_pop(vm), NUM_VAL(-6), "unary ret");
  ku_free(vm);

  // unterminated string
  vm = ku_new();
  ku_lex_init(vm, "\"hello");
  ku_lex_print_all(vm);
  ku_free(vm);

  // ku_print_val
  vm = ku_new();
  res = ku_exec(vm, "2+3");
  v = ku_pop(vm);
  tval_eq(vm, v, NUM_VAL(5), "ku_print_val ret");
  ku_print_val(vm, v);
  ku_free(vm);
  
  vm = ku_new();
  res = ku_exec(vm, "12.3");
  tval_eq(vm, ku_pop(vm), NUM_VAL(12.3), "ku_lex_peeknext ret");
  ku_free(vm);
  
  vm = ku_new();
  ku_lex_init(vm, "and class else false for fun if nil or print return super this true while {}!+-*/=!=><>=<= far\ttrick\nart\rcool eek too fund");
  kutok t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_AND, "[and]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_CLASS, "[class]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_ELSE, "[else]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_FALSE, "[false]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_FOR, "[for]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_FUN, "[fun]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_IF, "[if]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_NIL, "[nil]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_OR, "[or]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_PRINT, "[print]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_RETURN, "[return]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_SUPER, "[super]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_THIS, "[this]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_TRUE, "[true]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_WHILE, "[while]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_LBRACE, "[{]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_RBRACE, "[}]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_BANG, "[!]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_PLUS, "[+]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_MINUS, "[-]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_STAR, "[*]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_SLASH, "[/]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_EQ, "[=]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_NE, "[!=]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_GT, "[>]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_LT, "[<]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_GE, "[>=]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_LE, "[<=]");

  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_IDENT, "[identifier]");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_IDENT, "[identifier]");
  ku_free(vm);
  
  vm = ku_new();
  ku_lex_init(vm, "// this is a comment");
  t = ku_lex_scan(vm);
  tint_eq(vm, t.type, TOK_EOF, "comment");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "(12-2)/5");
  tint_eq(vm, res, KVM_OK, "sub div res");
  tval_eq(vm, ku_pop(vm), NUM_VAL(2), "sub div ret");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "-true");
  tint_eq(vm, res, KVM_ERR_RUNTIME, "negate err");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "true");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "true literal eval");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "false");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "false literal eval");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "nil");
  tval_eq(vm, ku_pop(vm), NIL_VAL, "nil literal eval");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "!true");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "!true eval");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "!false");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "!false eval");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "1==1");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "== true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "1==2");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "== false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "1!=2");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "!= true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "1!=1");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "!= false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "1<1");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "< false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "1<2");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "< true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "2<=1");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "<= false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "2<=3");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "<= true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "3>2");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "> true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "3>7");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "> false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "3>=7");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), ">= false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "3>=3");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), ">= true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "12 + true");
  tint_eq(vm, res, KVM_ERR_RUNTIME, "add num expected");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "\"hello \" + \"world\"");
  v = ku_pop(vm);
  tint_eq(vm, v.type, VAL_OBJ, "stradd type obj");
  tint_eq(vm, AS_OBJ(v)->type, OBJ_STR, "stradd obj is str");
  char* chars = AS_CSTR(v);
  tint_eq(vm, strcmp(chars, "hello world"), 0, "str val");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "\"hello \" == \"world\"");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(false), "str eq false");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "\"hello\" == \"hello\"");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "str eq true");
  ku_free(vm);

  vm = ku_new();
  res = ku_exec(vm, "\"hello \" != \"world\"");
  tval_eq(vm, ku_pop(vm), BOOL_VAL(true), "str ne true");
  ku_free(vm);

  ku_test_summary();
}

#endif

