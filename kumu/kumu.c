//  kumu.c

#include "kumu.h"
#include <stdio.h>


bool kval_eq(kval v1, kval v2) {
  if (v1.type != v2.type) {
    return false;
  }
  switch (v1.type) {
    case VAL_NIL: return true;
    case VAL_BOOL: return v1.as.bval == v2.as.bval;
    case VAL_NUM: return v1.as.dval == v2.as.dval;
  }
  return false;
}

// ------------------------------------------------------------
// Macros
// ------------------------------------------------------------
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)kalloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(vm, type, ptr, old) kalloc(vm, ptr, sizeof(type) * (old), 0)

// ------------------------------------------------------------
// ktypearr
// ------------------------------------------------------------
void tainit(kvm *vm, ktypearr *t) {
  t->count = 0;
  t->capacity = 0;
  t->types = NULL;
}

void tawrite(kvm *vm, ktypearr *t, const char *name) {
  if (t->capacity < t->count + 1) {
    int old = t->capacity;
    t->capacity = CAPACITY_GROW(old);
    t->types = ARRAY_GROW(vm, ktype, t->types, old, t->capacity);
  }
  t->types[t->count].name = name;
  t->count++;
}

void tafree(kvm *vm, ktypearr *t) {
  ARRAY_FREE(vm, ktype, vm->types.types, vm->types.capacity);
}

// ------------------------------------------------------------
// Scanner
// ------------------------------------------------------------
static char ladvance(kvm *vm) {
  vm->scanner.curr++;
  return vm->scanner.curr[-1];
}


static void linit(kvm *vm, const char *source) {
  vm->scanner.start = source;
  vm->scanner.curr = source;
  vm->scanner.line = 1;
}

static bool lisend(kvm *vm) {
  return (*(vm->scanner.curr) == '\0');
}

static bool lisdigit(char c) {
  return (c >= '0' && c <= '9');
}

static bool lisalpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
         (c == '_');
}

static char lpeeknext(kvm *vm) {
  if (lisend(vm)) return '\0';
  return vm->scanner.curr[1];
}

static char lpeek(kvm *vm) {
  return *vm->scanner.curr;
}

static void lskipspace(kvm *vm) {
  while (true) {
    char c = lpeek(vm);
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        ladvance(vm);
        break;
      case '\n':
        vm->scanner.line++;
        ladvance(vm);
        break;
      case '/':
        if (lpeeknext(vm) == '/') {
          while (lpeek(vm) != '\n' && !lisend(vm))
            ladvance(vm);
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

static ktok lmake(kvm *vm, ltype type) {
  ktok token;
  token.type = type;
  token.start = vm->scanner.start;
  token.len = (int) (vm->scanner.curr - vm->scanner.start);
  token.line = vm->scanner.line;
  return token;
}

static ktok lerror(kvm *vm, const char *msg) {
  ktok token;
  token.type = TOK_ERR;
  token.start = msg;
  token.len = (int)strlen(msg);
  token.line = vm->scanner.line;
  return token;
}

static bool lmatch(kvm *vm, char expected) {
  if (lisend(vm)) return false;
  if (*vm->scanner.curr != expected) return false;
  vm->scanner.curr++;
  return true;
}

static ktok lnumber(kvm *vm) {
  while(lisdigit(lpeek(vm))) ladvance(vm);
  
  if (lpeek(vm) == '.' && lisdigit(lpeeknext(vm))) {
    ladvance(vm);
  }
  while(lisdigit(lpeek(vm))) ladvance(vm);
  return lmake(vm, TOK_NUM);
}

static ktok lstring(kvm *vm) {
  while(lpeek(vm) != '"' && !lisend(vm)) {
    if (lpeek(vm) == '\n') vm->scanner.line++;
    ladvance(vm);
  }
  if (lisend(vm)) return lerror(vm, "unterminated string");
  ladvance(vm);
  return lmake(vm, TOK_STR);
}

static ltype lkeyword(kvm *vm, int start, int len,
                        const char *rest, ltype type) {
  if (vm->scanner.curr - vm->scanner.start == start + len &&
      memcmp(vm->scanner.start + start, rest, len) == 0) {
    return type;
  }
  return TOK_IDENT;
}

static ltype lidentitytype(kvm *vm) {
  switch(vm->scanner.start[0]) {
    case 'a': return lkeyword(vm, 1,2,"nd", TOK_AND);
    case 'c': return lkeyword(vm, 1,4,"lass", TOK_CLASS);
    case 'e': return lkeyword(vm, 1,3,"lse", TOK_ELSE);
    case 'f':
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch (vm->scanner.start[1]) {
          case 'a': return lkeyword(vm, 2, 3, "lse", TOK_FALSE);
          case 'o': return lkeyword(vm, 2, 1, "r", TOK_FOR);
          case 'u': return lkeyword(vm, 2, 1, "n", TOK_FUN);
        }
      }
    case 'i': return lkeyword(vm, 1,1,"f", TOK_IF);
    case 'n': return lkeyword(vm, 1,2,"il", TOK_NIL);
    case 'o': return lkeyword(vm, 1,1,"r", TOK_OR);
    case 'p': return lkeyword(vm, 1,4,"rint", TOK_PRINT);
    case 'r': return lkeyword(vm, 1,5,"eturn", TOK_RETURN);
    case 's': return lkeyword(vm, 1,4,"uper", TOK_SUPER);
    case 't':
      if (vm->scanner.curr - vm->scanner.start > 1) {
        switch(vm->scanner.start[1]) {
          case 'h': return lkeyword(vm, 2, 2, "is", TOK_THIS);
          case 'r': return lkeyword(vm, 2, 2, "ue", TOK_TRUE);
        }
      }
    case 'v': return lkeyword(vm, 1,2,"ar", TOK_VAR);
    case 'w': return lkeyword(vm, 1,4,"hile", TOK_WHILE);
  }
  return TOK_IDENT;
}

static ktok lidentifier(kvm *vm) {
  while (lisalpha(lpeek(vm)) || lisdigit(lpeek(vm))) {
    ladvance(vm);
  }
  return lmake(vm, lidentitytype(vm));
}

static ktok lscan(kvm *vm) {
  lskipspace(vm);
  vm->scanner.start = vm->scanner.curr;
  
  if (lisend(vm)) {
    return lmake(vm, TOK_EOF);
  }
  
  char c = ladvance(vm);
  if (lisalpha(c)) return lidentifier(vm);
  if (lisdigit(c)) return lnumber(vm);
  switch (c) {
    case '(': return lmake(vm, TOK_LPAR);
    case ')': return lmake(vm, TOK_RPAR);
    case '{': return lmake(vm, TOK_LBRACE);
    case '}': return lmake(vm, TOK_RBRACE);
    case ';': return lmake(vm, TOK_SEMI);
    case ',': return lmake(vm, TOK_COMMA);
    case '.': return lmake(vm, TOK_DOT);
    case '+': return lmake(vm, TOK_PLUS);
    case '-': return lmake(vm, TOK_MINUS);
    case '*': return lmake(vm, TOK_STAR);
    case '/':
      return lmake(vm, TOK_SLASH);
    case '!':
      return lmake(vm, lmatch(vm, '=') ? TOK_NE : TOK_BANG);
    case '=':
      return lmake(vm, lmatch(vm, '=') ? TOK_EQEQ : TOK_EQ);
    case '<':
      return lmake(vm, lmatch(vm, '=') ? TOK_LE : TOK_LT);
    case '>':
      return lmake(vm, lmatch(vm, '=') ? TOK_GE : TOK_GT);
    case '"':
      return lstring(vm);
  }
  return lerror(vm, "unexpected character");
}

void lprint(kvm *vm) {
  int line = -1;
  
  while (true) {
    ktok token = lscan(vm);
    if (token.line != line) {
      tprintf("%4d ", token.line);
    } else {
      tprintf("  |  ");
    }
    tprintf("%2d '%.*s'\n", token.type, token.len, token.start);
    
    if (token.type == TOK_EOF) {
      break;
    }
  }
}


// ------------------------------------------------------------
// Parser
// ------------------------------------------------------------
static void perrorat(kvm *vm, ktok *tok, const char *msg) {
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

static void perr(kvm *vm, const char *msg) {
  perrorat(vm, &vm->parser.curr, msg);
}

static void padvance(kvm *vm) {
  vm->parser.prev = vm->parser.curr;
  
  while (true) {
    vm->parser.curr = lscan(vm);
    if (vm->parser.curr.type != TOK_ERR) break;
    perr(vm, vm->parser.curr.start);
  }
}

static void pconsume(kvm *vm, ltype type, const char *msg) {
  if (vm->parser.curr.type == type) {
    padvance(vm);
    return;
  }
  perr(vm, msg);
}

kchunk *ccurr(kvm *vm) {
  return vm->chunk;
}

static void pemitbyte(kvm *vm, uint8_t byte) {
  cwrite(vm, ccurr(vm), byte, vm->parser.prev.line);
}

static void pend(kvm *vm) {
  pemitbyte(vm, OP_RET);
}

static void pemitbytes(kvm *vm, uint8_t b1, uint8_t b2) {
  pemitbyte(vm, b1);
  pemitbyte(vm, b2);
}


static uint8_t pmakeconst(kvm *vm, kval val) {
  int cons = caddconst(vm, ccurr(vm), val);
  if (cons > UINT8_MAX) {
    perr(vm, "out of constant space");
    return 0;
  }
  return (uint8_t)cons;
}
static void pemitconst(kvm *vm, kval val) {
  pemitbytes(vm, OP_CONST, pmakeconst(vm, val));
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
} kprecedence;

typedef void (*pfunc)(kvm *);

typedef struct {
  pfunc prefix;
  pfunc infix;
  kprecedence precedence;
} prule;

static prule *pgetrule(kvm *vm, ltype optype);

static void pprecedence(kvm *vm, kprecedence prec) {
  padvance(vm);
  pfunc prefix = pgetrule(vm, vm->parser.prev.type)->prefix;
  if (prefix == NULL) {
    perr(vm, "expected expression");
    return;
  }
  prefix(vm);
  
  while (prec <= pgetrule(vm, vm->parser.curr.type)->precedence) {
    padvance(vm);
    pfunc infix = pgetrule(vm, vm->parser.prev.type)->infix;
    infix(vm);
  }
}

static void pliteral(kvm *vm) {
  switch (vm->parser.prev.type) {
    case TOK_FALSE: pemitbyte(vm, OP_FALSE); break;
    case TOK_TRUE: pemitbyte(vm, OP_TRUE); break;
    case TOK_NIL: pemitbyte(vm, OP_NIL); break;
    default: return; // unreachable
  }
}

static void pnumber(kvm *vm) {
  double val = strtod(vm->parser.prev.start, NULL);
  pemitconst(vm, NUM_VAL(val));
}

static void pexpression(kvm *vm) {
  pprecedence(vm, P_ASSIGN);
}

static void pgrouping(kvm *vm) {
  pexpression(vm);
  pconsume(vm, TOK_RPAR, "')' expected");
}

static void punary(kvm *vm) {
  ltype optype = vm->parser.prev.type;
  
  pexpression(vm);
  
  switch(optype) {
    case TOK_MINUS: pemitbyte(vm, OP_NEG); break;
    case TOK_BANG: pemitbyte(vm, OP_NOT); break;
    default: return;
  }
}


static void pbinary(kvm *vm) {
  ltype optype = vm->parser.prev.type;
  prule *rule = pgetrule(vm, optype);
  pprecedence(vm, (kprecedence)(rule->precedence + 1));
  
  switch (optype) {
    case TOK_PLUS: pemitbyte(vm, OP_ADD); break;
    case TOK_MINUS: pemitbyte(vm, OP_SUB); break;
    case TOK_STAR: pemitbyte(vm, OP_MUL); break;
    case TOK_SLASH: pemitbyte(vm, OP_DIV); break;
    case TOK_NE: pemitbytes(vm, OP_EQ, OP_NOT); break;
    case TOK_EQEQ: pemitbyte(vm, OP_EQ); break;
    case TOK_GT: pemitbyte(vm, OP_GT); break;
    case TOK_GE: pemitbytes(vm, OP_LT, OP_NOT); break;
    case TOK_LT: pemitbyte(vm, OP_LT); break;
    case TOK_LE: pemitbytes(vm, OP_GT, OP_NOT); break;
    default: return;
  }
}

prule rules[] = {
  [TOK_LPAR] =      { pgrouping,   NULL,     P_NONE },
  [TOK_RPAR] =      { NULL,        NULL,     P_NONE },
  [TOK_LBRACE] =    { NULL,        NULL,     P_NONE },
  [TOK_RBRACE] =    { NULL,        NULL,     P_NONE },
  [TOK_COMMA] =     { NULL,        NULL,     P_NONE },
  [TOK_DOT] =       { NULL,        NULL,     P_NONE },
  [TOK_MINUS] =     { punary,      pbinary,  P_TERM },
  [TOK_PLUS] =      { NULL,        pbinary,  P_TERM },
  [TOK_SEMI] =      { NULL,        NULL,     P_NONE },
  [TOK_SLASH] =     { NULL,        pbinary,  P_FACTOR },
  [TOK_STAR] =      { NULL,        pbinary,  P_FACTOR },
  [TOK_BANG] =      { punary,      NULL,     P_NONE },
  [TOK_NE] =        { NULL,        pbinary,  P_EQ },
  [TOK_EQ] =        { NULL,        NULL,     P_NONE },
  [TOK_EQEQ] =      { NULL,        pbinary,  P_EQ },
  [TOK_GT] =        { NULL,        pbinary,  P_COMP },
  [TOK_GE] =        { NULL,        pbinary,  P_COMP },
  [TOK_LT] =        { NULL,        pbinary,  P_COMP },
  [TOK_LE] =        { NULL,        pbinary,  P_COMP },
  [TOK_IDENT] =     { NULL,        NULL,     P_NONE },
  [TOK_STR] =       { NULL,        NULL,     P_NONE },
  [TOK_NUM] =       { pnumber,     NULL,     P_NONE },
  [TOK_AND] =       { NULL,        NULL,     P_NONE },
  [TOK_CLASS] =     { NULL,        NULL,     P_NONE },
  [TOK_ELSE] =      { NULL,        NULL,     P_NONE },
  [TOK_FALSE] =     { pliteral,    NULL,     P_NONE },
  [TOK_FOR] =       { NULL,        NULL,     P_NONE },
  [TOK_FUN] =       { NULL,        NULL,     P_NONE },
  [TOK_IF] =        { NULL,        NULL,     P_NONE },
  [TOK_NIL] =       { pliteral,    NULL,     P_NONE },
  [TOK_OR] =        { NULL,        NULL,     P_NONE },
  [TOK_PRINT] =     { NULL,        NULL,     P_NONE },
  [TOK_SUPER] =     { NULL,        NULL,     P_NONE },
  [TOK_THIS] =      { NULL,        NULL,     P_NONE },
  [TOK_TRUE] =      { pliteral,    NULL,     P_NONE },
  [TOK_VAR] =       { NULL,        NULL,     P_NONE },
  [TOK_WHILE] =     { NULL,        NULL,     P_NONE },
  [TOK_ERR] =       { NULL,        NULL,     P_NONE },
  [TOK_EOF] =       { NULL,        NULL,     P_NONE },
};

static prule *pgetrule(kvm *vm, ltype optype) {
  return &rules[optype];
}

// ------------------------------------------------------------
// Virtual machine
// ------------------------------------------------------------
void kresetstack(kvm *vm) {
  vm->sp = vm->stack;
}

void kpush(kvm *vm, kval val) {
  *(vm->sp) = val;
  vm->sp++;
}

kval kpop(kvm *vm) {
  vm->sp--;
  return *(vm->sp);
}

static bool kisfalsy(kval v) {
  return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v));
}

kval kpeek(kvm *vm, int distance) {
  return vm->sp[-1 - distance];
}

kvm *knew(void) {
  kvm *vm = malloc(sizeof(kvm));
  vm->allocated = sizeof(kvm);
  vm->flags = 0;
  if (!vm) {
    return NULL;
  }

  vm->freed = 0;
  vm->stop = false;
  vm->chunk = NULL;

  kresetstack(vm);
  tainit(vm, &vm->types);
  tawrite(vm, &vm->types, "Int");
  tawrite(vm, &vm->types, "String");
  tawrite(vm, &vm->types, "Double");
  return vm;
}

#ifdef KVM_TRACE
void mprint(kvm *vm) {
  printf("allocated: %d, freed: %d, delta: %d\n",
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
}
#endif

void kfree(kvm *vm) {
  vm->freed += sizeof(kvm);
  tafree(vm, &vm->types);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

#ifdef KVM_TRACE
void kprintstack(kvm *vm) {
  printf(" [");
  for (kval* vp = vm->stack; vp < vm->sp; vp++) {
    vprint(vm, *vp);
    if (vp < vm->sp - 1) {
      printf(",");
    }
  }
  printf("]");
  printf("\n");
}
#endif

static void keruntime(kvm *vm, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fputs("\n", stderr);
  size_t instruction = vm->ip - vm->chunk->code - 1;
  int line = vm->chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  kresetstack(vm);
}

static kres krunloop(kvm *vm) {
#define BYTE_READ(vm) (*(vm->ip++))
#define CONST_READ(vm) (vm->chunk->constants.values[BYTE_READ(vm)])
#define BIN_OP(v, vt, op) \
  do { \
  if (!IS_NUM(kpeek(v,0)) || !IS_NUM(kpeek(v,1))) { \
    keruntime(v, "numbers expected"); \
    return KVM_ERR_RUNTIME; \
  } \
  double b = AS_NUM(kpop(v)); \
  double a = AS_NUM(kpop(v)); \
  kpush(v, vt(a op b)); \
} while (false)

  

  kres res = KVM_CONT;
  while (res == KVM_CONT) {
    uint8_t op;
#ifdef KVM_TRACE
    if (vm->flags & KVM_F_TRACE) {
     oprint(vm, vm->chunk, (int) (vm->ip - vm->chunk->code));
    }
#endif

    switch(op = BYTE_READ(vm)) {
      case OP_NOP:
        break;
      case OP_NIL:
        kpush(vm, NIL_VAL);
        break;
      case OP_TRUE:
        kpush(vm, BOOL_VAL(true));
        break;
      case OP_FALSE:
        kpush(vm, BOOL_VAL(false));
        break;
      case OP_EQ: {
        kval b = kpop(vm);
        kval a = kpop(vm);
        kpush(vm, BOOL_VAL(kval_eq(a, b)));
        break;;
      }
      case OP_RET: {
        res = KVM_OK;
        break;
      }
      case OP_CONST: {
        kval con = CONST_READ(vm);
        kpush(vm, con);
        break;
      }
      case OP_NEG: {
        if (! IS_NUM(kpeek(vm, 0))) {
          keruntime(vm, "number expected" );
          return KVM_ERR_RUNTIME;
        }
        kval v = kpop(vm);
        double dv = AS_NUM(v);
        kval nv = NUM_VAL(-dv);
        kpush(vm, nv);
        break;
      }
      case OP_ADD: BIN_OP(vm,NUM_VAL, +); break;
      case OP_SUB: BIN_OP(vm,NUM_VAL, -); break;
      case OP_MUL: BIN_OP(vm,NUM_VAL, *); break;
      case OP_DIV: BIN_OP(vm,NUM_VAL, /); break;
      case OP_GT: BIN_OP(vm, BOOL_VAL, >); break;
      case OP_LT: BIN_OP(vm,BOOL_VAL, <); break;
      case OP_NOT:
        kpush(vm, BOOL_VAL(kisfalsy(kpop(vm))));
        break;
    }
#ifdef KVM_TRACE
    if (vm->flags & KVM_F_TRACE && vm->flags & KVM_F_STACK) {
     kprintstack(vm);
    } else {
      if (vm->flags & KVM_F_TRACE) {
        tprintf("\n");
      }
    }
#endif
  }
  return KVM_OK;
#undef BYTE_READ
#undef CONST_READ
#undef BIN_OP
}

kres krun(kvm *vm, kchunk *chunk) {
  vm->chunk = chunk;
  vm->ip = vm->chunk->code;
  return krunloop(vm);
}

static kres kcompile(kvm *vm, char *source, kchunk *chunk) {
  linit(vm, source);
  vm->parser.err = false;
  vm->parser.panic = false;
  vm->chunk = chunk;
  padvance(vm);
  pexpression(vm);
  pconsume(vm, TOK_EOF, "expected expression end");
  pend(vm);
  
#ifdef DEBUG_PRINT_CODE
  cprint(vm, ccurr(vm), "code");
#endif
  return (vm->parser.err ? KVM_ERR_SYNTAX : KVM_OK);
}

static kres kexec(kvm *vm, char *source) {
  kchunk chunk;
  
  cinit(vm, &chunk);
  
  if (kcompile(vm, source, &chunk) != KVM_OK) {
    cfree(vm, &chunk);
    return KVM_ERR_SYNTAX;
  }
  
  vm->ip = chunk.code;
  kres res = krun(vm, &chunk);
  
  if (vm->flags & KVM_F_LIST) {
    cprint(vm, ccurr(vm), "code");
  }
  cfree(vm, &chunk);
  return res;
}

#ifdef KVM_MAIN
static char *kreadfile(kvm *vm, const char *path) {
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

kres krunfile(kvm *vm, const char *file) {
  char *source = kreadfile(vm, file);
  
  if (source == NULL) {
    return KVM_FILE_NOTFOUND;
  }
  kres res = kexec(vm, source);
  free(source);
  return res;
}


static bool flag_check(kvm *vm, char *line,
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
static void krepl(kvm *vm) {
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
      ktest();
      continue;
    }
    
    if (flag_check(vm, line, "trace", KVM_F_TRACE)) continue;
    if (flag_check(vm, line, "stack", KVM_F_STACK)) continue;
    if (flag_check(vm, line, "list", KVM_F_LIST)) continue;

    if (strcmp(line, ".mem\n") == 0) {
      mprint(vm);
      continue;
    }
    
    kexec(vm, line);
    if (vm->sp > vm->stack) {
      kval v = kpop(vm);
      vprint(vm, v);
      printf("\n");
    }

  }
}
#endif

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *kalloc(kvm *vm, void *ptr, size_t oldsize, size_t nsize) {
  
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
void cinit(kvm *vm, kchunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  vainit(vm, &chunk->constants);
}

void cwrite(kvm *vm, kchunk *chunk, uint8_t byte, int line) {
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

void cfree(kvm *vm, kchunk *chunk) {
  ARRAY_FREE(vm, uint8_t, chunk->code, chunk->capacity);
  ARRAY_FREE(vm, int, chunk->lines, chunk->capacity);
  ARRAY_FREE(vm, kval, chunk->constants.values, chunk->constants.capacity);
}

int caddconst(kvm *vm, kchunk *chunk, kval value) {
  vawrite(vm, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void vprint(kvm *vm, kval value) {
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
  }
}

void vainit(kvm* vm, kvalarr *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void vawrite(kvm* vm, kvalarr *array, kval value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = CAPACITY_GROW(old);
    array->values = ARRAY_GROW(vm, kval, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
#ifdef KVM_TRACE
void cprint(kvm *vm, kchunk *chunk, const char * name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; ) {
    offset = oprint(vm, chunk, offset);
    printf("\n");
  }
}

static int oprintsimple(const char *name, int offset) {
  printf("%-17s", name);
  return offset + 1;
}

static int oprintconst(kvm *vm, const char *name, kchunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  printf("%-6s %4d '", name, con);
  vprint(vm, chunk->constants.values[con]);
  printf("'");
  return offset+2;
}

int oprint(kvm *vm, kchunk *chunk, int offset) {
#define OP_DEF1(o) \
case o:\
return oprintsimple(#o, offset);
  
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
      return oprintconst(vm, "OP_CONST", chunk, offset);
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
int kmain(int argc, const char * argv[]) {
  kvm *vm = knew();
  
  if (argc == 2 && strcmp(argv[1],"--test") == 0) {
    ktest();
    return 0;
  }
  
  if (argc == 1) {
    krepl(vm);
  } else if (argc == 2) {
    kres res = krunfile(vm, argv[1]);
    if (res == KVM_ERR_RUNTIME) {
      kfree(vm);
      exit(70);
    }
    if (res == KVM_ERR_SYNTAX)  {
      kfree(vm);
      exit(65);
    }
    if (res == KVM_FILE_NOTFOUND) {
      fprintf(stderr, "file error '%s'\n", argv[1]);
      kfree(vm);
      exit(74);
    }
    
  } else {
    kfree(vm);
    fprintf(stderr, "usage kumu [file]\n");
    exit(64);
  }
  kfree(vm);
  return 0;
}
#endif

// ------------------------------------------------------------
// TEST
// ------------------------------------------------------------
#ifdef KVM_TEST

static void cwriteconst(kvm *vm, int cons, int line) {
  int index = caddconst(vm, vm->chunk, NUM_VAL(cons));
  cwrite(vm, vm->chunk, OP_CONST, line);
  cwrite(vm, vm->chunk, index, line);
}

static int ktest_pass = 0;
int ktest_fail = 0;

static void tint_eq(kvm *vm, int v1, int v2, const char *m) {
  if (v1 == v2) {
    ktest_pass++;
    return;
  }
  ktest_fail++;
  printf("expected: %d found %d [%s]\n", v1, v2, m);
}

static void tval_eq(kvm* vm, kval v1, kval v2, const char *msg) {
  if (kval_eq(v1, v2)) {
    ktest_pass++;
    return;
  }
  
  ktest_fail++;
  printf("expected: ");
  vprint(vm, v2);
  printf(" found: ");
  vprint(vm, v1);
  printf(" [%s]\n", msg);
}

static void ktest_summary() {
  printf("tests %d passed %d failed\n", ktest_pass, ktest_fail);
}

void ktest() {
  kvm *vm = knew();
  kchunk chunk;
  cinit(vm, &chunk);
  vm->chunk = &chunk;
  int line = 1;
  cwrite(vm, &chunk, OP_NOP, line++);
  cwriteconst(vm, 1, line);
  cwriteconst(vm, 2, line);
  cwrite(vm, &chunk, OP_ADD, line);
  cwrite(vm, &chunk, OP_NEG, line++);
  cwriteconst(vm, 4, line);
  cwrite(vm, &chunk, OP_SUB, line++);
  cwriteconst(vm, 5, line);
  cwrite(vm, &chunk, OP_MUL, line++);
  cwriteconst(vm, 6, line);
  cwrite(vm, &chunk, OP_DIV, line++);
  cwrite(vm, &chunk, OP_RET, line);
  kres res = krun(vm, &chunk);
  tint_eq(vm, res, KVM_OK, "krun res");
  kval v = kpop(vm);
  tval_eq(vm, v, NUM_VAL((-(1.0+2.0)-4.0)*5.0/6.0), "krun ret");
  cfree(vm, &chunk);
  kfree(vm);
  
  vm = knew();
  res = kexec(vm, "1+2");
  tint_eq(vm, res, KVM_OK, "kexec res");
  v = kpop(vm);
  tval_eq(vm, v, NUM_VAL(3), "kexec ret");
  kfree(vm);
  
  vm = knew();
  linit(vm, "12+3");
  lprint(vm);
  kfree(vm);
  
  vm = knew();
  res = kexec(vm, "12+");
  tint_eq(vm, res, KVM_ERR_SYNTAX, "12+");
  kfree(vm);
  
  vm = knew();
  res = kexec(vm, "(1+2)*3");
  tint_eq(vm, res, KVM_OK, "grouping res");
  tval_eq(vm, kpop(vm), NUM_VAL(9), "grouping ret");
  kfree(vm);
  
  vm = knew();
  vm->flags |= KVM_F_LIST;
  res = kexec(vm, "(1+2)*3");
  kfree(vm);

  vm = knew();
  mprint(vm);
  kfree(vm);
  
  vm = knew();
  linit(vm, "var x=30; \n  x=\"hello\";");
  lprint(vm);
  kfree(vm);

  vm = knew();
  kexec(vm, "2*3");
  kprintstack(vm);
  kfree(vm);

  vm = knew();
  res = kexec(vm, "-2*3");
  tint_eq(vm, res, KVM_OK, "unary res");
  tval_eq(vm, kpop(vm), NUM_VAL(-6), "unary ret");
  kfree(vm);

  // unterminated string
  vm = knew();
  linit(vm, "\"hello");
  lprint(vm);
  kfree(vm);

  // vprint
  vm = knew();
  res = kexec(vm, "2+3");
  v = kpop(vm);
  tval_eq(vm, v, NUM_VAL(5), "vprint ret");
  vprint(vm, v);
  kfree(vm);
  
  vm = knew();
  res = kexec(vm, "12.3");
  tval_eq(vm, kpop(vm), NUM_VAL(12.3), "lpeeknext ret");
  kfree(vm);
  
  vm = knew();
  linit(vm, "and class else false for fun if nil or print return super this true while {}!+-*/=!=><>=<= far\ttrick\nart\rcool eek too fund");
  ktok t = lscan(vm);
  tint_eq(vm, t.type, TOK_AND, "[and]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_CLASS, "[class]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_ELSE, "[else]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_FALSE, "[false]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_FOR, "[for]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_FUN, "[fun]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_IF, "[if]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_NIL, "[nil]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_OR, "[or]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_PRINT, "[print]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_RETURN, "[return]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_SUPER, "[super]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_THIS, "[this]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_TRUE, "[true]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_WHILE, "[while]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_LBRACE, "[{]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_RBRACE, "[}]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_BANG, "[!]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_PLUS, "[+]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_MINUS, "[-]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_STAR, "[*]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_SLASH, "[/]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_EQ, "[=]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_NE, "[!=]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_GT, "[>]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_LT, "[<]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_GE, "[>=]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_LE, "[<=]");

  t = lscan(vm);
  tint_eq(vm, t.type, TOK_IDENT, "[identifier]");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_IDENT, "[identifier]");
  kfree(vm);
  
  vm = knew();
  linit(vm, "// this is a comment");
  t = lscan(vm);
  tint_eq(vm, t.type, TOK_EOF, "comment");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "(12-2)/5");
  tint_eq(vm, res, KVM_OK, "sub div res");
  tval_eq(vm, kpop(vm), NUM_VAL(2), "sub div ret");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "-true");
  tint_eq(vm, res, KVM_ERR_RUNTIME, "negate err");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "true");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "true literal eval");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "false");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "false literal eval");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "nil");
  tval_eq(vm, kpop(vm), NIL_VAL, "nil literal eval");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "!true");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "!true eval");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "!false");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "!false eval");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "1==1");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "== true");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "1==2");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "== false");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "1!=2");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "!= true");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "1!=1");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "!= false");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "1<1");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "< false");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "1<2");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "< true");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "2<=1");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "<= false");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "2<=3");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "<= true");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "3>2");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), "> true");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "3>7");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), "> false");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "3>=7");
  tval_eq(vm, kpop(vm), BOOL_VAL(false), ">= false");
  kfree(vm);

  vm = knew();
  res = kexec(vm, "3>=3");
  tval_eq(vm, kpop(vm), BOOL_VAL(true), ">= true");
  kfree(vm);

  ktest_summary();
}

#endif

