//  kumu.c

#include "kumu.h"

void error(const char *msg) {
  fprintf(stderr, "error %s\n", msg);
  exit(-1);
}

// ------------------------------------------------------------
// Macros
// ------------------------------------------------------------
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)kalloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(vm, type, ptr, old) kalloc(vm, ptr, sizeof(type) * (old), 0)

#define DEBUG_TRACE_EXEC
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
      return lmake(vm, lmatch(vm, '=') ? TOK_EQ : TOK_EQEQ);
    case '<':
      return lmake(vm, lmatch(vm, '=') ? TOK_LE : TOK_LT);
    case '>':
      return lmake(vm, lmatch(vm, '=') ? TOK_GE : TOK_GT);
    case '"':
      return lstring(vm);
  }
  return lerror(vm, "Unexpected character");
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

static void perrorcur(kvm *vm, const char *msg) {
  perrorat(vm, &vm->parser.curr, msg);
}

static void padvance(kvm *vm) {
  vm->parser.prev = vm->parser.curr;
  
  while (true) {
    vm->parser.curr = lscan(vm);
    if (vm->parser.curr.type != TOK_ERR) break;
    perrorcur(vm, vm->parser.curr.start);
  }
}

static void pconsume(kvm *vm, ltype type, const char *msg) {
  if (vm->parser.curr.type == type) {
    padvance(vm);
    return;
  }
  perrorcur(vm, msg);
}

static void pemitbyte(kvm *vm, uint8_t byte) {
  cwrite(vm, vm->chunk, byte, vm->parser.prev.line);
}

kchunk *ccurr(kvm *vm) {
  return vm->compiling;
}

static void pend(kvm *vm) {
  pemitbyte(vm, OP_RET);
}

static void pemitbytes(kvm *vm, uint8_t b1, uint8_t b2) {
  pemitbyte(vm, b1);
  pemitbyte(vm, b2);
}


static uint8_t pmakeconst(kvm *vm, kval val) {
  int cons = kchunk_addconst(vm, ccurr(vm), val);
  if (cons > UINT8_MAX) {
    error("out of constant space");
    return 0;
  }
  return (uint8_t)cons;
}
static void pemitconst(kvm *vm, kval val) {
  pemitbyte(vm, pmakeconst(vm, val));
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

static void pprecedence(kvm *vm, kprecedence prec) {
  
}

static void pnumber(kvm *vm) {
  double val = strtod(vm->parser.prev.start, NULL);
  pemitconst(vm, val);
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
  
  pprecedence(vm, P_UNARY);
  pexpression(vm);
  
  switch(optype) {
    case TOK_MINUS: pemitbyte(vm, OP_NEG);
    default: return;
  }
}

static prule *pgetrule(kvm *vm, ltype optype);

static void pbinary(kvm *vm) {
  ltype optype = vm->parser.prev.type;
  prule *rule = pgetrule(vm, optype);
  pprecedence(vm, (kprecedence)(rule->precedence + 1));
  
  switch (optype) {
    case TOK_PLUS: pemitbyte(vm, OP_ADD); break;
    case TOK_MINUS: pemitbyte(vm, OP_SUB); break;
    case TOK_STAR: pemitbyte(vm, OP_MUL); break;
    case TOK_SLASH: pemitbyte(vm, OP_DIV); break;
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
  [TOK_BANG] =      { NULL,        NULL,     P_NONE },
  [TOK_NE] =        { NULL,        NULL,     P_NONE },
  [TOK_EQ] =        { NULL,        NULL,     P_NONE },
  [TOK_EQEQ] =      { NULL,        NULL,     P_NONE },
  [TOK_GT] =        { NULL,        NULL,     P_NONE },
  [TOK_GE] =        { NULL,        NULL,     P_NONE },
  [TOK_LT] =        { NULL,        NULL,     P_NONE },
  [TOK_LE] =        { NULL,        NULL,     P_NONE },
  [TOK_IDENT] =     { NULL,        NULL,     P_NONE },
  [TOK_STR] =       { NULL,        NULL,     P_NONE },
  [TOK_NUM] =       { pnumber,     NULL,     P_NONE },
  [TOK_AND] =       { NULL,        NULL,     P_NONE },
  [TOK_CLASS] =     { NULL,        NULL,     P_NONE },
  [TOK_ELSE] =      { NULL,        NULL,     P_NONE },
  [TOK_FOR] =       { NULL,        NULL,     P_NONE },
  [TOK_FUN] =       { NULL,        NULL,     P_NONE },
  [TOK_IF] =        { NULL,        NULL,     P_NONE },
  [TOK_NIL] =       { NULL,        NULL,     P_NONE },
  [TOK_OR] =        { NULL,        NULL,     P_NONE },
  [TOK_PRINT] =     { NULL,        NULL,     P_NONE },
  [TOK_SUPER] =     { NULL,        NULL,     P_NONE },
  [TOK_THIS] =      { NULL,        NULL,     P_NONE },
  [TOK_TRUE] =      { NULL,        NULL,     P_NONE },
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


kvm *knew(void) {
  kvm *vm = malloc(sizeof(kvm));
  vm->allocated = sizeof(kvm);

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

void kfree(kvm *vm) {
  vm->freed += sizeof(kvm);
  tafree(vm, &vm->types);
  printf("allocated: %d, freed: %d, delta: %d\n",
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

static void kprintstack(kvm *vm) {
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



static kres krunloop(kvm *vm) {
#define BYTE_READ(vm) (*(vm->ip++))
#define CONST_READ(vm) (vm->chunk->constants.values[BYTE_READ(vm)])
#define BIN_OP(v,op) \
  do { \
    kval b = kpop(v); \
    kval a = kpop(v); \
    kpush(v, a op b); \
  } while (false)

  kres res = KVM_CONT;
  while (res == KVM_CONT) {
    uint8_t op;
#ifdef DEBUG_TRACE_EXEC
   oprint(vm, vm->chunk, (int) (vm->ip - vm->chunk->code));
#endif

    switch(op = BYTE_READ(vm)) {
      case OP_NOP:
        break;
      case OP_RET: {
        kpop(vm);
        res = KVM_OK;
        break;
      }
      case OP_CONST: {
        kval con = CONST_READ(vm);
        kpush(vm, con);
        break;
      }
      case OP_NEG: kpush(vm, -kpop(vm)); break;
      case OP_ADD: BIN_OP(vm,+); break;
      case OP_SUB: BIN_OP(vm,-); break;
      case OP_MUL: BIN_OP(vm,*); break;
      case OP_DIV: BIN_OP(vm,/); break;
    }
#ifdef DEBUG_TRACE_EXEC
   kprintstack(vm);
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
  vm->compiling = chunk;
  padvance(vm);
  pexpression(vm);
  pconsume(vm, TOK_EOF, "expected expression end");
  pend(vm);
  return (vm->parser.err ? KVM_ERR_SYNTAX : KVM_OK);
  
  int line = -1;
  
  while (true) {
    ktok token = lscan(vm);
    if (token.line != line) {
      printf("%4d ", token.line);
    } else {
      printf("  |  ");
    }
    printf("%2d '%.*s'\n", token.type, token.len, token.start);
    
    if (token.type == TOK_EOF) {
      break;
    }
  }
  return KVM_OK;
}

static kres kexec(kvm *vm, char *source) {
  kchunk chunk;
  
  cinit(vm, &chunk);
  
  if (kcompile(vm, source, &chunk) != KVM_OK) {
    kchunk_free(vm, &chunk);
    return KVM_ERR_SYNTAX;
  }
  
  vm->chunk = &chunk;
  vm->ip = chunk.code;
  kres res = krun(vm, &chunk);
  kchunk_free(vm, &chunk);
  return res;
}

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
static void ktest(kvm *vm);

static void krepl(kvm *vm) {
  printf("kumu %d.%d\n", KVM_MAJOR, KVM_MINOR);
  char line[1024];
  
  while(true) {
    printf("k> ");
    
    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }
    
    if (strcmp(line, ".quit\n") == 0) {
      break;
    }

    if (strcmp(line, ".test\n") == 0) {
      ktest(vm);
      continue;
    }

    kexec(vm, line);
  }
}


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
    if (chunk->code == NULL) {
      vm->stop = true;
    }
  }
  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  chunk->count++;
}

void kchunk_free(kvm *vm, kchunk *chunk) {
  ARRAY_FREE(vm, uint8_t, chunk->code, chunk->capacity);
  ARRAY_FREE(vm, int, chunk->lines, chunk->capacity);
  ARRAY_FREE(vm, kval, chunk->constants.values, chunk->constants.capacity);
}

int kchunk_addconst(kvm *vm, kchunk *chunk, kval value) {
  vawrite(vm, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void vprint(kvm *vm, kval value) {
  printf("%g", value);
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

void vafree(kvm* vm, kvalarr *array) {
  kalloc(vm, array->values, array->capacity, 0);
  vainit(vm, array);
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void cprint(kvm *vm, kchunk *chunk, const char * name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; offset++) {
    oprint(vm, chunk, offset);
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
    case OP_CONST:
      return oprintconst(vm, "OP_CONST", chunk, offset);
    default:
      printf("Unknown opcode %d\n", op);
      return offset + 1;
  }
#undef OP_DEF1
}

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
int kmain(int argc, const char * argv[]) {
  kvm *vm = knew();
  
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

// ------------------------------------------------------------
// TEST
// ------------------------------------------------------------
static void cwriteconst(kvm *vm, int cons, int line) {
  int index = kchunk_addconst(vm, vm->chunk, cons);
  cwrite(vm, vm->chunk, OP_CONST, line);
  cwrite(vm, vm->chunk, index, line);
}

static void ktest(kvm *vm) {
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
  assert(res == KVM_OK);
  kchunk_free(vm, &chunk);
  
  res = kexec(vm, "x=20");
  assert(res == KVM_OK);
}
