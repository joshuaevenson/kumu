//  kumu.c

#include "kumu.h"

// #define KVM_TEST

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
void ktypearr_init(kvm *vm, ktypearr *t) {
  t->count = 0;
  t->capacity = 0;
  t->types = NULL;
}

void ktypearr_write(kvm *vm, ktypearr *t, const char *name) {
  if (t->capacity < t->count + 1) {
    int old = t->capacity;
    t->capacity = CAPACITY_GROW(old);
    t->types = ARRAY_GROW(vm, ktype, t->types, old, t->capacity);
  }
  t->types[t->count].name = name;
  t->count++;
}

void ktypearr_free(kvm *vm, ktypearr *t) {
  ARRAY_FREE(vm, ktype, vm->types.types, vm->types.capacity);
}

// ------------------------------------------------------------
// Scanner
// ------------------------------------------------------------
typedef enum {
// Single-character tokens.
  TOK_LPAR, TOK_RPAR, TOK_LBRACE, TOK_RBRACE, TOK_COMMA,
  TOK_DOT, TOK_MINUS, TOK_PLUS, TOK_SEMI, TOK_SLASH, TOK_STAR,
  // One or two character tokens.
  TOK_BANG, TOK_NE, TOK_EQ, TOK_EQEQ, TOK_GT, TOK_GE, TOK_LT,
  TOK_LE,
  // Literals.
  TOK_IDENT, TOK_STR, TOK_NUM,
  // Keywords.
  TOK_AND, TOK_CLASS, TOK_ELSE, TOK_FALSE, TOK_FOR, TOK_FUN,
  TOK_IF, TOK_NIL, TOK_OR, TOK_PRINT, TOK_RETURN, TOK_SUPER,
  TOK_THIS, TOK_TRUE, TOK_VAR, TOK_WHILE, TOK_ERR, TOK_EOF,
} ktoktype;

typedef struct {
  ktoktype type;
  const char *start;
  int len;
  int line;
} ktok;

static char klex_advance(kvm *vm) {
  vm->scanner.curr++;
  return vm->scanner.curr[-1];
}

static void klex_init(kvm *vm, const char *source) {
  vm->scanner.start = source;
  vm->scanner.curr = source;
  vm->scanner.line = 1;
}

static bool klex_isend(kvm *vm) {
  return (*(vm->scanner.curr) == '\0');
}

static bool kisdigit(char c) {
  return (c >= '0' && c <= '9');
}

static bool kisalpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
         (c == '_');
}

static char klex_peeknext(kvm *vm) {
  if (klex_isend(vm)) return '\0';
  return vm->scanner.curr[1];
}

static char kpeek(kvm *vm) {
  return *vm->scanner.curr;
}

static void klex_skipspace(kvm *vm) {
  while (true) {
    char c = kpeek(vm);
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        klex_advance(vm);
        break;
      case '\n':
        vm->scanner.line++;
        klex_advance(vm);
        break;
      case '/':
        if (klex_peeknext(vm) == '/') {
          while (kpeek(vm) != '\n' && !klex_isend(vm))
            klex_advance(vm);
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}
static ktok klex_make(kvm *vm, ktoktype type) {
  ktok token;
  token.type = type;
  token.start = vm->scanner.start;
  token.len = (int) (vm->scanner.curr - vm->scanner.start);
  token.line = vm->scanner.line;
  return token;
}

static ktok klex_err(kvm *vm, const char *msg) {
  ktok token;
  token.type = TOK_ERR;
  token.start = msg;
  token.len = (int)strlen(msg);
  token.line = vm->scanner.line;
  return token;
}

static bool klex_match(kvm *vm, char expected) {
  if (klex_isend(vm)) return false;
  if (*vm->scanner.curr != expected) return false;
  vm->scanner.curr++;
  return true;
}

static ktok klex_num(kvm *vm) {
  while(kisdigit(kpeek(vm))) klex_advance(vm);
  
  if (kpeek(vm) == '.' && kisdigit(klex_peeknext(vm))) {
    klex_advance(vm);
  }
  while(kisdigit(kpeek(vm))) klex_advance(vm);
  return klex_make(vm, TOK_NUM);
}

static ktok klex_str(kvm *vm) {
  while(kpeek(vm) != '"' && !klex_isend(vm)) {
    if (kpeek(vm) == '\n') vm->scanner.line++;
    klex_advance(vm);
  }
  if (klex_isend(vm)) return klex_err(vm, "unterminated string");
  klex_advance(vm);
  return klex_make(vm, TOK_STR);
}

static ktoktype klex_identype(kvm *vm) {
  return TOK_IDENT;
}

static ktok klex_ident(kvm *vm) {
  while (kisalpha(kpeek(vm)) || kisdigit(kpeek(vm))) {
    klex_advance(vm);
  }
  return klex_make(vm, klex_identype(vm));
}

static ktok klex_scan(kvm *vm) {
  klex_skipspace(vm);
  vm->scanner.start = vm->scanner.curr;
  
  if (klex_isend(vm)) {
    return klex_make(vm, TOK_EOF);
  }
  
  char c = klex_advance(vm);
  if (kisalpha(c)) return klex_ident(vm);
  if (kisdigit(c)) return klex_num(vm);
  switch (c) {
    case '(': return klex_make(vm, TOK_LPAR);
    case ')': return klex_make(vm, TOK_RPAR);
    case '{': return klex_make(vm, TOK_LBRACE);
    case '}': return klex_make(vm, TOK_RBRACE);
    case ';': return klex_make(vm, TOK_SEMI);
    case ',': return klex_make(vm, TOK_COMMA);
    case '.': return klex_make(vm, TOK_DOT);
    case '+': return klex_make(vm, TOK_PLUS);
    case '-': return klex_make(vm, TOK_MINUS);
    case '*': return klex_make(vm, TOK_STAR);
    case '/':
      return klex_make(vm, TOK_SLASH);
    case '!':
      return klex_make(vm, klex_match(vm, '=') ? TOK_NE : TOK_BANG);
    case '=':
      return klex_make(vm, klex_match(vm, '=') ? TOK_EQ : TOK_EQEQ);
    case '<':
      return klex_make(vm, klex_match(vm, '=') ? TOK_LE : TOK_LT);
    case '>':
      return klex_make(vm, klex_match(vm, '=') ? TOK_GE : TOK_GT);
    case '"':
      return klex_str(vm);
  }
  return klex_err(vm, "Unexpected character");
}
// ------------------------------------------------------------
// Virtual machine
// ------------------------------------------------------------
void kvm_resetstack(kvm *vm) {
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


kvm *kvm_new(void) {
  kvm *vm = malloc(sizeof(kvm));
  vm->allocated = sizeof(kvm);

  if (!vm) {
    return NULL;
  }

  vm->freed = 0;
  vm->stop = false;
  vm->chunk = NULL;

  kvm_resetstack(vm);
  ktypearr_init(vm, &vm->types);
  ktypearr_write(vm, &vm->types, "Int");
  ktypearr_write(vm, &vm->types, "String");
  ktypearr_write(vm, &vm->types, "Double");
  return vm;
}

void kvm_free(kvm *vm) {
  vm->freed += sizeof(kvm);
  ktypearr_free(vm, &vm->types);
  printf("allocated: %d, freed: %d, delta: %d\n",
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

static void _kvm_printstack(kvm *vm) {
  printf(" [");
  for (kval* vp = vm->stack; vp < vm->sp; vp++) {
    kval_print(vm, *vp);
    if (vp < vm->sp - 1) {
      printf(",");
    }
  }
  printf("]");
  printf("\n");
}



static kvmres _kvm_run(kvm *vm) {
#define BYTE_READ(vm) (*(vm->ip++))
#define CONST_READ(vm) (vm->chunk->constants.values[BYTE_READ(vm)])
#define BIN_OP(v,op) \
  do { \
    kval b = kpop(v); \
    kval a = kpop(v); \
    kpush(v, a op b); \
  } while (false)

  kvmres res = KVM_CONT;
  while (res == KVM_CONT) {
    uint8_t op;
#ifdef DEBUG_TRACE_EXEC
   kop_print(vm, vm->chunk, (int) (vm->ip - vm->chunk->code));
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
   _kvm_printstack(vm);
#endif
  }
  return KVM_OK;
#undef BYTE_READ
#undef CONST_READ
#undef BIN_OP
}

kvmres kvm_run(kvm *vm, kchunk *chunk) {
  vm->chunk = chunk;
  vm->ip = vm->chunk->code;
  return _kvm_run(vm);
}

static kvmres _kvm_compile(kvm *vm, char *source) {
  klex_init(vm, source);
  
  int line = -1;
  
  while (true) {
    ktok token = klex_scan(vm);
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

static kvmres _kvm_interpret(kvm *vm, char *source) {
  return _kvm_compile(vm, source);
}

static char *_kvm_readfile(kvm *vm, const char *path) {
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

kvmres kvm_runfile(kvm *vm, const char *file) {
  char *source = _kvm_readfile(vm, file);
  
  if (source == NULL) {
    return KVM_FILE_NOTFOUND;
  }
  kvmres res = _kvm_interpret(vm, source);
  free(source);
  return res;
}

#ifndef KVM_TEST
static void _kvm_repl(kvm *vm) {
  printf("kumu %d.%d\n", KVM_MAJOR, KVM_MINOR);
  char line[1024];
  
  while(true) {
    printf("k> ");
    
    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }
    
    _kvm_interpret(vm, line);
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
void kchunk_init(kvm *vm, kchunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  kvalarr_init(vm, &chunk->constants);
}

void kchunk_write(kvm *vm, kchunk *chunk, uint8_t byte, int line) {
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
  kvalarr_write(vm, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void kval_print(kvm *vm, kval value) {
  printf("%g", value);
}

void kvalarr_init(kvm* vm, kvalarr *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void kvalarr_write(kvm* vm, kvalarr *array, kval value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = CAPACITY_GROW(old);
    array->values = ARRAY_GROW(vm, kval, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

void kvalarr_free(kvm* vm, kvalarr *array) {
  kalloc(vm, array->values, array->capacity, 0);
  kvalarr_init(vm, array);
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void kchunk_print(kvm *vm, kchunk *chunk, const char * name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; offset++) {
    kop_print(vm, chunk, offset);
  }
}
static int kop_printsimple(const char *name, int offset) {
  printf("%-17s", name);
  return offset + 1;
}

static int kop_printconst(kvm *vm, const char *name, kchunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  printf("%-6s %4d '", name, con);
  kval_print(vm, chunk->constants.values[con]);
  printf("'");
  return offset+2;
}

int kop_print(kvm *vm, kchunk *chunk, int offset) {
#define OP_DEF1(o) \
case o:\
return kop_printsimple(#o, offset);
  
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
      return kop_printconst(vm, "OP_CONST", chunk, offset);
    default:
      printf("Unknown opcode %d\n", op);
      return offset + 1;
  }
#undef OP_DEF1
}


// ------------------------------------------------------------
// TEST
// ------------------------------------------------------------
#ifdef KVM_TEST
static void _kchunk_writeconst(kvm *vm, int cons, int line) {
  int index = kchunk_addconst(vm, vm->chunk, cons);
  kchunk_write(vm, vm->chunk, OP_CONST, line);
  kchunk_write(vm, vm->chunk, index, line);
}

static void _kvm_test(kvm *vm) {
  kchunk chunk;
  kchunk_init(vm, &chunk);
  vm->chunk = &chunk;

  int line = 1;
  kchunk_write(vm, &chunk, OP_NOP, line++);
  _kchunk_writeconst(vm, 1, line);
  _kchunk_writeconst(vm, 2, line);
  kchunk_write(vm, &chunk, OP_ADD, line);
  kchunk_write(vm, &chunk, OP_NEG, line++);

  _kchunk_writeconst(vm, 4, line);
  kchunk_write(vm, &chunk, OP_SUB, line++);

  _kchunk_writeconst(vm, 5, line);
  kchunk_write(vm, &chunk, OP_MUL, line++);

  _kchunk_writeconst(vm, 6, line);
  kchunk_write(vm, &chunk, OP_DIV, line++);

  kchunk_write(vm, &chunk, OP_RET, line);
  
  kvmres res = kvm_run(vm, &chunk);
  printf("res=%d\n", res);
  kchunk_free(vm, &chunk);
}
#endif

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KVM_MAIN
int kmain(int argc, const char * argv[]) {
  kvm *vm = kvm_new();
  
#ifdef KVM_TEST
  _kvm_test(vm);
#else
  if (argc == 1) {
    _kvm_repl(vm);
  } else if (argc == 2) {
    kvmres res = kvm_runfile(vm, argv[1]);
    if (res == KVM_ERR_RUNTIME) {
      kvm_free(vm);
      exit(70);
    }
    if (res == KVM_ERR_SYNTAX)  {
      kvm_free(vm);
      exit(65);
    }
    if (res == KVM_FILE_NOTFOUND) {
      fprintf(stderr, "file error '%s'\n", argv[1]);
      kvm_free(vm);
      exit(74);
    }
    
  } else {
    kvm_free(vm);
    fprintf(stderr, "usage kumu [file]\n");
    exit(64);
  }
#endif
  kvm_free(vm);
  return 0;
}
#endif
