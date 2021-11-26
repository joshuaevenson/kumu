//  kumu.c

#include "kumu.h"

// ------------------------------------------------------------
// Macros
// ------------------------------------------------------------
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)kalloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(vm, type, ptr, old) kalloc(vm, ptr, sizeof(type) * (old), 0)

#define DEBUG_PREFIX  ""
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
// State
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
  printf("%sallocated: %d, freed: %d, delta: %d\n",
         DEBUG_PREFIX,
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

static void _kvm_printstack(kvm *vm) {
  printf("%s", DEBUG_PREFIX);
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
// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *kalloc(kvm *vm, void *ptr, size_t oldsize, size_t nsize) {
  
#ifdef MEMORY_TRACE
  printf("%smalloc %d -> %d\n", DEBUG_PREFIX, (int)oldsize, (int)nsize);
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
  printf("%s== %s ==\n", DEBUG_PREFIX, name);
  for (int offset = 0; offset < chunk->count; offset++) {
    kop_print(vm, chunk, offset);
  }
}
static int OpSimpleDisassemble(const char *name, int offset) {
  printf("%-17s", name);
  return offset + 1;
}

static int OpConstDisassemble(kvm *vm, const char *name, kchunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  printf("%-6s %4d '", name, con);
  kval_print(vm, chunk->constants.values[con]);
  printf("'");
  return offset+2;
}

int kop_print(kvm *vm, kchunk *chunk, int offset) {
#define OP_DEF1(o) \
case o:\
return OpSimpleDisassemble(#o, offset);
  
  printf("%s%04d ", DEBUG_PREFIX, offset);

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
      return OpConstDisassemble(vm, "OP_CONST", chunk, offset);
    default:
      printf("Unknown opcode %d\n", op);
      return offset + 1;
  }
#undef OP_DEF1
}

static void _kchunk_writeconst(kvm *vm, int cons, int line) {
  int index = kchunk_addconst(vm, vm->chunk, cons);
  kchunk_write(vm, vm->chunk, OP_CONST, line);
  kchunk_write(vm, vm->chunk, index, line);
}

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KUMU_REPL
int kmain(int argc, const char * argv[]) {
  kvm *vm = kvm_new();
  printf("kumu %d.%d\n", KUMU_MAJOR, KUMU_MINOR);
  
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
  printf("%sres=%d\n", DEBUG_PREFIX, res);
  kchunk_free(vm, &chunk);
  kvm_free(vm);
  return 0;
}
#endif
