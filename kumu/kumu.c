//  kumu.c

#include "kumu.h"

// ------------------------------------------------------------
// Macros
// ------------------------------------------------------------
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)MemAlloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(vm, type, ptr, old) MemAlloc(vm, ptr, sizeof(type) * (old), 0)

#define DEBUG_PREFIX  ""
#define DEBUG_TRACE_EXEC
// ------------------------------------------------------------
// Types
// ------------------------------------------------------------
void TypesInit(VM *vm, Types *t) {
  t->count = 0;
  t->capacity = 0;
  t->types = NULL;
}

void TypesAdd(VM *vm, Types *t, const char *name) {
  if (t->capacity < t->count + 1) {
    int old = t->capacity;
    t->capacity = CAPACITY_GROW(old);
    t->types = ARRAY_GROW(vm, Type, t->types, old, t->capacity);
  }
  t->types[t->count].name = name;
  t->count++;
}

void TypesFree(VM *vm, Types *t) {
  ARRAY_FREE(vm, Type, vm->types.types, vm->types.capacity);
}

// ------------------------------------------------------------
// State
// ------------------------------------------------------------
void StackReset(VM *vm) {
  vm->sp = vm->stack;
}

void push(VM *vm, Value val) {
  *(vm->sp) = val;
  vm->sp++;
}

Value pop(VM *vm) {
  vm->sp--;
  return *(vm->sp);
}


VM *VMNew(void) {
  VM *vm = malloc(sizeof(VM));
  vm->allocated = sizeof(VM);

  if (!vm) {
    return NULL;
  }

  vm->freed = 0;
  vm->stop = false;
  vm->chunk = NULL;

  StackReset(vm);
  TypesInit(vm, &vm->types);
  TypesAdd(vm, &vm->types, "Int");
  TypesAdd(vm, &vm->types, "String");
  TypesAdd(vm, &vm->types, "Double");
  return vm;
}

void VMFree(VM *vm) {
  vm->freed += sizeof(VM);
  TypesFree(vm, &vm->types);
  printf("%sallocated: %d, freed: %d, delta: %d\n",
         DEBUG_PREFIX,
         vm->allocated,
         vm->freed, vm->allocated - vm->freed);
  assert(vm->allocated - vm->freed == 0);
  free(vm);
}

static void StackPrint(VM *vm) {
  printf("%s", DEBUG_PREFIX);
  printf(" [");
  for (Value* vp = vm->stack; vp < vm->sp; vp++) {
    ValuePrint(vm, *vp);
    if (vp < vm->sp - 1) {
      printf(",");
    }
  }
  printf("]");
  printf("\n");
}



static VMResult _VMRun(VM *vm) {
#define BYTE_READ(vm) (*(vm->ip++))
#define CONST_READ(vm) (vm->chunk->constants.values[BYTE_READ(vm)])
#define BIN_OP(v,op) \
  do { \
    Value b = pop(v); \
    Value a = pop(v); \
    push(v, a op b); \
  } while (false)

  VMResult res = VM_CONT;
  while (res == VM_CONT) {
    uint8_t op;
#ifdef DEBUG_TRACE_EXEC
   OpDisassemble(vm, vm->chunk, (int) (vm->ip - vm->chunk->code));
#endif

    switch(op = BYTE_READ(vm)) {
      case OP_NOP:
        break;
      case OP_RET: {
        pop(vm);
        res = VM_OK;
        break;
      }
      case OP_CONST: {
        Value con = CONST_READ(vm);
        push(vm, con);
        break;
      }
      case OP_NEG: push(vm, -pop(vm)); break;
      case OP_ADD: BIN_OP(vm,+); break;
      case OP_SUB: BIN_OP(vm,-); break;
      case OP_MUL: BIN_OP(vm,*); break;
      case OP_DIV: BIN_OP(vm,/); break;
    }
#ifdef DEBUG_TRACE_EXEC
   StackPrint(vm);
#endif
  }
  return VM_OK;
#undef BYTE_READ
#undef CONST_READ
#undef BIN_OP
}

VMResult VMRun(VM *vm, Chunk *chunk) {
  vm->chunk = chunk;
  vm->ip = vm->chunk->code;
  return _VMRun(vm);
}
// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *MemAlloc(VM *vm, void *ptr, size_t oldsize, size_t nsize) {
  
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
void ChunkInit(VM *vm, Chunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  ValueArrayInit(vm, &chunk->constants);
}

void ChunkWrite(VM *vm, Chunk *chunk, uint8_t byte, int line) {
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

void ChunkFree(VM *vm, Chunk *chunk) {
  ARRAY_FREE(vm, uint8_t, chunk->code, chunk->capacity);
  ARRAY_FREE(vm, int, chunk->lines, chunk->capacity);
  ARRAY_FREE(vm, Value, chunk->constants.values, chunk->constants.capacity);
}

int ConstantAdd(VM *vm, Chunk *chunk, Value value) {
  ValueArrayWrite(vm, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void ValuePrint(VM *vm, Value value) {
  printf("%g", value);
}

void ValueArrayInit(VM* vm, ValueArray *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void ValueArrayWrite(VM* vm, ValueArray *array, Value value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = CAPACITY_GROW(old);
    array->values = ARRAY_GROW(vm, Value, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

void ValueArrayFree(VM* vm, ValueArray *array) {
  MemAlloc(vm, array->values, array->capacity, 0);
  ValueArrayInit(vm, array);
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void ChunkDisassemble(VM *vm, Chunk *chunk, const char * name) {
  printf("%s== %s ==\n", DEBUG_PREFIX, name);
  for (int offset = 0; offset < chunk->count; offset++) {
    OpDisassemble(vm, chunk, offset);
  }
}
static int OpSimpleDisassemble(const char *name, int offset) {
  printf("%-17s", name);
  return offset + 1;
}

static int OpConstDisassemble(VM *vm, const char *name, Chunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  printf("%-6s %4d '", name, con);
  ValuePrint(vm, chunk->constants.values[con]);
  printf("'");
  return offset+2;
}

int OpDisassemble(VM *vm, Chunk *chunk, int offset) {
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

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KUMU_REPL
int Main(int argc, const char * argv[]) {
  VM *vm = VMNew();
  printf("kumu %d.%d\n", KUMU_MAJOR, KUMU_MINOR);
  
  Chunk chunk;
  ChunkInit(vm, &chunk);
  int c1 = ConstantAdd(vm, &chunk, 1);
  int c2 = ConstantAdd(vm, &chunk, 2);

  int line = 1;
  ChunkWrite(vm, &chunk, OP_NOP, line);
  ChunkWrite(vm, &chunk, OP_CONST, ++line);
  ChunkWrite(vm, &chunk, c1, line);
  ChunkWrite(vm, &chunk, OP_CONST, line);
  ChunkWrite(vm, &chunk, c2, line);
  ChunkWrite(vm, &chunk, OP_ADD, line);
  ChunkWrite(vm, &chunk, OP_NEG, 122);
  
  int c3 = ConstantAdd(vm, &chunk, 4);
  ChunkWrite(vm, &chunk, OP_CONST, ++line);
  ChunkWrite(vm, &chunk, c3, line);
  ChunkWrite(vm, &chunk, OP_SUB, line);

  int c4 = ConstantAdd(vm, &chunk, 5);
  ChunkWrite(vm, &chunk, OP_CONST, ++line);
  ChunkWrite(vm, &chunk, c4, line);
  ChunkWrite(vm, &chunk, OP_MUL, line);

  int c5 = ConstantAdd(vm, &chunk, 6);
  ChunkWrite(vm, &chunk, OP_CONST, ++line);
  ChunkWrite(vm, &chunk, c5, line);
  ChunkWrite(vm, &chunk, OP_DIV, line);


  ChunkWrite(vm, &chunk, OP_RET, 122);
  
  VMResult res = VMRun(vm, &chunk);
  printf("%sres=%d\n", DEBUG_PREFIX, res);
//  ChunkDisassemble(vm, &chunk, "test");
  ChunkFree(vm, &chunk);
  VMFree(vm);
  return 0;
}
#endif
