//  kumu.c

#include "kumu.h"

// ------------------------------------------------------------
// Macros
// ------------------------------------------------------------
#define CAPACITY_GROW(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define ARRAY_GROW(k, type, ptr, old, new)\
(type*)MemAlloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))
#define ARRAY_FREE(S, type, ptr, old) MemAlloc(S, ptr, sizeof(type) * (old), 0)

#define STOP_NULL(k,v) if (!v) { k->stop = true; return NULL; }
#define STOP_RET(k,v) if (!v) { k->stop = true; return; }

#define DEBUG_PREFIX  ""

// ------------------------------------------------------------
// Types
// ------------------------------------------------------------
void TypesInit(State *S, Types *t) {
  t->count = 0;
  t->capacity = 0;
  t->types = NULL;
}

void TypesAdd(State *S, Types *t, const char *name) {
  if (t->capacity < t->count + 1) {
    int old = t->capacity;
    t->capacity = CAPACITY_GROW(old);
    t->types = ARRAY_GROW(S, Type, t->types, old, t->capacity);
  }
  t->types[t->count].name = name;
  t->count++;
}

void TypesFree(State *S, Types *t) {
  ARRAY_FREE(S, Type, S->types.types, S->types.capacity);
}

// ------------------------------------------------------------
// State
// ------------------------------------------------------------
State *StateNew(void) {
  State *S = malloc(sizeof(State));
  S->allocated = sizeof(State);

  if (!S) {
    return NULL;
  }

  S->freed = 0;
  S->stop = false;

  TypesInit(S, &S->types);
  TypesAdd(S, &S->types, "Int");
  TypesAdd(S, &S->types, "String");
  TypesAdd(S, &S->types, "Double");
  return S;
}

void StateFree(State *S) {
  S->freed += sizeof(State);
  TypesFree(S, &S->types);
  assert(S->allocated - S->freed == 0);
  free(S);
}

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *MemAlloc(State *S, void *ptr, size_t oldsize, size_t nsize) {
  S->allocated += nsize;
  S->freed += oldsize;
  
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  return realloc(ptr, nsize);
}

// ------------------------------------------------------------
// Chunks
// ------------------------------------------------------------
void ChunkInit(State *S, Chunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  ValueArrayInit(S, &chunk->constants);
}

void ChunkWrite(State *S, Chunk *chunk, uint8_t byte) {
  if (chunk->capacity < chunk->count + 1) {
    int cap = chunk->capacity;
    chunk->capacity = CAPACITY_GROW(cap);
    chunk->code = ARRAY_GROW(S, uint8_t, chunk->code, cap, chunk->capacity);
    if (chunk->code == NULL) {
      S->stop = true;
    }
  }
  chunk->code[chunk->count] = byte;
  chunk->count++;
}

void ChunkFree(State *S, Chunk *chunk) {
  ARRAY_FREE(S, uint8_t, chunk->code, chunk->capacity);
  ARRAY_FREE(S, Value, chunk->constants.values, chunk->constants.capacity);
}

int ConstantAdd(State *S, Chunk *chunk, Value value) {
  ValueArrayWrite(S, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void ValuePrint(State *S, Value value) {
  printf("%g", value);
}

void ValueArrayInit(State* S, ValueArray *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void ValueArrayWrite(State* S, ValueArray *array, Value value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = CAPACITY_GROW(old);
    array->values = ARRAY_GROW(S, Value, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

void ValueArrayFree(State* S, ValueArray *array) {
  MemAlloc(S, array->values, array->capacity, 0);
  ValueArrayInit(S, array);
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void ChunkDisassemble(State *S, Chunk *chunk, const char * name) {
  printf("%s== %s ==\n", DEBUG_PREFIX, name);
  for (int offset = 0; offset < chunk->count; offset++) {
    OpDisassemble(S, chunk, offset);
  }
}
static int OpSimpleDisassemble(const char *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

static int OpConstDisassemble(State *S, const char *name, Chunk *chunk, int offset) {
  uint8_t con = chunk->code[offset+1];
  printf("%-16s %4d '", name, con);
  ValuePrint(S, chunk->constants.values[con]);
  printf("'\n");
  return offset+2;
}

int OpDisassemble(State *S, Chunk *chunk, int offset) {
  printf("%s%04d ", DEBUG_PREFIX, offset);
  uint8_t op = chunk->code[offset];
  switch (op) {
    case OP_NOP:
      return OpSimpleDisassemble("OP_NOP", offset);
    case OP_CONST:
      return OpConstDisassemble(S, "OP_CONST", chunk, offset);
    default:
      printf("Unknown opcode %d\n", op);
      return offset + 1;
  }
}

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KUMU_REPL
int Main(int argc, const char * argv[]) {
  State *S = StateNew();
  printf("kumu %d.%d\n", KUMU_MAJOR, KUMU_MINOR);
  
  Chunk chunk;
  ChunkInit(S, &chunk);
  int con = ConstantAdd(S, &chunk, 3.14);
  ChunkWrite(S, &chunk, OP_NOP);
  ChunkWrite(S, &chunk, OP_CONST);
  ChunkWrite(S, &chunk, con);
  
  
  
  ChunkDisassemble(S, &chunk, "test");
  ChunkFree(S, &chunk);
  
  printf("%sallocated: %d, freed: %d, delta: %d\n",
         DEBUG_PREFIX,
         S->allocated,
         S->freed, S->allocated - S->freed);
  StateFree(S);
  return 0;
}
#endif
