//  kumu.c

#include "kumu.h"
#define STOP_NULL(k,v) if (!v) { k->stop = true; return NULL; }
#define STOP_RET(k,v) if (!v) { k->stop = true; return; }

// ------------------------------------------------------------
// State
// ------------------------------------------------------------
void k_regtype(State *S, const char *name) {
  if (S->tcap < S->tcount + 1) {
    int newcap = S->tcap * 2;
    size_t newsize = newcap * sizeof(Type);
    size_t oldsize = S->tcap * sizeof(Type);
    S->types = (Type*) MemAlloc(S, S->types, oldsize, newsize);
    STOP_RET(S, S->types)
    S->tcap = newcap;
    S->types[S->tcount].name = name;
    S->tcount++;
  }
}

State *StateNew(void) {
  State *S = malloc(sizeof(State));
  S->allocated = sizeof(State);

  if (!S) {
    return NULL;
  }

  S->tcap = 8;
  S->types = (Type*) MemAlloc(S, NULL, 0, S->tcap * sizeof(Type));
  STOP_NULL(S, S->types)
  k_regtype(S, "Int");
  k_regtype(S, "String");
  k_regtype(S, "Double");
  S->tcount = 3;
  S->freed = 0;
  S->stop = false;
  return S;
}

void StateFree(State *S) {
  S->freed += sizeof(State);
  MemAlloc(S, S->types, S->tcap * sizeof(Type), 0);
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
// Macros
// ------------------------------------------------------------
#define GROW_CAP(cap)  ((cap) < 8 ? 8 : (cap) * 2)
#define GROW(k, type, ptr, old, new)\
(type*)MemAlloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))


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
    chunk->capacity = GROW_CAP(cap);
    chunk->code = GROW(S, uint8_t, chunk->code, cap, chunk->capacity);
    if (chunk->code == NULL) {
      S->stop = true;
    }
  }
  chunk->code[chunk->count] = byte;
  chunk->count++;
}

void ChunkFree(State *S, Chunk *chunk) {
  MemAlloc(S, chunk->code, chunk->capacity, 0);
}

int ConstantAdd(State *S, Chunk *chunk, Value value) {
  ValueArrayWrite(S, &chunk->constants, value);
  return chunk->constants.count - 1;
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void ValueArrayInit(State* S, ValueArray *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void ValueArrayWrite(State* S, ValueArray *array, Value value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = GROW_CAP(old);
    array->values = GROW(S, Value, array->values, old, array->capacity);
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
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; offset++) {
    OpDisassemble(S, chunk, offset);
  }
}
static int OpSimpleDisassemble(const char *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

int OpDisassemble(State *S, Chunk *chunk, int offset) {
  printf("%04d ", offset);
  uint8_t op = chunk->code[offset];
  switch (op) {
    case OP_NOP:
      return OpSimpleDisassemble("OP_NOP", offset);
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
  StateFree(S);
  
  Chunk chunk;
  ChunkInit(S, &chunk);
  ChunkWrite(S, &chunk, OP_NOP);
  
  ChunkDisassemble(S, &chunk, "test");
  ChunkFree(S, &chunk);
  
  printf(">> allocated: %d, freed: %d, delta: %d\n", S->allocated,
         S->freed, S->allocated - S->freed);
  return 0;
}
#endif
