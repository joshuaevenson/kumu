//  kumu.c

#include "kumu.h"
#define STOP_NULL(k,v) if (!v) { k->stop = true; return NULL; }
#define STOP_RET(k,v) if (!v) { k->stop = true; return; }

// ------------------------------------------------------------
// State
// ------------------------------------------------------------
void k_regtype(k_state *K, const char *name) {
  if (K->tcap < K->tcount + 1) {
    int newcap = K->tcap * 2;
    size_t newsize = newcap * sizeof(k_type);
    size_t oldsize = K->tcap * sizeof(k_type);
    K->types = (k_type*) km_alloc(K, K->types, oldsize, newsize);
    STOP_RET(K, K->types)
    K->tcap = newcap;
    K->types[K->tcount].name = name;
    K->tcount++;
  }
}

k_state *k_new(void) {
  k_state *K = malloc(sizeof(k_state));
  K->allocated = sizeof(k_state);

  if (!K) {
    return NULL;
  }

  K->tcap = 8;
  K->types = (k_type*) km_alloc(K, NULL, 0, K->tcap * sizeof(k_type));
  STOP_NULL(K, K->types)
  k_regtype(K, "Int");
  k_regtype(K, "String");
  k_regtype(K, "Double");
  K->tcount = 3;
  K->freed = 0;
  K->stop = false;
  return K;
}

void k_close(k_state *K) {
  K->freed += sizeof(k_state);
  km_alloc(K, K->types, K->tcap * sizeof(k_type), 0);
  assert(K->allocated - K->freed == 0);
  free(K);
}

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------
char *km_alloc(k_state *K, void *ptr, size_t oldsize, size_t nsize) {
  
  K->allocated += nsize;
  K->freed += oldsize;
  
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
(type*)km_alloc(k, ptr, sizeof(type) * (old), sizeof(type) * (new))


// ------------------------------------------------------------
// Chunks
// ------------------------------------------------------------
void kc_init(k_state *K, k_chunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
}

void kc_write(k_state *K, k_chunk *chunk, uint8_t byte) {
  if (chunk->capacity < chunk->count + 1) {
    int cap = chunk->capacity;
    chunk->capacity = GROW_CAP(cap);
    chunk->code = GROW(K, uint8_t, chunk->code, cap, chunk->capacity);
    if (chunk->code == NULL) {
      K->stop = true;
    }
  }
  chunk->code[chunk->count] = byte;
  chunk->count++;
}

void kc_free(k_state *K, k_chunk *chunk) {
  km_alloc(K, chunk->code, chunk->capacity, 0);
}

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
void kva_init(k_state* K, k_val_array *array) {
  array->values = NULL;
  array->count = 0;
  array->capacity = 0;
}

void kva_write(k_state* K, k_val_array *array, k_val value) {
  if (array->capacity < array->count + 1) {
    int old = array->capacity;
    array->capacity = GROW_CAP(old);
    array->values = GROW(K, k_val, array->values, old, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

void kva_free(k_state* K, k_val_array *array) {
  km_alloc(K, array->values, array->capacity, 0);
  kva_init(K, array);
}

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void kc_dis_chunk(k_state *K, k_chunk *chunk, const char * name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count; offset++) {
    kc_dis_op(K, chunk, offset);
  }
}
static int kc_dis_simple(const char *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

int kc_dis_op(k_state *K, k_chunk *chunk, int offset) {
  printf("%04d ", offset);
  uint8_t op = chunk->code[offset];
  switch (op) {
    case OP_NOP:
      return kc_dis_simple("OP_NOP", offset);
    default:
      printf("Unknown opcode %d\n", op);
      return offset + 1;
  }
}


// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KUMU_REPL
int k_main(int argc, const char * argv[]) {
  k_state *K = k_new();
  printf("kumu %d.%d\n", KUMU_MAJOR, KUMU_MINOR);
  k_close(K);
  
  k_chunk chunk;
  kc_init(K, &chunk);
  kc_write(K, &chunk, OP_NOP);
  
  kc_dis_chunk(K, &chunk, "test");
  kc_free(K, &chunk);
  
  printf(">> allocated: %d, freed: %d, delta: %d\n", K->allocated,
         K->freed, K->allocated - K->freed);
  return 0;
}
#endif
