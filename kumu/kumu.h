// ------------------------------------------------------------
// Kumu - Hawaiian for "basic"
// Small, fast, familiar, portable
// ------------------------------------------------------------
// [ ] printf(format, ...)
// [ ] KVM_F_LIST list all code
// [ ] ku_save(), ku_load()
// [ ] ku_suspend(), ku_resume()
// [ ] lexer string escape \n, \r, \t, \0, \x
// [ ] repl readline support
// [ ] vm limits for code coverage
// [ ] kuobj use type bitmask for marked flag
// [ ] lexer number sci notation
// [ ] lexer hex notation
// [ ] arrays
// [ ] error code enum, #ifdef for string literals
// ------------------------------------------------------------
#ifndef KUMU_H
#define KUMU_H

// ------------------------------------------------------------
// Includes
// ------------------------------------------------------------
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

// ------------------------------------------------------------
// Versioning
// ------------------------------------------------------------
#define KVM_MAJOR          0
#define KVM_MINOR          1

#define UINT8_COUNT (UINT8_MAX + 1)

// ------------------------------------------------------------
// Forward
// ------------------------------------------------------------
struct _vm;
typedef struct _vm kuvm;

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
typedef enum {
  OBJ_FUNC,
  OBJ_CFUNC,
  OBJ_CLOSURE,
  OBJ_STR,
  OBJ_UPVAL,
  OBJ_CLASS,
  OBJ_INSTANCE,
  OBJ_BOUND_METHOD,
} kuobjtype;

typedef enum {
  FUNC_STD,
  FUNC_MAIN,
  FUNC_METHOD,
  FUNC_INIT,
} kufunctype;

typedef struct kuobj {
  kuobjtype type;
  bool marked;
  struct kuobj *next;
} kuobj;

void ku_obj_free(kuvm* vm, kuobj* obj);

typedef struct {
  kuobj obj;
  int len;
  char* chars;
  uint32_t hash;
} kustr;

typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUM,
  VAL_OBJ,
} kuvaltype;

typedef struct {
  kuvaltype type;
  union {
    bool bval;
    double dval;
    kuobj* oval;
  } as;
} kuval;

bool ku_obj_istype(kuval v, kuobjtype ot);

#define BOOL_VAL(v) ((kuval){ VAL_BOOL, { .bval = v }})
#define NIL_VAL ((kuval) { VAL_NIL, { .dval = 0 }})
#define NUM_VAL(v) ((kuval) { VAL_NUM, { .dval = v }})
#define OBJ_VAL(v) ((kuval) { VAL_OBJ, { .oval = (kuobj*)v} })

#define IS_BOOL(v) ((v).type == VAL_BOOL)
#define IS_NIL(v) ((v).type == VAL_NIL)
#define IS_NUM(v) ((v).type == VAL_NUM)
#define IS_OBJ(v) ((v).type == VAL_OBJ)
#define IS_STR(v) (ku_obj_istype(v, OBJ_STR))
#define IS_FUNC(v) (ku_obj_istype(v, OBJ_FUNC))
#define IS_CFUNC(v) (ku_obj_istype(v, OBJ_CFUNC))
#define IS_CLOSURE(v) (ku_obj_istype(v, OBJ_CLOSURE))
#define IS_CLASS(v) (ku_obj_istype(v, OBJ_CLASS))
#define IS_INSTANCE(v) (ku_obj_istype(v, OBJ_INSTANCE))
#define IS_BOUND_METHOD(v) (ku_obj_istype(v, OBJ_BOUND_METHOD))

#define AS_BOOL(v) ((v).as.bval)
#define AS_NUM(v) ((v).as.dval)
#define AS_OBJ(v) ((v).as.oval)
#define AS_STR(v) ((kustr*)AS_OBJ(v))
#define AS_CSTR(v) (((kustr*)AS_OBJ(v))->chars)
#define AS_FUNC(v) ((kufunc*)AS_OBJ(v))
#define AS_CFUNC(v) (((kucfunc*)AS_OBJ(v))->fn)
#define AS_CLOSURE(v) ((kuclosure*)AS_OBJ(v))
#define AS_CLASS(v) ((kuclass*)AS_OBJ(v))
#define AS_INSTANCE(v) ((kuinstance*)AS_OBJ(v))
#define AS_BOUND_METHOD(v) ((kuboundmethod*)AS_OBJ(v))

#define OBJ_TYPE(v) (AS_OBJ(v)->type)

bool ku_val_eq(kuval v1, kuval v2);

void ku_print_val(kuvm* vm, kuval value);

typedef struct {
    int capacity;
    int count;
    kuval* values;
} kuarr;

void ku_arr_init(kuvm* vm, kuarr* array);
void ku_arr_write(kuvm* vm, kuarr* array, kuval value);

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------

// 0,N => malloc, N,0 => free, N,M => realloc
char* ku_alloc(kuvm* vm, void* ptr, size_t old, size_t nsize);

// ------------------------------------------------------------
// OP codes
// ------------------------------------------------------------
typedef enum {
  OP_CALL,
  OP_CLASS,
  OP_CLOSURE,
  OP_CLOSE_UPVAL,
  OP_CONST,
  OP_RET,
  OP_NEG,
  OP_ADD,
  OP_SUB,
  OP_METHOD,
  OP_MUL,
  OP_DIV,
  OP_INVOKE,
  OP_INHERIT,
  OP_NOT,
  OP_NIL,
  OP_PRINT,
  OP_POP,
  OP_TRUE,
  OP_FALSE,
  OP_GT,
  OP_DEF_GLOBAL,
  OP_GET_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_GET_UPVAL,
  OP_SET_UPVAL,
  OP_GET_PROP,
  OP_SET_PROP,
  OP_GET_SUPER,
  OP_SUPER_INVOKE,
  OP_LT,
  OP_EQ,
  OP_JUMP_IF_FALSE,
  OP_JUMP,
  OP_LOOP,
} k_op;


// ------------------------------------------------------------
// Chunk
// ------------------------------------------------------------
typedef struct {
  int count;
  int capacity;
  uint8_t* code;
  int* lines;
  kuarr constants;
} kuchunk;

void ku_chunk_init(kuvm* vm, kuchunk* chunk);
void ku_chunk_write(kuvm* vm, kuchunk* chunk, uint8_t byte, int line);
void ku_chunk_free(kuvm* vm, kuchunk* chunk);
int ku_chunk_add_const(kuvm* vm, kuchunk* chunk, kuval value);

// ------------------------------------------------------------
// Upvalues
// ------------------------------------------------------------
typedef struct kuupobj {
  kuobj obj;
  kuval *location;
  kuval closed;
  struct kuupobj *next;
} kuupobj;

kuupobj *ku_upobj_new(kuvm *vm, kuval *slot);


// ------------------------------------------------------------
// Functions
// ------------------------------------------------------------
typedef struct {
  kuobj obj;
  int arity;
  int upcount;
  kuchunk chunk;
  kustr *name;
} kufunc;

kufunc *ku_func_new(kuvm *vm);
void ku_print_func(kuvm *vm, kufunc *fn);

// ------------------------------------------------------------
// Closures
// ------------------------------------------------------------
typedef struct {
  kuobj obj;
  kufunc *func;
  kuupobj **upvals;
  int upcount;
} kuclosure;

kuclosure *ku_closure_new(kuvm *vm, kufunc *f);

// ------------------------------------------------------------
// C Functions
// ------------------------------------------------------------
typedef kuval (*cfunc)(kuvm *vm, int argc, kuval *argv);

typedef struct {
  kuobj obj;
  cfunc fn;
} kucfunc;

kucfunc *ku_cfunc_new(kuvm *vm, cfunc f);
void ku_cfunc_def(kuvm *vm, const char *name, cfunc f);
void ku_reglibs(kuvm *vm);

// ------------------------------------------------------------
// Map / Hash table
// ------------------------------------------------------------
typedef struct {
  kustr* key;
  kuval value;
} kuentry;

typedef struct {
  int count;
  int capacity;
  kuentry* entries;
} kutable;

void ku_table_init(kuvm *vm, kutable* map);
void ku_table_free(kuvm *vm, kutable* map);
bool ku_table_set(kuvm* vm, kutable* map, kustr *key, kuval value);
bool ku_table_get(kuvm* vm, kutable* map, kustr* key, kuval *value);
bool ku_table_del(kuvm* vm, kutable* map, kustr* key);
void ku_table_copy(kuvm* vm, kutable* from, kutable* to);
kustr* ku_table_find(kuvm* vm, kutable* map, const char* chars, int len, uint32_t hash);

// ------------------------------------------------------------
// Classes
// ------------------------------------------------------------
typedef struct {
  kuobj obj;
  kustr *name;
  kutable methods;
} kuclass;

kuclass *ku_class_new(kuvm *vm, kustr *name);

// ------------------------------------------------------------
// Instances
// ------------------------------------------------------------
typedef struct {
  kuobj obj;
  kuclass *klass;
  kutable fields;
} kuinstance;

kuinstance *ku_instance_new(kuvm *vm, kuclass *klass);

// ------------------------------------------------------------
// Bound methods
// ------------------------------------------------------------
typedef struct {
  kuobj obj;
  kuval receiver;
  kuclosure *method;
} kuboundmethod;

kuboundmethod *ku_boundmethod_new(kuvm *vm, kuval receiver, kuclosure *method);

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
} kutoktype;

typedef struct {
  kutoktype type;
  const char* start;
  int len;
  int line;
} kutok;

typedef struct {
  const char* start;
  const char* curr;
  int line;
} kulex;


// ------------------------------------------------------------
// Locals
// ------------------------------------------------------------
#define MAX_LOCALS    (UINT8_MAX + 1)

typedef struct {
  kutok name;
  int depth;
  bool captured;
} kulocal;

typedef struct {
  uint8_t index;
  bool local;
} kuupval;

typedef struct kucompiler {
  struct kucompiler *enclosing;
  kufunc *function;
  kufunctype type;
  
  kulocal locals[MAX_LOCALS];
  int count;
  kuupval upvals[UINT8_COUNT];
  int depth;
} kucompiler;

void ku_compiler_init(kuvm *vm, kucompiler *compiler, kufunctype type);
void ku_block(kuvm *vm);
void ku_beginscope(kuvm *vm);
void ku_endscope(kuvm *vm);
void ku_declare_var(kuvm *vm);
void ku_addlocal(kuvm *vm, kutok name);
bool ku_identeq(kuvm *vm, kutok *a, kutok *b);
int ku_resolvelocal(kuvm *vm, kucompiler *compiler, kutok *name);
void ku_markinit(kuvm *vm);
int ku_print_byte_op(kuvm *vm, const char *name, kuchunk *chunk, int offset);


// ------------------------------------------------------------
// Branching
// ------------------------------------------------------------
void ku_ifstatement(kuvm *vm);
int ku_emitjump(kuvm *vm, k_op op);
void ku_patchjump(kuvm *vm, int offset);
void ku_emitloop(kuvm *vm, int start);
void ku_whilestatement(kuvm *vm);
void ku_forstatement(kuvm *vm);
int ku_print_jump_op(kuvm *vm, const char *name,
                     int sign, kuchunk *chunk, int offset);
void ku_parse_and(kuvm *vm, bool lhs);
void ku_parse_or(kuvm *vm, bool lhs);

// ------------------------------------------------------------
// Parser
// ------------------------------------------------------------
typedef struct {
  kutok curr;
  kutok prev;
  bool err;
  bool panic;
} kuparser;

// ------------------------------------------------------------
// Functions
// ------------------------------------------------------------
typedef struct {
  kuclosure *closure;
  uint8_t *ip;
  kuval *bp;
} kuframe;

// ------------------------------------------------------------
// Class compiler
// ------------------------------------------------------------
typedef struct kuclasscompiler {
  struct kuclasscompiler *enclosing;
  bool hassuper;
} kuclasscompiler;

// ------------------------------------------------------------
// VM
// ------------------------------------------------------------
typedef enum {
  KVM_OK,
  KVM_CONT,
  KVM_ERR_SYNTAX,
  KVM_ERR_RUNTIME,
  KVM_FILE_NOTFOUND,
} kures;

#define KVM_F_TRACE     0x00000001   // trace each instruction as it runs
#define KVM_F_STACK     0x00000002   // print stack in repl
#define KVM_F_LIST      0x00000004   // list instructions after compile
#define KVM_F_QUIET     0x00000008   // Supress error output (for tests)
#define KVM_F_TRACEMEM  0x00000010   // Trace memory
#define KVM_F_DISASM    0x00000020   // Disassemble after compile
#define KVM_F_NOEXEC    0x00000040   // Disable execution only compile
#define KVM_F_GCSTRESS  0x00000080   // GC every alloc increase
#define KVM_F_GCLOG     0x00000100   // Log GC action

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct _vm {
  uint64_t flags;
  uint8_t max_params;
  bool stop;
  size_t allocated;
  size_t gcnext;
  kuclasscompiler *curclass;

  kuframe frames[FRAMES_MAX];
  int framecount;
  kuval stack[STACK_MAX];
  kuval* sp;

  kustr *initstr;
  kuobj* objects;
  kutable strings;
  kutable globals;

  kuupobj *openupvals;
  
  kucompiler *compiler;
  kulex scanner;
  kuparser parser;
  
  int gccount;      // gray object count
  int gccap;        // gray object capacity
  kuobj **gcstack;  // gray object stack
  
} kuvm;

kuvm* ku_new(void);
void ku_free(kuvm* vm);
kures ku_run(kuvm* vm);
kures ku_runfile(kuvm* vm, const char* file);

// ------------------------------------------------------------
// Stack
// ------------------------------------------------------------
void ku_reset_stack(kuvm* vm);
void ku_push(kuvm* vm, kuval val);
kuval ku_pop(kuvm* vm);

// ------------------------------------------------------------
// GC
// ------------------------------------------------------------
void ku_gc(kuvm *vm);

// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void ku_print_chunk(kuvm* vm, kuchunk* chunk, const char* name);
int ku_print_op(kuvm* vm, kuchunk* chunk, int offset);

// ------------------------------------------------------------
// Config
// ------------------------------------------------------------

void ku_print_mem(kuvm* vm);
void ku_print_stack(kuvm* vm);
void ku_print_chunk(kuvm* vm, kuchunk* chunk, const char* name);


kures ku_exec(kuvm *vm, char *source);
kustr* ku_str_copy(kuvm* vm, const char* chars, int len);
void ku_lex_init(kuvm *vm, const char *source);
void ku_lex_print_all(kuvm *vm);
kutok ku_lex_scan(kuvm *vm);

kuchunk *ku_chunk(kuvm *vm);

#endif /* KUMU_H */

