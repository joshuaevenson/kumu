/* ----------------------------------------------------------------------
 Kumu 1.0
 Hawaiian for "basic"

 Small, fast, familiar, typed, imperative, portable, extensible

 // ------------------------------------------------------------
 // JS/TS/Swift familiar syntax
 // ------------------------------------------------------------
 func mult(x: Int, y: Int) -> Int {
   return x*y;
 }

 mult(2,3)
 >> 6
 mult(2, nil);
 >> runtime error line 7 - Int expected

 // ------------------------------------------------------------
 // Strong typing
 // ------------------------------------------------------------
 func add(x: Double, y: Double) -> Double {
   return x+y;
 }
 print(add(2,3));
 >> 5

 var z = func(x: Int, y: Int) -> Int {
   return x*y;
 }

 print(z(2,3))
 >> 6

 var x = 20*z
 >> runtime error line 12 - number expected

 z = "hello"
 var x:Int = 20*z
 >> runtime error line 14 - number expected

 func process(input: String, callback: Func) {
   var output = doWork(input)
   callback(output, true)
 }


 process("hello", func(x: Int, y: String) {
   print(x, y);
 })
 >> runtime error line 22 - function mismatch (Int, String)

 // ------------------------------------------------------------
 // Explicit variable declaration
 // ------------------------------------------------------------
 var x: Int = 2;
 var y = 2;

 x = 3;
 y = "hello";
 >> runtime error line 5 - Int expected
 z = 9;
 >> syntax error line 6 - undefined variable

 var t: Int
 print(t)
 >> 0

 var m;
 >> syntax error, variables must be typed or initialized

 // ------------------------------------------------------------
 // Optional semicolon (rules tbd)
 // ------------------------------------------------------------
 var x = 20
 var y = 30
 var z = x*y

 // ------------------------------------------------------------
 // Block lexical scoping
 // ------------------------------------------------------------
 var x = 10
 {
   var x = 20
   print(x)
 }
 >> 20
 >> 10

 var a = 8
 func foo() {
   return func bar() {
     return a*30    // resolves to scoped local +
   }                //                          |
   var a = 10       // <------------------------+
 }

 var m = foo()
 print(m())
 >> 300

 // ------------------------------------------------------------
 // Logical operators
 // ------------------------------------------------------------
 print(true && false)
 >> false
 print(true || false)
 >> true
 print(!true)
 >> false

 if (21 > 20) print("ok")
 >> syntax error line 8 - '{' expected

 // condition bracketing optional `{` end of condition
 if 21 > 20 { print("ok") }
 >> ok

 // ------------------------------------------------------------
 // Loops
 // ------------------------------------------------------------
 for(var i=0; i < 5; i=i+1) {
   print(i, ",")
 }
 print("\n")
 >> 0,1,2,3,4,

 var i = 0;
 >> runtime error line 7 - variable 'i' already declared

 i = 0
 while i < 5 {
   print(i, ",")
   i = i + 1
 }
 print("\n")
 >> 0,1,2,3,4

 // ------------------------------------------------------------
 // Closures
 // ------------------------------------------------------------
 func F() {
   var z = 20;
   return func(n) {
     return n*z
   }
 }
 var m = F()
 print(m(5))
 >> 100

 // ------------------------------------------------------------
 // Classes and objects
 // ------------------------------------------------------------
 class Foo {
   init(x: int) {
     this.x = x;
   }
 }

 func fn(i: Foo) {
   print(i.x);
 }

 var f: Foo = Foo();
 fn(f);
 var b: any = { x: 20 };
 fn(b);
 >> runtime error line 12 - Foo type expected
 var c = { x: 20 };
 fn(c);
 >> Runtime error line 14 - Foo type expected
 fn(d)
 >> syntax error line 14 - undefined variable 'd'

 // ------------------------------------------------------------
 // Arrays
 // ------------------------------------------------------------
 var arr: [Int] = { 1, 2, 3 };
print(typeof(arr))
 >> [Int]
 
var a2 = { 7, "banana", 9 };
 print(typeof(a2))
 >> Array


 // ------------------------------------------------------------
 // Constants and enums
 // ------------------------------------------------------------
 let a = 30
 a = 40
 >> syntax error line 2 - assign to constant variable 'a'

 // ------------------------------------------------------------
 // Modules and searchers
 // ------------------------------------------------------------

 
 // ------------------------------------------------------------
 // Constructors
 // ------------------------------------------------------------
 class Stock {
   init(t: String, v: Double) {
     this.ticker = t
     this.value = v
    }
 }

 var stocks: [Stock] = {
   { "MSFT", 350 },
   { "FB", 330 },
   { "GOOG", 2100 },
   { "AAPL", 279 },
   { "TSLA", 990 },
 };
 print(stocks.count)
 >> 5
 print(stocks[1].ticker)
 >> FB

 // ------------------------------------------------------------
 // Inheritance
 // ------------------------------------------------------------
 class Foo {
   func foo() {
     this.x = 20
     print("foo");
   }
 }

 class Bar: Foo {
   func foo() {
     super.foo()
     print("bar")
   }
 }


 // ------------------------------------------------------------
 // Resolver
 // ------------------------------------------------------------
 func b(z: Int) {
   var a = 10;
   return 30;
 }
 >> warning line 1 - unreferenced variable 'z'
 >> warning line 3 - unreferenced variable 'a'

 return 30
 >> syntax error line 5 - return in global scope

 break
 >> syntax error line 7 - break outside a loop

 class Foo() {
   var x = 20
 }

 var f = Foo()
 print(f.x)
 >> 20
 print(f.y)
 >> runtime error line 2 - field 'y' not found

 print(this)
 >> syntax error line 1 - cannot use 'this' outside a class

 class Foo {
   init() {
     return "hello"
   }
 }
 >> syntax error line 3 - cannot return value from constructor

 class Foo: Foo  {
   ...
 }
 >> syntax error line 1 - cannot inherit from self class

 */


/*
    Backlog
    - ku_save(), ku_load() state restoration for await(persist)
    - adjust <type> *<val> to <type>* <val> VS auto correct
    - Tune hash TABLE_MAX_LOAD based on benchmarks
    - Runtime flag for print last error
    - Print statement to buffer into vm optional string
    - Code coverage ku_test.c #include "kumu.c"
    - OP_POPN to remove all variables end of scope

    + remove type for now
    + kuvm, kuvar, kuvartype, kutable, kuarray, kuchunk, kuobj
    + ku_fn(vm), kuv_fn(value), kul_fn(lex), kup_fn(parse), kua_fn(arr)
    + kuc_* => ku_chunk_*, kul_* => ku_lex_*, ... etc.
    x simplevars[27] per global env? not worth it 
    + EXPECT_INT(), EXPECT_VAL() functions
*/

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

// ------------------------------------------------------------
// Forward
// ------------------------------------------------------------
struct _vm;
typedef struct _vm kuvm;

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
typedef enum {
  OBJ_STR,
} kuobjtype;

typedef struct {
  kuobjtype type;
  struct kuobj *next;
} kuobj;

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

#define AS_BOOL(v) ((v).as.bval)
#define AS_NUM(v) ((v).as.dval)
#define AS_OBJ(v) ((v).as.oval)
#define AS_STR(v) ((kustr*)AS_OBJ(v))
#define AS_CSTR(v) (((kustr*)AS_OBJ(v))->chars)

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
  OP_NOP,
  OP_CONST,
  OP_RET,
  OP_NEG,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
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
  OP_LT,
  OP_EQ,
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
} kumap;

void ku_map_init(kuvm *vm, kumap* map);
void ku_map_free(kuvm *vm, kumap* map);
bool ku_map_set(kuvm* vm, kumap* map, kustr *key, kuval value);
bool ku_map_get(kuvm* vm, kumap* map, kustr* key, kuval *value);
bool ku_map_del(kuvm* vm, kumap* map, kustr* key);
void ku_map_copy(kuvm* vm, kumap* from, kumap* to);
kustr* ku_map_find_str(kuvm* vm, kumap* map, const char* chars, int len, uint32_t hash);

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
// Parser
// ------------------------------------------------------------
typedef struct {
  kutok curr;
  kutok prev;
  bool err;
  bool panic;
} kuparser;

// ---------`---------------------------------------------------
// VM
// ------------------------------------------------------------
#define STACK_MAX 256
typedef enum {
  KVM_OK,
  KVM_CONT,
  KVM_ERR_SYNTAX,
  KVM_ERR_RUNTIME,
  KVM_FILE_NOTFOUND,
} kures;

#define KVM_F_TRACE     0x0001        // trace each instruction as it runs
#define KVM_F_STACK     0x0002        // print stack in repl
#define KVM_F_LIST      0x0004        // list instructions after compile
#define KVM_F_QUIET     0x0008        // Supress error output (for tests)
#define KVM_F_TRACEMEM  0x0010        // Trace memory
#define KVM_F_DISASM    0x0020        // Disassemble after compile

typedef struct _vm {
  uint64_t flags;
  bool stop;
  size_t allocated;
  size_t freed;

  kuchunk* chunk;
  uint8_t* ip;

  kuval stack[STACK_MAX];
  kuval* sp;

  kuobj* objects;
  kumap strings;
  kumap globals;

  char* last_err;
  kulex scanner;
  kuparser parser;
} kuvm;

kuvm* ku_new(void);
void ku_free(kuvm* vm);
kures ku_run(kuvm* vm, kuchunk* chunk);
kures ku_runfile(kuvm* vm, const char* file);

// ------------------------------------------------------------
// Stack
// ------------------------------------------------------------
void ku_reset_stack(kuvm* vm);
void ku_push(kuvm* vm, kuval val);
kuval ku_pop(kuvm* vm);


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

#endif /* KUMU_H */

