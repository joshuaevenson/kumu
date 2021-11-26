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
 var arr: [Int] = { 1, 2, 3 }; print(typeof(arr))
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

 // ------------------------------------------------------------
 // C API
 // ------------------------------------------------------------
 * kumu.h - language spec, tutorial, and API spec
 * kumu.c - lexer, parser, code gen, interpreter, and REPL
 * Compiler flags for lexer, parser, code-gen, test or REPL
 * 100% code coverage (how?)
 * Cross platform compile day 1

 ku_state *vm = ku_new();
 ku_getglobal(vm, "x");
 ku_pushinteger(vm, 20);
 ku_setglobal(vm);

 // ------------------------------------------------------------
 // Native class libraries
 // ------------------------------------------------------------
 * RegEx()
 * Utf8()
 * DateTime()
 * Crypto() - Hash, Block, BlockChain, Distributed, Tokens,
              Coinbase, Keys, Transactions
 

 // ------------------------------------------------------------
 // Command line arguments
 // ------------------------------------------------------------

 kumu --help

 kumu version 0.7.1.9
 --help                    -h  Show help
 --run <file>              -r  Load and run a file
 --lib <file>              -l  Include library/module
 --compile <file> <out>    -c  Compile to bytecodes
 --test <glob>             -t  Run tests using glob
 --selftest                -x  Run self test
 --lex                     -s  Scan only
 --parse                   -p  Parse only
 --verbose                 -v  Verbose output
---------------------------------------------------------------------- */

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
#include <assert.h>

// ------------------------------------------------------------
// Versioning
// ------------------------------------------------------------
#define KUMU_MAJOR          0
#define KUMU_MINOR          1

// ------------------------------------------------------------
// Configuration flags
// ------------------------------------------------------------
#define KUMU_REPL
#define KUMU_TEST

// ------------------------------------------------------------
// Forward
// ------------------------------------------------------------
struct _vm;
typedef struct _vm kvm;

// ------------------------------------------------------------
// Value
// ------------------------------------------------------------
typedef double kval;

void kval_print(kvm *vm, kval value);

typedef struct {
  int capacity;
  int count;
  kval *values;
} kvalarr;

void kvalarr_init(kvm* vm, kvalarr *array);
void kvalarr_write(kvm* vm, kvalarr *array, kval value);
void kvalarr_free(kvm* vm, kvalarr *array);

// ------------------------------------------------------------
// Type
// ------------------------------------------------------------
typedef struct {
  const char *name;
} ktype;

typedef struct  {
  int count;
  int capacity;
  ktype *types;
} ktypearr;

void ktypearr_init(kvm *vm, ktypearr *t);
void ktypearr_write(kvm *vm, ktypearr *t, const char *name);
void ktypearr_free(kvm *vm, ktypearr *t);

// ------------------------------------------------------------
// Memory
// ------------------------------------------------------------

// 0,N => malloc, N,0 => free, N,M => realloc
char *kalloc(kvm *vm, void *ptr, size_t old, size_t nsize);

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
} k_op;

// ------------------------------------------------------------
// Chunk
// ------------------------------------------------------------
typedef struct {
  int count;
  int capacity;
  uint8_t *code;
  int *lines;
  kvalarr constants;
} kchunk;

void kchunk_init(kvm *vm, kchunk *chunk);
void kchunk_write(kvm *vm, kchunk *chunk, uint8_t byte, int line);
void kchunk_free(kvm *vm, kchunk *chunk);
int kchunk_addconst(kvm *vm, kchunk *chunk, kval value);

// ------------------------------------------------------------
// VM
// ------------------------------------------------------------
#define STACK_MAX 256
typedef enum {
  KVM_OK,
  KVM_CONT,
  KVM_ERR_SYNTAX,
  KVM_ERR_RUNTIME,
} kvmres;

typedef struct _vm {
  ktypearr types;
  
  bool stop;
  int allocated;
  int freed;
  
  kchunk *chunk;
  uint8_t *ip;
  
  kval stack[STACK_MAX];
  kval *sp;
} kvm;

kvm *kvm_new(void);
void kvm_free(kvm *vm);
kvmres kvm_run(kvm *vm, kchunk *chunk);

// ------------------------------------------------------------
// Stack
// ------------------------------------------------------------
void kvm_resetstack(kvm *vm);
void kpush(kvm *vm, kval val);
kval kpop(kvm *vm);


// ------------------------------------------------------------
// Debug
// ------------------------------------------------------------
void kchunk_print(kvm *vm, kchunk *chunk, const char * name);
int kop_print(kvm *vm, kchunk *chunk, int offset);

// ------------------------------------------------------------
// REPL
// ------------------------------------------------------------
#ifdef KUMU_REPL
#include <stdio.h>

int kmain(int argc, const char * argv[]);
#endif

#endif /* KUMU_H */
