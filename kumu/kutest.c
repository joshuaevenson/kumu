//
//  kutest.c
//  tests_macOS
//
//  Created by Mohsen Agsen on 12/7/21.
//

#include "kumu.h"
#include "kutest.h"

static void ku_chunk_write_const(kuvm *vm, int cons, int line) {
  int index = ku_chunk_add_const(vm, ku_chunk(vm), NUM_VAL(cons));
  ku_chunk_write(vm, ku_chunk(vm), OP_CONST, line);
  ku_chunk_write(vm, ku_chunk(vm), index, line);
}

static int ktest_pass = 0;
int ktest_fail = 0;

 
static void EXPECT_TRUE(kuvm* vm, bool b, const char* msg) {
  if (b) {
    ktest_pass++;
    return;
  }
  ktest_fail++;
  printf("expected true found false [%s]", msg);
}

static void EXPECT_INT(kuvm *vm, int v1, int v2, const char *m) {
  if (v1 == v2) {
    ktest_pass++;
    return;
  }
  ktest_fail++;
  printf("expected: %d found %d [%s]\n", v1, v2, m);
}

static void EXPECT_VAL(kuvm* vm, kuval v1, kuval v2, const char *msg) {
  if (ku_val_eq(v1, v2)) {
    ktest_pass++;
    return;
  }
  
  uint64_t f = vm->flags;
  vm->flags &= ~KVM_F_QUIET;
  ktest_fail++;
  printf("expected: ");
  ku_print_val(vm, v2);
  printf(" found: ");
  ku_print_val(vm, v1);
  printf(" [%s]\n", msg);
  vm->flags = f;
}

static void ku_test_summary() {
  printf(">>> tests %d passed %d failed\n", ktest_pass, ktest_fail);
}

kuval ku_get_global(kuvm* vm, const char* name) {
  kuval value;
  kustr* key = ku_str_copy(vm, name, (int)strlen(name));
  if (!ku_map_get(vm, &vm->globals, key, &value)) {
    return NIL_VAL;
  }

  return value;
}


kuval ku_test_eval(kuvm* vm, const char* expr) {
  char buff[255];
  sprintf(buff, "var x = %s;", expr);
  kures res = ku_exec(vm, buff);
  if (res != KVM_OK) {
    return NIL_VAL;
  }
  return ku_get_global(vm, "x");
}

kuvm *kut_new(void) {
  kuvm *vm = ku_new();
  vm->flags |= KVM_F_QUIET | KVM_F_LIST;
  return vm;
}


void ku_test() {
  kuvm *vm = kut_new();
  
  kucompiler compiler;
  ku_compiler_init(vm, &compiler, FUNC_MAIN);
  kuchunk *chunk = &compiler.function->chunk;
  int line = 1;
  ku_chunk_write(vm, chunk, OP_NOP, line++);
  ku_chunk_write_const(vm, 1, line);
  ku_chunk_write_const(vm, 2, line);
  ku_chunk_write(vm, chunk, OP_ADD, line);
  ku_chunk_write(vm, chunk, OP_NEG, line++);
  ku_chunk_write_const(vm, 4, line);
  ku_chunk_write(vm, chunk, OP_SUB, line++);
  ku_chunk_write_const(vm, 5, line);
  ku_chunk_write(vm, chunk, OP_MUL, line++);
  ku_chunk_write_const(vm, 6, line);
  ku_chunk_write(vm, chunk, OP_DIV, line++);
  ku_chunk_write(vm, chunk, OP_RET, line);
  kures res = ku_run(vm, chunk);
  EXPECT_INT(vm, res, KVM_OK, "ku_run res");
  kuval v = ku_pop(vm);
  EXPECT_VAL(vm, v, NUM_VAL((-(1.0+2.0)-4.0)*5.0/6.0), "ku_run ret");
  ku_free(vm);
  
  vm = kut_new();
  res = ku_exec(vm, "var x = 1+2;");
  EXPECT_INT(vm, res, KVM_OK, "ku_exec res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(3), "ku_exec ret");
  ku_free(vm);
  
  vm = kut_new();
  ku_lex_init(vm, "var x = 12+3;");
  ku_lex_print_all(vm);
  ku_free(vm);
  
  vm = kut_new();
  res = ku_exec(vm, "12+");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "12+");
  ku_free(vm);
  
  vm = kut_new();
  res = ku_exec(vm, "var x = (1+2)*3;");
  EXPECT_INT(vm, res, KVM_OK, "grouping res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(9), "grouping ret");
  ku_free(vm);
  
  vm = kut_new();
  vm->flags |= KVM_F_LIST;
  res = ku_exec(vm, "(1+2)*3");
  ku_free(vm);

  vm = kut_new();
  ku_print_mem(vm);
  ku_free(vm);
  
  vm = kut_new();
  ku_lex_init(vm, "var x=30; \n  x=\"hello\";");
  ku_lex_print_all(vm);
  ku_free(vm);

  vm = kut_new();
  ku_exec(vm, "2*3");
  ku_print_stack(vm);
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = -2*3;");
  EXPECT_INT(vm, res, KVM_OK, "unary res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(-6), "unary ret");
  ku_free(vm);

  // unterminated string
  vm = kut_new();
  ku_lex_init(vm, "\"hello");
  ku_lex_print_all(vm);
  ku_free(vm);

  // ku_print_val
  vm = kut_new();
  res = ku_exec(vm, "var x = 2+3;");
  v = ku_get_global(vm, "x");
  EXPECT_VAL(vm, v, NUM_VAL(5), "ku_print_val ret");
  ku_print_val(vm, v);
  ku_free(vm);
  
  vm = kut_new();
  res = ku_exec(vm, "var x = 12.3;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(12.3), "ku_lex_peeknext ret");
  ku_free(vm);
  
  vm = kut_new();
  ku_lex_init(vm, "and class else false for fun if nil or print return super this true while {}!+-*/=!=><>=<= far\ttrick\nart\rcool eek too fund");
  kutok t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_AND, "[and]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_CLASS, "[class]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_ELSE, "[else]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_FALSE, "[false]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_FOR, "[for]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_FUN, "[fun]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_IF, "[if]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_NIL, "[nil]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_OR, "[or]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_PRINT, "[print]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_RETURN, "[return]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_SUPER, "[super]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_THIS, "[this]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_TRUE, "[true]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_WHILE, "[while]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_LBRACE, "[{]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_RBRACE, "[}]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_BANG, "[!]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_PLUS, "[+]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_MINUS, "[-]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_STAR, "[*]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_SLASH, "[/]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_EQ, "[=]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_NE, "[!=]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_GT, "[>]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_LT, "[<]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_GE, "[>=]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_LE, "[<=]");

  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_IDENT, "[identifier]");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_IDENT, "[identifier]");
  ku_free(vm);
  
  vm = kut_new();
  ku_lex_init(vm, "// this is a comment");
  t = ku_lex_scan(vm);
  EXPECT_INT(vm, t.type, TOK_EOF, "comment");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "(12-2)/5");
  EXPECT_VAL(vm, v, NUM_VAL(2), "sub div ret");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "-true");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "negate err");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "true");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "true literal eval");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "false");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "false literal eval");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "nil");
  EXPECT_VAL(vm, v, NIL_VAL, "nil literal eval");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "!true");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "!true eval");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "!false");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "!false eval");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "1==1");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "== true");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "1==2");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "== false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "1!=2");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "!= true");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "1!=1");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "!= false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "1<1");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "< false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "1<2");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "< true");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "2<=1");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "<= false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "2<=3");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "<= true");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "3>2");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "> true");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "3>7");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "> false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "3>=7");
  EXPECT_VAL(vm, v, BOOL_VAL(false), ">= false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "3>=3");
  EXPECT_VAL(vm, v, BOOL_VAL(true), ">= true");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 12 + true;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "add num expected");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "\"hello \" + \"world\"");
  EXPECT_INT(vm, v.type, VAL_OBJ, "stradd type obj");
  EXPECT_INT(vm, AS_OBJ(v)->type, OBJ_STR, "stradd obj is str");
  char* chars = AS_CSTR(v);
  EXPECT_INT(vm, strcmp(chars, "hello world"), 0, "str val");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "\"hello \" == \"world\"");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "str == false");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "\"hello\" == \"hello\"");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "str == true");
  ku_free(vm);

  vm = kut_new();
  v = ku_test_eval(vm, "\"hello \" != \"world\"");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "str != true");
  ku_free(vm);

  vm = kut_new();
  kumap map;
  ku_map_init(vm, &map);
  kustr* k1 = ku_str_copy(vm, "key1", 4);
  kustr* k2 = ku_str_copy(vm, "key1", 4);
  EXPECT_TRUE(vm, k1 == k2, "string intern equal");
  kustr* k3 = ku_str_copy(vm, "key2", 4);
  EXPECT_TRUE(vm, k3 != k2, "string intern not equal");
  bool isnew = ku_map_set(vm, &map, k1, NUM_VAL(3.14));
  EXPECT_TRUE(vm, isnew, "map set new");
  bool found = ku_map_get(vm, &map, k1, &v);
  EXPECT_TRUE(vm, found, "map get found");
  EXPECT_VAL(vm, v, NUM_VAL(3.14), "map get found value");
  found = ku_map_get(vm, &map, k3, &v);
  EXPECT_TRUE(vm, !found, "map get not found");
  found = ku_map_del(vm, &map, k1);
  EXPECT_TRUE(vm, found, "map del found");
  found = ku_map_get(vm, &map, k1, &v);
  EXPECT_TRUE(vm, !found, "map del not found");
  kumap map2;
  ku_map_init(vm, &map2);
  ku_map_copy(vm, &map, &map2);
  ku_map_free(vm, &map);
  ku_map_free(vm, &map2);
  ku_map_del(vm, &map, k1);
  found = ku_map_get(vm, &map, k1, &v);
  EXPECT_TRUE(vm, !found, "empty map get");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "x = 20;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "undeclard global assign");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 20;");
  EXPECT_INT(vm, res, KVM_OK, "global decl");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(20), "global init");
  res = ku_exec(vm, "x = 30;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(30), "global set");
  ku_free(vm);

  vm = kut_new();
  vm->flags |= KVM_F_TRACE | KVM_F_DISASM | KVM_F_QUIET;
  res = ku_exec(vm, "print 12/3;");
  res = ku_exec(vm, "print \"hello\";");
  res = ku_exec(vm, "print true;");
  ku_free(vm);
  
  vm = kut_new();
  res = ku_exec(vm, "var x = 20; var y = 0; { var a=x*20; y = a; }");
  EXPECT_INT(vm, res, KVM_OK, "local decl");
  EXPECT_VAL(vm, ku_get_global(vm, "y"), NUM_VAL(400), "local init");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "{ var a = a; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "local own init");
  ku_free(vm);

  vm = kut_new();
  vm->flags = KVM_F_DISASM | KVM_F_QUIET;
  res = ku_exec(vm, "{ var a = 1; var b = 2; }");
  EXPECT_INT(vm, res, KVM_OK, "local op print");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 10; if (true) { x = 30; }");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(30), "if true");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 10; if (false) { x = 30; }");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(10), "if false");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 10; if (false) x = 30;  else x = 20;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(20), "else");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "if (true) { print 222; }");
  EXPECT_INT(vm, res, KVM_OK, "if print");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "if true) { print 222; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "if no (");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "if (true { print 222; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "if no )");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = false and true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "false and true");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = false and false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "false and false");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = true and false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "true and false");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = true and true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "true and true");
  ku_free(vm);
  
  vm = kut_new();
  res = ku_exec(vm, "var x = false or true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "false or true");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = false or false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "false or false");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = true or false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "true or false");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = true or true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "true or true");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 1; while(x < 20) { x = x + 1; }");
  EXPECT_INT(vm, res, KVM_OK, "while parse");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(20), "while simple");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 1; while x < 20) { x = x + 1; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "while no lpar");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 1; while (x < 20 { x = x + 1; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "while no rpar");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 0; for(var j=0; j < 10; j=j+1) x = j;");
  EXPECT_INT(vm, res, KVM_OK, "for parse");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(9), "for simple");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 0; for var j=0; j < 10; j=j+1) x = j;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "for no lpar");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 0; for(; x < 10; x=x+1) print x;");
  EXPECT_INT(vm, res, KVM_OK, "for no init");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(10), "for no init");
  ku_free(vm);

  vm = kut_new();
  res = ku_exec(vm, "var x = 0; for(; x < 10; ) x=x+1;");
  EXPECT_INT(vm, res, KVM_OK, "for no inc");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(10), "for no inc");
  ku_free(vm);

  ku_test_summary();
}
