//
//  kutest.c
//  tests_macOS
//
//  Created by Mohsen Agsen on 12/7/21.
//

#include "kumu.h"
#include "kutest.h"

static int ktest_pass = 0;
int ktest_fail = 0;

 
static void EXPECT_TRUE(kuvm* vm, bool b, const char* msg) {
  if (b) {
    ktest_pass++;
    return;
  }
  ktest_fail++;
  printf(">>> expected true found false [%s]\n", msg);
}

static void EXPECT_INT(kuvm *vm, int v1, int v2, const char *m) {
  if (v1 == v2) {
    ktest_pass++;
    return;
  }
  ktest_fail++;
  printf(">>> expected: %d found %d [%s]\n", v2, v1, m);
}

static void EXPECT_VAL(kuvm* vm, kuval v1, kuval v2, const char *msg) {
  if (ku_equal(v1, v2)) {
    ktest_pass++;
    return;
  }
  
  uint64_t f = vm->flags;
  vm->flags &= ~KVM_F_QUIET;
  ktest_fail++;
  printf(">>> expected: ");
  ku_printval(vm, v2);
  printf(" found: ");
  ku_printval(vm, v1);
  printf(" [%s]\n", msg);
  vm->flags = f;
}

static void ku_test_summary() {
  printf(">>> tests %d passed %d failed\n", ktest_pass, ktest_fail);
}

kuval ku_get_global(kuvm* vm, const char* name) {
  kuval value;
  kustr* key = ku_strfrom(vm, name, (int)strlen(name));
  if (!ku_tabget(vm, &vm->globals, key, &value)) {
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

kuvm *kut_new(bool reglibs) {
  kuvm *vm = ku_new();
  vm->flags |= KVM_F_QUIET | KVM_F_LIST;
  
  if (reglibs) {
    ku_reglibs(vm);
  }
  return vm;
}

static kuval kutest_native_add(kuvm *vm, int argc, kuval *argv) {
  kuval b = argv[0];
  kuval a = argv[1];
  return NUM_VAL(AS_NUM(a) + AS_NUM(b));
}

static int kut_table_count(kuvm *vm, kutab *tab) {
  int count = 0;
  for (int i=0; i < tab->capacity; i++) {
    kuentry *s = &tab->entries[i];
    if (s->key != NULL) {
      count++;
    }
  }
  return count;
}

int tclass_cons = 0;
int tclass_scall = 0;
int tclass_sget = 0;
int tclass_sput = 0;
int tclass_sfree = 0;
int tclass_smark = 0;
int tclass_icall = 0;
int tclass_iget = 0;
int tclass_iput = 0;
int tclass_imark = 0;
int tclass_ifree = 0;

kuval test_cons(kuvm *vm, int argc, kuval *argv) {
  tclass_cons++;
  return NIL_VAL;
}

kuval test_scall(kuvm *vm, kustr *m, int argc, kuval *argv) {
  tclass_scall = argc;
  return NIL_VAL;
}

kuval test_sget(kuvm *vm, kustr *p) {
  tclass_sget++;
  return NIL_VAL;
}
kuval test_sput(kuvm *vm, kustr *p, kuval v) {
  tclass_sput = (int)AS_NUM(v);
  return NIL_VAL;
}

kuval test_sfree(kuvm *vm, kuobj *cc) {
  tclass_sfree++;
  return NIL_VAL;
}

kuval test_smark(kuvm *vm, kuobj *cc) {
  tclass_smark++;
  return NIL_VAL;
}

kuval test_icall(kuvm *vm, kustr *m, int argc, kuval *argv) {
  tclass_icall++;
  return NIL_VAL;
}

kuval test_iget(kuvm *vm, kustr *p) {
  tclass_iget++;
  return NIL_VAL;
}

kuval test_iput(kuvm *vm, kustr *p, kuval v) {
  tclass_iput++;
  return NIL_VAL;
}

kuval test_ifree(kuvm *vm, kuobj *o) {
  tclass_ifree++;
  return NIL_VAL;
}

kuval test_imark(kuvm *vm, kuobj *cc) {
  tclass_imark++;
  return NIL_VAL;
}

void tclass_reset(kuvm *vm) {
  tclass_cons = 0;
  tclass_scall = 0;
  tclass_sget = 0;
  tclass_sput = 0;
  tclass_sfree = 0;
  tclass_smark = 0;
  tclass_icall = 0;
  tclass_iget = 0;
  tclass_iput = 0;
  tclass_imark = 0;
  tclass_ifree = 0;
}

#define SCALL   0x00000001
#define SGET    0x00000002
#define SPUT    0x00000004
#define SMARK   0x00000008
#define SFREE   0x00000010
#define CONS    0x00000020
#define IGET    0x00000040
#define IPUT    0x00000080
#define IMARK   0x00000100
#define IFREE   0x00000200

#define ALL     0x00000fff

void tclass_init(kuvm *vm, uint64_t flags) {
  tclass_reset(vm);
  kucclass *cc = ku_cclassnew(vm, "test");
  
  if (flags & SCALL) cc->scall = test_scall;
  if (flags & SGET) cc->sget = test_sget;
  if (flags & SPUT) cc->sput = test_sput;
  if (flags & SMARK) cc->smark = test_smark;
  if (flags & SFREE) cc->sfree = test_sfree;
  if (flags & CONS) cc->cons = test_cons;
  if (flags & IGET) cc->iget = test_iget;
  if (flags & IPUT) cc->iput = test_iput;
  if (flags & IMARK) cc->imark = test_imark;
  if (flags & IFREE) cc->ifree = test_ifree;
  
  ku_cclassdef(vm, cc);
}

#define APPROX(a,b) ((AS_NUM(a)-b) < 0.00000001)

void ku_test() {
  kuvm *vm = kut_new(false);
  kures res = ku_exec(vm, "var x= -1+4;");
  EXPECT_INT(vm, res, KVM_OK, "var x= -1+4 res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = -1+4;");
  EXPECT_INT(vm, res, KVM_OK, "-1+4 res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(-1+4), "-1+4 ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = (-(1+2)-4)*5/6;");
  EXPECT_INT(vm, res, KVM_OK, "ku_run res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL((-(1.0+2.0)-4.0)*5.0/6.0), "ku_run ret");
  ku_free(vm);
  
  vm = kut_new(false);
  res = ku_exec(vm, "var x = 1+2;");
  EXPECT_INT(vm, res, KVM_OK, "ku_exec res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(3), "ku_exec ret");
  ku_free(vm);
  
  vm = kut_new(false);
  ku_lexinit(vm, "var x = 12+3;");
  ku_lexdump(vm);
  ku_free(vm);
  
  vm = kut_new(false);
  res = ku_exec(vm, "12+");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "12+");
  ku_free(vm);
  
  vm = kut_new(false);
  res = ku_exec(vm, "var x = (1+2)*3;");
  EXPECT_INT(vm, res, KVM_OK, "grouping res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(9), "grouping ret");
  ku_free(vm);
  
  vm = kut_new(false);
  vm->flags |= KVM_F_LIST;
  res = ku_exec(vm, "(1+2)*3");
  ku_free(vm);

  vm = kut_new(false);
  ku_printmem(vm);
  ku_free(vm);
  
  vm = kut_new(false);
  ku_lexinit(vm, "var x=30; \n  x=\"hello\";");
  ku_lexdump(vm);
  ku_free(vm);

  vm = kut_new(false);
  ku_exec(vm, "2*3");
  ku_printstack(vm);
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = -2*3;");
  EXPECT_INT(vm, res, KVM_OK, "unary res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(-6), "unary ret");
  ku_free(vm);

  // unterminated string
  vm = kut_new(false);
  ku_lexinit(vm, "\"hello");
  ku_lexdump(vm);
  ku_free(vm);

  // ku_print_val
  vm = kut_new(false);
  res = ku_exec(vm, "var x = 2+3;");
  kuval v = ku_get_global(vm, "x");
  EXPECT_VAL(vm, v, NUM_VAL(5), "ku_print_val ret");
  ku_printval(vm, v);
  ku_free(vm);
  
  vm = kut_new(false);
  res = ku_exec(vm, "var x = 12.3;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(12.3), "ku_lex_peeknext ret");
  ku_free(vm);
  
  vm = kut_new(false);
  ku_lexinit(vm, "and class else false for fun if nil or return super this true while {}!+-*/=!=><>=<= == => break continue far\ttrick\nart\rcool eek too fund");
  kutok t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_AND, "[and]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_CLASS, "[class]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_ELSE, "[else]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_FALSE, "[false]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_FOR, "[for]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_FUN, "[fun]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_IF, "[if]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_NIL, "[nil]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_OR, "[or]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_RETURN, "[return]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_SUPER, "[super]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_THIS, "[this]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_TRUE, "[true]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_WHILE, "[while]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_LBRACE, "[{]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_RBRACE, "[}]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_BANG, "[!]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_PLUS, "[+]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_MINUS, "[-]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_STAR, "[*]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_SLASH, "[/]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_EQ, "[=]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_NE, "[!=]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_GT, "[>]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_LT, "[<]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_GE, "[>=]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_LE, "[<=]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_EQEQ, "[==]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_ARROW, "[=>]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_BREAK, "[break]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_CONTINUE, "[continue]");

  
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_IDENT, "[identifier]");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_IDENT, "[identifier]");
  ku_free(vm);
  
  vm = kut_new(false);
  ku_lexinit(vm, "// this is a comment");
  t = ku_scan(vm);
  EXPECT_INT(vm, t.type, TOK_EOF, "comment");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "(12-2)/5");
  EXPECT_VAL(vm, v, NUM_VAL(2), "sub div ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "-true");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "negate err");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "true");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "true literal eval");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "false");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "false literal eval");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "nil");
  EXPECT_VAL(vm, v, NIL_VAL, "nil literal eval");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "!true");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "!true eval");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "!false");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "!false eval");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1==1");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "== true");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1==false");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "== mismatch types");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1==2");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "== false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1!=2");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "!= true");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1!=1");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "!= false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1<1");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "< false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "1<2");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "< true");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "2<=1");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "<= false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "2<=3");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "<= true");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "3>2");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "> true");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "3>7");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "> false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "3>=7");
  EXPECT_VAL(vm, v, BOOL_VAL(false), ">= false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "3>=3");
  EXPECT_VAL(vm, v, BOOL_VAL(true), ">= true");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 12 + true;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "add num expected");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "\"hello \" + \"world\"");
  EXPECT_INT(vm, IS_OBJ(v), true, "stradd type obj");
  EXPECT_INT(vm, AS_OBJ(v)->type, OBJ_STR, "stradd obj is str");
  char* chars = AS_CSTR(v);
  EXPECT_INT(vm, strcmp(chars, "hello world"), 0, "str val");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "\"hello \" == \"world\"");
  EXPECT_VAL(vm, v, BOOL_VAL(false), "str == false");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "\"hello\" == \"hello\"");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "str == true");
  ku_free(vm);

  vm = kut_new(false);
  v = ku_test_eval(vm, "\"hello \" != \"world\"");
  EXPECT_VAL(vm, v, BOOL_VAL(true), "str != true");
  ku_free(vm);

  vm = kut_new(false);
  kutab map;
  ku_tabinit(vm, &map);
  kustr* k1 = ku_strfrom(vm, "key1", 4);
  kustr* k2 = ku_strfrom(vm, "key1", 4);
  EXPECT_TRUE(vm, k1 == k2, "string intern equal");
  kustr* k3 = ku_strfrom(vm, "key2", 4);
  EXPECT_TRUE(vm, k3 != k2, "string intern not equal");
  bool isnew = ku_tabset(vm, &map, k1, NUM_VAL(3.14));
  EXPECT_TRUE(vm, isnew, "map set new");
  bool found = ku_tabget(vm, &map, k1, &v);
  EXPECT_TRUE(vm, found, "map get found");
  EXPECT_VAL(vm, v, NUM_VAL(3.14), "map get found value");
  found = ku_tabget(vm, &map, k3, &v);
  EXPECT_TRUE(vm, !found, "map get not found");
  found = ku_tabdel(vm, &map, k1);
  EXPECT_TRUE(vm, found, "map del found");
  found = ku_tabdel(vm, &map, k3);
  EXPECT_TRUE(vm, !found, "map del not found");

  
  found = ku_tabget(vm, &map, k1, &v);
  EXPECT_TRUE(vm, !found, "map del not found");
  kutab map2;
  ku_tabinit(vm, &map2);
  ku_tabcopy(vm, &map, &map2);
  ku_tabfree(vm, &map);
  ku_tabfree(vm, &map2);
  ku_tabdel(vm, &map, k1);
  found = ku_tabget(vm, &map, k1, &v);
  EXPECT_TRUE(vm, !found, "empty map get");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "x = 20;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "undeclard global assign");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 20;");
  EXPECT_INT(vm, res, KVM_OK, "global decl");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(20), "global init");
  res = ku_exec(vm, "x = 30;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(30), "global set");
  ku_free(vm);

  vm = kut_new(true);
  vm->flags |= KVM_F_TRACE | KVM_F_DISASM | KVM_F_QUIET;
  res = ku_exec(vm, "printf(12/3);");
  res = ku_exec(vm, "printf(\"hello\");");
  res = ku_exec(vm, "printf(true);");
  ku_free(vm);
  
  vm = kut_new(false);
  res = ku_exec(vm, "var x = 20; var y = 0; { var a=x*20; y = a; }");
  EXPECT_INT(vm, res, KVM_OK, "local decl");
  EXPECT_VAL(vm, ku_get_global(vm, "y"), NUM_VAL(400), "local init");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "{ var a = a; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "local own init");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_DISASM | KVM_F_QUIET;
  res = ku_exec(vm, "{ var a = 1; var b = 2; }");
  EXPECT_INT(vm, res, KVM_OK, "local op print");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 10; if (true) { x = 30; }");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(30), "if true");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 10; if (false) { x = 30; }");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(10), "if false");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 10; if (false) x = 30;  else x = 20;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(20), "else");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "if (true) { printf(222); }");
  EXPECT_INT(vm, res, KVM_OK, "if print");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "if true) { printf(222); }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "if no (");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "if (true { printf(222); }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "if no )");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = false and true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "false and true");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = false and false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "false and false");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = true and false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "true and false");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = true and true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "true and true");
  ku_free(vm);
  
  vm = kut_new(false);
  res = ku_exec(vm, "var x = false or true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "false or true");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = false or false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(false), "false or false");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = true or false;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "true or false");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = true or true;");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), BOOL_VAL(true), "true or true");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 1; while(x < 20) { x = x + 1; }");
  EXPECT_INT(vm, res, KVM_OK, "while parse");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(20), "while simple");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 1; while x < 20) { x = x + 1; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "while no lpar");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 1; while (x < 20 { x = x + 1; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "while no rpar");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 0; for(var j=0; j < 10; j=j+1) x = j;");
  EXPECT_INT(vm, res, KVM_OK, "for parse");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(9), "for simple");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 0; for var j=0; j < 10; j=j+1) x = j;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "for no lpar");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x = 0; for(; x < 10; x=x+1) printf(x);");
  EXPECT_INT(vm, res, KVM_OK, "for no init");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(10), "for no init");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = 0; for(; x < 10; ) x=x+1;");
  EXPECT_INT(vm, res, KVM_OK, "for no inc");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(10), "for no inc");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "fun foo(a) { print(\"ok\"); }");
  EXPECT_INT(vm, res, KVM_OK, "fun def");
  v = ku_get_global(vm, "foo");
  EXPECT_INT(vm, IS_OBJ(v),true, "fun object");
  ku_free(vm);

  vm = kut_new(true);
  vm->max_params = 1;
  res = ku_exec(vm, "fun foo(a,b) { printf(555); }; foo(4,5,6);");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "too many params");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "fun foo(a,b) { printf(2); }");
  EXPECT_INT(vm, res, KVM_OK, "func call def");
  res = ku_exec(vm, "foo(1,2,3);");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "func call mismatch");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "a=7; a();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "non-func call");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun a() { b(); }\nfun b() { b(12); }\na();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "too many args print");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "return 2;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "return from __main__");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 return fun foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip return");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 fun foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip fun");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 class foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip class");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 var foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip var");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 for foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip for");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 if foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip if");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=2 while foo()");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "parse_skip while");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun f(a) { return a*2; }\nvar x = f(3);");
  EXPECT_INT(vm, res, KVM_OK, "return expr res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(6), "return expr val");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun f(a) { var z = 2; }\nvar x = f(3);");
  EXPECT_INT(vm, res, KVM_OK, "implicit return res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NIL_VAL, "implicit return val");
  ku_free(vm);

  vm = kut_new(false);
  ku_cfuncdef(vm, "nadd", kutest_native_add);
  res = ku_exec(vm, "var x = nadd(3,4);");
  EXPECT_INT(vm, res, KVM_OK, "cfunc res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(7), "cfunc return");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x = clock();");
  EXPECT_INT(vm, res, KVM_OK, "clock res");
  v = ku_get_global(vm, "x");
  EXPECT_INT(vm, IS_NUM(v), true, "clock return");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "printf(12);");
  EXPECT_INT(vm, res, KVM_OK, "printf res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun M(x) { var m = x; fun e(n) { return m*n; } return e; }\n var z = M(5); var x = z(3);");
  EXPECT_INT(vm, res, KVM_OK, "closure res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(15), "closure val");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun o() { var a=7; var b=8; fun i() { return a+b; } return i; }\n var z = o(); var x = z();");
  EXPECT_INT(vm, res, KVM_OK, "closure2 res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(15), "closure2 val");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun f1(){var a1=1; fun f2() {var a2=2; fun f3(){ return a1+a2; } return f3; } return f2; }\n var v1=f1(); var v2=v1(); var v3=v2();");
  EXPECT_INT(vm, res, KVM_OK, "closure3 res");
  EXPECT_VAL(vm, ku_get_global(vm, "v3"), NUM_VAL(3), "closure3 val");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun M(x) { var m = x; fun e() { return m*m; } return e; }\n var z = M(5); var x = z();");
  EXPECT_INT(vm, res, KVM_OK, "closure4 res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(25), "closure4 val");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_GCSTRESS | KVM_F_GCLOG | KVM_F_QUIET;
  res = ku_exec(vm, "var x = \"hello\"; x=nil;");
  ku_gc(vm);
  EXPECT_INT(vm, res, KVM_OK, "gc res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NIL_VAL, "gc val");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_GCLOG | KVM_F_QUIET;
  vm->gcnext = 0;
  res = ku_exec(vm, "var x = \"hello\"; x=nil;");
  EXPECT_INT(vm, res, KVM_OK, "gcnext res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NIL_VAL, "gcnext val");
  ku_free(vm);

  
  vm = kut_new(true);
  res = ku_exec(vm, "printf(nil);");
  EXPECT_INT(vm, res, KVM_OK, "printf nil");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_GCSTRESS | KVM_F_GCLOG | KVM_F_QUIET;
  res = ku_exec(vm, "fun M(x) { var m = x; fun e() { return m*m; } return e; }\n var z = M(5); var x = z(); x = nil;");
  ku_gc(vm);
  EXPECT_INT(vm, res, KVM_OK, "gc closure res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NIL_VAL, "gc closure val");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_GCSTRESS | KVM_F_GCLOG | KVM_F_QUIET;
  res = ku_exec(vm, "fun M(x) { var m = x; var mm=x*2; fun e(n) { return m*n*mm; } return e; }\n var z = M(5); var x = z(3); x = nil;");
  EXPECT_INT(vm, res, KVM_OK, "gc closure2 res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NIL_VAL, "gc closure2 val");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = \"hello \" + \"world\";");
  int sc = kut_table_count(vm, &vm->strings);
  res = ku_exec(vm, "var y = \"hello\" + \" world\";");
  // +1 for the y value, +2 for two different substrings
  EXPECT_INT(vm, kut_table_count(vm, &vm->strings), sc+3, "string intern");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_TRACEMEM | KVM_F_TRACE | KVM_F_STACK;
  res = ku_exec(vm, "var x=1; var y=x*2;");
  EXPECT_INT(vm, res, KVM_OK, "tracing and printing");
  ku_free(vm);
  
  vm = kut_new(true);
  res = ku_exec(vm, "class Foo {}\nprintf(Foo);");
  EXPECT_INT(vm, res, KVM_OK, "class decl");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "class Foo {}\nvar f = Foo(); printf(f);");
  EXPECT_INT(vm, res, KVM_OK, "class cons");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var c = 7; c.p = 9;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "non-instance setprop");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var c = 7; var x = c.p;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "non-instance getprop");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class C{}\nvar c=C(); c.p=9; var x=c.p;");
  EXPECT_INT(vm, res, KVM_OK, "set/get prop res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(9), "set/get prop ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class C{}\nvar c=C(); c.p=9; var x=c.z;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "set/get prop not found");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=1; class C{ M() { x=3; } }\nvar c=C(); var m=c.M; m();");
  EXPECT_INT(vm, res, KVM_OK, "bound method res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(3), "bound method ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=1; class C{ M() { this.z=3; } }\nvar c=C(); c.M(); x=c.z;");
  EXPECT_INT(vm, res, KVM_OK, "this res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(3), "this ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = this;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "global this res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class C {}\nvar c=C(12,14);");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "no init with args");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class C { init(x) { this.x = x; }}\nvar c=C(12);var x = c.x;");
  EXPECT_INT(vm, res, KVM_OK, "init args res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(12), "init args ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class C { init(x) { this.x = x; return 7; }}\nvar c=C(12);var x = c.x;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "init return res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=1; class C { init() { fun f() { x=8; } this.f = f; } }\nvar c = C(); c.f();");
  EXPECT_INT(vm, res, KVM_OK, "field invoke res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(8), "field invoke ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A < A {}");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "class ownsubclass res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A < 12 {}");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "class bad inherit static res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var B = 9; class A < B {}");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "class bad inherit run res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x = super.foo();");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "global super res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A { f() { var x = super.foo(); } }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "no superclass super res");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=0; class A { f() { x=2; } }\nclass B < A {}\nvar b=B(); b.f();");
  EXPECT_INT(vm, res, KVM_OK, "super res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(2), "super ret");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = 0;
  res = ku_exec(vm, "class A { f() { return 2; } }\nclass B < A { f() { var z=super.f; return z()*3; }}\nvar b=B(); var x = b.f();");
  EXPECT_INT(vm, res, KVM_OK, "super call res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(6), "super call ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A { f() { return 2; } }\nclass B < A { f() { return super.f()*3; }}\nvar b=B(); var x = b.f();");
  EXPECT_INT(vm, res, KVM_OK, "super invoke res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(6), "super invoke ret");
  ku_free(vm);

  vm = kut_new(false);
  vm->max_const = 1;
  res = ku_exec(vm, "var x=1; var y=2;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "too many const");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun=7; ");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "invalid assign");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun f() { return; }");
  EXPECT_INT(vm, res, KVM_OK, "simple ret");
  ku_free(vm);

  vm = kut_new(false);
  vm->max_closures = 1;
  res = ku_exec(vm, "fun O() { var a=1; var b=2; fun I() { return a*b; } return e; }\n var z=M(); var x=z();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "too many closures res");
  ku_free(vm);

  vm = kut_new(false);
  vm->max_jump = 1;
  res = ku_exec(vm, "var x = 0; for(var j=0; j < 10; j=j+1) x = j;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "max jump");
  ku_free(vm);

  vm = kut_new(false);
  vm->max_body = 1;
  res = ku_exec(vm, "var x = 0; for(var j=0; j < 10; j=j+1) x = j;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "max body");
  ku_free(vm);

  vm = kut_new(false);
  vm->max_frames = 1;
  res = ku_exec(vm, "fun a(){} fun b(){a();} fun c(){b();} c();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "stack overflow");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A{}\nvar a=A(); a.x();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "invoke invalid prop");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A{}\nvar a=7; a.x();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "invoke invalid receiver");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A{} class B<A{ f() { super.x(); }} var b=B(); b.f();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "invoke invalid super");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=true; var y=-x;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "negate non-number");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = 0;
  res = ku_exec(vm, "fun O() { var a=1; var b=2; fun e() { a=9; return a*b; } return e; }\n var z=O(); var x=z();");
  EXPECT_INT(vm, res, KVM_OK, "closure set res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(18), "closure set ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "class A{} class B<A{ f() { var m = super.x; }} var b=B(); b.f();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "invalid get super");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_NOEXEC;
  res = ku_exec(vm, "var x=9;");
  EXPECT_INT(vm, res, KVM_OK, "noexec");
  ku_free(vm);

  vm = kut_new(false);
  vm->max_locals = 1;
  res = ku_exec(vm, "fun f() { var x=9; var m=2; }");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "max_locals");
  ku_free(vm);

  vm = kut_new(false);
  vm->flags = KVM_F_GCSTRESS | KVM_F_GCLOG | KVM_F_QUIET;
  res = ku_exec(vm, "class A{ f(){}} var a=A(); var z=a.f; a=nil;");
  ku_gc(vm);
  EXPECT_INT(vm, res, KVM_OK, "gc class res");
  EXPECT_VAL(vm, ku_get_global(vm, "a"), NIL_VAL, "gc class val");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var f=fun(a) {return a*2;}; var x=f(3);");
  EXPECT_INT(vm, res, KVM_OK, "anonymous function res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(6), "anonymous func ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun f(x) { return x(7);} var x=f(fun(a) { return a*2; });");
  EXPECT_INT(vm, res, KVM_OK, "fun arg res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(14), "fun arg ret");
  ku_free(vm);

  ku_lexinit(vm, "var x = 12+3;\nvar m=2;\nvar mm=99;");
  ku_lexdump(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var f = a => a*2; var x=f(3);");
  EXPECT_INT(vm, res, KVM_OK, "lambda res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(6), "lambda ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "fun f(x) { return x(2); } var x = f(a => a*3);");
  EXPECT_INT(vm, res, KVM_OK, "lambda arg res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(6), "lambda arg ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var f = { a,b => a*b }; var x=f(3,4);");
  EXPECT_INT(vm, res, KVM_OK, "lambda args res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(12), "lambda args ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var max = { a,b => { if (a>b) return a; else return b; }}; var x=max(3,14);");
  EXPECT_INT(vm, res, KVM_OK, "lambda args body res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(14), "lambda args body ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var abs = { a=> { if (a<0) return -a; else return a; }}; var x=abs(-12);");
  EXPECT_INT(vm, res, KVM_OK, "lambda body res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(12), "lambda body ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x =3; break;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "global break");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x =3; continue;");
  EXPECT_INT(vm, res, KVM_ERR_SYNTAX, "global continue");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=0; while(x < 5) { if (x > 2) break; x=x+1; }");
  EXPECT_INT(vm, res, KVM_OK, "while break res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(3), "while break ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=0; for(var i=0; i<10; i=i+1) { if (i > 2) break;  x = i;}");
  EXPECT_INT(vm, res, KVM_OK, "for break res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(2), "for break ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var x=0; var y=0; while(x < 5) { x=x+1; if (x > 2) continue; y=x; }");
  EXPECT_INT(vm, res, KVM_OK, "while continue res");
  EXPECT_VAL(vm, ku_get_global(vm, "y"), NUM_VAL(2), "while continue ret");
  ku_free(vm);

  vm = kut_new(false);
  res = ku_exec(vm, "var y=0; for(var x=0; x<5; x=x+1) { if (x > 2) continue; y=x; }");
  EXPECT_INT(vm, res, KVM_OK, "for continue res");
  EXPECT_VAL(vm, ku_get_global(vm, "y"), NUM_VAL(2), "for continue ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var y=\"12\"; var x=strlen(y);");
  EXPECT_INT(vm, res, KVM_OK, "strlen res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(2), "strlen ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=strlen();");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "strlen() nil");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=strlen(12);");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "strlen() non num");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var y=\"12\\n\\r\\t\"; var x=strlen(y);");
  EXPECT_INT(vm, res, KVM_OK, "strlen res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(5), "strlen ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=1.2e3;");
  EXPECT_INT(vm, res, KVM_OK, "1.2e3 res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(1.2e3), "1.2e3 ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=0xcafeb10b;");
  EXPECT_INT(vm, res, KVM_OK, "hex res");
  EXPECT_VAL(vm, ku_get_global(vm, "x"), NUM_VAL(0xcafeb10b), "hex ret");
  ku_free(vm);

  vm = kut_new(false);
  tclass_init(vm, 0);
  res = ku_exec(vm, "var x=test;");
  EXPECT_INT(vm, res, KVM_OK, "class res");
  EXPECT_TRUE(vm, IS_CCLASS(ku_get_global(vm, "x")), "class ret");
  ku_free(vm);
  EXPECT_INT(vm, tclass_sfree, 0, "class no sfree");

  vm = kut_new(false);
  tclass_init(vm, SFREE);
  res = ku_exec(vm, "var x=test;");
  EXPECT_INT(vm, res, KVM_OK, "class res");
  EXPECT_TRUE(vm, IS_CCLASS(ku_get_global(vm, "x")), "class ret");
  ku_free(vm);
  EXPECT_INT(vm, tclass_sfree, 1, "class sfree");

  vm = kut_new(false);
  tclass_init(vm, 0);
  res = ku_exec(vm, "var x=test.prop;");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "class no sget res");
  ku_free(vm);

  vm = kut_new(false);
  tclass_init(vm, SGET);
  res = ku_exec(vm, "var x=test.prop;");
  EXPECT_INT(vm, res, KVM_OK, "class sget res");
  EXPECT_INT(vm, tclass_sget, 1, "class sget ret");
  ku_free(vm);

  vm = kut_new(false);
  tclass_init(vm, SPUT);
  res = ku_exec(vm, "test.prop=8;");
  EXPECT_INT(vm, res, KVM_OK, "class sput res");
  EXPECT_INT(vm, tclass_sput, 8, "class sput ret");
  ku_free(vm);

  vm = kut_new(false);
  tclass_init(vm, 0);
  res = ku_exec(vm, "test.method(5,2,1);");
  EXPECT_INT(vm, res, KVM_ERR_RUNTIME, "class no scall res");
  ku_free(vm);
  
  vm = kut_new(false);
  tclass_init(vm, SCALL);
  res = ku_exec(vm, "test.method(5,2,1);");
  EXPECT_INT(vm, res, KVM_OK, "class scall res");
  EXPECT_INT(vm, tclass_scall, 3, "class scall ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=math.sin(math.pi);");
  EXPECT_INT(vm, res, KVM_OK, "math.sin res");
  EXPECT_TRUE(vm, APPROX(ku_get_global(vm, "x"), 0), "math.sin ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=math.cos(math.pi);");
  EXPECT_INT(vm, res, KVM_OK, "math.cos res");
  EXPECT_TRUE(vm, APPROX(ku_get_global(vm, "x"), -1), "math.cos ret");
  ku_free(vm);

  vm = kut_new(true);
  res = ku_exec(vm, "var x=math.tan(math.pi/4);");
  EXPECT_INT(vm, res, KVM_OK, "math.tan res");
  EXPECT_TRUE(vm, APPROX(ku_get_global(vm, "x"), 1), "math.tan ret");
  ku_free(vm);

  vm = kut_new(false);
  tclass_init(vm, SMARK);
  res = ku_exec(vm, "var x=test;");
  ku_gc(vm);
  EXPECT_INT(vm, tclass_smark, 1, "class smark false");
  res = ku_exec(vm, "x=nil;");
  ku_gc(vm);
  EXPECT_TRUE(vm, tclass_smark > 1, "class smark false");
  ku_free(vm);

  ku_test_summary();
}
