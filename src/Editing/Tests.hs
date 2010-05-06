module Main (main) where

import qualified Editing.EditsPreparation
import qualified Editing.Parse
import qualified Editing.Execute
import qualified Editing.Diff
import qualified Cxx.Show
import qualified Cxx.Parse

import Parsers (eof, parseOrFailE)
import Util (test_cmp, (<<), (.))
import Request (EditableRequest(..), EditableRequestKind(..))

import Prelude hiding ((.))
import Data.SetOps

basic_tests :: IO ()
basic_tests = do

  f "" "make foo bar bas" "Unexpected end of command. Expected \"::\" or template-arguments."
  f "" "make " "Unexpected end of command. Expected \"declaration\", \"body\", production-name, \"all\", \"any\", \"every\", \"each\", ordinal, or declarator-id."
  f "" "make i pointer to" "Unexpected end of command. Expected type description."
  s "mutable int i;" "make i const" "const mutable int i;"
  s "int i;" "make i a const double" "double const i;"
  s "int const i;" "make i a double" "double const i;"
  s "void g(int); void f() { bind(&g)(3, 2); }" "make g a function taking two integers" "void g(int, int); void f() { bind(&g)(3, 2); }"
    -- "bind(&g)(3, 2);" is determined not to be declaration because it would be a reference initialized with two arguments.
  s "int i;" "make i a *&" "int *& i;"
  s "int * p = f();" "make p auto" "auto p = f();"
  s "inline void f();" "make f noninline" "void f();"
  s "int (* const i)(double);" "make i nonconst" "int (* i)(double);"
  s "int i;" "make i a pointer&" "int *& i;"
  f "int i;" "make j const" "Could not find free declaration of j."
  s "void f() { int i; i; i = 0; }" "make i a pointer" "void f() { int * i; i; i = 0; }"
  s "struct X { static void f(); }" "append semicolon and make f pure" "struct X { virtual void f()= 0 ; };"
  s "struct X { virtual void f() = 0; };" "make f static" "struct X { static void f() ; };"
  s "struct X { void f(); };" "make f virtual pure const inline explicit" "struct X { virtual inline explicit void f()const = 0 ; };"
  s "struct X { ~X(); };" "make ~ X virtual" "struct X { virtual ~X(); };"
  s "struct X { virtual inline explicit void f()const = 0 ; };" "make f impure nonvirtual nonexplicit noninline" "struct X { void f()const ; };"
  s "struct X { X(int i); };" "make i a reference to const and make constructor explicit" "struct X { explicit X(const int & i); };"
  s "int *p;" "make p a const*volatile" "const int *volatile p;"
  s "void N::f() {}" "make N::f inline" "inline void N::f() {}"
  s "struct X { void operator++(); };" "make operator++ a const function returning an X&" "struct X { X & operator++()const ; };"
  s "int x /*haha*/ ; // ok dan /* mja */" "make x const" "const int x /*haha*/ ; // ok dan /* mja */"
  s "T const ( &f() )[3] { T x = {v}; }" "make f inline" "inline T const ( &f() )[3] { T x = {v}; }"
  s "int *i; int * & j;" "make i a pointer to a pointer" "int ** i; int * & j;"
  s "struct Factorial { static int v = N; };" "make v const" "struct Factorial { const static int v = N; };"
  s "{ struct S {}; int (S:: * p) = 0; int (S:: * & r) = p; }" "make p and r const" "{ struct S {}; int (S:: * const p) = 0; int (S:: * const & r) = p; }"
  s "int i;" "make i an array" "int i[];"
  s "void f() { try { int long i; } catch(int long j) {} }" "make i and j long long" "void f() { try { long long int i; } catch(long long int j) {} }"
  s "void f() { try { long long i; } catch(long long j) {} }" "make i and j long" "void f() { try { long i; } catch(long j) {} }"
  s "int i;" "make i a const array" "const int i[];"
  s "int i;" "make i a const array of pointers" "int *const i[];"
  s "int i[];" "make i const" "const int i[];"
  s "struct X { ~X(); }" "append ; and make destructor virtual and show" "struct X { virtual ~X(); };"
  s "{ try{}catch(const int i){} }" "make i volatile" "{ try{}catch(volatile const int i){} }"
  s "void f(int i, double d){}" "make i and d long" "void f(long int i, long double d){}"
  s "void f(int i, int j, int k);" "make second parameter-declaration const and erase third parameter-declaration" "void f(int i, const int j);"
  s "void f(float, int, char, double);" "make second and third parameter-declaration const" "void f(float, const int, const char, double);"
  s "{ if(int i = 3) ; }" "make i const" "{ if(const int i = 3) ; }"
  s "int i;" "make i an array of const" "const int i[];"
  s "int i;" "make i a static const function" "static int i()const ;"
  s "int i;" "make i a pointer to a function" "int (* i)();"
  s "int i;" "make i a pointer to a function taking two doubles" "int (* i)(double, double);"
  s "int i;" "make i a function returning a const" "const int i();"
  s "int i;" "make i a function returning a const pointer and taking a bool" "int *const i(bool);"
  s "{ cout << 3, 3.2, 3ul, 3e+2, 0x3, 03, .3, 3., .3e1, 3.E0; int i; }" "make i const" "{ cout << 3, 3.2, 3ul, 3e+2, 0x3, 03, .3, 3., .3e1, 3.E0; const int i; }"
  s "void f(int i) { try {} catch(int j) {} }" "make f and i and j const" "void f(const int i) const { try {} catch(const int j) {} }"
  f "void f(int i) { try {} catch(int j) {} }" "make j mutable" "Invalid decl-specifier for type-specifier-seq: mutable"
  s "struct X { void f(int p); };" "make f static inline and make p a reference to long" "struct X { static inline void f(long int & p); };"
  s "struct X { operator ()(); };" "make operator() const" "struct X { operator ()()const ; };" -- Spaces don't matter when matching declarator-ids.
  s "<< 3, i; int i = 2;" "make i const" "<< 3, i; const int i = 2;"
  putStrLn "All make tests passed."

  t "erase all 2 and insert x before second 3 and prepend y" $ Right "y1  3  x3 4 5"
  t "erase everything before last 2 and replace everything after 4 with x" $ Right "2 3 4x"
  t "insert x after all 2 and append y" $ Right "1 2x 3 2x 3 4 5y"
  t "erase first 2 and replace second 2 with x and omit last  " $ Right "1  3 x 3 45"
  t "erase first and last 2 and all 3 and 4 and everything before first 2 and everything after 4" $ Right "    "
  t "erase third last space and kill everything after second last  " $ Right "1 2 3 23 "
  t "insert x after 1 2 and before all 3 and erase 5 and prepend y" $ Right "y1 2x x3 2 x3 4 "
  t "erase everything before 4 and everything after 5" $ Right "4 5"
  t "insert x before 3 4 and 5" $ Right "1 2 3 2 x3 4 x5"
  t "insert x after 1 and all 3 and before 5 and erase 4" $ Right "1x 2 3x 2 3x  x5"
  t "erase second space before 4" $ Right "1 2 3 23 4 5"
  t "erase first 2 3 and 3 2 and 4 5 and last  " $ Right "1  3 "
  t "move 1 till second 3 to before 5" $ Right "3 4 1 2 3 2 5"
  t "move 1 till after second 3 to before 5" $ Right " 4 1 2 3 2 35"
  t "swap last 2 3 and last space before 4" $ Right "1 2 3  2 34 5"
  t "swap last 2 3 with last space before 4" $ Right "1 2 3  2 34 5"
  t "swap 1 and first 3 and second 3 and 5" $ Right "3 2 1 2 5 4 3"
  t "replace 2 after second space with x" $ Right "1 2 3 x 3 4 5"
  t "move everything till 4  to end and erase 4 " $ Right "51 2 3 2 3 "
  t "erase everything" $ Right ""
  t "insert x after second space before 4" $ Right "1 2 3 2 x3 4 5"
  t "add < and > around second 3" $ Right "1 2 3 2 <3> 4 5"
  t "add braces around everything" $ Right "{1 2 3 2 3 4 5}"
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "move second 3 to end and 5 to begin" $ Right "51 2 3 2  4 3"
  t "erase first space until second space before 4" $ Right "1 3 4 5"
  t "erase spaces" $ return "1232345"
  t "erase second last space before 4" $ Right "1 2 3 23 4 5"
  t "erase after second 2" $ Right "1 2 3 2"
  t "erase after second 2 until before 5" $ Right "1 2 3 25"
  t "erase 1 and erase 1" $ Right " 2 3 2 3 4 5"
  t "erase before second last space" $ Right " 4 5"
  t "move 4 5 to before 1 and erase everything after second 2" $ Right "4 51 2 3 2"
  t "erase everything between last and first 2" $ Right "1 22 3 4 5"
  t "erase everything between second 2 and begin" $ Right "2 3 4 5"
  t "erase all space between second 2 and 4" $ Right "1 2 3 234 5"
  t "erase from first 3 until after everything" $ Right "1 2 "
  t "erase 2 after and before first 3" $ return "1  3  3 4 5"
  t "add + before and after 4" $ return "1 2 3 2 3 +4+ 5"
  t "move before second 3 until 4 to begin" $ Right "3 1 2 3 2 4 5"
  t "erase everything between begin and end" $ Right ""
  t "move everything between first 3 and 4 to begin" $ Right " 2 3 1 2 34 5"
  t "erase before second 3 until 4" $ Right "1 2 3 2 4 5"
  t "insert x before second 3 and at end" $ Right "1 2 3 2 x3 4 5x"
  t "erase until after second 3" $ Right " 4 5"
  t "erase from before second 3" $ Right "1 2 3 2 "
  t "replace all but first and second last space with x" $ Right "1 2x3x2x3 4x5"
  t "change all but first and second last space to x" $ Right "1 2x3x2x3 4x5"
  t "erase between second and fourth space and 1" $ Right " 2  3 4 5"
  t "erase between the first and third space and prepend x" $ Right "x1  2 3 4 5"
  t "add parentheses around every space between first 2 and 4 and around 5 and erase second last 3" $ Right "1 2( )( )2( )3( )4 (5)"
  t "move from first 3 until 4 to begin" $ Right "3 2 3 41 2  5"
  t "erase everything from before everything until the second 3" $ Right "3 4 5"
  t "add parentheses around first 2 and 5" $ Right "1 (2) 3 2 3 4 (5)"
  t "erase everything between first space and last space" $ Right "1  5"
  t "erase all 3 and all 2 between begin and end" $ Right "1     4 5"
  t "erase everything between second and first 2 " $ Right "1 2 2 3 4 5"
  t "erase from second 2 till last space" $ Right "1 2 3 5"
  t "erase from second 2 until after 3 and add x before 4" $ Right "1 2 3  x4 5"
  -- t "erase everything between 1 and the second 2 and everything between 4 and 5" $ Right "12 3 45"
  t "erase from before 4 until end" $ Right "1 2 3 2 3 "
  t "use 5x and y4" $ Right "1 2 3 2 3 y4 5x"
  t "erase everything from after 1 until second last space" $ Right "1 4 5"
  t "add parentheses around everything between 1 and second space before 4" $ Right "1( 2 3 2) 3 4 5"
  t "add + and - around all 3 and second 2 and prepend x" $ Right "x1 2 +3- +2- +3- 4 5"
  t "move 4 till end to front" $ Right "4 51 2 3 2 3 "
  t "erase all space after first 2" $ Right "1 232345"
  t "add x before first 3 after second 2" $ Right "1 2 3 2 x3 4 5"
  t "add x before second space after second 3" $ Right "1 2 3 2 3 4x 5"
  t "add x after all space before first 3" $ Right "1 x2 x3 2 3 4 5"
  t "erase all space before last 3" $ Right "12323 4 5"
  t "erase second 2 till end" $ Right "1 2 3 "
  t "append x before 5 and prepend y before all 2 between first 3 and 5" $ Right "1 2 3 y2 3 4 x5"
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "replace first 2 with x and replace all 2 with x" $ Right "1 x 3 x 3 4 5"
  t "erase everything until 4 and 5" $ Right "4 "
  t "erase 3 2 till 5" $ Right "1 2 5"
  t "move second 2 to back and prepend x" $ Right "x1 2 3  3 4 52"
  t "cut everything before first 2 and first and second 3 and everything after 4 and prepend x" $ Right "x2  2  4"
  t "replace all 2 with 3 and erase all 3 and add x after second 2" $ Right "1 3  3x  4 5"
  t "insert spacer before 4 and insert a semicolon after 1 and erase last space" $ Right "1; 2 3 2 3 spacer45"
  t "erase first and second last  " $ Right "12 3 2 34 5"
  t "replace 1 with x and all 2 with y and erase second 3" $ Right "x y 3 y  4 5"
  t "move everything after last 3 to front and erase 5" $ Right " 4 1 2 3 2 3"
  t "erase spaces around first 2" $ Right "123 2 3 4 5"
  t "erase spaces around 3 after second 2" $ Right "1 2 3 234 5"
  t "erase 1 and 2 around first space" $ Right "  3 2 3 4 5"
  t "erase all but first two spaces" $ Right "1 2 32345"
  t "add x after 4 and add y after 4" $ Right "1 2 3 2 3 4yx 5"
  t "erase 4 and add y before 4 and add x after 4" $ Right "1 2 3 2 3 yx 5"
  t "move 4 to end and insert x before 5" $ Right "1 2 3 2 3  x54"
  t "erase 4 and move 5 to begin" $ Right "51 2 3 2 3  "
  t "move 3 2 3 to front and insert x before second 2" $ Right "3 x2 31 2  4 5"
  t "append float x; int y;" $ Right "1 2 3 2 3 4 5float x; int y;"
  t "erase 5 and move everything after last 3 to front" $ Right " 4 1 2 3 2 3"
  t "move everything before 4 to end and move first 3 to front" $ Right "34 51 2  2 3 "
  t "move 3 4 5 to before 4" $ Left "Move destination lies in source range."
  t "move 1 and 4 to after first 3" $ Right " 2 314 2 3  5"
  -- Order-sensitive edits:
  t "add parentheses around everything and append x" $ Right "(1 2 3 2 3 4 5)x"
  t "add parentheses around all 3 and all 2 and erase 4" $ Right "1 (2) (3) (2) (3)  5"
  t "append x and add parentheses around everything" $ Right "(1 2 3 2 3 4 5x)"
  t "append x after everything before 4 and add y before 4" $ Right "1 2 3 2 3 xy4 5"
  t "insert y before 4 and insert z after second 3 " $ Right "1 2 3 2 3 zy4 5"
  t "prepend x and move 5 to begin and insert y before 1 and insert z before everything" $ Right "z5xy1 2 3 2 3 4 "
  t "add parentheses around everything and prepend x" $ Right "x(1 2 3 2 3 4 5)"
  t "prepend x and add parentheses around everything" $ Right "(x1 2 3 2 3 4 5)"
  t "prepend x before everything after 4 and add y after 4" $ Right "1 2 3 2 3 4yx 5"
  t "add y after 4 and prepend x before everything after 4" $ Right "1 2 3 2 3 4xy 5"
  -- Semantic edits:
  ct "struct X { operator bool() { return true; } };" "move declaration of operator bool to end" $ Right "struct X { };operator bool() { return true; } "
  ct "namespace N { void f(); } class C { void g() {} };" "swap body of N and body of C" $ Right "namespace N { void g() {} } class C { void f(); };"
  ct "void f() { int i; } void g() { int j; } void h() {}" "prepend BARK; in f and append BARK; in g and in h" $ Right "void f() { BARK;int i; } void g() { int j; BARK;} void h() {BARK;}"
  ct "class C { int f() {} };" "add int x; in C and add return x; in f" $ Right "class C { int f() {return x;} int x;};"
  ct "void f() { cout << 3; g(); }" "erase everything from after 3; until after body of f" $ Right "void f() { cout << 3;}"
  ct "int main() { cout << 3; }" "move << 3 to front and erase declaration of main" $ Right "<< 3"
  ct "{ cout << x; } enum E { x };" "add `cout, y;` in main and add ,y in E" $ Right "{ cout << x; cout, y;} enum E { x ,y};"
  ct "namespace N { void f(); void g(); void f(); } void h();" "add int x; after declaration of g in N" $ Right "namespace N { void f(); void g(); int x;void f(); } void h();"
  ct "struct X { void f(); }; struct Y { void f(); };" "erase declaration of f in Y" $ Right "struct X { void f(); }; struct Y { };"
  ct "namespace N { void f(); void g(); void f(); } void h();" "move declaration of h to after declaration of g in N" $ Right "namespace N { void f(); void g(); void h();void f(); } "
  ct "void f() {} void g() {} namespace N { void f() {} } void f(int) {}" "add int x; in f in N and in g and append y" $ Right "void f() {} void g() {int x;} namespace N { void f() {int x;} } void f(int) {}y"
  ct "void f() { int x; } void g() { int x; } void h() { int x; }" "erase declaration of x in f and h" $ Right "void f() { } void g() { int x; } void h() { }"
  ct "namespace N { int x; } int x; void f() { int x; }" "use int y; in N and f" $ Right "namespace N { int y; } int x; void f() { int y; }"
  ct "void f() {} void f(int) {} void f(double) {} void f(char) {}" "add int x; in first f and in third and fourth body of f" $ Right "void f() {int x;} void f(int) {} void f(double) {int x;} void f(char) {int x;}"
  ct "namespace A { int x; namespace B { int x; namespace C { int x; namespace D { int x; } } } }" "erase declaration of x in D in C in B in A" $ Right "namespace A { int x; namespace B { int x; namespace C { int x; namespace D { } } } }"
  ct "struct X { X(int); };" "erase second declaration of X" $ Right "struct X { };"
  ct "template <typename> void f(); void f(int);" "erase second declaration of f" $ Right "template <typename> void f(); "
  ct "struct X { template <typename U> X(U); };" "erase second declaration of X" $ Right "struct X { };"
  ct "struct X { int x; X(); void f(); ~X(); struct Y {}; operator int(); X(int); };" "erase constructors and replace destructor with int y;" $ Right "struct X { int x; void f(); int y; struct Y {}; operator int(); };"
  ct "template <typename T> struct X { template <typename U> inline X(T, U); ~X(); }; template <typename T> template <typename U> X<T>::X(T, U) {} template<typename T> X<T>::~X() {}" "erase second constructor and second destructor" $ Right "template <typename T> struct X { template <typename U> inline X(T, U); ~X(); }; "
  ct "void f(int i);" "erase declaration of i" $ Right "void f();"
  ct "struct X { int i; };" "erase declaration of i" $ Right "struct X { };"
  ct "{ 3; 4; 5; }" "swap first and last statement" $ Right "{ 5; 4; 3; }"
  ct "{ 3; 4; 5; }" "swap statements around 4;" $ Right "{ 5; 4; 3; }"
  ct "{ 3; 4; 5; 6; }" "swap statements before 5 with last statement" $ Right "{ 6; 5; 3; 4; }"
  ct "void f(int,double,char);" "swap first two parameter-declarations" $ return "void f(double,int,char);"
  ct "{ if(b) 3; 4; 5; }" "add curlies around first two statements after if" $ Right "{ if(b) {3; 4; }5; }"
  ct "{ 3; 4; 5; }" "add curlies around first and second statement" $ Right "{ {3; 4; }5; }"
  ct "{ 3; 4; 5; }" "add curlies around first statement and around second statement" $ Right "{ {3; }{4; }5; }"
  ct "<< max(3, 2, 1)" "add curlies around code between parens" $ return "<< max({3, 2, 1})"
  ct "{ 1;2;3;4;5; }" "swap first two statements with last two statements" $ Right "{ 4;5; 3;1;2;}"
  ct "{ 1;2;3; }" "swap first two statements" $ Right "{ 2;1;3; }"
  --ct "void f() { 1; } void g() { 1; 2; 3; 4; }" "replace first two and last statement in g with 0;" $ Right "void f() { 1; } void g() { 0;3; 0;}"
  ct "void f() { int x = 3; cout << x; } int i;" "replace everything after first statement in f with BARK;" $ Right "void f() { int x = 3; BARK;} int i;"
  ct "template <typename T, int i> struct S;" "erase second template-parameter" $ return "template <typename T> struct S;"
  ct "void g(string s); void f(int i, double d, char c, float, bool b);" "erase first and last two parameter-declarations in declaration of f and replace third parameter-declaration in declaration of f with wchar_t c" $ return "void g(string s); void f(double d, wchar_t c);"
  ct "int f();" "replace type-specifier with void" $ Right "void f();"
  ct "void g() { 0; struct X { void f() { 1;2;3;4; } }; 1; }" "erase statements around 2 in f" $ Right "void g() { 0; struct X { void f() { 2;} }; 1; }"
  ct "int a;in@@@t x;int y;int z;" "erase @@@ and erase int a; and erase second declaration" $ Right "int y;int z;"
  ct "@     int x;" "erase @ and replace declaration of x by bla" $ return "     bla"
  ct "@@@@@ void f(){} int x;"  "erase @@@@@ and erase first x after function-definition" $ Right " void f(){} int ;"
  ct "@@@@int x; int y; int z;" "erase @@@@ and erase from start until second declaration" $ Right "int z;"
  ct "@@@@int x; int y; int z;" "erase @@@@ and erase from second declaration until last space" $ Right "int x; z;"
  ct "class X extends Y { int i:2; };" "replace extends with : and replace literal after : with 3" $ Right "class X : Y { int i:3; };"
  ct "T f() { 0;x; } U y;" "erase everything after 0 in f" $ Right "T f() { 0} U y;"
  ct "T f() { 0;x; } U y;" "erase everything before x in f" $ Right "T f() { x; } U y;"
  ct "T f() { 0;1;2;3;4; } U y;" "erase everything between second and fourth statement in f" $ Right "T f() { 0;1;3;4; } U y;"
  ct ";     void g() {} void f() { {1;} }" "erase first semicolon and erase curlies around 1;" $ return "     void g() {} void f() { 1; }"
  -- Edit errors:
  ct "struct X { void f() { cout << 3; int x; cout << 3; } };" "erase second 3 after declaration of x in f in X" $ Left "String `3` does not occur twice after free declaration of x in f in X."
  t "erase second 4 after 1" $ Left "String `4` does not occur twice after 1."
  t "move second 2 to x" $ Left "Unexpected `x` after `second 2 to `. Expected \"beginning\", \"begin\", \"front\", \"start\", \"end\", \"back\", \"before\", or \"after\"."
  t "replace alligators with chickens" $ Left "String `alligators` does not occur."
  t "use banana" $ Left "No non-exact match for banana."
  t "use 5426" $ Left "No non-exact match for 5426."
  t "erase 2" $ Left "String `2` occurs multiple times."
  t "replace 1 and erase with 4" $ Left "String `erase` does not occur."
  t "replace tenth last 2 by x" $ Left "String `2` does not occur 10 times."
  t "erase second 9" $ Left "String `9` does not occur."
  t "replace all 2 with 3 and replace second 2 with x" $ Left "Overlapping edits: replace 2 with 3 and replace 2 with x."
  t "erase everything before first 3 and replace first 2 with x" $ Left "Overlapping edits: erase `1 2 ` and replace 2 with x."
  t "erase 5 between 1 and 4" $ Left "String `5` does not occur between 1 and 4."
  t "prepend x and replace 4 with a and replace 4 with b" $ fail "Overlapping edits: replace 4 with a and replace 4 with b."
  -- Syntax errors:
  t "isnert 3 before 4" $ Left "Unexpected `isnert` at start. Expected edit command."
  t "insert " $ Left "Unexpected end of command. Expected option, wrapping description, or verbatim string."
  t "insert kung fu" $ Left "Unexpected end of command. Expected \" and\", \" in\", \" at\", \" around\", \" before\", or \" after\"."
  t "move " $ fail "Unexpected end of command. Expected \"till\", \"until\", \"from\", \"everything\", \"code\", \"before\", \"between\", \"after\", \"declaration\", \"body\", production-name, verbatim string, \"all\", \"any\", \"every\", \"each\", or ordinal."
  t "move x " $ Left "Unexpected end of command. Expected \" till\", \" until\", \" around\", \" before\", \" after\", \" between\", \" in\", \" and\", or \" to\"."
  t "move x to "$ Left "Unexpected end of command. Expected \"beginning\", \"begin\", \"front\", \"start\", \"end\", \"back\", \"before\", or \"after\"."
  t "erase all 2 and " $ fail "Unexpected end of command. Expected verbatim string, \"till\", \"until\", \"from\", \"everything\", \"code\", \"before\", \"between\", \"after\", \"declaration\", \"body\", production-name, \"all\", \"any\", \"every\", \"each\", ordinal, wrapping description, option, or edit command."
  putStrLn "All basics tests passed."

 where
  ct :: String → String → Either String String → IO ()
  ct x c o = test_cmp c o $ case parseOrFailE (Editing.Parse.commandsP << eof) c "command" of
    Left e → Left e
    Right (cmds, _) → editable_body . Editing.Execute.execute cmds (EditableRequest (Evaluate (∅)) x)

  s, f :: String → String → String → IO ()
  s code cmdstring expectation = ct code cmdstring (Right expectation) -- test for success
  f code cmdstring expectation = ct code cmdstring (Left expectation) -- test for failure

  t = ct "1 2 3 2 3 4 5"

diff_tests :: IO ()
diff_tests = do

  dt' "foo; bar; monkey; chicken; bas;" "bar; monkey; chicken;" "Erased `foo;` and `bas;`." "Prepended `foo;` and appended `bas;`."

  dt' "{ fstream f(\"t.cpp\"); string s(istreambuf_iterator<char>(f), istreambuf_iterator<char>()); }"
    "{ fstream f(\"t.cpp\"); string s((istreambuf_iterator<char>(f)), istreambuf_iterator<char>()); }"
    "Inserted ( before `istreambuf_iterator<char>`, and inserted ) after `istreambuf_iterator<char>(f)`."
    "Erased ( before `istreambuf_iterator<char>` and ) after `istreambuf_iterator<char>(f)`."
        -- Todo: One day, the former should say "wrapped ...".

  dt' "int f(int & i) { i = 4; return i;} int g(int && i){return f(i);} int main() { cout << g(2); }"
    "int & f(int & i) { i = 4; return i;} int & g(int && i){return f(i);} int & g(int & i){return f(i);} int main() { cout << g(2); }"
    "Replaced `int f` with `int & f`, replaced `int g` with `int & g`, and inserted `int & g(int & i){return f(i);}` before `int main`."
    "Replaced `int & f` with `int f`, replaced `int & g` with `int g`, and erased `int & g(int & i){return f(i);}`."

  dt' "{string foo = \"kangaroo\"; auto m = foo.find_first_of('m'); cout << *m;}"
    "{string foo = \"kangaroo\"; size_t m = foo.find_first_of('m'); cout << foo[m];}"
    "Replaced `auto m` with `size_t m`, and replaced `<< *m` with `<< foo[m]`."
    "Replaced `size_t m` with `auto m`, and replaced `<< foo[m]` with `<< *m`."
      -- Todo: Group.

  dt "{int i=0,t=time(0);for(;i<5000000;i++)asm(\".org 0xffff\");cout << time(0)-t; }"
    "{int i=0,t=time(0);for(;i<5000;i++)int tmp=1*88+71/4000^66;cout << time(8)-t; }"
    "Replaced i<5000000 with i<5000, replaced `asm(\".org 0xffff\")` with `int tmp=1*88+71/4000^66`, and replaced 0 with 8."

  dt' "struct curr_func { string name; cf(string n):name(n){} }; \\ #define DEFUN(name) name try { throw curr_func(#name); } catch(curr_func& cfun) \\ DEFUN(void haha(int a)) { cout << cfun.name; } int main() { cout << haha; }"
    "struct curr_func { string name; curr_func(string n):name(n){} }; \\ #define DEFUN(name) name try { throw curr_func(#name); } catch(curr_func& cfun) \\ DEFUN(void haha(int a)) { cout << cfun.name << \": \" << a; } int main() { haha(42); }"
    "Replaced cf with curr_func, inserted `<< \": \" << a` after `<< cfun.name`, and replaced `cout << haha` with haha(42)."
    "Replaced curr_func with cf, erased `<< \": \" << a`, and replaced haha(42) with `cout << haha`."

  dt "{float i=0,t=time(0);for(;i<5000000;i++) { if(x == 3) f(reinterpret_cast<long>(x)); } }"
    "{int i=0,t=time(null);for(;i<5000000;++i) { if(x != 3) f(static_cast<long>(x)); } }"
    "Replaced `float i` with `int i`, replaced 0 with null, replaced i++ with ++i, replaced `x == 3` with `x != 3`, and replaced `reinterpret_cast<long>` with `static_cast<long>`."
{-
  dt' "struct a { int b; a():b([]{ return 1 + 2 + 3; }){}}a_; int main() { cout << a_.b }"
    "struct a { int b; a({}):b([]{ return 1 + 2 + 3; }()){}}a_; int main() { cout << a_.b; }"
    "Replaced \"a():b([]{\" with \"a({}):b([]{\", replaced \")\" after \"return 1 + 2 + 3; }\" with \"())\", and inserted \";\" after \"cout << a_.b\"."
    "Replaced \"a({}):b([]{\" with \"a():b([]{\", replaced \"}()){}}a_\" with \"}){}}a_\", and erased \";\" after \"cout << a_.b\"."
-}
  dt "struct curr_func { string name; cf(string n):name(n){} }; \\ #define DEFUN(name) name try { throw curr_func(#name); } catch(curr_func& cfun) \\ DEFUN(void haha(int a)) { cout << cfun.name; } int main() { cout << haha; }"
    "struct { string name; cf(string n){} }; \\ #define DEFUN(name) name try { } catch(curr_func& cfun) \\ DEFUN(void haha(int a) { cout << \"tortoise\"; } ) { cout << ETYPE(cfun.name); } int main() { do_something(complicated); cout << haha; }"
    "Replaced `struct curr_func` with struct, erased :name(n) and `throw curr_func(#name);`, inserted `{ cout << \"tortoise\"; }` before `) { cout`, replaced `<< cfun.name` with `<< ETYPE(cfun.name)`, and inserted `do_something(complicated);` before `cout << haha`."

  dt "{ char str[] = \"giraffe\"; char * pch; pch=(char*) memchr(str, 'y', (int)strlen(str)); if (pch!=NULL) { printf(\"'y' was found at position #%d\", pch-str+1); } }"
    "{ char * str = \"giraffe\"; char * pch; pch=(char*) memchr(str, 'v', (size_t)strlen(str)); if (pch!=NULL) { printf(\"'v' was found at position #%d\", pch-str-1); memset(str, '*', 6); puts(str); printf(\"%s\", str); } }"
    "Replaced `char str[]` with `char * str`, replaced 'y' with 'v', replaced int with size_t, replaced `\"'y' was` with `\"'v' was`, and replaced pch-str+1 with `pch-str-1); memset(str, '*', 6); puts(str); printf(\"%s\", str`."

  dt "geordi: { char y(34); stringstream i(\"geordi: { char y(34); stringstream i(!); string t; getline(i, t, '!'); cout << t << y << i.str() << y << i.rdbuf(); }\"); string t; getline(i, t, '!'); cout<< t << y << i.str() << y << i.rdbuf(); }"
    "geordi: { stringstream i(\"geordi: { stringstream i(!); string t; getline(i, t, '!'); cout << t << i.str() << i.rdbuf(); }\"); string t; getline(i, t, '!'); cout << t << i.str() << i.rdbuf(); }"
    "Erased `char y(34);` and `char y(34);` and `<< y` and `<< y` and `<< y` and `<< y`." -- Todo: "Erased all \"char y(34);\" and all \"<< y\"."

  dt "char *& f() { static char *p; cout << &p << endl; return p; } int main() { char *p = f(); cout << &p << endl; }"
    "char *& f() { static char *p; cout << &p << ' '; return p; } int main() { char *p = f(); cout << &p; }"
    "Replaced `<< endl` with `<< ' '`, and erased `<< endl`." -- Todo: say "first" and "last".

  putStrLn "All diff tests passed."
 where
  dt :: String → String → String → IO ()
  dt x y r = test_cmp x r $ show $ Editing.Diff.diff x y
  dt' :: String → String → String → String → IO ()
  dt' x y xy yx = do
    test_cmp x xy $ show $ Editing.Diff.diff x y
    test_cmp y yx $ show $ Editing.Diff.diff y x
  dts :: String → [(String, String)] → IO ()
  dts _ [] = return ()
  dts s ((s', d) : r) = dt s s' d >> dts s' r

make_type_tests :: IO ()
make_type_tests = do
  t "void(pointer)" $ Right "void(T *)"
  t "pointer to const pointer to long double" $ Right "long double*const *"
  t "function taking an int and returning nothing" $ Right "void(int )"
  let q = "void(int (* const (** (* const volatile (X::* volatile* (* )(int) )(char*)) [2])(long) ) [3])" in t q $ Right q
  t "function returning nothing and taking a pointer to a function returning a bool and taking an int" $ Right "void (bool (*)(int))"
  t "function taking a pointer to a (function returning a bool and taking an int), returning nothing" $ Right "void(bool (*)(int))"
  t "reference to a pointer to a function returning nothing, and taking an int" $ Right "void(*&)(int)"
  t "function taking an int, a char, a double, and returning nothing" $ Right "void(int, char, double)"
  t "function returning nothing and taking an int, a char, and a double" $ Right "void (int, char, double)"
  t "function returning a B<C<T>>" $ Right "B<C<T>>()"
  t "vector<T*>" $ Right "vector<T*>"
  t "function returning a ::B<T, (3>>2)>::value_type and taking a vector<vector<int>>" $ Right "::B<T, (3>>2)>::value_type (vector<vector<int>>)"
  t "function taking no arguments, returning a reference to an array of three pointers to functions taking integers and returning nothing" $ Right "void(*(&())[3])(int )"
  t "pointer to a constant volatile pointer to an array of 3 ints" $ Right "int(*volatile const *)[3]"
  t "function returning nothing and taking a reference to an array of three pointers to void" $ Right "void (void*(&)[3])"
  t "pointer to void(void)" $ Right "void(*)(void)"
  t "reference to pointer to const array" $ Right "T const (*&)[]"
  t "function taking a const (pointer to int) and a volatile function" $ Right "T (int*const , T ()volatile )"
  t "function taking two integers, three doubles, and returning a boolean" $ Right "bool(int, int, double, double, double)"
  t "function taking two pointers to integers" $ Right "T (int*, int*)"
  t "rvalue reference to function" $ Right "T (&&)()"
  t "constant constant pointer to volatile volatile int" $ Right "volatile volatile int*const "
  --t "pointer to a function from string_bla to int" $ Right "int(*)(string_bla )"
  t "pointer to " $ Left "Unexpected end of type description. Expected type description."
  t "pointer to function taking pointer to function" $ Right "T (*)(T (*)())"
  --t "pointer to int and double" $ Left "Unexpected \"a\" after \"to int \". Expected type-specifier, abstract-declarator, or end of type description."
  t "function returning " $ Left "Unexpected end of type description. Expected type description."
  t "function taking a pointer and a reference and returning a reference to a constant array" $ Right "T const (&(T *, T &))[]"
  t "array of " $ Left "Unexpected end of type description. Expected type description or integer-literal."
  t "foo bar bas" $ Left "Unexpected `bas` after `foo bar `. Expected \"::\" or template-arguments."
  t "array of seven characters" $ Right "char[7]"
  t "pointer to (function taking constant integer)*" $ Right "T (**)(const int)"
  t "pointer to void()" $ Right "void(*)()"
  t "function taking a pointer to void() returning a reference to an int[3]" $ Right "int(&(void(*)() ))[3]"
  t "void(pointer to function)" $ Right "void(T (*)())"
  t "function*(const int)" $ Right "T (*(const int))()"
  t "constant pointer to a function" $ Right "T (*const )()"
  t "(function taking an int[3] )*" $ Right "T (*)(int[3] )"
  t "(function*[3])&()" $ Right "T (*(&())[3])()"
  t "int(::T::U::V::**)" $ Right "int(::T::U::V::**)"
  t "int(X::*)()const volatile" $ Right "int(X::*)()const volatile"
  t "int volatile X::* Y::* const Z::*" $ Right "int volatile X::* Y::* const Z::*"
{-
  t "pointer to a member function" $ Right "void(T::*)()"
  t "pointer to member integer" $ Right "int T::*"
  t "pointer to member" $ Right "T T::*"
  t "pointer to member pointer to member pointer to member array" $ Right "T(T::* T::* T::*)[]"
-}
  t "array*const" $ Right "T (*const)[]"
  t "function taking (pointer to function returning int taking int) returning int" $ Right "int(int (*)(int))"
  --t "function+" $ Left "Unexpected \"+\" after \"function\". Expected \"taking\", \"returning\", \"from\", abstract-declarator, or end of type description."
  t "void(register pointer to string)" $ Right "void(register string*)"
  t "array of three arrays of 2 arrays of 1 integer" $ Right "int[3][2][1]"
  putStrLn "All make-type tests passed."
 where
  t :: String → Either String String → IO ()
  t i o = test_cmp i o (either (Left . id) (Right . Cxx.Show.show_simple) $ Cxx.Parse.makeType i)

precedence_tests :: IO ()
precedence_tests = do
  s "x::y" "x::y"
  s "if (b) int *ptr = new int(3), a(2);" "if (b) {int *ptr = (new int(3));int a(2);}"
  s "if(b) if(c) foo; else bar;" "if(b) { if(c) { foo; } else { bar; } }"
  s "a=b<c?d=e:f=g" "a=((b<c)?(d=e):(f=g))" -- Example from TC++PL, section 6.2.
  s "x?y?z?a:b:c:d" "x?(y?(z?a:b):c):d"
  s "*i-40?(*i-x?S(1,*i):a)+r(i+1,e,x,a):'('+(i[1]-46?r(i+1,o-1,x,a):'.'+r(i+2,o-1,x+1,a))+')'+r(o,e,x,a)" "((*i)-40)?((((*i)-x)?(S(1,(*i))):a)+(r((i+1),e,x,a))):((('('+(((i[1])-46)?(r((i+1),(o-1),x,a)):('.'+(r((i+2),(o-1),(x+1),a)))))+')')+(r(o,e,x,a)))" -- Taken from the lambda calculus snippet.
  s "x->x->*x.x.*x" "((x->x)->*(x.x)).*x"
  s "x || x && x | x ^ x bitand x == x < x << x" "x || (x && (x | (x ^ (x bitand (x == (x < (x << x)))))))"
  s "*++vector<int>{2,5,4}.rbegin()" "*(++(((vector<int>{2,5,4}).rbegin)()))"
  s "x---------x" "((((x--)--)--)--)-x"
  s "z+operator+(x,y)" "z+(operator+(x,y))"
  s "x.operator()() * operator new[ ]()" "((x.operator())()) * (operator new[ ]())"
  s "throw 2 + 1, throw, 9" "((throw (2 + 1)), throw), 9"
  s "x + x *= x + x /= x + x" "(x + x) *= ((x + x) /= (x + x))"
  s "(x)y + x(y)" "((x)y) + (x(y))"
  f "(x+y)z" "Unexpected `z` after (x+y). Expected postfix operator, binary operator, ternary operator, or end of code."
  s "a+++a, b++ +b, c+ ++c, d+++ +d, e+ +++e" "(((((a++)+a), ((b++) +b)), (c+ (++c))), ((d++)+ (+d))), (e+ (++(+e)))"
  s "x += operatornew, b(), !c" "((x += operatornew), (b())), (!c)"
  s "sizeof u + sizeof(x) + sizeof(y*z)" "((sizeof u) + sizeof(x)) + (sizeof(y*z))"
  s "x.template a<int>()" "(x.template a<int>)()"
  s "const_cast<vector<vector<int>>>(x)" "const_cast<vector<vector<int>>>(x)" -- Tricky closing angle brackets.
  s "x.~y" "x.~y"
  s "f  (x+3,y)-g(x+3)" "(f  ((x+3),y))-(g(x+3))"
  s "::x + y::z + a::b::c::d::e" "(::x + y::z) + a::b::c::d::e"
  s "x[y + 'z']" "x[y + 'z']" -- No parens around x + z or around 'z'.
  f "x*" "Unexpected end of code. Expected pm-expression."
  f "x && throw 3" "Unexpected `throw` after `x && `. Expected inclusive-or-expression."
  f "&operator" "Unexpected end of code. Expected overloadable operator or conversion-type-id."
  f "x.*" "Unexpected end of code. Expected cast-expression."
  f "x--x" "Unexpected `x` after x--. Expected postfix operator, binary operator, ternary operator, or end of code."
  f "x." "Unexpected end of code. Expected id-expression, \"template\", or pseudo-destructor-name."
  f "x-" "Unexpected end of code. Expected multiplicative-expression."
  f "x( " "Unexpected end of code. Expected \")\" or expression-list."
  f "x(y" "Unexpected end of code. Expected \")\", \"(\", braced-init-list, \"::\", template-arguments, postfix operator, binary operator, ternary operator, or comma."
  f "x?y" "Unexpected end of code. Expected colon, \"(\", braced-init-list, \"::\", template-arguments, postfix operator, binary operator, or ternary operator."
  f "x[" "Unexpected end of code. Expected expression or braced-init-list."
  f "x.operator foo bar()" "Unexpected `(` after `foo bar`. Expected \"::\" or template-arguments."
  putStrLn "All precedence tests passed."
 where
  s :: String → String → IO () -- Test for success.
  s i o = test_cmp i (Right o) (Cxx.Parse.precedence i)
  f :: String → String → IO () -- Test for failure.
  f i o = test_cmp i (Left o) (Cxx.Parse.precedence i)

parse_tests :: IO ()
parse_tests = do
  t "struct A<int>;"
  t "void f(int, ...);"
  t "{ if(int i = 3); }"
  t "int (::x::y);"
  putStrLn "All parse tests passed."
 where
  t :: String → IO ()
  t s = test_cmp s (Right s) $ fmap Cxx.Show.show_simple $ Cxx.Parse.parseRequest s

main :: IO ()
main = do
  basic_tests
  Editing.EditsPreparation.use_tests
  diff_tests
  make_type_tests
  precedence_tests
  parse_tests
