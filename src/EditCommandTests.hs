module EditCommandTests (test) where

import qualified Text.ParserCombinators.Parsec as PS

import Text.ParserCombinators.Parsec (eof, parse)
import EditCommandGrammar (UseClause(..))
import Util (test_cmp, (<<), commas_and, capitalize, (.))

import Prelude hiding ((.))
import Request (EditableRequest(..), EditableRequestKind(..))
import EditCommandParseError (showParseError)
import EditCommandParse (commandsP)
import EditCommandBasics (execute, findInStr, selectRange, replaceRange, Edit(..))
import EditCommandDiff (diff)

test :: IO ()
test = do
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
  t "erase begin until second 3" $ Right "3 4 5"
  t "erase first 2 3 and 3 2 and 4 5 and last  " $ Right "1  3 "
  t "move 1 till second 3 to before 5" $ Right "3 4 1 2 3 2 5"
  t "move 1 till after second 3 to before 5" $ Right " 4 1 2 3 2 35"
  t "replace 2 after second space with x" $ Right "1 2 3 x 3 4 5"
  t "move everything till 4  to end and erase 4 " $ Right "51 2 3 2 3 "
  t "erase everything" $ Right ""
  t "insert x after second space before 4" $ Right "1 2 3 2 x3 4 5"
  t "wrap < and > around second 3" $ Right "1 2 3 2 <3> 4 5"
  t "wrap braces around everything" $ Right "{1 2 3 2 3 4 5}"
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "move second 3 to end and 5 to begin" $ Right "51 2 3 2  4 3"
  t "erase first space until second space before 4" $ Right "1 3 4 5"
  t "erase second last space before 4" $ Right "1 2 3 23 4 5"
  t "erase after second 2" $ Right "1 2 3 2"
  t "erase after second 2 until before 5" $ Right "1 2 3 25"
  t "erase before second last space" $ Right " 4 5"
  t "move 4 5 to before 1 and erase everything after second 2" $ Right "4 51 2 3 2"
  t "erase everything between last and first 2" $ Right "1 22 3 4 5"
  t "erase everything between second 2 and begin" $ Right "2 3 4 5"
  t "erase all space between second 2 and 4" $ Right "1 2 3 234 5"
  t "erase from first 3 until after everything" $ Right "1 2 "
  t "move before second 3 until 4 to begin" $ Right "3 1 2 3 2 4 5"
  t "erase everything between begin and end" $ Right ""
  t "move everything between first 3 and 4 to begin" $ Right " 2 3 1 2 34 5"
  t "erase before second 3 until 4" $ Right "1 2 3 2 4 5"
  t "insert x before second 3 and at end" $ Right "1 2 3 2 x3 4 5x"
  t "erase until after second 3" $ Right " 4 5"
  t "erase from before second 3" $ Right "1 2 3 2 "
  t "replace all but first and second last space with x" $ Right "1 2x3x2x3 4x5"
  t "erase between second and fourth space and 1" $ Right " 2  3 4 5"
  t "erase between the first and third space and prepend x" $ Right "x1  2 3 4 5"
  t "wrap parentheses around every space between first 2 and 4 and around 5 and erase second last 3" $ Right "1 2( )( )2( )3( )4 (5)"
  t "move from first 3 until 4 to begin" $ Right "3 2 3 41 2  5"
  t "erase everything from before everything until the second 3" $ Right "3 4 5"
  t "wrap parentheses around first 2 and 5" $ Right "1 (2) 3 2 3 4 (5)"
  t "erase everything between first space and last space" $ Right "1  5"
  t "erase all 3 and all 2 between begin and end" $ Right "1     4 5"
  t "erase everything between second and first 2 " $ Right "1 2 2 3 4 5"
  t "erase from second 2 till last space" $ Right "1 2 3 5"
  t "erase from second 2 until after 3 and add x before 4" $ Right "1 2 3  x4 5"
  t "erase between 1 and the second 2 and between 4 and 5" $ Right "12 3 45"
  t "erase from before 4 until end" $ Right "1 2 3 2 3 "
  t "use 5x and y4" $ Right "1 2 3 2 3 y4 5x"
  t "erase everything from after 1 until second last space" $ Right "1 4 5"
  t "wrap parentheses around everything between 1 and second space before 4" $ Right "1( 2 3 2) 3 4 5"
  t "wrap all 3 and second 2 in + and - and prepend x" $ Right "x1 2 +3- +2- +3- 4 5"
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
  -- Order-sensitive edits:
  t "wrap parentheses around everything and append x" $ Right "(1 2 3 2 3 4 5)x"
  t "append x and wrap parentheses around everything" $ Right "(1 2 3 2 3 4 5x)"
  t "append x after everything before 4 and add y before 4" $ Right "1 2 3 2 3 xy4 5"
  t "insert y before 4 and insert z after second 3 " $ Right "1 2 3 2 3 zy4 5"
  t "prepend x and move 5 to begin and insert y before 1 and insert z before everything" $ Right "z5xy1 2 3 2 3 4 "
  t "wrap parentheses around everything and prepend x" $ Right "x(1 2 3 2 3 4 5)"
  t "prepend x and wrap parentheses around everything" $ Right "(x1 2 3 2 3 4 5)"
  t "prepend x before everything after 4 and add y after 4" $ Right "1 2 3 2 3 4yx 5"
  t "add y after 4 and prepend x before everything after 4" $ Right "1 2 3 2 3 4xy 5"
  -- Edit errors:
  t "move second 2 to x" $ Left "Unexpected \"x\" after \"second 2 to \". Expected \"begin\", \"front\", \"end\", \"back\", \"before\", or \"after\"."
  t "replace alligators with chickens" $ Left "String \"alligators\" does not occur."
  t "use banana" $ Left "No match."
  t "use 5426" $ Left "No match."
  t "erase 2" $ Left "String \"2\" occurs multiple times."
  t "replace 1 and erase with 4" $ Left "String \"erase\" does not occur."
  t "replace tenth last 2 by x" $ Left "String \"2\" does not occur 10 times."
  t "erase second 9" $ Left "String \"9\" does not occur."
  t "replace all 2 with 3 and replace second 2 with x" $ Left "Overlapping edits: replace \"2\" with \"3\" and replace \"2\" with \"x\"."
  t "erase everything before first 3 and replace first 2 with x" $ Left "Overlapping edits: erase \"1 2 \" and replace \"2\" with \"x\"."
  -- Syntax errors:
  t "isnert 3 before 4" $ Left "Unexpected \"s\" at start. Expected edit command."
  t "insert " $ Left "Unexpected end of command. Expected verbatim string."
  t "erase first and " $ Left "Unexpected end of command. Expected ordinal."
  t "erase between second " $ Left "Unexpected end of command. Expected \"last\", \"and\", or verbatim string."
  t "insert kung fu" $ Left "Unexpected end of command. Expected \" at\", \" before\", or \" after\"."
  t "move " $ Left "Unexpected end of command. Expected \"till\", \"until\", \"from\", \"everything\", \"begin\", \"before\", \"between\", \"after\", ordinal, or verbatim string."
  t "move x " $ Left "Unexpected end of command. Expected \" till\", \" until\", \" before\", \" after\", \" between\", or \" to\"."
  t "move x to "$ Left "Unexpected end of command. Expected \"begin\", \"front\", \"end\", \"back\", \"before\", or \"after\"."
--  t "wrap x and y" $ Left $ "Unexpected end of command. Expected \" around \" or \" in \"."
  t "append x and erase first " $ Left "Unexpected end of command. Expected \"and\" or verbatim string."
  t "erase all 2 and " $ Left "Unexpected end of command. Expected \"insert\", \"add\", \"append\", \"prepend\", \"erase\", \"remove\", \"kill\", \"cut\", \"omit\", \"delete\", \"replace\", \"use\", \"move\", \"wrap\", \"-\", \"till\", \"until\", \"from\", \"everything\", \"begin\", \"before\", \"between\", \"after\", \"all\", \"any\", \"every\", \"each\", ordinal, or verbatim string."
    -- Todo: Why doesn't this say "edit command"?
  -- "use" tests:
  ut "ETYPE_DESC" "ETPYE" "Replaced \"<< ETPYE\" with \"<< ETYPE_DESC\"." "Replaced \"<< ETYPE_DESC\" with \"<< ETPYE\"."
  ut "kip(a.~T)" "a.~T" "Replaced \"a.~T\" with \"kip(a.~T)\"." "Replaced \"kip(a.~T)\" with \"a.~T\"."
  -- ut "cos(a.~T)" "a.~T" -- Fails, but can probably be made to work by rewarding successive skips.
  ut "size_type" "size_t" "Replaced \"string::size_t- siz\" with \"string::size_type- siz\"." "Replaced \"string::size_type- siz\" with \"string::size_t- siz\"."
  ut "size = 9" "siz = 2" "Replaced \"string::size_t- siz = 2\" with \"string::size_t- size = 9\"." "Replaced \"string::size_t- size = 9\" with \"string::size_t- siz = 2\"."
  ut "ETYPE" "ETPYE" "Replaced \"<< ETPYE\" with \"<< ETYPE\"." "Replaced \"<< ETYPE\" with \"<< ETPYE\"."
  ut "std::string" "string" "Replaced \"string::size_t- siz\" with \"std::string::size_t- siz\"." "Replaced \"std::string::size_t- siz\" with \"string::size_t- siz\"."
  ut "; float x" "; int x" "Replaced \"int x\" with \"float x\"." "Replaced \"float x\" with \"int x\"."
  ut "x-" "x -" "Replaced \"x - size\" with \"x- size\"." "Replaced \"x- size\" with \"x - size\"."
  ut ") cin <<" ") cout <<" "Replaced \"cout\" with \"cin\"." "Replaced \"cin\" with \"cout\"."
  ut "x = 4" "x = 3" "Replaced \"3\" with \"4\"." "Replaced \"4\" with \"3\"."
  ut "x - 8);" "x - size);" "Replaced \"x - size\" with \"x - 8\"." "Replaced \"x - 8\" with \"x - size\"."
  ut "(!i)" "(i == 0)" "Replaced \"i == 0\" with \"!i\"."  "Replaced \"!i\" with \"i == 0\"."
  ut "seekp" "seek" "Replaced \"a.seek\" with \"a.seekp\"." "Replaced \"a.seekp\" with \"a.seek\"."
  ut "<char>" "<unsigned char>" "Replaced \"vector<unsigned char> & r\" with \"vector<char> & r\"." "Replaced \"vector<char> & r\" with \"vector<unsigned char> & r\"."
  ut "<const fish>" "<fish>" "Replaced \"reinterpret_cat<fish>\" with \"reinterpret_cat<const fish>\"." "Replaced \"reinterpret_cat<const fish>\" with \"reinterpret_cat<fish>\"."
  ut "&); };" "&) };" "Inserted \";\" after \"C const &)\"." "Erased \";\" after \"C const &)\"."
  ut "> * r = v" "> & r = v" "Replaced \"& r\" with \"* r\"." "Replaced \"* r\" with \"& r\"."
  ut "v.cbegin()" "v.begin()" "Replaced \"v.begin\" with \"v.cbegin\"." "Replaced \"v.cbegin\" with \"v.begin\"."
  -- Todo: "void foo" should match "voidfoo".
  ut "x - sizeof(y))" "x - size)" "Replaced \"x - size\" with \"x - sizeof(y)\"." "Replaced \"x - sizeof(y))\" with \"x - size)\"."
  ut "int a(2);" "int a;" "Inserted \"(2)\" after \"{ int a\"." "Erased \"(2)\" after \"{ int a\"."
  ut "int const * w" "int * w" "Replaced \"int * w\" with \"int const * w\"." "Replaced \"int const * w\" with \"int * w\"."
  ut "main(int argc) {" "main() {" "Inserted \"int argc\" after \"void main(\"." "Erased \"int argc\"."
  ut "operator-" "operator+" "Replaced \"C & operator+\" with \"C & operator-\"." "Replaced \"C & operator-\" with \"C & operator+\"."
  ut "_cast" "_cat" "Replaced \"reinterpret_cat<fish>\" with \"reinterpret_cast<fish>\"." "Replaced \"reinterpret_cast<fish>\" with \"reinterpret_cat<fish>\"."
  ut "(++a)" "(a++)" "Replaced \"a++\" with \"++a\"." "Replaced \"++a\" with \"a++\"."
  ut "list<int>" "vector<int>" "Replaced \"vector<int> v\" with \"list<int> v\"." "Replaced \"list<int> v\" with \"vector<int> v\"."
  ut "a->seekp" "a.seek" "Replaced \"a.seek\" with \"a->seekp\"." "Replaced \"a->seekp\" with \"a.seek\"."
  ut "vector<int>::iterator i" "vector<int> i" "Replaced \"vector<int> i\" with \"vector<int>::iterator i\"." "Replaced \"vector<int>::iterator i\" with \"vector<int> i\"."
  ut "runtime_error(" "runtime_exception(" "Replaced \"throw runtime_exception\" with \"throw runtime_error\"." "Replaced \"throw runtime_error\" with \"throw runtime_exception\"."
  ut "~T();" "~T;" "Inserted \"()\" after \") { a.~T\"." "Erased \"()\" after \") { a.~T\"." -- Todo: ugly.
  ut "int const * w" "int * w" "Replaced \"int * w\" with \"int const * w\"." "Replaced \"int const * w\" with \"int * w\"."
  ut "(T & a)" "(T a)" "Replaced \"T a\" with \"T & a\"." "Replaced \"T & a\" with \"T a\"."
  ut "& r(v);" "& r = v;" "Replaced \"= v\" after \"vector<unsigned char> & r\" with \"(v)\"." "Replaced \"(v)\" after \"vector<unsigned char> & r\" with \"= v\"."
  ut "ios_base::end_t" "ios::end" "Replaced \"ios::end\" with \"ios_base::end_t\".""Replaced \"ios_base::end_t\" with \"ios::end\"."
  ut "95" "94" "Replaced \"94\" with \"95\"." "Replaced \"95\" with \"94\"."
  ut "vector<int> const v { 3, 2 };" "vector<int> v; v = { 3, 2 };" "Replaced \"vector<int> v; v =\" with \"vector<int> const v\"." "Replaced \"vector<int> const v\" with \"vector<int> v; v =\"."
  ut "class C" "struct C" "Replaced \"struct C\" with \"class C\"." "Replaced \"class C\" with \"struct C\"."
  ut "B z{p};" "B z = B{p};" "Erased \"= B\" after \"B z\"." "Inserted \"= B\" after \"B z\"."
  ut "friend C & operator+" "C & operator+" "Inserted \"friend\" before \"C & operator+\"." "Erased \"friend\" before \"C & operator+\"."
  ut "char const(&here)[N]" "char(const&here)[N]" "Replaced \"char(const&here\" with \"char const(&here\"." "Replaced \"char const(&here\" with \"char(const&here\"."
  ut "z = shared_ptr<B>{new p}" "z = B{p}" "Replaced \"B{p\" with \"shared_ptr<B>{new p\"." "Replaced \"shared_ptr<B>{new p\" with \"B{p\"." -- Todo: ugly.
  ut "(X(y));" "X(y);" "Inserted \"(\" before \"X(y)\", and inserted \")\" after \"} X(y)\"." "Erased \"(\" before \"X(y))\" and \")\" after \"} (X(y)\"." -- Todo: ugly.
  ut "2000" "1800" "Replaced \"1800\" with \"2000\"." "Replaced \"2000\" with \"1800\"."
  ut "8000100808" "10000000000" "Replaced \"10000000000\" with \"8000100808\"." "Replaced \"8000100808\" with \"10000000000\"."
  ut "> 7" ">= 7" "Replaced \"x >= 7\" with \"x > 7\"." "Replaced \"x > 7\" with \"x >= 7\"."
  ut "private: fstream" "public: fstream" "Replaced \"public: fstream p\" with \"private: fstream p\"." "Replaced \"private: fstream p\" with \"public: fstream p\"." -- Todo: "replaced public: with private: before fstream p".
  ut "int main" "void main" "Replaced \"void main\" with \"int main\"." "Replaced \"int main\" with \"void main\"."
  ut "<char>" "<unsigned char>" "Replaced \"vector<unsigned char> & r\" with \"vector<char> & r\"." "Replaced \"vector<char> & r\" with \"vector<unsigned char> & r\"."
  ut "int const u =" "int x =" "Replaced \"int x\" with \"int const u\"." "Replaced \"int const u\" with \"int x\"."
  ut "u - -j" "u--j" "Replaced \"&u--j\" with \"&u - -j\"." "Replaced \"&u - -j\" with \"&u--j\"."
  ut "struct C{" "struct C(){" "Erased \"()\" after \"struct C\"." "Inserted \"()\" after \"struct C\"."

  dt' "foo; bar; monkey; chicken; bas;" "bar; monkey; chicken;" "Erased \"foo;\" and \"bas;\"." "Prepended \"foo;\" and appended \"bas;\"."

  dt' "{ fstream f(\"t.cpp\"); string s(istreambuf_iterator<char>(f), istreambuf_iterator<char>()); }"
    "{ fstream f(\"t.cpp\"); string s((istreambuf_iterator<char>(f)), istreambuf_iterator<char>()); }"
    "Inserted \"(\" before \"istreambuf_iterator<char>\", and inserted \")\" after \"istreambuf_iterator<char>(f)\"."
    "Erased \"(\" before \"istreambuf_iterator<char>\" and \")\" after \"istreambuf_iterator<char>(f)\"."
        -- Todo: One day, the former should say "wrapped ...".

  dt' "int f(int & i) { i = 4; return i;} int g(int && i){return f(i);} int main() { cout << g(2); }"
    "int & f(int & i) { i = 4; return i;} int & g(int && i){return f(i);} int & g(int & i){return f(i);} int main() { cout << g(2); }"
    "Replaced \"int f\" with \"int & f\", replaced \"int g\" with \"int & g\", and inserted \"int & g(int & i){return f(i);}\" before \"int main\"."
    "Replaced \"int & f\" with \"int f\", replaced \"int & g\" with \"int g\", and erased \"int & g(int & i){return f(i);}\"."

  dt' "{string foo = \"kangaroo\"; auto m = foo.find_first_of('m'); cout << *m;}"
    "{string foo = \"kangaroo\"; size_t m = foo.find_first_of('m'); cout << foo[m];}"
    "Replaced \"auto m\" with \"size_t m\", and replaced \"<< *m\" with \"<< foo[m]\"."
    "Replaced \"size_t m\" with \"auto m\", and replaced \"<< foo[m]\" with \"<< *m\"."
      -- Todo: Group.

  dt "{int i=0,t=time(0);for(;i<5000000;i++)asm(\".org 0xffff\");cout << time(0)-t; }"
    "{int i=0,t=time(0);for(;i<5000;i++)int tmp=1*88+71/4000^66;cout << time(8)-t; }"
    "Replaced \"i<5000000\" with \"i<5000\", replaced \"asm(\\\".org 0xffff\\\")\" with \"int tmp=1*88+71/4000^66\", and replaced \"0\" with \"8\"."

  dt' "struct curr_func { string name; cf(string n):name(n){} }; \\ #define DEFUN(name) name try { throw curr_func(#name); } catch(curr_func& cfun) \\ DEFUN(void haha(int a)) { cout << cfun.name; } int main() { cout << haha; }"
    "struct curr_func { string name; curr_func(string n):name(n){} }; \\ #define DEFUN(name) name try { throw curr_func(#name); } catch(curr_func& cfun) \\ DEFUN(void haha(int a)) { cout << cfun.name << \": \" << a; } int main() { haha(42); }"
    "Replaced \"cf\" with \"curr_func\", inserted \"<< \\\": \\\" << a\" after \"<< cfun.name\", and replaced \"cout << haha\" with \"haha(42)\"."
    "Replaced \"curr_func\" with \"cf\", erased \"<< \\\": \\\" << a\", and replaced \"haha(42)\" with \"cout << haha\"."

  dt "{float i=0,t=time(0);for(;i<5000000;i++) { if(x == 3) f(reinterpret_cast<long>(x)); } }"
    "{int i=0,t=time(null);for(;i<5000000;++i) { if(x != 3) f(static_cast<long>(x)); } }"
    "Replaced \"float i\" with \"int i\", replaced \"0\" with \"null\", replaced \"i++\" with \"++i\", replaced \"x == 3\" with \"x != 3\", and replaced \"reinterpret_cast<long>\" with \"static_cast<long>\"."
{-
  dt' "struct a { int b; a():b([]{ return 1 + 2 + 3; }){}}a_; int main() { cout << a_.b }"
    "struct a { int b; a({}):b([]{ return 1 + 2 + 3; }()){}}a_; int main() { cout << a_.b; }"
    "Replaced \"a():b([]{\" with \"a({}):b([]{\", replaced \")\" after \"return 1 + 2 + 3; }\" with \"())\", and inserted \";\" after \"cout << a_.b\"."
    "Replaced \"a({}):b([]{\" with \"a():b([]{\", replaced \"}()){}}a_\" with \"}){}}a_\", and erased \";\" after \"cout << a_.b\"."
-}
  dt "struct curr_func { string name; cf(string n):name(n){} }; \\ #define DEFUN(name) name try { throw curr_func(#name); } catch(curr_func& cfun) \\ DEFUN(void haha(int a)) { cout << cfun.name; } int main() { cout << haha; }"
    "struct { string name; cf(string n){} }; \\ #define DEFUN(name) name try { } catch(curr_func& cfun) \\ DEFUN(void haha(int a) { cout << \"tortoise\"; } ) { cout << ETYPE(cfun.name); } int main() { do_something(complicated); cout << haha; }"
    "Replaced \"struct curr_func\" with \"struct\", erased \":name(n)\" and \"throw curr_func(#name);\", inserted \"{ cout << \\\"tortoise\\\"; }\" before \") { cout\", replaced \"<< cfun.name\" with \"<< ETYPE(cfun.name)\", and inserted \"do_something(complicated);\" before \"cout << haha\"."

  dt "{ char str[] = \"giraffe\"; char * pch; pch=(char*) memchr(str, 'y', (int)strlen(str)); if (pch!=NULL) { printf(\"'y' was found at position #%d\", pch-str+1); } }"
    "{ char * str = \"giraffe\"; char * pch; pch=(char*) memchr(str, 'v', (size_t)strlen(str)); if (pch!=NULL) { printf(\"'v' was found at position #%d\", pch-str-1); memset(str, '*', 6); puts(str); printf(\"%s\", str); } }"
    "Replaced \"char str[]\" with \"char * str\", replaced \"'y'\" with \"'v'\", replaced \"int\" with \"size_t\", replaced \"\\\"'y' was\" with \"\\\"'v' was\", and replaced \"pch-str+1\" with \"pch-str-1); memset(str, '*', 6); puts(str); printf(\\\"%s\\\", str\"."

  dt "geordi: { char y(34); stringstream i(\"geordi: { char y(34); stringstream i(!); string t; getline(i, t, '!'); cout << t << y << i.str() << y << i.rdbuf(); }\"); string t; getline(i, t, '!'); cout<< t << y << i.str() << y << i.rdbuf(); }"
    "geordi: { stringstream i(\"geordi: { stringstream i(!); string t; getline(i, t, '!'); cout << t << i.str() << i.rdbuf(); }\"); string t; getline(i, t, '!'); cout << t << i.str() << i.rdbuf(); }"
    "Erased \"char y(34);\" and \"char y(34);\" and \"<< y\" and \"<< y\" and \"<< y\" and \"<< y\"." -- Todo: "Erased all \"char y(34);\" and all \"<< y\"."

  dt "char *& f() { static char *p; cout << &p << endl; return p; } int main() { char *p = f(); cout << &p << endl; }"
    "char *& f() { static char *p; cout << &p << ' '; return p; } int main() { char *p = f(); cout << &p; }"
    "Replaced \"<< endl\" with \"<< ' '\", and erased \"<< endl\"." -- Todo: say "first" and "last".

  putStrLn "No test failures."
 where
  t :: String -> Either String String -> IO ()
  t c o = (print c >>) $ test_cmp c o $ case PS.parse (commandsP << eof) "" c of
    Left e -> Left $ showParseError "command" c True e
    Right cmds -> editable_body . execute cmds (EditableRequest (Evaluate (const False)) "1 2 3 2 3 4 5")
  ut :: String -> String -> String -> String -> IO ()
  ut pattern match d rd = do
    let txt = "{ string::size_t- siz = 2; int x = 3; if(i == 0) cout << ETPYE(x - size); vector<int> v; v = { 3, 2 }; vector<int> i = reinterpret_cat<fish>(10000000000, v.begin()); } X(y); using tracked::B; B z = B{p}; int const u = 94; int * w = &u--j; !B && !D; vector<unsigned char> & r = v; struct C(){ C & operator+(ostream &, char(const&here)[N], C const &) }; template<typename T> voidfoo(T a) { a.~T; } void main() { int a; a.seek(1800, ios::end); foo(a++); if(x >= 7) throw runtime_exception(y); } class Qbla { public: fstream p; };"
    RangeReplaceEdit rng _ <- findInStr txt (UseString pattern)
    test_cmp pattern match (selectRange rng txt)
    let r = replaceRange rng pattern txt
    test_cmp pattern d $ pretty_diff txt r
    test_cmp pattern rd $ pretty_diff r txt
  dt :: String -> String -> String -> IO ()
  dt x y r = test_cmp x r $ pretty_diff x y
  dt' :: String -> String -> String -> String -> IO ()
  dt' x y xy yx = do
    test_cmp x xy $ pretty_diff x y
    test_cmp y yx $ pretty_diff y x
  dts :: String -> [(String, String)] -> IO ()
  dts _ [] = return ()
  dts s ((s', d) : r) = dt s s' d >> dts s' r
  pretty_diff :: String -> String -> String
  pretty_diff x y = capitalize (commas_and $ show . diff x y) ++ "."

{- Command grammar:

  command = (insert | append | prepend | erase | replace | move | wrap | use)*

  insert = ("insert" | "add") ... positions*
  append = "append" ... [positions*]
  prepend = "prepend" ... [positions*]
  erase = ("erase" | "remove" | "delete" | "cut" | "omit" | "kill") substrs*
  replace = "replace" (substrs* ("with" | "by") ...)*
  move = "move" (substr "to" position)*
  wrap = "wrap" wrapping ("around" substrs*)* | "wrap" substrs* "in" wrapping
  use = "use" verbatim*

  wrapping = ... "and" ... | "parentheses" | "parens" | "braces" | "curlies"
    | ("square" | "angle" | "curly" | "round") "brackets" | ("single" | "double") "quotes"
  relative = between | befaft ranked
  substrs = range | ("everything" | rankeds) [relative]
  substr = range | ("everything" | ranked) [relative]
  position = limit | befaft ("everything" | ranked)
  befaft = "before" | "after"
  positions = "at" limit | befaft (("everything" | rankeds) [befaft ranked])*
  ordinal = "first" | ("second" | "third" | etc) ["last"] | "last"
  ranked = [ordinal] ...
  rankeds = "all" [("except" | "but") ordinal*] ... | ["each" | "every" | "any" | ordinal*] ...
  between = "between" (bound "and" relative-bound | ordinal "and" ordinal ...)
  range = ([["everything"] "from"] bound | "everything") ("till" | "until") relative-bound
  limit = "begin" | "front" | "end" | "back"
  bound = limit | [befaft] ("everything" | ranked)
  relative-bound = limit | [befaft] ("everything" | ranked) [relative]

  Ellipsis denote a verbatim string, and x* = x ["and" x*].

  This is an idealized grammar with LOTS of ambiguity. The parsers do their best to choose the most sensible interpretation of ambiguous commands.

Design notes:

  "use" is heavily biased toward whole-token edits, so users are encouraged to use those. This will not only produce better edits, it is also more readable.

  Giving moves a single target makes "move 4 to end and 5 to begin" work, because otherwise it would be parsed as a move with two targets, the second of which, 5, is not a valid target.

  Should "second last x before y" in "xxaxxy" designate the 'x' before or after 'a'?

    We choose the latter, because it seems more natural, despite the curious result that "first x before y" now means the same as "last x before y".

  Should "erase all x and y" mean "erase all x and all y" or "erase all x and the sole y"?

    We choose the latter, because:
    - it's easier to implement, because we don't need "and"-repetition nested under "all";
    - it's safe, because if y occurs multiple times, a clear "y occurs multiple times" error will be emitted, whereas the former solution could result in unintended edit results.

  Should "first and second last x" mean "(first and second) last x" or "first and (second last) x"?

    I have no strong feelings on the matter. Currently it is interpreted to mean the second.

  Should "from ..." / "until ..." bounds be inclusive or exclusive?

    We choose the former, for no particular reason. Note that this default can be overridden by by saying "from after ..." / "until before ...".

  Grammar guidelines:

    - Ranges are never relative (but their bounds may be).
    - Position specifications never mention ranges (this means that "everything" must not be a range).

  Semantic choices:

    - Ordinal specifications are always relative to the full string, so "erase from x until second b" in "bxabcb" produces "bxbcb", not "bxb".

-}
