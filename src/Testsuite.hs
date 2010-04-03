import qualified Request
import qualified RequestEval

import Control.Exception ()
import Control.Monad (unless, forM_)
import Control.Monad.Error ()
import System.Environment (getArgs)
import Text.Regex (matchRegex, mkRegex)
import Data.List (sort, isPrefixOf)

-- Ascii art:

esc :: Char
esc = '\x1b'

colored :: Integer → String → String
colored c s = [esc] ++ "[" ++ show c ++ "m" ++ s ++ [esc] ++ "[0m"

red, green, yellow, cyan :: String → String
red = colored 31
green = colored 32
yellow = colored 33
cyan = colored 36

box :: String → String
box s = '+' : bar ++ "+\n| " ++ s ++ " |\n+" ++ bar ++ "+"
  where bar = replicate (length s + 2) '-'

-- Infrastructure:

class Show t ⇒ TestPred t where testPred :: t → String → Bool

newtype ExactMatch = ExactMatch String deriving Show
instance TestPred ExactMatch where testPred (ExactMatch s) = (== s)

newtype PrefixMatch = PrefixMatch String deriving Show
instance TestPred PrefixMatch where testPred (PrefixMatch s) = isPrefixOf s

newtype RegexMatch = RegexMatch String deriving Show
instance TestPred RegexMatch where testPred (RegexMatch r) s = matchRegex (mkRegex r) s /= Nothing

data NoOutput = NoOutput deriving Show
instance TestPred NoOutput where testPred NoOutput = (== "")

data Test = Test { test_name, test_request :: String, test_pred :: String → Bool, test_pred_name :: String }

test :: TestPred p ⇒ String → String → p → Test
test n r p = Test n r (testPred p) (show p)

utest :: TestPred p ⇒ String → p → Test
utest = test "unnamed test"

main :: IO ()
main = do

  evalRequest ← RequestEval.evaluator (const ("", ""))

  putStrLn
    "\nNote: In several tests, output is expected to include an error (sometimes on a separate line), so seeing an error in a test's output does not mean the test failed. A test failed if its output is colored red.\n"

  let default_test_sets = ["resources", "misc", "diagnostics", "utilities"]

  args ← getArgs
  forM_ (case args of [] → default_test_sets; _ → args) $ \set → do
    putStrLn $ box $ "Test set: " ++ set
    forM_ (tests set) $ \(Test n r p pn) → do
      putStrLn $ "\nTest: " ++ yellow n
      putStrLn $ "Request: " ++ cyan r
      out ← fmap Request.response_output $ evalRequest r (Request.Context [])
      let success = p out
      putStrLn $ "Output: " ++ (if success then green else red) (if out == "" then "<none>" else out)
      unless success $ putStrLn $ "Expected: " ++ pn
    putStrLn ""

  putStrLn "Done running tests."

-- Actual test:

tests :: String → [Test]

tests "resources" =
  [ test "Program timeout" "{ for(;;) ; }" $ ExactMatch "Killed"
  , test "Stack overflow" "<< f(3); int f(int i) { if (i % 10000 == 0) cout << '+' << flush; return -f(++i); }" $
    RegexMatch "\\++ Undefined behavior detected."
  , test "Open FDs" "{ for(int i = 0; i != 1024; ++i) assert(i == 1 || i == 2 || close(i) == -1); }" NoOutput
  , test "File creation" "{ ofstream f (\"bla\"); assert(errno == EACCES); }" NoOutput
  , test "Working directory" "<< get_current_dir_name()" $ ExactMatch "/"
  , test "File I/O" "{ { ofstream f (__FILE__); f << \"foo\"; } cout << ifstream(__FILE__).rdbuf(); }" $
    ExactMatch "foo"
  , test "Memory limit" "{ int i = 0; while (new (nothrow) char [1 << 20]) ++i; assert(i < 250); }" NoOutput
  , test "Fd limit" "{ int i = 0; while (open(__FILE__, 0) != -1) ++i; assert(errno == EMFILE); assert(i < 50); }  extern \"C\" int open (char const *, int);" NoOutput
  , test "File size limit" "{ ofstream f (__FILE__); string meg (1 << 20, 'x'); for (;;) { f << meg << flush; cout << '+' << flush; } }" $
    RegexMatch "\\+{1,50} File size limit exceeded$"
  , test "System call interception" "<< fork()" $ RegexMatch "SYS_[^:]*: Operation not permitted"
  , test "Signal" "{ int x = 0; cout << 3 / x; }" $ ExactMatch "Floating point exception"
  , test "Recursive exec()"
    "int main (int const argc, char const * const * argv) { string s; if (argc >= 2) s = argv[1]; s += 'x'; if (s.size() % 100 == 0) cout << '+' << flush; execl(\"/t\", \"/t\", s.c_str(), 0); }" $
    RegexMatch "\\++( Killed)?$"
  ]

tests "misc" =
  [ test "Simple output" "<< 3" $ ExactMatch "3"
  , let quine = "{string t,u,y(1,34);stringstream i(\"{string t,u,y(1,34);stringstream i(!);getline(i,t,'!')>>u;cout<<t<<y<<i.str()<<y<<u;}\");getline(i,t,'!')>>u;cout<<t<<y<<i.str()<<y<<u;}" in test "Quine" quine $ ExactMatch quine
  , test "UTF-8 handling" "<< 'a' << char(144) << 'b' << char(215) << char(144) << 'c'" $
    ExactMatch "a�bאc"
  , let s = "dicekjhbagfl" in
    test "Nontrivial program (Brainfuck interpreter)" ("{b(program);}char program[]=\">>,[>>,]<<[[-<+<]>[>[>>]<[.[-]<[[>>+<<-]<]>>]>]<<]\",input[]=\"" ++ s ++ "\", *i=input,m[512]={},*p=m;void b(char*c){for(;*c&&*c!=']';++c){(*((p+=*c=='>')-=*c=='<')+=*c=='+') -=*c=='-';*c=='.'&&cout<<*p;if(*c==',')*p=*i++;if(*c=='['){for(++c;*p;)b(c);for(int d=0;*c!=']'||d--;++c)d+=*c=='[';}}}") $ ExactMatch (sort s)
  , test "srand()/time()" "{ srand(time(0)); }" NoOutput
  , test "line breaks" "#define X \"\\\\\" \\ #define Y X \\ int main() { cout \\ << Y Y; }" $ ExactMatch "\\\\"
  , test "-v" "-v" $ PrefixMatch "g++ (GCC) 4"
  , test "getopt" "-monkey chicken" $ ExactMatch "error: No such option: -m"
  , test "operator new/delete overriding" "{ cerr << \"| \"; list<int> v(5); } void * operator new(size_t const s) throw(bad_alloc) { cerr << s << ' '; return malloc(s); } void operator delete(void * const p) throw() { free(p); }" $ RegexMatch "[^-]*\\| [[:digit:] ]+"
  ]

tests "diagnostics" =
  [ test "-fstack-protector-all" "-w { char buf [10]; fill(buf, buf+30, 'x'); }" $
    PrefixMatch "*** stack smashing detected ***: /t terminated\n"
  , test "-mcheck diagnostic" "{ int * p = new int [3]; p[3] = 6; delete[] p; }" $
    PrefixMatch "memory clobbered past end of allocated block\n"
  , test "Ditto" "{ int * p = new int [3]; p[-1] = 6; delete[] p; }" $
    PrefixMatch "memory clobbered before allocated block\n"
  , test "Checking global allocation/deallocation operators" "{ delete new int[3]; }" $
    PrefixMatch "error: tried to apply non-array operator delete to pointer returned by new[]. Aborted"
  , test "Ditto." "{ int * const p = new int; delete p; new int; delete p; }" $ ExactMatch "error: tried to delete already deleted pointer. Aborted"
  , test "Custom terminate() handler" "{ throw std::logic_error(\"It is not logical, Captain.\"); }" $
    ExactMatch "terminated by logic_error: It is not logical, Captain."
  , test "libstdc++ debug mode" "{ boost::rational<int> r(2, 3); cout << r << flush; vector<int>::iterator x, y(x); }" $ PrefixMatch "2/3 error: "
  , test "Fatal warnings" "{} int f() {}" $ PrefixMatch "warning: "
  ]

tests "tracked" =
  [ test "Operation on destructed B" "{ using tracked::B; B x(0), y(x); x.B::~B(); x.f(); }" $ ExactMatch "B0*(0) B1*(B0) B0~ error: tried to call B::f() on destructed B0. Aborted"
  , test "Re-destruction" "{ using tracked::B; B b; b.~B(); }" $ ExactMatch "B0* B0~ B0~ error: tried to re-destruct destructed B0. Aborted"
  , test "Stack leak" "{ using tracked::B; { union { double d; char c[sizeof(B)]; }; new (c) B; } B b; }" $ ExactMatch "B0* B1* B1~ leaked: B0. Aborted"
  , test "Stack overwrite" "{ using tracked::B; B b; new (&b) B; }" $ ExactMatch "B0* error: leaked: B0. Aborted"
  , test "new[]/delete[]." "{ boost::shared_array<tracked::B> a(new tracked::D[2]); }" $ ExactMatch "new(D[]) B0* D0* B1* D1* D1~ B1~ D0~ B0~ delete[D0, D1]"
  , test "Operation on non-existent object" "{ tracked::B * p = 0; p->f(); }" $ ExactMatch "error: tried to call B::f() on non-existent object. Aborted"
  , test "Read from dead object." "-w << f(); using tracked::B; B const & f() { return B(); }" $ ExactMatch "B0* B0~ error: tried to read destructed B0. Aborted"
  , test "Initialization" "{ tracked::B b = 1, c(1); }" $ ExactMatch "B0*(1) B1*(B0) B0~ B2*(1) B2~ B1~"
  ]

tests "utilities" =
  [ test "-t/-c" "-tc use ns boost; tmp <tpn T> cls C { expl C (C co &); pvt: stc dub d; pub: void op() (); };" $
    ExactMatch "Success"
  , test "ETYPE" "{ int i = 4; cout << ETYPE(++i); }" $ ExactMatch "lvalue int"
  , test "Range printing" "{ vector<int> v; v += 3, 5, 9, 4, 1; cout << v; }" $ ExactMatch "{3, 5, 9, 4, 1}"
  , test "Demangled printable typeid" "<< typeid(int)" $ ExactMatch "int"
  , test "Custom assert()/abort()" "{ assert(4 > 9); }" $ ExactMatch "Assertion `4 > 9' fails."
  , test "bin IO manipulator" "<< showbase << uppercase << bin << setfill('_') << internal << setw(10) << 53" $ ExactMatch "0B__110101"
  , test "BARK" "{ Y y; y.f(0); y.Xi::f(0); } template <typename> struct X {}; template <typename T> struct X<T const> { virtual void f(void(*)()) { BARK; } }; typedef X<std::string const> Xi; struct Y: Xi { void f(void(*)()) { BARK; } };" $ ExactMatch "Y::f(void (*)()) X<const string>::f(void (*)())"
  , test "SHOW" "{ cout << \"hm\"; SHOW(3 * 2), SHOW(9 - 3); cout << \"ok\"; }" $ ExactMatch "hm 3 * 2 = 6, 9 - 3 = 6 ok"
    -- (Tests parsep handling.)
  ]

tests "errorfilters" =
  [ test "Type cleanup" "{ wistringstream is; !is.str(); }" $
    ExactMatch "error: no match for 'operator!' in '!wistringstream::str() const()'"
  , test "Ditto" "<< ETYPE(&vector<queue<istream_iterator<int> > >::foo)" $
    ExactMatch "error: 'foo' is not a member of 'vector<queue<istream_iterator<int>>>'"
  , test "Preprocessor error" "<< 08" $ ExactMatch "error: invalid digit \"8\" in octal constant"
  , test "[with ...]-replacement" "<< 1 == 1" $ ExactMatch "error: no match for 'operator==' in 'cout.ostream::operator<<(1) == 1'"
  , test "Ditto" "template <typename T> void f(); template <> void f<int>() {} template <> void f<int>() {}" $ ExactMatch "error: redefinition of 'void f() [with T = int]'"
  ]

tests "uncategorized" =
  [ utest "-version" $ ExactMatch "error: No such option: -e." -- .. rather than "No such option: -v", which we got when certain options (like -v) were treated separately from evaluation options.
  ]

tests s = error $ "no such test set: " ++ s
