import qualified Request

import Control.Exception ()
import Control.Monad (when)
import Control.Monad.Error ()
import Text.Regex (matchRegex, mkRegex)
import Data.List (sort, isPrefixOf)

import Prelude hiding (catch, (.))
import Util

esc :: Char
esc = '\x1b'

colored :: Integer -> String -> String
colored c s = [esc] ++ "[" ++ show c ++ "m" ++ s ++ [esc] ++ "[0m"

red, green, yellow, cyan :: String -> String
red = colored 31
green = colored 32
yellow = colored 33
cyan = colored 36

class Show t => Test t where do_test :: t -> String -> Bool

newtype ExactMatch = ExactMatch String deriving Show
instance Test ExactMatch where do_test (ExactMatch s) = (== s)

newtype PrefixMatch = PrefixMatch String deriving Show
instance Test PrefixMatch where do_test (PrefixMatch s) = isPrefixOf s

newtype RegexMatch = RegexMatch String deriving Show
instance Test RegexMatch where do_test (RegexMatch r) s = matchRegex (mkRegex r) s /= Nothing

data NoOutput = NoOutput deriving Show
instance Test NoOutput where do_test NoOutput = (== "")

main :: IO ()
main = do

  evalRequest <- Request.prepare_evaluator

  let
    test :: Test t => String -> String -> t -> IO ()
    test title req t = do
      putStrLn $ "\nTest: " ++ yellow title
      putStrLn $ "Request: " ++ cyan req
      out <- evalRequest req
      let success = do_test t out
      putStrLn $ "Output: " ++ (if success then green else red) (if out == "" then "<none>" else out)
      when (not success) $ putStr "Expected: " >> print t

  jail

  putStrLn $
    "\nNote: In several tests, output is expected to include an error (sometimes on a separate line), so seeing an error in a test's output does not mean the test failed. A test failed if its output is colored red."

  test "Simple output" "<< 3" $ ExactMatch "3"

  test "Stack overflow" "void f(int i) { if (i % 10000 == 0) cout << '+' << flush; f(++i); } int main () { f(0); }" $
    RegexMatch "\\++ Segmentation fault"

  test "close()" "{ for(int i = 0; i != 1024; ++i) assert(i == 1 || i == 2 || close(i) == -1); }" NoOutput

  test "srand()/time()" "{ srand(time(0)); }" NoOutput

  test "Working directory" "<< get_current_dir_name()" $ ExactMatch "/"

  test "File creation" "{ ofstream f (\"bla\"); assert(errno == EACCES); }" $ ExactMatch ""

  test "-t/-c" "-tc use ns boost; tmpl <tpn T> cls C { expl C (C co &); pvt: stc dub d; pub: void op() (); };" $
    ExactMatch "Compilation successful"

  let quine = "{string t,u,y(1,34);stringstream i(\"{string t,u,y(1,34);stringstream i(!);getline(i,t,'!')>>u;cout<<t<<y<<i.str()<<y<<u;}\");getline(i,t,'!')>>u;cout<<t<<y<<i.str()<<y<<u;}"
  test "Quine" quine $ ExactMatch quine

  test "-v" "-v" $ PrefixMatch "g++ (GCC) 4"

  test "getopt" "-monkey chicken" $ ExactMatch "unrecognized option `-m'\n"

  test "Program timeout" "{ for(;;) ; }" $ RegexMatch "Timeout|Killed"

  test "Custom assert()/abort()" "{ assert(4 > 9); }" $ ExactMatch "Assertion `4 > 9' fails.\nAborted."

  test "File I/O" "{ { ofstream f (__FILE__); f << \"foo\"; } cout << ifstream(__FILE__).rdbuf(); }" $
    ExactMatch "foo"

  test "Memory limit" "{ int i = 0; while (new (nothrow) char [1 << 20]) ++i; assert(i < 250); }" NoOutput

  test "Fd limit" "extern \"C\" int open (char const *, int); int main () { int i = 0; while (open(__FILE__, 0) != -1) ++i; assert(errno == EMFILE); assert(i < 50); }" NoOutput

  test "File size limit" "{ ofstream f (__FILE__); string meg (1 << 20, 'x'); for (;;) { f << meg << flush; cout << '+' << flush; } }" $
    RegexMatch "\\+{1,50} File size limit exceeded$"

  test "Signal" "{ int x = 0; cout << 3 / x; }" $
    ExactMatch "Floating point exception"

  test "System call interception" "<< fork()" $
    PrefixMatch "Disallowed system call: "

  test "libstdc++ debug mode" "{ vector<int> s (3); cout << *(s.begin() + 4); }" $
    ExactMatch "attempt to advance a dereferenceable (start-of-sequence) iterator 4 steps, which falls outside its valid range."

  test "-mcheck diagnostic" "{ int * p = new int [3]; p[3] = 6; delete[] p; }" $
    PrefixMatch "memory clobbered past end of allocated block\n"

  test "Ditto" "{ int * p = new int [3]; p[-1] = 6; delete[] p; }" $
    PrefixMatch "memory clobbered before allocated block\n"

  test "Ditto" "{ int * p = new int [3]; delete[] p; delete[] p; }" $
    PrefixMatch "block freed twice\n"

  let s = "dicekjhbagfl" in
    test "Nontrivial program (Brainfuck interpreter)" ("char program[]=\">>,[>>,]<<[[-<+<]>[>[>>]<[.[-]<[[>>+<<-]<]>>]>]<<]\",input[]=\"" ++ s ++ "\", *i=input,m[512]={},*p=m;void b(char*c){for(;*c&&*c!=']';++c){(*((p+=*c=='>')-=*c=='<')+=*c=='+') -=*c=='-';*c=='.'&&cout<<*p;if(*c==',')*p=*i++;if(*c=='['){for(++c;*p;)b(c);for(int d=0;*c!=']'||d--;++c)d+=*c=='[';}}}int main(){b(program);}") $ ExactMatch (sort s)

  test "Compiler timeout" "-c #include __FILE__" $
    ExactMatch "g++: Timeout"

  test "bin IO manipulator" "<< showbase << uppercase << bin << setfill('_') << internal << setw(10) << 53" $
    ExactMatch "0B__110101"

  test "Tracked" "{ using tracked::B; B x(0), y(x); x.B::~B(); x.f(); }" $
    ExactMatch " B0*(0)  B1*(B0)  B0~  Error: Tried to call B::f() on destructed B0.\nAborted."

  test "Custom terminate() handler" "{ throw std::logic_error(\"It is not logical, Captain.\"); }" $
    ExactMatch "logic_error: It is not logical, Captain.\nAborted."

  test "UTF-8 handling" "<< 'a' << char(144) << 'b' << char(215) << char(144) << 'c'" $
    ExactMatch "a�bאc"

  test "Error filters" "{ wistringstream is; !is.str(); }" $
    ExactMatch "no match for 'operator!' in '!wistringstream::str() const()'"

  test "Ditto" "<< ETYPE(&vector<queue<istream_iterator<int> > >::foo)" $
    ExactMatch "'foo' is not a member of 'vector<queue<istream_iterator<int>>>'"

  test "-fstack-protector-all" "{ char buf [10]; fill(buf, buf+30, 'x'); }" $
    PrefixMatch "*** stack smashing detected ***: /t terminated\n"

  test "ETYPE" "{ int i = 4; cout << ETYPE(++i); }" $ ExactMatch "lvalue int"

  test "Recursive exec()"
    "int main (int const argc, char const * const * argv) { string s; if (argc >= 2) s = argv[1]; s += 'x'; if (s.size() % 20 == 0) cout << '+' << flush; execl(\"/t\", \"/t\", s.c_str(), 0); }" $
    RegexMatch "\\++( Timeout)?$"

  test "Range printing" "{ vector<int> v; v += 3, 5, 9, 4, 1; cout << v; }" $ ExactMatch "[3, 5, 9, 4, 1]"
