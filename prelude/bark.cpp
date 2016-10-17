#include "bark.hpp"
#include "geordi.hpp"

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/find.hpp>

namespace
{
  using std::string;

  char closer(char const c) {
    switch(c) {
      case '<': return '>';
      case '>': return '<';
      case '{': return '}';
      case '}': return '{';
      case '(': return ')';
      case ')': return '(';
      case '[': return ']';
      case ']': return '[';
      default: return 0;
  } }

  template <typename I>
  I find_toplevel(char const c, I i, I const e)
  {
    for(;;)
    {
      if(i == e) return e;
      else if(*i == c) return i;
      else if(char const cl = closer(*i)) i = find_toplevel(cl, i+1, e)+1;
      else ++i;
    }
  }

  string bark(string const pf, string const func) {

    string::size_type const with = pf.find(" [with ");
    if(with != string::npos)
      return bark(pf.substr(0, with), func) + pf.substr(with);

    string::const_iterator p = boost::ends_with(pf, ")>") ? pf.end() : boost::find_first(pf, func + '(').begin();
    if(p == pf.end() || (p != pf.begin() && *(p-1) == ':')) {
      std::reverse_iterator<string::const_iterator> j(p);
      p = find_toplevel(' ', j, pf.rend()).base();
    }

    return string(p, pf.end());
  }
}

namespace bark_detail
{
  void do_bark(char const * pf, char const * f)
  {
    std::printf("%s%s%s", geordi::parsep, bark(pf, f).c_str(), geordi::parsep);
    std::fflush(stdout);
  }
}

#ifdef BARK_TEST

#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <boost/range.hpp>

#undef BARK
#define BARK(expected) \
  { std::string const s(bark(__PRETTY_FUNCTION__, __func__)); std::cout << s << '\n'; assert(s == expected); }

#include <boost/function.hpp>
#include <boost/implicit_cast.hpp>

void * operator new(std::size_t, char) throw() { BARK("operator new(std::size_t, char)"); return 0; }
boost::function<void()> f() { BARK("f()"); return 0; }
std::pair<int, int> g() { BARK("g()"); return std::make_pair(0, 0); }
struct X { operator unsigned int() { BARK("X::operator unsigned int()"); return 0; } void f(){ BARK("X::f()");} };
void operator<<(X,X){ BARK("operator<<(X, X)"); }
std::vector<unsigned int> vf(){ BARK("vf()"); return std::vector<unsigned int>(); }
template<typename A, typename B> struct Y { void f(){ BARK("Y<A, B>::f() [with A = int; B = int]"); } };
signed char h(void(*)()) { BARK("h(void (*)())"); return 0; }
template<void(*)(X,X)> void bla(){ BARK("bla() [with void (* <anonymous>)(X, X) = operator<<]"); }


int main()
{
  BARK("main()");
  X().f();
  Y<int,int>().f();
  vf();
  f(); g(); h(0);
  boost::implicit_cast<unsigned int>(X());
  ::operator new(0, 'x');
  [](int){ BARK("main()::<lambda(int)>"); }(3);
  [](int, auto){ BARK("main()::<lambda(int, auto:1)> [with auto:1 = int]"); }(3, 2);
  bla<&operator<< >();
  X()<<X();

  std::cout << "-----------------\nNo test failures.\n";
}

#endif // tests
