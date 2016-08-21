#ifndef BARK_HPP
#define BARK_HPP

#include <cstdio>
#include <string>

namespace bark_detail
{
  using std::string;

  string bark(string pf, string func);
}

#ifndef BARK_TEST

#include "geordi.hpp"

#define BARK \
  (::std::printf("%s%s%s", ::geordi::parsep, ::bark_detail::bark(__PRETTY_FUNCTION__, __func__).c_str(), ::geordi::parsep), ::std::fflush(stdout))
  // We don't include a trailing semicolon, because a function body looking like { BARK } confuses geordi's C++ parser.

#else

#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <boost/range.hpp>

#define BARK(expected) \
  { std::string const s(bark_detail::bark(__PRETTY_FUNCTION__, __func__)); std::cout << s << '\n'; assert(s == expected); }

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

#endif // header guard
