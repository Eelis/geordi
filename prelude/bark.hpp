#ifndef BARK_HPP
#define BARK_HPP

#include <utility>
#include <cstdio>
#include <cstring>
#include "geordi.hpp"

#include "more_ostreaming.hpp"

namespace bark_detail
{
  template <typename T, typename I>
  I find_first_toplevel(T const open, T const close, T const v, I i, I const e) {
    for(int nested = 0; i != e && !(nested == 0 && *i == v); ++i)
    { if(*i == open) ++nested; if(*i == close) --nested; }
    return i;
  }

  template <std::size_t N>
  char const * bark(char const (& s)[N]) {
    char const * r = 0;
    for(std::reverse_iterator<char const *> i(find_first_toplevel('<', '>', '(', s, s+N)), e(s); i != e; ) {
      i = find_first_toplevel('>', '<', ' ', i+1, e);
      if(!r) r = i.base();
      if(std::strstr(i.base(), "operator ") == i.base() || std::strstr(i.base(), "::operator "))
        return i.base();
    }
    return r;
  }
}

#ifndef BARK_TEST

#define BARK \
  (::std::printf("%s%s%s", ::geordi::parsep, ::bark_detail::bark(__PRETTY_FUNCTION__), ::geordi::parsep), ::std::fflush(stdout))
  // We don't include a trailing semicolon, because a function body looking like { BARK } confuses geordi's C++ parser.

#else

#include <sstream>
#include <cassert>
#include <string>
#include <boost/range.hpp>

#define BARK(expected) \
  { std::string const s(bark_detail::bark(__PRETTY_FUNCTION__)); std::cout << s << '\n'; assert(s == expected); }

#include <boost/function.hpp>
#include <boost/implicit_cast.hpp>

void * operator new(std::size_t, char) throw() { BARK("operator new(size_t, char)"); return 0; }
boost::function<void()> f() { BARK("f()"); return 0; }
std::pair<int, int> g() { BARK("g()"); return std::make_pair(0, 0); }
struct X { operator unsigned int() { BARK("X::operator unsigned int()"); return 0; } };

signed char h(void(*)()) { BARK("h(void (*)())"); return 0; }

int main()
{
  BARK("main()");
  f(); g(); h(0);
  boost::implicit_cast<unsigned int>(X());
  ::operator new(0, 'x');

  std::cout << "-----------------\nNo test failures.\n";
}

#endif // tests

#endif // header guard
