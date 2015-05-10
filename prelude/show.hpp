#ifndef SHOW_HPP
#define SHOW_HPP

#include <iostream>
#include "geordi.hpp"

namespace show_detail
{
  template <typename T> struct shower
  {
    T const & v;
    char const * const s;
    mutable bool due;

    shower(T const & v, char const * const s): v(v), s(s), due(true) {}
    shower(shower const & o): v(o.v), s(o.s), due(o.due) { o.due = false; }
    ~shower() { if(due) std::cout << *this; }

    private: void operator=(shower const&);
  };

  template <typename C, typename Tr, typename T>
  std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, shower<T> const & s)
  { s.due = false; return o << geordi::parsep << s.s << " = " << s.v << geordi::parsep; }

  template <typename T, typename U>
  shower<U> const & operator,(shower<T> const & x, shower<U> const & y)
  {
    if(x.due) std::cout << x;
    std::cout << ", " << y;
    return y;
  }

  template <typename T> shower<T> show(T const & v, char const * const s) { return shower<T>(v, s); }
}

#define SHOW(x) ::show_detail::show((x), #x)

#endif // header guard

#ifdef SHOW_TEST

#include <sstream>
#include <cassert>
#include <iomanip>

template <typename Ch, typename Tr, typename T>
inline std::basic_ostream<Ch, Tr> & operator, (std::basic_ostream<Ch, Tr> & o, T const & t)
{ return o << ", " << t; }

#define PS "\342\200\251"

int main()
{
  using std::cout;

  int const x = 20; double y = 1.3; char z = 'z';
  std::hex(cout);
  std::ostringstream o;
  cout.rdbuf(o.rdbuf());

  cout << SHOW(x), SHOW(y), SHOW(z);
  cout << '\n';
  SHOW(x), SHOW(y), SHOW(z);

  cout.rdbuf(0);

  assert(o.str() ==
    PS "x = 14" PS ", " PS "y = 1.3" PS ", " PS "z = z" PS "\n"
    PS "x = 14" PS ", " PS "y = 1.3" PS ", " PS "z = z" PS);
}

#endif // testing
