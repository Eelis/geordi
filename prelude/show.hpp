#ifndef SHOW_HPP
#define SHOW_HPP

#include <iostream>

namespace show_detail
{
  template <typename T> struct shower { T const & v; char const * const s; };

  template <typename C, typename Tr, typename T>
  std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, shower<T> const & s)
  { return o << s.s << " = " << s.v; }

  template <typename T>
  shower<T> show(T const & v, char const * const s) { shower<T> const r = { v, s }; return r; }
}

#define SHOW(x) ::show_detail::show((x), #x)
  // The more obvious   #define SHOW(x) #x " = " << (x)   does not work in   cout << SHOW(x), SHOW(y);. The alternative   #define SHOW(x) (#x " = " + ::boost::lexical_cast<::std::string>(x))   does not use the stream's formatting flags.

#endif // header guard
