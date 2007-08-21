
#ifndef DELIMITED_COUT_HPP
#define DELIMITED_COUT_HPP

#include <iostream>
#include <ios>

namespace delimited_cout_detail
{
  struct C
  {
    template <typename T> C operator<< (T const & t) const { std::cout << ", " << t; return C(); }
    C operator<< (std::ostream & (* const f) (std::ostream &)) const { std::cout << f; return C(); }
    C operator<< (std::ios_base & (* const f) (std::ios_base &)) const { std::cout << f; return C(); }
  };

  enum Del { del };
  template <typename T> C operator<< (Del, T const & t) { std::cout << t; return C(); }
  Del operator<< (Del, std::ostream & (* const f) (std::ostream &)) { std::cout << f; return del; }
  Del operator<< (Del, std::ios_base & (* const f) (std::ios_base &)) { std::cout << f; return del; }

  // The <<'s taking std::ios_base&(std::ios_base&)'s are for things like std::hex.
  // The <<'s taking std::ostream&(std::ostream&)'s are for things like std::endl.

  Del operator<< (std::ostream &, Del) { return del; }
    // To allow:  geordi << del << 2 << 4
}

using delimited_cout_detail::del;

#endif // header guard
