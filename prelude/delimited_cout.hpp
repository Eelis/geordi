
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
      // Last one for things like std::hex which shouldn't be delimited.
  };

  enum Del { del }; // Enum instead of class is header-friendly.
  template <typename T> C operator<< (Del, T const & t) { std::cout << t; return C(); }
  C operator<< (Del, std::ostream & (* const f) (std::ostream &)) { std::cout << f; return C(); }

  // The <<'s taking std::ostream&(std::ostream&)'s are for things like std::endl.

  Del operator<< (std::ostream &, Del) { return del; }
    // To allow:  geordi << del << 2 << 4
}

using delimited_cout_detail::del;

#endif // header guard
