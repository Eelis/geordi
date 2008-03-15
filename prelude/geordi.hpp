#ifndef GEORDI_HPP
#define GEORDI_HPP

#include <iostream>
#include <string>

namespace geordi
{
  void abort();

  extern char const parsep[];

  struct error: boost::noncopyable
  {
    error() { std::cout << parsep << "error: "; }
    ~error() { geordi::abort(); }
    std::ostream & operator()() const { return std::cout; }
  };

  struct initializer_t { initializer_t (); };

  std::string advice();
}

#endif // header guard
