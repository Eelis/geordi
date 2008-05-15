#ifndef GEORDI_HPP
#define GEORDI_HPP

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <boost/noncopyable.hpp>

namespace geordi
{
  namespace { char const parsep[] = "\342\200\251"; }
    // UTF-8 encoding of PARAGRAPH SEPARATOR (U+2029). See "Output separators" in notes.txt.

  struct error: boost::noncopyable
  {
    error() { std::printf("%serror: ", parsep); }
    ~error() { std::abort(); }
    std::ostream & operator()() const { return std::cout; }
  };

  struct initializer_t { initializer_t (); };

  std::string advice();
}

#endif // header guard
