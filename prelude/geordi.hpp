#ifndef GEORDI_HPP
#define GEORDI_HPP

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <boost/noncopyable.hpp>
#include <sys/utsname.h>

#define GNUC_GE(major, minor, patchlevel) \
  ((__GNUC__ * 100 + __GNUC_MINOR__) * 100 + __GNUC_PATCHLEVEL__ >= \
  (major * 100 + minor) * 100 + patchlevel)

#if GNUC_GE(4,4,0) && defined(__GXX_EXPERIMENTAL_CXX0X__)
  #define GEORDI_USE_EXTERN_TEMPLATE
  #define GEORDI_USE_CHRONO
#endif

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

  utsname uname();
}

#endif // header guard
