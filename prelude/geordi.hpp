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

  template <typename> struct is_character { enum { value = false }; };
  #define YES(T) template <> struct is_character<T> { enum { value = true }; };
  YES(char) YES(wchar_t)
  #ifdef __GXX_EXPERIMENTAL_CXX0X__
    YES(char16_t) YES(char32_t)
  #endif
  #undef YES
}

std::ostream & operator<<(std::ostream &, wchar_t);
std::ostream & operator<<(std::ostream &, wchar_t const *);
std::ostream & operator<<(std::ostream &, std::wstring const &);

#endif // header guard
