#ifndef GEORDI_HPP
#define GEORDI_HPP

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <sys/utsname.h>

namespace geordi
{
  namespace { char const parsep[] = "\342\200\251"; }
    // UTF-8 encoding of PARAGRAPH SEPARATOR (U+2029). See "Output separators" in notes.txt.

  struct error
  {
    error() { std::printf("%serror: ", parsep); }
    ~error() { std::abort(); }
    std::ostream & operator()() const { return std::cout; }
  };

  struct initializer_t { initializer_t (); };

  utsname uname();

  template <typename> struct is_character { enum { value = false }; };
  #define YES(T) template <> struct is_character<T> { enum { value = true }; };
  YES(char) YES(wchar_t) YES(char16_t) YES(char32_t)
  #undef YES

  char const * demangle(char const *);
}

std::ostream & operator<<(std::ostream &, wchar_t);
std::ostream & operator<<(std::ostream &, wchar_t const *);
std::ostream & operator<<(std::ostream &, std::wstring const &);

#endif // header guard
