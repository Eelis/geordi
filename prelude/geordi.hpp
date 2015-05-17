#ifndef GEORDI_HPP
#define GEORDI_HPP

#include <cstdio>
#include <cstdlib>
#include <string>
#include <map>
#include <iostream>
#include <sys/utsname.h>

namespace geordi
{
  namespace
  {
    char const parsep[] = "\342\200\251";
      // UTF-8 encoding of PARAGRAPH SEPARATOR (U+2029). See "Output separators" in notes.txt.

    char const compiler_description[] =
      #ifdef __clang__
        "Clang " __clang_version__;
      #else
        "GCC " __VERSION__;
      #endif
  }

  struct error
  {
    error() { std::printf("%serror: ", parsep); }
    ~error() { std::abort(); }
    std::ostream & operator()() const { return std::cout; }
  };

  utsname uname();

  template <typename> struct is_character { enum { value = false }; };
  #define YES(T) template <> struct is_character<T> { enum { value = true }; };
  YES(char) YES(wchar_t) YES(char16_t) YES(char32_t)
  #undef YES

  char const * demangle(char const *);

  std::map<std::string, std::string> depersist();
  void persist(std::string key, std::string value);
}

std::ostream & operator<<(std::ostream &, wchar_t);
std::ostream & operator<<(std::ostream &, wchar_t const *);
std::ostream & operator<<(std::ostream &, std::wstring const &);

#endif // header guard
