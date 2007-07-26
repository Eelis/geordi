
/* todo:
  automatic parentheses and/or comma placement
  other containers, variants, tuples, more parameters, more cv-qualifiers
  what about throw specifications? -- can't seem to be detect them
  aggregating successive parameters of identical type. (eg "taking two long unsigned ints" instead of "taking long a unsigned integer and a long unsigned integer" (probably very tricky)
  consider generating some of the (member) function specializations with Boost.PP
  it's been suggested that "function which returns ..." is nicer than "function returning ..."

  weird: struct S { void f () {} }; typedef __typeof__(S().*(&S::f)) U;  int main () { cout << cxa_demangle(typeid(U).name()) << ", " << TYPE_DESC(U); } // bug in gcc?

  also weird: TYPE_DESC(const void (*) ());  (comeau warns that it is meaningless, but rejects the conversion in  void const f () {} void g () { typedef void (* P) (); P p = &f; },  thus contradicting itself)

*/

#ifndef TYPE_STRINGS_HPP
#define TYPE_STRINGS_HPP

#include <tr1/array>
#include <list>
#include <map>
#include <boost/shared_ptr.hpp>
#include <string>
#include <vector>
#include <set>
#include <utility>
#include <deque>
#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>
#include <boost/array.hpp>
#include <boost/bind.hpp>
#include <cxxabi.h>
#include "ScopeGuard.hpp"

// Type strings in ordinary C++ syntax

  template <typename>
  std::string type ()
  { std::string r (__PRETTY_FUNCTION__); r.resize(r.size() - 1); return r.substr(r.find('=') + 2); }
    // Note: Depends on specific string format used by __PRETTY_FUNCTION__.

  // Note: Since type relies on passing the type as a template argument, it will not work for locally defined types.

  template <> std::string type<std::string> () { return "string"; }
  template <> std::string type<std::wstring> () { return "wstring"; }

// Demangling toys

  std::string cxa_demangle (const char * const name)
  {
    int st;
    char * const p = abi::__cxa_demangle(name, 0, 0, &st);

    switch (st)
    {
      case 0: { ScopeGuard const freeer (boost::bind(&std::free, p)); return p; }
      case -1: throw std::runtime_error("A memory allocation failure occurred.");
      case -2: throw std::runtime_error("Not a valid name under the GCC C++ ABI mangling rules.");
      case -3: throw std::runtime_error("One of the arguments is invalid.");
      default: assert(!"unexpected demangle status");
    }
  }

  template <typename T> std::string typeid_name (T const & t) { return cxa_demangle(typeid(t).name()); }
  template <typename T> std::string typeid_name () { return cxa_demangle(typeid(T).name()); }

// Verbose type description strings

  template <typename> std::string type_desc (bool plural = false);

  namespace textual_type_descriptions
  {
    std::string pl (std::string const & s, bool const b) { return s + (b ? "s" : ""); }

    template <typename> struct type_desc_t;

    template <typename T> std::string an (bool const plural = false, char const * = 0)
    {
      if (plural) return type_desc<T>(true);
      else return (type_desc_t<T>::vowel ? "an " : "a ") + type_desc<T>(false);
    }

    template <> std::string an<void> (bool, char const * const s) { assert(s); return s; }

    template <typename T> struct type_desc_t
    { static std::string s (bool b) { return pl(type<T>(), b); } enum { vowel = false }; };

    // built-in types

    #define DEF_BUILTIN_SPEC(type, str, vow) \
      template <> struct type_desc_t<type> \
      { static std::string s (bool plural) { return pl(str, plural); } enum { vowel = vow }; };

    DEF_BUILTIN_SPEC(bool, "boolean", false)
    DEF_BUILTIN_SPEC(char, "character", false)
    DEF_BUILTIN_SPEC(signed char, "signed character", false)
    DEF_BUILTIN_SPEC(unsigned char, "unsigned character", true)
    DEF_BUILTIN_SPEC(wchar_t, "wide character", false)
    DEF_BUILTIN_SPEC(int, "integer", true)
    DEF_BUILTIN_SPEC(long int, "long integer", false)
    DEF_BUILTIN_SPEC(unsigned int, "unsigned integer", true)
    DEF_BUILTIN_SPEC(long unsigned int, "long unsigned integer", false)

    #undef DEF_BUILTIN_SPEC

    // primitive constructs

    template <> struct type_desc_t<void>
    { static std::string s (bool) { return "void"; } }; // has no plural

    template <typename T> struct type_desc_t<T const>
    { static std::string s (bool b) { return "constant " + type_desc<T>(b); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<T volatile>
    { static std::string s (bool b) { return "volatile " + type_desc<T>(b); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<T const volatile>
    { static std::string s (bool b) { return "constant volatile " + type_desc<T>(b); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<T *>
    { static std::string s (bool b) { return pl("pointer", b) + " to " + an<T>(b, "anything"); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<T &>
    { static std::string s (bool b) { return pl("reference", b) + " to " + an<T>(b); } enum { vowel = false }; };

    template <typename T, size_t N> struct type_desc_t<T [N]>
    { static std::string s (bool b) { return pl("array", b) + " of " +  boost::lexical_cast<std::string>(N) + " " + type_desc<T>(N != 1); } enum { vowel = true }; };

    template <typename T> struct type_desc_t<T []>
    { static std::string s (bool b) { return pl("array", b) + " of " +  type_desc<T>(true); } enum { vowel = true }; };

    template <typename T> struct type_desc_t<T const []>
    { static std::string s (bool b) { return pl("array", b) + " of constant " +  type_desc<T>(true); } enum { vowel = true }; };

    template <typename T, size_t N> struct type_desc_t<T const [N]>
    { static std::string s (bool b) { return pl("array", b) + " of " +  boost::lexical_cast<std::string>(N) + " constant " + type_desc<T>(N != 1); } enum { vowel = true }; }; // disambiguates

    // function pointers

    extern char const
      cv_ [] = " ",
      cv_c [] = " constant ",
      cv_v [] = " volatile ",
      cv_cv [] = " constant volatile ";

    template <typename T> struct type_desc_t<T ()>
    { static std::string s (bool b) { return pl("function", b) + " taking no arguments and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U> struct type_desc_t<T (U)>
    { static std::string s (bool b) { return pl("function", b) + " taking " + an<U>(b) + " and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U, typename V> struct type_desc_t<T (U, V)>
    { static std::string s (bool b) { return pl("function", b) + " taking " + an<U>(false) + " and " + textual_type_descriptions::an<V>(false) + " and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U, typename V, typename W> struct type_desc_t<T (U, V, W)>
    { static std::string s (bool b) { return pl("function", b) + " taking " + an<U>(false) + " and " + an<V>(false) + " and " + an<W>(false) + " and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    // data member

    template <typename T, typename U> struct type_desc_t<T (U:: *)>
    { static std::string s (bool b) { return pl("pointer", b) + " to data " + pl("member", b) + " of class " + type<U>() + " of type " + type_desc<T>(); } enum { vowel = false }; };

    // member function with 0 params

    template <typename T, typename U, char const * e> struct type_desc_t_mem0
    { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking no arguments returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U>
    struct type_desc_t<T (U:: *) ()>: type_desc_t_mem0<T, U, cv_> {};
    template <typename T, typename U>
    struct type_desc_t<T (U:: *) () const>: type_desc_t_mem0<T, U, cv_c> {};
    template <typename T, typename U>
    struct type_desc_t<T (U:: *) () volatile>: type_desc_t_mem0<T, U, cv_v> {};
    template <typename T, typename U>
    struct type_desc_t<T (U:: *) () const volatile>: type_desc_t_mem0<T, U, cv_cv> {};

    // member function with 1 param

    template <typename T, typename U, typename V, char const * e> struct type_desc_t_mem1
    { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + an<V>(b) + " and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U, typename V>
    struct type_desc_t<T (U:: *) (V)>: type_desc_t_mem1<T, U, V, cv_> {};
    template <typename T, typename U, typename V>
    struct type_desc_t<T (U:: *) (V) const>: type_desc_t_mem1<T, U, V, cv_c> {};
    template <typename T, typename U, typename V>
    struct type_desc_t<T (U:: *) (V) volatile>: type_desc_t_mem1<T, U, V, cv_v> {};
    template <typename T, typename U, typename V>
    struct type_desc_t<T (U:: *) (V) const volatile>: type_desc_t_mem1<T, U, V, cv_cv> {};

    // member function with 2 params

    template <typename T, typename U, typename V, typename W, char const * e>
    struct type_desc_t_mem2
    { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + an<V>(b) + " and " + an<W>(b) + " and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U, typename V, typename W>
    struct type_desc_t<T (U:: *) (V, W)>: type_desc_t_mem2<T, U, V, W, cv_> {};
    template <typename T, typename U, typename V, typename W>
    struct type_desc_t<T (U:: *) (V, W) const>: type_desc_t_mem2<T, U, V, W, cv_c> {};
    template <typename T, typename U, typename V, typename W>
    struct type_desc_t<T (U:: *) (V, W) volatile>: type_desc_t_mem2<T, U, V, W, cv_v> {};
    template <typename T, typename U, typename V, typename W>
    struct type_desc_t<T (U:: *) (V, W) const volatile>: type_desc_t_mem2<T, U, V, W, cv_cv> {};

    // member function with 3 params

    template <typename T, typename U, typename V, typename W, typename X, char const * e>
    struct type_desc_t_mem3
    { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + an<V>(b) + ", " + an<W>(b) + ", " + an<X>(b) + ", and returning " + an<T>(b, "nothing"); } enum { vowel = false }; };

    template <typename T, typename U, typename V, typename W, typename X>
    struct type_desc_t<T (U:: *) (V, W, X)>: type_desc_t_mem3<T, U, V, W, X, cv_> {};
    template <typename T, typename U, typename V, typename W, typename X>
    struct type_desc_t<T (U:: *) (V, W, X) const>: type_desc_t_mem3<T, U, V, W, X, cv_c> {};
    template <typename T, typename U, typename V, typename W, typename X>
    struct type_desc_t<T (U:: *) (V, W, X) volatile>: type_desc_t_mem3<T, U, V, W, X, cv_v> {};
    template <typename T, typename U, typename V, typename W, typename X>
    struct type_desc_t<T (U:: *) (V, W, X) const volatile>: type_desc_t_mem3<T, U, V, W, X, cv_cv> {};

    // boost utilities

    template <typename T, size_t N> struct type_desc_t<boost::array<T, N> >
    { static std::string s (bool b) { return pl("array", b) + " of " +  boost::lexical_cast<std::string>(N) + " " + type_desc<T>(N != 1); } enum { vowel = true }; };

    template <typename T> struct type_desc_t<boost::optional<T> >
    { static std::string s (bool b) { return "optional " + type_desc<T>(b); } enum { vowel = true }; };

    template <typename T> struct type_desc_t<boost::shared_ptr<T> >
    { static std::string s (bool b) { return pl("shared pointer", b) + " to " + an<T>(b, "anything"); } enum { vowel = false }; };

    // Standard utilities

    // Todo: Add allocator/predicate parameters where appropriate.

    template <typename T, typename A> struct type_desc_t<std::vector<T, A> >
    { static std::string s (bool b) { return pl("vector", b) + " of " + type_desc<T>(true); } enum { vowel = false }; };

    template <typename T, typename U> struct type_desc_t<std::pair<T, U> >
    { static std::string s (bool b) { return pl("pair", b) + " of " + an<T>(b) + " and " + an<U>(b); } enum { vowel = false }; };

    template <typename T, typename A> struct type_desc_t<std::set<T, A> >
    { static std::string s (bool b) { return pl("set", b) + " of " + type_desc<T>(true); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<std::multiset<T> >
    { static std::string s (bool b) { return pl("multi-set", b) + " of " + type_desc<T>(true); } enum { vowel = false }; };

    template <typename T, typename U> struct type_desc_t<std::map<T, U> >
    { static std::string s (bool b) { return pl("map", b) + " from " + type_desc<T>(true) + " to " + type_desc<U>(true); } enum { vowel = false }; };

    template <typename T, typename U> struct type_desc_t<std::multimap<T, U> >
    { static std::string s (bool b) { return pl("multi-map", b) + " from " + type_desc<T>(true) + " to " + type_desc<U>(true); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<std::list<T> >
    { static std::string s (bool b) { return pl("list", b) + " of " + type_desc<T>(true); } enum { vowel = false }; };

    template <typename T> struct type_desc_t<std::deque<T> >
    { static std::string s (bool b) { return pl("double-ended queue", b) + " of " + type_desc<T>(true); } enum { vowel = false }; };

    // TR1

    template <typename T, size_t N> struct type_desc_t<std::tr1::array<T, N> >
    { static std::string s (bool b) { return pl("array", b) + " of " +  boost::lexical_cast<std::string>(N) + " " + type_desc<T>(N != 1); } enum { vowel = true }; };
  }

  template <typename T> std::string type_desc (bool const plural)
  { return textual_type_descriptions::type_desc_t<T>::s(plural); }

// Macros (variadic so that things like TYPE(pair<int, bool>) work (note the comma)).

  #define TYPE(...) (type<__typeof__( __VA_ARGS__ )>())
  #define TYPEID_NAME(...) (cxa_demangle(typeid( __VA_ARGS__ ).name()))
  #define TYPE_DESC(...) (type_desc<__typeof__( __VA_ARGS__ )>())

#endif // header guard

#ifdef TYPE_STRINGS_TEST

#include <iostream>

const double f (int) { return 2; }

void g () throw () {}

struct S
{
  int x;
  void f () const volatile {}
};

struct dr{ void wtf(int, int, double) const volatile {} };

int main ()
{
  std::cout
    << TYPE_DESC(&dr::wtf) << '\n'
    << TYPE_DESC(std::pair<int, char>) << '\n'
    << TYPE_DESC(std::vector<std::pair<int &, boost::optional<wchar_t> > >) << '\n'
    << TYPE_DESC(volatile int(S::* [3])(std::set<char>) const) << '\n'
    << TYPE_DESC(std::vector<std::vector<int> *>) << '\n'
    << TYPE_DESC(&g) << '\n'
    << TYPE_DESC(int(&)[10]) << '\n'
    << TYPE_DESC(double[1]) << '\n'
    << TYPE_DESC(float) << '\n'
    << TYPE_DESC(int(S::* [3])(std::set<char>)) << '\n'
    << TYPE_DESC(&std::system) << '\n'
    << TYPE_DESC(&std::string::substr) << '\n'
    << TYPE_DESC(&std::list<int>::size) << '\n'
    << TYPE_DESC(void (* [2]) (int, int)) << '\n'
    << TYPE_DESC(void *) << '\n'
    << TYPE_DESC(&f) << '\n'
    << TYPE_DESC(int (&(*)())[5]) << '\n'
    << TYPE_DESC(&std::list<int>::size) << '\n'
    << TYPE_DESC(void (*[1]) (int, int)) << '\n';
}

#endif // tests
