
// Todo: Automatic parentheses placement.

#ifndef TYPE_STRINGS_HPP
#define TYPE_STRINGS_HPP

#include <tr1/array>

#include <list>
#include <map>
#include <string>
#include <vector>
#include <set>
#include <utility>
#include <deque>
#include <iostream>
#include <fstream>
#include <sstream>
#include <ios>
#include <cassert>

#include <boost/shared_ptr.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>
#include <boost/utility.hpp>
#include <boost/array.hpp>
#include <boost/bind.hpp>

#include <cxxabi.h>

#include "ScopeGuard.hpp"

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  #include <array>
  #include <tuple>
  #include <unordered_set>
  #include <unordered_map>
  #include "unique_ptr.hpp"
  #include "tlists.hpp"
#endif

template <typename I>
std::string commas_and (I b, I e)
{
  assert(b != e);
  if (e - b == 3) { std::string r = *b++; r += ", " + *b++; return r + ", and " + *b; }
  else if (e - b == 2) return *b + " and " + *boost::next(b);
  else if (e - b == 1) return *b;
  else return *b + ", " + commas_and(boost::next(b), e);
}

// Type strings in ordinary C++ syntax

template <typename>
std::string type ()
{ std::string r (__PRETTY_FUNCTION__); r.resize(r.size() - 1); return r.substr(r.find('=') + 2); }
  // Note: Depends on specific string format used by __PRETTY_FUNCTION__.

// Note: Since type relies on passing the type as a template argument, it will not work for locally defined types.

#define TYPEDEF_TYPE(n) template <> std::string type<std::n> () { return #n; }

TYPEDEF_TYPE(ios)
TYPEDEF_TYPE(fstream)
TYPEDEF_TYPE(ifstream)
TYPEDEF_TYPE(ofstream)
TYPEDEF_TYPE(ostringstream)
TYPEDEF_TYPE(istringstream)
TYPEDEF_TYPE(stringstream)
TYPEDEF_TYPE(streambuf)
TYPEDEF_TYPE(iostream)
TYPEDEF_TYPE(ostream)
TYPEDEF_TYPE(istream)
TYPEDEF_TYPE(string)
TYPEDEF_TYPE(wstring)

#undef TYPEDEF_TYPE

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

  template <typename T> std::string many () { return type_desc<T>(true); }

  template <typename> struct type_desc_t;

  template <typename T> std::string an ()
  { return (type_desc_t<T>::vowel ? "an " : "a ") + type_desc<T>(); }

  template <typename T> std::string an_or_many (bool const plural = false)
  { if (plural) return many<T>(); else return an<T>(); }

  template <typename T> std::string count (size_t const i)
  { return boost::lexical_cast<std::string>(i) + " " + type_desc<T>(i != 1); }

  template <typename T> std::string an_or_count (size_t const i) { return i == 1 ? an<T>() : count<T>(i); }

  struct Vowel { enum { vowel = true }; };
  struct consonant { enum { vowel = false }; };

  template <typename T> struct type_desc_t: consonant
  { static std::string s (bool b) { return pl(type<T>(), b); } };

  template <typename T> std::string returning (bool const plural)
  { return "returning " + an_or_many<T>(plural); }

  template <> std::string returning<void> (bool) { return "returning nothing"; }

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

  // stdlib typedefs

  #define TYPEDEF_TYPE_DESC(n, pl, vow) \
    template <> struct type_desc_t<std::n> \
    { static std::string s (bool b) { return b ? #n pl : #n; } enum { vowel = vow }; };

  TYPEDEF_TYPE_DESC(ios, "es", true)
  TYPEDEF_TYPE_DESC(fstream, "s", false)
  TYPEDEF_TYPE_DESC(ifstream, "s", true)
  TYPEDEF_TYPE_DESC(ofstream, "s", true)
  TYPEDEF_TYPE_DESC(ostringstream, "s", true)
  TYPEDEF_TYPE_DESC(istringstream, "s", true)
  TYPEDEF_TYPE_DESC(stringstream, "s", false)
  TYPEDEF_TYPE_DESC(streambuf, "s", false)
  TYPEDEF_TYPE_DESC(iostream, "s", true)
  TYPEDEF_TYPE_DESC(ostream, "s", true)
  TYPEDEF_TYPE_DESC(istream, "s", true)
  TYPEDEF_TYPE_DESC(string, "s", false)
  TYPEDEF_TYPE_DESC(wstring, "s", false)

  #undef TYPEDEF_TYPE_DESC

  // primitive constructs

  template <> struct type_desc_t<void>
  { static std::string s (bool) { return "void"; } }; // has no plural

  template <typename T> struct type_desc_t<T const>: consonant
  { static std::string s (bool b) { return "constant " + type_desc<T>(b); } };

  template <typename T> struct type_desc_t<T volatile>: consonant
  { static std::string s (bool b) { return "volatile " + type_desc<T>(b); } };

  template <typename T> struct type_desc_t<T const volatile>: consonant
  { static std::string s (bool b) { return "constant volatile " + type_desc<T>(b); } };

  template <typename T> struct type_desc_t<T *>: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to " + an_or_many<T>(b); } };

  template <> struct type_desc_t<void *>: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to anything"; } };

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename T> struct type_desc_t<T &>: Vowel
  { static std::string s (bool b) { return pl("lvalue reference", b) + " to " + an_or_many<T>(b); } };

  template <typename T> struct type_desc_t<T &&>: Vowel
  { static std::string s (bool b) { return pl("rvalue reference", b) + " to " + an_or_many<T>(b); } };

  #else

  template <typename T> struct type_desc_t<T &>: consonant
  { static std::string s (bool b) { return pl("reference", b) + " to " + an_or_many<T>(b); } };

  #endif

  #define ARRAY_SPEC(cv) \
    template <typename T> struct type_desc_t<T cv []>: Vowel \
    { static std::string s (bool b) { return pl("array", b) + " of " + many<T cv>(); } };

  ARRAY_SPEC()
  ARRAY_SPEC(const)
  ARRAY_SPEC(volatile)
  ARRAY_SPEC(const volatile)

  #undef ARRAY_SPEC

  #define ARRAY_SPEC(cv) \
    template <typename T, size_t N> struct type_desc_t<T cv [N]>: Vowel \
    { static std::string s (bool b) { return pl("array", b) + " of " + count<T cv>(N); } };

  ARRAY_SPEC()
  ARRAY_SPEC(const)
  ARRAY_SPEC(volatile)
  ARRAY_SPEC(const volatile)

  #undef ARRAY_SPEC

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename> struct group_list_desc;

  template<size_t N, typename T, typename... U>
  struct group_list_desc<tlists::tlist<tlists::group<N, T>, U...> > {
    static void s (std::vector<std::string> & v) {
      v.push_back(an_or_count<T>(N)); group_list_desc<tlists::tlist<U...> >::s(v);
  } };

  template <> struct group_list_desc<tlists::tlist<> > { static void s (std::vector<std::string> & v) {} };

  template <typename... T>
  struct list_desc: group_list_desc<typename tlists::group_successive<T...>::type> {};

  #endif

  // functions

  template <typename T> struct type_desc_t<T ()>: consonant
  { static std::string s (bool b) { return pl("nullary function", b) + " " + returning<T>(b); } };

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename T, typename... U> struct type_desc_t<T (U...)>: consonant {
    static std::string s (bool b) {
      std::vector<std::string> v; list_desc<U...>::s(v); v.push_back(returning<T>(b));
      return pl("function", b) + " taking " + commas_and(v.begin(), v.end());
  } };

  #else

  template <typename T, typename U> struct type_desc_t<T (U)>: consonant
  { static std::string s (bool b) { return pl("function", b) + " taking " + an_or_many<U>(b) + " and " + returning<T>(b); } };

  template <typename T, typename U, typename V> struct type_desc_t<T (U, V)>: consonant
  { static std::string s (bool b) { return pl("function", b) + " taking " + an<U>() + ", " + an<V>() + ", and " + returning<T>(b); } };

  template <typename T, typename U, typename V, typename W>
  struct type_desc_t<T (U, V, W)>: consonant
  { static std::string s (bool b) { return pl("function", b) + " taking " + an<U>() + ", " + an<V>() + ", " + an<W>() + ", and " + returning<T>(b); } };

  #endif

  // data members

  template <typename T, typename U> struct type_desc_t<T (U:: *)>: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to data " + pl("member", b) + " of class " + type<U>() + " of type " + type_desc<T>(); } };

  // member function pointers

  extern char const // Todo: Will cause linker errors when included in different TUs.
    cv_ [] = " ",
    cv_c [] = " constant ",
    cv_v [] = " volatile ",
    cv_cv [] = " constant volatile ";

  template <typename T, typename U, char const * e> struct type_desc_t_mem0: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "nullary member " + pl("function", b) + " of class " + type<U>() + " " + returning<T>(b); } };

  template <typename T, typename U>
  struct type_desc_t<T (U:: *) ()>: type_desc_t_mem0<T, U, cv_> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) () const>: type_desc_t_mem0<T, U, cv_c> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) () volatile>: type_desc_t_mem0<T, U, cv_v> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) () const volatile>: type_desc_t_mem0<T, U, cv_cv> {};

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename T, typename U, char const * e, typename... P>
  struct type_desc_t_memN: consonant {
    static std::string s (bool b) {
      std::vector<std::string> v; list_desc<P...>::s(v); v.push_back(returning<T>(b));
      return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + commas_and(v.begin(), v.end());
    }
  };

  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P...)>: type_desc_t_memN<T, U, cv_, P...> {};
  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P...) const>: type_desc_t_memN<T, U, cv_c, P...> {};
  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P...) volatile>: type_desc_t_memN<T, U, cv_v, P...> {};
  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P...) const volatile>: type_desc_t_memN<T, U, cv_cv, P...> {};

  #else

  // member function with 1 param

  template <typename T, typename U, typename V, char const * e> struct type_desc_t_mem1: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + an_or_many<V>(b) + " and " + returning<T>(b); } };

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
  struct type_desc_t_mem2: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + an_or_many<V>(b) + ", " + an_or_many<W>(b) + ", and " + returning<T>(b); } };

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
  struct type_desc_t_mem3: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of class " + type<U>() + " taking " + an_or_many<V>(b) + ", " + an_or_many<W>(b) + ", " + an_or_many<X>(b) + ", and " + returning<T>(b); }  };

  template <typename T, typename U, typename V, typename W, typename X>
  struct type_desc_t<T (U:: *) (V, W, X)>: type_desc_t_mem3<T, U, V, W, X, cv_> {};
  template <typename T, typename U, typename V, typename W, typename X>
  struct type_desc_t<T (U:: *) (V, W, X) const>: type_desc_t_mem3<T, U, V, W, X, cv_c> {};
  template <typename T, typename U, typename V, typename W, typename X>
  struct type_desc_t<T (U:: *) (V, W, X) volatile>: type_desc_t_mem3<T, U, V, W, X, cv_v> {};
  template <typename T, typename U, typename V, typename W, typename X>
  struct type_desc_t<T (U:: *) (V, W, X) const volatile>: type_desc_t_mem3<T, U, V, W, X, cv_cv> {};

  #endif

  // Library components:

    // C++03

    // Todo: Add allocator/predicate parameters where appropriate.

    template <typename T, typename A> struct type_desc_t<std::vector<T, A> >: consonant
    { static std::string s (bool b) { return pl("vector", b) + " of " + many<T>(); } };

    template <typename T, typename U> struct type_desc_t<std::pair<T, U> >: consonant
    { static std::string s (bool b) { return pl("pair", b) + " of " + an_or_many<T>(b) + " and " + an_or_many<U>(b); } };

    template <typename T> struct type_desc_t<std::pair<T, T> >: consonant
    { static std::string s (bool b) { return pl("pair", b) + " of " + many<T>(); } };

    template <typename T, typename A> struct type_desc_t<std::set<T, A> >: consonant
    { static std::string s (bool b) { return pl("set", b) + " of " + many<T>(); } };

    template <typename T> struct type_desc_t<std::multiset<T> >: consonant
    { static std::string s (bool b) { return pl("multi-set", b) + " of " + many<T>(); } };

    template <typename T, typename U> struct type_desc_t<std::map<T, U> >: consonant
    { static std::string s (bool b) { return pl("map", b) + " from " + many<T>() + " to " + many<U>(); } };

    template <typename T, typename U> struct type_desc_t<std::multimap<T, U> >: consonant
    { static std::string s (bool b) { return pl("multi-map", b) + " from " + many<T>() + " to " + many<U>(); } };

    template <typename T> struct type_desc_t<std::list<T> >: consonant
    { static std::string s (bool b) { return pl("list", b) + " of " + many<T>(); } };

    template <typename T> struct type_desc_t<std::deque<T> >: consonant
    { static std::string s (bool b) { return pl("double-ended queue", b) + " of " + many<T>(); } };

    // Boost

    template <typename T, size_t N> struct type_desc_t<boost::array<T, N> >: Vowel
    { static std::string s (bool b) { return pl("array", b) + " of " +  count<T>(N); } };

    template <typename T> struct type_desc_t<boost::optional<T> >: Vowel
    { static std::string s (bool b) { return "optional " + type_desc<T>(b); } };

    template <typename T> struct type_desc_t<boost::shared_ptr<T> >: consonant
    { static std::string s (bool b) { return "shared " + type_desc<T*>(b); } };

    // TR1

    template <typename T, size_t N> struct type_desc_t<std::tr1::array<T, N> >: Vowel
    { static std::string s (bool b) { return pl("array", b) + " of " + count<T>(N); } };

    // C++0x

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
    
    template <typename T, size_t N> struct type_desc_t<std::array<T, N> >: Vowel
    { static std::string s (bool b) { return pl("array", b) + " of " + count<T>(N); } };
    
    template<size_t N> struct num_vowel { enum { vowel = N == 8 || N == 18 }; };
    
    template <typename... T> struct type_desc_t<std::tuple<T...>> {
      static std::string s (bool b) {
        std::vector<std::string> v; list_desc<T...>::s(v);
        return boost::lexical_cast<std::string>(sizeof...(T)) + "-" + pl("tuple", b) + " of " + commas_and(v.begin(), v.end());
      }
      enum { vowel = num_vowel<sizeof...(T)>::vowel };
    };
    
    template <typename T> struct type_desc_t<unique_ptr<T>>: consonant
    { static std::string s (bool b) { return "unique " + type_desc<T*>(b); } };

    template <typename T> struct type_desc_t<std::reference_wrapper<T>>: consonant
    { static std::string s (bool b) { return "reference wrapped " + type_desc_t<T>::s(b); } };
    
    template <typename T, typename U, typename H, typename P, typename A> struct type_desc_t<std::unordered_map<T, U, H, P, A>>: consonant
    { static std::string s (bool b) { return pl("unordered map", b) + " from " + many<T>() + " to " + many<U>(); } };
    
    template <typename T, typename U, typename H, typename P, typename A> struct type_desc_t<std::unordered_multimap<T, U, H, P, A>>: consonant
    { static std::string s (bool b) { return pl("unordered multi-map", b) + " from " + many<T>() + " to " + many<U>(); } };
  
    template <typename T, typename H, typename P, typename A>
    struct type_desc_t<std::unordered_set<T, H, P, A>>: consonant
    { static std::string s (bool b) { return pl("unordered set", b) + " of " + many<T>(); } };

    template <typename T, typename H, typename P, typename A> struct type_desc_t<std::unordered_multiset<T, H, P, A>>: consonant
    { static std::string s (bool b) { return pl("unordered multi-set", b) + " of " + many<T>(); } };

    #endif // C++0x

} // textual_type_descriptions

template <typename T> std::string type_desc (bool const plural)
{ return textual_type_descriptions::type_desc_t<T>::s(plural); }

// The following macros are variadic so that things like TYPE(pair<int, bool>) work (note the comma).
// The E* variants are necessary because decltype does not allow type arguments, and eventually we will want to remove the __typeof__ implementation.

#define TYPEID_NAME(...) (cxa_demangle(typeid( __VA_ARGS__ ).name()))
#define TYPE(...) (type< __VA_ARGS__ >())
#define TYPE_DESC(...) (type_desc< __VA_ARGS__ >())

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  #define ETYPE(...) (type<decltype( __VA_ARGS__ )>())
  #define ETYPE_DESC(...) (type_desc<decltype( __VA_ARGS__ )>())
#else
  #define ETYPE(...) (type<__typeof__( __VA_ARGS__ )>())
  #define ETYPE_DESC(...) (type_desc<__typeof__( __VA_ARGS__ )>())
#endif

#endif // header guard
