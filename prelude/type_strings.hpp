
// Todo: Automatic parentheses placement.

#ifndef TYPE_STRINGS_HPP
#define TYPE_STRINGS_HPP

#include "lvalue_rvalue.hpp"

#include <tr1/array>

#include <list>
#include <map>
#include <string>
#include <vector>
#include <stack>
#include <queue>
#include <set>
#include <utility>
#include <deque>
#include <iostream>
#include <fstream>
#include <sstream>
#include <ios>
#include <cassert>

#include <boost/shared_ptr.hpp>
#include <boost/static_assert.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>
#include <boost/utility.hpp>
#include <boost/array.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/is_union.hpp>

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  #include <array>
  #include <tuple>
  #include <unordered_set>
  #include <unordered_map>
  #include <type_traits>
  #include "tlists.hpp"
#endif

namespace type_strings_detail {

struct type_info: std::type_info { char const * name() const; static type_info const & from_std(std::type_info const&); };

BOOST_STATIC_ASSERT(sizeof(type_info) == sizeof(std::type_info));

template<typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator<<(std::basic_ostream<Ch, Tr> & o, type_info const & t)
{ return o << t.name(); }

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

template <typename> struct identity {};

template <typename T> std::string type() {
  std::string r(type_info::from_std(typeid(identity<T>)).name()
    + sizeof("type_strings_detail::identity<") - 1);
  r.resize(r.size() - 1);
  return r;
}

// Note: Since type relies on passing the type as a template argument, it will not work for locally defined types (in C++03).

#define TYPEDEF_TYPE(n) template <> inline std::string type<std::n> () { return #n; }

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

// Verbose type description strings

template <typename> std::string type_desc (bool plural = false);

namespace textual_type_descriptions
{
  inline std::string pl (std::string const & s, bool const b) { return s + (b ? "s" : ""); }

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

  template <> inline std::string returning<void> (bool) { return "returning nothing"; }

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

  #define TYPE_STRINGS_EMPTY

  #define ARRAY_SPEC(cv) \
    template <typename T> struct type_desc_t<T cv []>: Vowel \
    { static std::string s (bool b) { return pl("array", b) + " of " + many<T cv>(); } };

  ARRAY_SPEC(TYPE_STRINGS_EMPTY)
  ARRAY_SPEC(const)
  ARRAY_SPEC(volatile)
  ARRAY_SPEC(const volatile)

  #undef ARRAY_SPEC

  #define ARRAY_SPEC(cv) \
    template <typename T, size_t N> struct type_desc_t<T cv [N]>: Vowel \
    { static std::string s (bool b) { return pl("array", b) + " of " + count<T cv>(N); } };

  ARRAY_SPEC(TYPE_STRINGS_EMPTY)
  ARRAY_SPEC(const)
  ARRAY_SPEC(volatile)
  ARRAY_SPEC(const volatile)

  #undef ARRAY_SPEC

  #undef TYPE_STRINGS_EMPTY

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

  template <typename T> struct type_desc_t<T (...)>: consonant
  { static std::string s (bool b) { return pl("variadic function", b) + " " + returning<T>(b); } };

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename T, typename... U> struct type_desc_t<T (U...)>: consonant {
    static std::string s (bool b) {
      std::vector<std::string> v; list_desc<U...>::s(v); v.push_back(returning<T>(b));
      return pl("function", b) + " taking " + commas_and(v.begin(), v.end());
  } };

  template <typename T, typename... U> struct type_desc_t<T (U..., ...)>: consonant {
    static std::string s (bool b) {
      std::vector<std::string> v; list_desc<U...>::s(v); v.push_back(returning<T>(b));
      return pl("variadic function", b) + " taking at least " + commas_and(v.begin(), v.end());
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

  template<typename T> std::string class_key()
  { return boost::is_union<T>::value ? "union" : "class";  }

  // data members

  template <typename T, typename U> struct type_desc_t<T (U:: *)>: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to data " + pl("member", b) + " of " + class_key<U>() + " " + type<U>() + " of type " + type_desc<T>(); } };

  // member function pointers

  namespace // Prevents linker errors when included in different TUs.
  {
    extern char const
      cv_ [] = " ",
      cv_c [] = " constant ",
      cv_v [] = " volatile ",
      cv_cv [] = " constant volatile ";
  }

  template <typename T, typename U, char const * e> struct type_desc_t_mem0: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "nullary member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " " + returning<T>(b); } };

  template <typename T, typename U>
  struct type_desc_t<T (U:: *) ()>: type_desc_t_mem0<T, U, cv_> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) () const>: type_desc_t_mem0<T, U, cv_c> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) () volatile>: type_desc_t_mem0<T, U, cv_v> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) () const volatile>: type_desc_t_mem0<T, U, cv_cv> {};

  template <typename T, typename U, char const * e> struct type_desc_t_mem_vari: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "variadic member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " " + returning<T>(b); } };

  template <typename T, typename U>
  struct type_desc_t<T (U:: *) (...)>: type_desc_t_mem_vari<T, U, cv_> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) (...) const>: type_desc_t_mem_vari<T, U, cv_c> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) (...) volatile>: type_desc_t_mem_vari<T, U, cv_v> {};
  template <typename T, typename U>
  struct type_desc_t<T (U:: *) (...) const volatile>: type_desc_t_mem_vari<T, U, cv_cv> {};

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename T, typename U, char const * e, typename... P>
  struct type_desc_t_memN: consonant {
    static std::string s (bool b) {
      std::vector<std::string> v; list_desc<P...>::s(v); v.push_back(returning<T>(b));
      return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " taking " + commas_and(v.begin(), v.end());
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

  template <typename T, typename U, char const * e, typename... P>
  struct type_desc_t_memN_vari: consonant {
    static std::string s (bool b) {
      std::vector<std::string> v; list_desc<P...>::s(v); v.push_back(returning<T>(b));
      return pl("pointer", b) + " to" + e + "variadic member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " taking at least " + commas_and(v.begin(), v.end());
    }
  };

  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P..., ...)>: type_desc_t_memN_vari<T, U, cv_, P...> {};
  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P..., ...) const>: type_desc_t_memN_vari<T, U, cv_c, P...> {};
  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P..., ...) volatile>: type_desc_t_memN_vari<T, U, cv_v, P...> {};
  template <typename T, typename U, typename... P>
  struct type_desc_t<T (U:: *) (P..., ...) const volatile>: type_desc_t_memN_vari<T, U, cv_cv, P...> {};

  #else

  // member function with 1 param

  template <typename T, typename U, typename V, char const * e> struct type_desc_t_mem1: consonant
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " taking " + an_or_many<V>(b) + " and " + returning<T>(b); } };

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
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " taking " + an_or_many<V>(b) + ", " + an_or_many<W>(b) + ", and " + returning<T>(b); } };

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
  { static std::string s (bool b) { return pl("pointer", b) + " to" + e + "member " + pl("function", b) + " of " + class_key<U>() + " " + type<U>() + " taking " + an_or_many<V>(b) + ", " + an_or_many<W>(b) + ", " + an_or_many<X>(b) + ", and " + returning<T>(b); }  };

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

      /* Like most containers, std::map has a couple of parameters with default arguments. We only want to return a nice verbose string for specializations which use the default arguments. Naively, we might expect that to get this effect, we should simply partially specialize for type_desc_t<map<T, U> >. However, that partial specialization will not match type_desc_t<map<int const, int> >, because this is really:

        type_desc_t<map<int const, int, less<int const>, ...> > (note the double-const collapse),

      while the partial specialization is really for:

        type_desc_t<map<T, U, less<T const>, ...> >,

      which doesn't match! This is why we use boost::is_same below. As far as I can see map is the only container affected because it has this double-const collapse in its default arguments. */

    template <typename T, typename U, typename V, typename W>
    struct type_desc_t<std::map<T, U, V, W> >: consonant
    { static std::string s (bool const b) {
        if (boost::is_same<std::map<T, U>, std::map<T, U, V, W> >::value)
          return pl("map", b) + " from " + many<T>() + " to " + many<U>();
        else return pl(type<std::map<T, U, V, W> >(), b);
    } };

    template <typename T, typename U> struct type_desc_t<std::multimap<T, U> >: consonant
    { static std::string s (bool b) { return pl("multi-map", b) + " from " + many<T>() + " to " + many<U>(); } };

    template <typename T> struct type_desc_t<std::list<T> >: consonant
    { static std::string s (bool b) { return pl("list", b) + " of " + many<T>(); } };

    template <typename T> struct type_desc_t<std::deque<T> >: consonant
    { static std::string s (bool b) { return pl("double-ended queue", b) + " of " + many<T>(); } };

    template <typename T> struct type_desc_t<std::queue<T> >: consonant
    { static std::string s (bool b) { return pl("queue", b) + " of " + many<T>(); } };

    template <typename T> struct type_desc_t<std::priority_queue<T> >: consonant
    { static std::string s (bool b) { return pl("priority queue", b) + " of " + many<T>(); } };

    template <typename T> struct type_desc_t<std::stack<T> >: consonant
    { static std::string s (bool b) { return pl("stack", b) + " of " + many<T>(); } };

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

    #ifdef _UNIQUE_PTR_H
    template <typename T> struct type_desc_t<std::unique_ptr<T>>: consonant
    { static std::string s (bool b) { return "unique " + type_desc<T*>(b); } };
    #endif

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

// The following macros are variadic so that things like TYPE(pair<int, bool>) and ETYPE(pair<bool, int>(true, 3)) work (note the commas).
// The E* variants are necessary because decltype does not allow type arguments, and eventually we will want to remove the __typeof__ implementation.

#define TYPE(...) (::type_strings_detail::type<__VA_ARGS__>())
#define TYPE_DESC(...) (::type_strings_detail::type_desc<__VA_ARGS__>())

/* Regarding ETYPE(_DESC) semantics:

  We want ETYPE(_DESC) to return (a string representation of) "the type of the expression". It is not immediately clear what this means. For example, is there a difference between an expression having type int and one having type int&&? And is decltype not exactly what we want? Answers come from chapter 5, paragraphs 5 and 6 (in n2521), which state respectively:

    If an expression initially has the type "lvalue reference to T", the type is adjusted to T prior to any further analysis, the expression designates the object or function denoted by the lvalue reference, and the expression is an lvalue.

    If an expression initially has the type "rvalue reference to T", the type is adjusted to T prior to any further analysis, and the expression designates the object or function denoted by the rvalue reference. If the expression is the result of calling a function, whether implicitly or explicitly, it is an rvalue; otherwise, it is an lvalue.

  Here, we immediately see two important points, on which we will base our ETYPE(_DESC) semantics. First, for all intents and purposes (in particular reference binding), expressions never have reference types. Second, expressions are instead classified as lvalues or rvalues.

  We currently have ETYPE(_DESC) indicate lvalue/rvalue-ness by explicit "lvalue "/"rvalue " prefixes. A nicer approach would be to show lvalue Ts as T&, and show rvalue Ts as plain T. However, there is no room in this rather terse "encoding" to signal the ambiguity that arises when our C++03 lvalue/rvalue tests cannot determine the classification (see lvalue_rvalue.hpp), whereas with the explicit prefix approach we can simply omit any prefix in such cases. Hence, until C++0x mode (in which there is no ambiguity because our tests are conclusive) becomes the default, we stick with explicit prefixes.

  Note that our first question regarding the differences between an expression of type int and one of type int&& was ambiguous, because the answer depends on (1) whether the expression of type int denotes and lvalue or an rvalue, and (2) whether the expression of type int&& is the result of calling a function.

  Furthermore, It is now also easy to see that decltype is in fact /not/ exactly what we want, because it does not include sufficient lvalue/rvalue information: decltype(3) and decltype(i) both denote the type int when i has been declared as int i;, while the former is an rvalue and the latter an lvalue.

*/

#ifdef __GXX_EXPERIMENTAL_CXX0X__

  template <typename T> std::string etype()
  { return type<typename std::remove_reference<T>::type>(); }

  template <typename T> std::string etype_desc()
  { return type_desc<typename std::remove_reference<T>::type>(); }

  // We can't do the remove_reference in the ETYPE(_DESC) macros because there would be no correct choice for whether or not to use typename, since whether std::remove_reference<T>::type is a dependent type depends on the context. Hence the separate etype(_desc) functions.

  #define ETYPE(...) \
    ((IS_LVALUE(__VA_ARGS__) ? "lvalue " : "rvalue ") + \
    ::type_strings_detail::etype<decltype(void(), (__VA_ARGS__))>())

  #define ETYPE_DESC(...) \
    ((IS_LVALUE(__VA_ARGS__) ? "lvalue " : "rvalue ") + \
    ::type_strings_detail::etype_desc<decltype(void(), (__VA_ARGS__))>())

#else

  #define TYPE_STRINGS_DETAIL_LVALUE_RVALUE_TAG(...) \
    (MAY_BE_LVALUE(__VA_ARGS__) \
      ? (MAY_BE_RVALUE(__VA_ARGS__) ? "" : "lvalue ") \
      : "rvalue ")

  #define ETYPE(...) \
    (TYPE_STRINGS_DETAIL_LVALUE_RVALUE_TAG(__VA_ARGS__) + \
    ::type_strings_detail::type<__typeof__((__VA_ARGS__))>())

  #define ETYPE_DESC(...) \
    (TYPE_STRINGS_DETAIL_LVALUE_RVALUE_TAG(__VA_ARGS__) + \
    ::type_strings_detail::type_desc<__typeof__((__VA_ARGS__))>())

  // The double parentheses around __VA_ARGS__ in the __typeof__ expressions are a workaround for GCC bug 11701.

#endif

} // type_strings_detail

#endif // header guard
