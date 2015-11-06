// Todo: Automatic parentheses placement.

#ifndef TYPE_STRINGS_HPP
#define TYPE_STRINGS_HPP

#include "lvalue_rvalue.hpp"

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
#include <memory>

#include <array>
#include <tuple>
#include <unordered_set>
#include <unordered_map>
#include <type_traits>

#if __cplusplus >= 201500
#include <experimental/type_traits>
#endif

#include "tlists.hpp"

namespace type_strings_detail {

using std::basic_ostream;

template <typename I>
std::string commas_and (I b, I e)
{
  assert(b != e);
  if (e - b == 3) { std::string r = *b++; r += ", " + *b++; return r + ", and " + *b; }
  else if (e - b == 2) return *b + " and " + *std::next(b);
  else if (e - b == 1) return *b;
  else return *b + ", " + commas_and(std::next(b), e);
}

// Type strings in ordinary C++ syntax

template <typename> struct identity {};

template <typename T> std::string type() {
  std::string r(typeid(identity<T>).name() + sizeof("type_strings_detail::identity<") - 1);
  r.pop_back();
  return r;
}

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
  { return std::to_string(i) + " " + type_desc<T>(i != 1); }

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

  template <typename T> struct type_desc_t<T &>: Vowel
  { static std::string s (bool b) { return pl("lvalue reference", b) + " to " + an_or_many<T>(b); } };

  template <typename T> struct type_desc_t<T &&>: Vowel
  { static std::string s (bool b) { return pl("rvalue reference", b) + " to " + an_or_many<T>(b); } };

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

  template <typename> struct group_list_desc;

  template<size_t N, typename T, typename... U>
  struct group_list_desc<tlists::tlist<tlists::group<N, T>, U...> > {
    static void s (std::vector<std::string> & v) {
      v.push_back(an_or_count<T>(N)); group_list_desc<tlists::tlist<U...> >::s(v);
  } };

  template <> struct group_list_desc<tlists::tlist<> > { static void s (std::vector<std::string> &) {} };

  template <typename... T>
  struct list_desc: group_list_desc<typename tlists::group_successive<T...>::type> {};

  // functions

  template <typename T> struct type_desc_t<T ()>: consonant
  { static std::string s (bool b) { return pl("nullary function", b) + " " + returning<T>(b); } };

  template <typename T> struct type_desc_t<T (...)>: consonant
  { static std::string s (bool b) { return pl("variadic function", b) + " " + returning<T>(b); } };

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

  template<typename T> std::string class_key()
  { return std::is_union<T>::value ? "union" : "class";  }

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

      which doesn't match! This is why we use std::is_same below. As far as I can see map is the only container affected because it has this double-const collapse in its default arguments. */

    template <typename T, typename U, typename V, typename W>
    struct type_desc_t<std::map<T, U, V, W> >: consonant
    { static std::string s (bool const b) {
        if (std::is_same<std::map<T, U>, std::map<T, U, V, W> >::value)
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

    // C++0x

    template <typename T> struct type_desc_t<std::shared_ptr<T> >: consonant
    { static std::string s (bool b) { return "shared " + type_desc<T*>(b); } };

    template <typename T, size_t N> struct type_desc_t<std::array<T, N> >: Vowel
    { static std::string s (bool b) { return pl("array", b) + " of " + count<T>(N); } };

    template<size_t N> struct num_vowel { enum { vowel = N == 8 || N == 18 }; };

    template <typename... T> struct type_desc_t<std::tuple<T...>> {
      static std::string s (bool b) {
        std::vector<std::string> v; list_desc<T...>::s(v);
        return std::to_string(sizeof...(T)) + "-" + pl("tuple", b) + " of " + commas_and(v.begin(), v.end());
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

} // textual_type_descriptions

enum expr_cat { lvalue, xvalue, prvalue };

char const * to_string(expr_cat c)
{
  switch (c)
  {
    case lvalue: return "lvalue";
    case xvalue: return "xvalue";
    case prvalue: return "prvalue";
    default: return "?";
  }
}

template <typename Ch, typename Tr>
basic_ostream<Ch, Tr> & operator<<(basic_ostream<Ch, Tr> & o, expr_cat c)
{
  return o << to_string(c);
}

template<typename T>
constexpr expr_cat expression_category()
{
  if (std::is_lvalue_reference<T>::value) return lvalue;
  if (std::is_rvalue_reference<T>::value) return xvalue;
  return prvalue;
}

template <typename T> std::string type_desc (bool const plural)
{ return textual_type_descriptions::type_desc_t<T>::s(plural); }

template <typename> struct type_desc_tag {};
template <typename> struct type_tag {};

template <typename T> struct etype_tag
{
  static constexpr expr_cat category = expression_category<T>();
};

template <typename> struct etype_desc_tag {};

struct adl_hint {};

template <typename Ch, typename Tr, typename T>
basic_ostream<Ch, Tr> & operator<<(basic_ostream<Ch, Tr> & o, adl_hint(type_desc_tag<T>))
{ return o << type_desc<T>(); }

template <typename Ch, typename Tr, typename T>
basic_ostream<Ch, Tr> & operator<<(basic_ostream<Ch, Tr> & o, adl_hint(type_tag<T>))
{ return o << type<T>(); }

template <typename Ch, typename Tr, typename T>
basic_ostream<Ch, Tr> & operator<<(basic_ostream<Ch, Tr> & o, etype_tag<T>)
{ return o << expression_category<T>() << ' ' << type<typename std::remove_reference<T>::type>(); }

template <typename Ch, typename Tr, typename T>
basic_ostream<Ch, Tr> & operator<<(basic_ostream<Ch, Tr> & o, etype_desc_tag<T>)
{ return o << expression_category<T>() << ' ' << type_desc<typename std::remove_reference<T>::type>(); }

} // type_strings_detail

template <typename T> type_strings_detail::adl_hint
  TYPE_DESC(type_strings_detail::type_desc_tag<T>) { return type_strings_detail::adl_hint(); }
template <typename T> type_strings_detail::adl_hint
  TYPE(type_strings_detail::type_tag<T>) { return type_strings_detail::adl_hint(); }

#define TYPE(...) ::type_strings_detail::etype_tag<decltype(void(), (__VA_ARGS__))>{}
#define TYPE_DESC(...) ::type_strings_detail::etype_desc_tag<decltype(void(), (__VA_ARGS__))>{}

#endif // header guard
