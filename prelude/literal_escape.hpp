/* This code was basically written under the assumption that char strings are UTF-8 encoded, and that wchar_t strings are UTF-32 encoded. In other words, it was written specifically for the environment geordi runs in.

Todo:
  * Proper char16_t and char32_t support. We'll probably need codecvt crap for the conversions.
  * Consider separately compiled explicit instantiations.
*/

#ifndef LITERAL_ESCAPE_HPP
#define LITERAL_ESCAPE_HPP

#include <boost/io/ios_state.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/make_unsigned.hpp>

#include <ostream>
#include <iomanip>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>
#include <iterator>
#include <string>
#include <cstring>

namespace escape_detail
{
  template <typename ChA, typename ChB, typename TrB>
  void conv(ChA const c, std::basic_ostream<ChB, TrB> & o) { o << c; }

  inline void conv(wchar_t const c, std::ostream & o)
  {
    char buf[4];
    int const i = wctomb(buf, c);
    if(i < 0) o << '?';
    else copy(buf, buf + i, std::ostreambuf_iterator<char>(o));
  }

  inline bool isprint(wchar_t const c) { return iswprint(c); }
  inline bool isdigit(wchar_t const c) { return iswdigit(c); }
  inline std::size_t strlen(wchar_t const * const s) { return wcslen(s); }

  template <typename C> bool ishexdigit(C const c)
  { return isdigit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'); }

  template <typename Ch> char char_literal_key();

  template <> inline char char_literal_key<char>() { return 0; }
  template <> inline char char_literal_key<wchar_t>() { return 'L'; }

  #ifdef __GXX_EXPERIMENTAL_CXX0X__
    template <> inline char char_literal_key<char16_t>() { return 'u'; }
    template <> inline char char_literal_key<char32_t>() { return 'U'; }
  #endif

  enum unterm { unterm_hex, unterm_oct, unterm_none };

  template <typename ChA, typename ChB, typename TrB>
  unterm escape_char(unterm const u, ChA const c, std::basic_ostream<ChB, TrB> & o)
  {
    using escape_detail::isprint; using std::isprint;
    using escape_detail::isdigit; using std::isdigit;

    switch(c) {
      case '\0': o << "\\0"; return unterm_oct;
      case '\v': o << "\\v"; break; case '\b': o << "\\b"; break; case '\n': o << "\\n"; break;
      case '\r': o << "\\r"; break; case '\f': o << "\\f"; break; case '\a': o << "\\a"; break;
      case '\t': o << "\\t"; break; case '\\': o << "\\\\"; break; case '"': o << "\\\""; break;
      default:
        if(isprint(c)) {
          if((u == unterm_hex && ishexdigit(c)) || (u == unterm_oct && isdigit(c))) o << "\"\"";
          conv(c, o);
          break;
        } else {
          unsigned long const l = typename boost::make_unsigned<ChA>::type(c);
          o << "\\x" << std::hex << l;
          return unterm_hex;
    }   }

    return unterm_none;
  }

  template <typename I, typename Ch, typename Tr>
  void escape_string(I i, I const e, std::basic_ostream<Ch, Tr> & o)
  {
    boost::io::ios_flags_saver const ifs(o);
    if(char const c = char_literal_key<typename std::iterator_traits<I>::value_type>()) o << c;
    o << '"';
    unterm u = unterm_none;
    for(; i != e; ++i) u = escape_char(u, *i, o);
    o << '"';
  }

  template <typename> struct is_character { enum { value = false }; };
  #define YES(T) template <> struct is_character<T> { enum { value = true }; };
  YES(char) YES(wchar_t)
  #ifdef __GXX_EXPERIMENTAL_CXX0X__
    YES(char16_t) YES(char32_t)
  #endif
  #undef YES

  template <typename Ch, typename I> struct string_escaper { I b, e; };
  template <typename Ch> struct cstring_escaper { Ch const * s; };
  template <typename Ch> struct char_escaper { Ch const c; };

  template <typename ChA, typename TrA, typename ChB, typename I>
  std::basic_ostream<ChA, TrA> & operator<<(std::basic_ostream<ChA, TrA> & o, string_escaper<ChB, I> const e)
  { escape_string(e.b, e.e, o); return o; }

  template <typename Ch, typename Tr, typename ChB>
  std::basic_ostream<Ch, Tr> & operator<<(std::basic_ostream<Ch, Tr> & o, cstring_escaper<ChB> const e)
  {
    using escape_detail::strlen; using std::strlen;
    if(e.s) escape_string(e.s, e.s + strlen(e.s), o); else o << 0;
    return o;
  }

  template <typename ChA, typename TrA, typename ChB>
  std::basic_ostream<ChA, TrA> & operator<<(std::basic_ostream<ChA, TrA> & o, char_escaper<ChB> const e)
  {
    boost::io::ios_flags_saver const ifs(o);
    if(char const c = char_literal_key<ChB>()) o << c;
    o << '\''; escape_char(unterm_none, e.c, o); return o << '\'';
  }
}

template <typename Ch, typename Tr>
escape_detail::string_escaper<Ch, typename std::basic_string<Ch, Tr>::const_iterator> escape(std::basic_string<Ch, Tr> const & s)
{ escape_detail::string_escaper<Ch, typename std::basic_string<Ch, Tr>::const_iterator> const r = { s.begin(), s.end() }; return r; }

template <typename C>
typename boost::enable_if<escape_detail::is_character<C>, escape_detail::cstring_escaper<C> >::type
  escape(C const * b) { escape_detail::cstring_escaper<C> const r = { b }; return r; }

template <typename C>
typename boost::enable_if<escape_detail::is_character<C>, escape_detail::char_escaper<C> >::type
  escape(C const c) { escape_detail::char_escaper<C> const r = { c }; return r; }

template <typename T>
typename boost::disable_if<escape_detail::is_character<T>, T const &>::type
  escape(T const & x) { return x; }

#endif // Header guard.

#ifdef LITERAL_ESCAPE_TEST

// When running these tests, make sure LC_ALL is set to en_US.utf-8.

#include <sstream>
#include <cassert>
#include <iostream>
#include <clocale>

int main()
{
  std::setlocale(LC_ALL, "");

  typedef std::ostringstream oss;
  typedef std::wostringstream woss;
  { oss o; o << escape("a∀b"); assert(o.str() == BOOST_STRINGIZE("a\xe2\x88\x80""b")); }
  { oss o; o << escape(L"a∀b"); assert(o.str() == BOOST_STRINGIZE(L"a∀b")); }
  { woss o; o << escape("a∀b"); assert(o.str() == L"\"a\\xe2\\x88\\x80\"\"b\""); }
  { woss o; o << escape(L"a∀b"); assert(o.str() == L"L\"a∀b\""); }
  { oss o; char const * const s = 0; o << escape(s); assert(o.str() == "0"); }
  { oss o; o << escape(std::string("\t\n\v\f\\\0""3\b\r\"\a", 11)); assert(o.str() == BOOST_STRINGIZE("\t\n\v\f\\\0""3\b\r\"\a")); }
}

#endif // Testing.
