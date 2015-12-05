#ifndef MORE_OSTREAMING_HPP
#define MORE_OSTREAMING_HPP

#include <ostream>
#include <valarray>
#include <utility>
#include <stack>
#include <queue>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include "geordi.hpp"
#include "literal_escape.hpp"

#if !defined(__clang__) && __cplusplus >= 201500 // TODO
#include <experimental/filesystem>
#endif

template <typename C, typename Tr>
std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, utsname const & u)
{ return o << u.sysname << ' ' << u.release << ' ' << u.version, u.machine; }


#if __cplusplus >= 201103

template <typename C, typename Tr, typename T>
std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, std::reference_wrapper<T> const & rw)
{ T & r(rw); return o << r; }

#endif

template <typename C, typename Tr, typename A, typename B>
std::basic_ostream<C, Tr> & operator<< (std::basic_ostream<C, Tr> & o, std::pair<A, B> const & p)
{ return o << '{' << escape(p.first) << ", " << escape(p.second) << '}'; }

template <typename C, typename Tr, typename D>
void print_div_t(std::basic_ostream<C, Tr> & o, D const d) { o << '{' << d.quot << ", " << d.rem << "}"; }

template <typename C, typename Tr>
std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, std::div_t const d)
{ print_div_t(o, d); return o; }

template <typename C, typename Tr>
std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, std::ldiv_t const d)
{ print_div_t(o, d); return o; }

#if __cplusplus >= 201103

template <typename C, typename Tr>
std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, std::lldiv_t const d)
{ print_div_t(o, d); return o; }

template <typename C, typename T, typename R, typename... A>
std::basic_ostream<C, T> & operator<<(std::basic_ostream<C, T> & o, R (* const p) (A...))
{
  void * q;
  static_assert(sizeof(p) == sizeof(q), "void- and function-pointer differ in size");
  std::copy(reinterpret_cast<char const *>(&p),
    reinterpret_cast<char const *>(&p) + sizeof(p), reinterpret_cast<char *>(&q));
  return o << q;
}

#include <tuple>

namespace more_ostreaming { namespace detail {

  template <int O>
  struct tuple_printer
  {
    template <typename C, typename Tr, typename... P>
    static void print (std::basic_ostream<C, Tr> & o, std::tuple<P...> const & t)
    {
      if (O != 1) { tuple_printer<O-1>::print(o, t); o << ", "; }
      o << escape(std::get<O-1>(t));
    }
  };

  template <>
  struct tuple_printer<0>
  {
    template <typename C, typename Tr, typename T>
    static void print (std::basic_ostream<C, Tr> &, T const &) {}
  };
}}

template <typename C, typename Tr, typename... P>
std::basic_ostream<C, Tr> & operator<< (std::basic_ostream<C, Tr> & o, std::tuple<P...> const & t)
{ o << '{'; more_ostreaming::detail::tuple_printer<sizeof...(P)>::print(o, t); return o << '}'; }

#endif

#if __cplusplus >= 201103

namespace more_ostreaming
{
  template <typename C, typename Tr, typename Range>
  void delimit(std::basic_ostream<C, Tr> & o, Range const & r)
  {
    bool first = true;
    for (auto && x : r) { if (first) first = false; else o << ", "; o << escape(x); }
  }

  extern template void delimit<char, std::char_traits<char>, std::vector<int> >(std::ostream &, std::vector<int> const &);
    // vector<int> is the most used container for demonstrations, so it's worth making it print fast.
}

#endif

namespace more_ostreaming { namespace detail {
  struct bytes_t { unsigned char const * p; std::size_t n; };
  template <typename C, typename Tr>
  std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, bytes_t const b) {
    typedef unsigned int uint;
    o << '{';
    if(b.n != 0) o << uint(*b.p);
    for(std::size_t i = 1; i != b.n; ++i) o << ", " << uint(b.p[i]);
    return o << '}';
  }
}}

template <typename T>
more_ostreaming::detail::bytes_t bytes(T const & x) {
  more_ostreaming::detail::bytes_t const r =
    { reinterpret_cast<unsigned char const *>(&x), sizeof(T) };
  return r;
}

inline more_ostreaming::detail::bytes_t bytes(void const * const p, std::size_t const n) {
  more_ostreaming::detail::bytes_t const r =
    { reinterpret_cast<unsigned char const *>(p), n };
  return r;
}

namespace more_ostreaming { namespace detail {
  template <typename, typename T> struct snd { typedef T type; };
}}

#if __cplusplus >= 201103

template <typename C, typename Tr, typename R>
typename more_ostreaming::detail::snd<typename R::iterator, std::basic_ostream<C, Tr> &>::type
  operator<<(std::basic_ostream<C, Tr> & o, R const & r)
{ o << '{'; more_ostreaming::delimit(o, r); return o << '}'; }

// Since we defined our generic operator<< for ranges in the global namespace, boost::lexical_cast won't find it when used on range types defined in namespaces other than the global namespace. For now, our quick fix for standard library containers is the following:

namespace std { using ::operator<<; }

template <typename C, typename Tr, typename T, size_t N>
typename std::enable_if<!geordi::is_character<T>::value, std::basic_ostream<C, Tr> &>::type
  operator<<(std::basic_ostream<C, Tr> & o, T const (& a) [N])
{ o << '{'; more_ostreaming::delimit(o, a); return o << '}'; }

#endif

template <typename C, typename Tr>
std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, std::tm const * const t)
{
  o.write(std::asctime(t), 24); // Omit the final \n.
  return o;
}

template <typename T, typename C>
C const & underlying(std::stack<T, C> const & s) {
  struct S: std::stack<T, C> {
    static C const & peek(std::stack<T, C> const & s) { return s.*(&S::c); }
  };
  return S::peek(s);
}

template <typename T, typename C>
C const & underlying(std::queue<T, C> const & s) {
  struct S: std::queue<T, C> {
    static C const & peek(std::queue<T, C> const & s) { return s.*(&S::c); }
  };
  return S::peek(s);
}

template <typename T, typename C, typename P>
C const & underlying(std::priority_queue<T, C, P> const & s) {
  struct S: std::priority_queue<T, C> {
    static C const & peek(std::priority_queue<T, C> const & s) { return s.*(&S::c); }
  };
  return S::peek(s);
}

namespace std
{
  template <typename Ch, typename Tr, typename T>
  std::basic_ostream<Ch, Tr> & operator<< (std::basic_ostream<Ch, Tr> & o, std::valarray<T> const & v)
  {
    o << '{';
    for (std::size_t i = 0; i != v.size(); ++i) { if (i != 0) o << ", "; o << v[i]; }
    return o << '}';
  }

  template <typename Ch, typename Tr, typename T, typename C>
  std::basic_ostream<Ch, Tr> &
    operator<<(std::basic_ostream<Ch, Tr> & o, std::stack<T, C> const & s)
  { return o << underlying(s); }

  template <typename Ch, typename Tr, typename T, typename C>
  std::basic_ostream<Ch, Tr> &
    operator<<(std::basic_ostream<Ch, Tr> & o, std::queue<T, C> const & q)
  { return o << underlying(q); }

  template <typename Ch, typename Tr, typename T, typename C, typename P>
  std::basic_ostream<Ch, Tr> &
    operator<<(std::basic_ostream<Ch, Tr> & o, std::priority_queue<T, C, P> const & q)
  { return o << underlying(q); }

  template <typename Ch, typename Tr>
  std::basic_ostream<Ch, Tr> & operator<<(std::basic_ostream<Ch, Tr> & o, std::type_info const & t)
  { return o << t.name(); }
}

#if !defined(__clang__) && __cplusplus >= 201500

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator<<(
  std::basic_ostream<Ch, Tr> & o, std::experimental::filesystem::perms const p)
{
  std::ios::fmtflags f(o.flags());
  std::oct(o);
  o << std::underlying_type_t<std::experimental::filesystem::perms>(p);
  o.flags(f);
  return o;
}

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator<<(
  std::basic_ostream<Ch, Tr> & o, std::experimental::filesystem::file_type const t)
{
  switch (t)
  {
    #define CASE(c) \
      case std::experimental::filesystem::file_type::c: return o << "file_type::" #c;

    CASE(none)
    CASE(not_found)
    CASE(regular)
    CASE(directory)
    CASE(symlink)
    CASE(block)
    CASE(character)
    CASE(fifo)
    CASE(socket)
    CASE(unknown)

    #undef CASE

    default: return o << "?";
  }
}

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator<<(
  std::basic_ostream<Ch, Tr> & o, std::experimental::filesystem::file_status const & s)
{ return o << '{' << s.type() << ", " << s.permissions() << '}'; }

#endif


#if __cplusplus >= 201103

#include <chrono>

template <typename Ch, typename Tr, typename K, typename D>
std::basic_ostream<Ch, Tr> &
  operator<<(std::basic_ostream<Ch, Tr> & o, std::chrono::time_point<K, D> const & t)
{ return o << "epoch + " << t.time_since_epoch(); }

template<typename Ch, typename Tr, typename R, typename F>
std::basic_ostream<Ch, Tr> & operator<<(std::basic_ostream<Ch, Tr> & o, std::chrono::duration<R, F> const & d)
{ return o << d.count(); }

#define T(r, s) \
  template <typename Ch, typename Tr, typename R> std::basic_ostream<Ch, Tr> & \
    operator<<(std::basic_ostream<Ch, Tr> & o, std::chrono::duration<R, std::r> const & d) \
  { return o << d.count() << s; }

T(ratio<86400>, " d")
T(ratio<3600>, " h")
T(ratio<60>, " min")
T(ratio<1>, " s")
T(milli, " ms")
T(micro, " µs")
T(nano, " ns")

#undef T

#endif


#if __cplusplus >= 201103

#include <system_error>

std::ostream & operator<<(std::ostream &, std::error_category const &);

#endif

#endif // header guard

#ifdef MORE_OSTREAMING_TEST

#include <vector>
#include <set>
#include <map>
#include <list>
#include <utility>
#include <deque>
#include <array>
#include <iostream>

int main()
{
  std::map<int, int> m;
  m[1]= 1; m[2]= 2;
  std::set<int> s;
  s.insert(10);
  s.insert(20);
  std::vector<int> v(2, 2);
  int a [] = { 3, 9, 5 };

  std::wcout << m << '\n' << s << '\n' << v << '\n' << a << '\n';
}

#endif // testing
