
#ifndef MORE_STDLIB_OSTREAMING_HPP
#define MORE_STDLIB_OSTREAMING_HPP

#include <ostream>
#include <valarray>
#include <utility>
#include <boost/range.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/utility/enable_if.hpp>

template <typename C, typename Tr, typename A, typename B>
std::basic_ostream<C, Tr> & operator<< (std::basic_ostream<C, Tr> & o, std::pair<A, B> const & p)
{ return o << '(' << p.first << ", " << p.second << ')'; }

#ifdef __GXX_EXPERIMENTAL_CXX0X__

  #include <tr1/tuple>
  #include <tuple>

  namespace more_stdlib_ostreaming_detail
  {
    template <int O>
    struct tuple_printer
    {
      template <typename C, typename Tr, typename... P>
      static void print (std::basic_ostream<C, Tr> & o, std::tr1::tuple<P...> const & t)
      {
        if (O != 1) { tuple_printer<O-1>::print(o, t); o << ", "; }
        o << std::tr1::get<O-1>(t);
      }

      template <typename C, typename Tr, typename... P>
      static void print (std::basic_ostream<C, Tr> & o, std::tuple<P...> const & t)
      {
        if (O != 1) { tuple_printer<O-1>::print(o, t); o << ", "; }
        o << std::get<O-1>(t);
      }
    };

    template <>
    struct tuple_printer<0>
    {
      template <typename C, typename Tr, typename T>
      static void print (std::basic_ostream<C, Tr> &, T const &) {}
    };
  }

  template <typename C, typename Tr, typename... P>
  std::basic_ostream<C, Tr> & operator<< (std::basic_ostream<C, Tr> & o, std::tr1::tuple<P...> const & t)
  { o << '('; more_stdlib_ostreaming_detail::tuple_printer<sizeof...(P)>::print(o, t); return o << ')'; }

  template <typename C, typename Tr, typename... P>
  std::basic_ostream<C, Tr> & operator<< (std::basic_ostream<C, Tr> & o, std::tuple<P...> const & t)
  { o << '('; more_stdlib_ostreaming_detail::tuple_printer<sizeof...(P)>::print(o, t); return o << ')'; }

#endif

namespace more_stdlib_ostreaming_detail
{
  template <typename C, typename Tr, typename Range>
  void print_range (std::basic_ostream<C, Tr> & o, Range const & r)
  {
    typename boost::range_const_iterator<Range>::type b = boost::begin(r), e = boost::end(r);
    o << '[';
    if (b != e) { o << *b; while (++b != e) o << ", " << *b; }
    o << ']';
  }

  template <typename, typename T> struct snd { typedef T type; };
}

template <typename C, typename Tr, typename R>
typename more_stdlib_ostreaming_detail::snd<typename R::iterator, std::basic_ostream<C, Tr> &>::type
  operator<<(std::basic_ostream<C, Tr> & o, R const & r)
{ more_stdlib_ostreaming_detail::print_range(o, r); return o; }

// Since we defined our generic operator<< for ranges in the global namespace, boost::lexical_cast won't find it when used on range types defined in namespaces other than the global namespace. For now, our quick fix for standard library containers is the following:

namespace std { using ::operator<<; }

template <typename C, typename Tr, typename T, size_t N>
typename boost::disable_if<boost::is_same<T, char>, std::basic_ostream<C, Tr> &>::type
  operator<<(std::basic_ostream<C, Tr> & o, T const (& a) [N])
{ more_stdlib_ostreaming_detail::print_range(o, a); return o; }

namespace std
{
  template <typename Ch, typename Tr, typename T>
  std::basic_ostream<Ch, Tr> & operator<< (std::basic_ostream<Ch, Tr> & o, std::valarray<T> const & v)
  {
    o << '[';
    for (std::size_t i = 0; i != v.size(); ++i) { if (i != 0) o << ", "; o << v[i]; }
    return o << ']';
  }
}

#endif // header guard

#ifdef MORE_STDLIB_OSTREAMING_TEST

#include <vector>
#include <set>
#include <map>
#include <list>
#include <utility>
#include <deque>

#include <tr1/array>

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
