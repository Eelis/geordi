#ifndef DELIMITED_OSTREAM_HPP
#define DELIMITED_OSTREAM_HPP

#include <iostream>
#include <ios>

#if __cplusplus >= 201103

#include <type_traits>

namespace del_ostream_detail
{
  template<typename T> constexpr bool take_by_value()
  { return std::is_integral<T>::value || std::is_enum<T>::value; }
}

template <typename Ch, typename Tr, typename T>
inline typename std::enable_if<
    !del_ostream_detail::take_by_value<T>(),
    std::basic_ostream<Ch, Tr> >::type &
  operator,(std::basic_ostream<Ch, Tr> & o, T const & t)
{ return o << ", " << t; }

template <typename Ch, typename Tr, typename T>
inline typename std::enable_if<
    del_ostream_detail::take_by_value<T>(),
    std::basic_ostream<Ch, Tr> >::type &
  operator,(std::basic_ostream<Ch, Tr> & o, T const t)
{ return o << ", " << t; }

#else

template <typename Ch, typename Tr, typename T>
inline std::basic_ostream<Ch, Tr> &
  operator,(std::basic_ostream<Ch, Tr> & o, T const & t)
{ return o << ", " << t; }

#endif

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator, (std::basic_ostream<Ch, Tr> & o, std::ios_base & (* const f) (std::ios_base &))
{ return o << f; }

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator, (std::basic_ostream<Ch, Tr> & o, std::basic_ostream<Ch, Tr> & (* const f) (std::basic_ostream<Ch, Tr> &))
{ return o << f; }

#endif // header guard
