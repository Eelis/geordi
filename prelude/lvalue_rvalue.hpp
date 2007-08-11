
// n2369 interpretation.

#ifndef LVALUE_RVALUE_HPP
#define LVALUE_RVALUE_HPP

#include <type_traits>
#include <utility>

#define IS_LVALUE(x) (std::identity<decltype(lvalue_rvalue_detail::deduce(x))>::type::value)
#define IS_RVALUE(x) (!IS_LVALUE(x))

namespace lvalue_rvalue_detail
{
  template <typename T> std::is_reference<T> deduce (T &&);

  int a;
  int b ();
  int & c ();
  int && d ();
  int const & e ();
  int const && f ();

  static_assert(IS_LVALUE(a), "");
  static_assert(IS_LVALUE(b), "");
  static_assert(IS_RVALUE(b()), "");
  static_assert(IS_LVALUE(c()), "");
  static_assert(IS_RVALUE(d()), "");
  static_assert(IS_LVALUE(e()), "");
  static_assert(IS_RVALUE(f()), "");
}

#endif // header guard
