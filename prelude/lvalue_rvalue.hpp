
// Section/paragraph references refer to N2461.

// Some of the tests may fail in the presence of user-defined conversion operators to reference types.

// Todo: Maybe add some array/volatile tests.

#ifndef LVALUE_RVALUE_HPP
#define LVALUE_RVALUE_HPP

#include <utility>

#include <type_traits>

#define IS_LVALUE(...) \
  (::lvalue_rvalue_detail::identity<decltype(::lvalue_rvalue_detail::deduce(((::lvalue_rvalue_detail::helper(), (__VA_ARGS__)), ::lvalue_rvalue_detail::helper())))>::type::value)
#define IS_RVALUE(...) (!IS_LVALUE(__VA_ARGS__))

#define MAY_BE_LVALUE(...) (IS_LVALUE(__VA_ARGS__))
#define MAY_BE_RVALUE(...) (IS_RVALUE(__VA_ARGS__))

namespace lvalue_rvalue_detail
{
  template <typename T> struct identity { typedef T type; };
  struct helper {};

  template <typename T> struct copy_ref { typedef int type; };
  template <typename T> struct copy_ref<T&> { typedef int& type; };

  template <typename T> typename copy_ref<T>::type operator,(helper, T &&);
  template <typename T> T && operator,(T &&, helper);

  template <typename T> std::is_reference<T> deduce (T &&);
}

#endif // header guard

#ifdef LVALUE_RVALUE_TEST

#define L(e) static_assert(IS_LVALUE(e), "")
#define R(e) static_assert(IS_RVALUE(e), "")

#include <iostream>

template <typename T> std::ostream & operator,(std::ostream &, T const &);

namespace lvalue_rvalue_test
{
  struct T {}; // If we use something like int, const will be redundant in  int const f ();

  L(std::cout);

  T a; L(a);
  T b (); R(b()); // 3.10p5
  T & c = a; L(c); // 5p5
  T & d (); L(d); // 3.10p3
  T const e = T();
  T const f ();
  T const & g = a;
  T const & h ();
  void v();

  L(e);
  R(f()); // 3.10p5
  L(g); // 5p5
  L(h()); // 5p5
  R(v());

  T && i = std::move(a); L(i); // 5p6
  T && j (); R(j()); // 3.10p5, 5p6
  T const && k = std::move(a); L(k); // 5p6
  T const && l (); R(l()); // 3.10p5, 5p6

  #undef L
  #undef R
}

#endif // testing
