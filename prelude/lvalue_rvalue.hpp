
// Section/paragraph references refer to N2461.

// Some of the tests may fail in the presence of user-defined conversion operators to reference types.

// Todo: Maybe add some array/volatile tests.

#ifndef LVALUE_RVALUE_HPP
#define LVALUE_RVALUE_HPP

#include <utility>

#ifdef __GXX_EXPERIMENTAL_CXX0X__

  #include <type_traits>

  #define IS_LVALUE(...) \
    (std::identity<decltype(::lvalue_rvalue_detail::deduce(((::lvalue_rvalue_detail::helper(), (__VA_ARGS__)), ::lvalue_rvalue_detail::helper())))>::type::value)
  #define IS_RVALUE(...) (!IS_LVALUE(__VA_ARGS__))

  #define MAY_BE_LVALUE(...) (IS_LVALUE(__VA_ARGS__))
  #define MAY_BE_RVALUE(...) (IS_RVALUE(__VA_ARGS__))

  namespace lvalue_rvalue_detail
  {
    struct helper {};

    template <typename T> struct copy_ref { typedef int type; };
    template <typename T> struct copy_ref<T&> { typedef int& type; };

    template <typename T> typename copy_ref<T>::type operator,(helper, T &&);
    template <typename T> T && operator,(T &&, helper);

    template <typename T> std::is_reference<T> deduce (T &&);
  }

#else

  #include <boost/type_traits/is_const.hpp>
  #include <boost/utility/enable_if.hpp>

  namespace lvalue_rvalue_detail
  {
    typedef char no;
    typedef int yes;

    template <typename T> no is_nonconst_rvalue (T &, void *);
    template <typename T> yes is_nonconst_rvalue (T const &, ...);

    #define IS_NONCONST_RVALUE(...) \
      (sizeof(::lvalue_rvalue_detail::is_nonconst_rvalue((__VA_ARGS__), 0))==sizeof(::lvalue_rvalue_detail::yes))

    template <typename T> typename boost::disable_if<boost::is_const<T>, yes>::type
      is_nonconst_lvalue (T &, void *);
    template <typename T> no is_nonconst_lvalue (T const &, ...);

    #define IS_NONCONST_LVALUE(...) \
      (sizeof(::lvalue_rvalue_detail::is_nonconst_lvalue((__VA_ARGS__), 0))==sizeof(::lvalue_rvalue_detail::yes))
  }

  // In C++03, there does not seem to be a way to distinguish a const lvalue from a const rvalue at compile-time other than by trying to take the address or perform a const_cast to T& (where T is not const). The following two "may be ..." tests are the best we can do.

  #define MAY_BE_LVALUE(...) (!IS_NONCONST_RVALUE(__VA_ARGS__))
  #define MAY_BE_RVALUE(...) (!IS_NONCONST_LVALUE(__VA_ARGS__))

#endif

#endif // header guard

#ifdef LVALUE_RVALUE_TEST

#ifdef __GXX_EXPERIMENTAL_CXX0X__

  #define L(e) static_assert(IS_LVALUE(e), "")
  #define R(e) static_assert(IS_RVALUE(e), "")

#else

  #include <boost/static_assert.hpp>

  #define L(e) \
    BOOST_STATIC_ASSERT(MAY_BE_LVALUE(e)); \
    BOOST_STATIC_ASSERT(!MAY_BE_RVALUE(e))
  #define R(e) \
    BOOST_STATIC_ASSERT(MAY_BE_RVALUE(e)); \
    BOOST_STATIC_ASSERT(!MAY_BE_LVALUE(e))

#endif

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

  #ifdef __GXX_EXPERIMENTAL_CXX0X__
    L(e);
    R(f()); // 3.10p5
    L(g); // 5p5
    L(h()); // 5p5
    R(v());
  #else

    #define I(e) /* Indeterminate */ \
      BOOST_STATIC_ASSERT(MAY_BE_RVALUE(e)); \
      BOOST_STATIC_ASSERT(MAY_BE_LVALUE(e))

    I(e);
    I(f());
    I(g);
    I(h());

    #undef I

  #endif

  #ifdef __GXX_EXPERIMENTAL_CXX0X__
    T && i = a; L(i); // 5p6
    T && j (); R(j()); // 3.10p5, 5p6
    T const && k = a; L(k); // 5p6
    T const && l (); R(l()); // 3.10p5, 5p6
  #endif

  #undef L
  #undef R
}

#endif // testing
