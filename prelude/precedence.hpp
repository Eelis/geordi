/*

Usage:

  { using precedence::_; cout << _ + _ + _ + _ * _; }

  Output: (((_ + _) + _) + (_ * _))

Todo:

- Operators (), .*, and ->*.
- Long format, where _+_ is shown as operator+(_, _).
- Get rid of the (harmless) UB.

*/

#ifndef PRECEDENCE_HPP
#define PRECEDENCE_HPP

#include <iostream>

namespace precedence {

namespace detail {

using std::ostream;

template<typename> struct p {};
template<typename T> ostream & operator<<(ostream & o, p<T> const &) { T::print(o); return o; }

template <typename> struct expr;

template<typename E>
struct arrow
{ static void print(ostream& o) { o << '(' << p<E>() << "->_)"; }
  static expr<arrow<E> > const & _;
};

template<typename E> expr<arrow<E> > const & arrow<E>::_(_); // UB, but we don't care.

template<typename E> struct underscore
{ static void print(ostream & o) { o << '(' << p<E>() << "._)"; } };
template<typename E, typename F> struct subscript
{ static void print(ostream & o) { o << '(' << p<E>() << '[' << p<F>() << "])"; } };
template<typename E, typename F> struct assign
{ static void print(ostream & o) { o << '(' << p<E>() << " = " << p<F>() << ')'; } };
template<typename E, typename F> struct comma
{ static void print(ostream & o) { o << '(' << p<E>() << " , " << p<F>() << ')'; } };
template<typename E> struct postfix_incr
{ static void print(ostream & o) { o << '(' << p<E>() << " ++)"; } };
template<typename E> struct postfix_decr
{ static void print(ostream & o) { o << '(' << p<E>() << " --)"; } };

template<typename E> struct expr
{ static expr<underscore<E> > const & _;
  expr(int) {}
  arrow<E> const * operator->() const { return 0; }
  template<typename F> expr<assign<E, F> > operator=(expr<F> const &) const { return 0; }
  template<typename F> expr<subscript<E, F> > operator[](expr<F> const &) const { return 0; }
};

template<typename E> expr<underscore<E> > const & expr<E>::_(_); // UB, but we don't care.

struct atom { static void print(ostream & o) { o << '_'; } };

#define BINOP(n, op) \
  template<typename E, typename F> struct n \
  { static void print(ostream & o) { o << '(' << p<E>() << " " #op " " << p<F>() << ')'; } }; \
  template<typename E, typename F> \
  expr<n<E, F> > operator op(expr<E> const &, expr<F> const &) { return 0; }

#define UNOP(n, op) \
  template<typename E> struct n { static void print(ostream & o) { o << "(" #op " " << p<E>() << ')'; } }; \
  template<typename E> expr<n<E> > operator op(expr<E> const &) { return 0; }

// Below, we use the (de-camel-cased) names of the C++1x concepts listed in [concept.operator].
BINOP(plus, +) BINOP(minus, -) BINOP(multiply, *) BINOP(divide, /) BINOP(modulus, %)
UNOP(unary_plus, +) UNOP(negate, -)
BINOP(less, <) BINOP(greater, >) BINOP(less_equal, <=) BINOP(greater_equal, >=)
BINOP(equal_to, ==) BINOP(not_equal_to, !=)
BINOP(logical_and, &&) BINOP(logical_or, ||) UNOP(logical_not, !)
BINOP(bit_and, &) BINOP(bit_or, |) BINOP(bit_xor, ^) UNOP(complement, ~)
BINOP(left_shift, <<) BINOP(right_shift, >>)
UNOP(dereference, *) UNOP(address_of, &)
BINOP(plus_assign, +=) BINOP(minus_assign, -=)
BINOP(multiply_assign, *=) BINOP(divide_assign, /=) BINOP(modulus_assign, %=)
BINOP(bit_and_assign, &=) BINOP(bit_or_assign, |=) BINOP(bit_xor_assign, ^=)
BINOP(left_shift_assign, <<=) BINOP(right_shift_assign, >>=)
UNOP(pre_increment, ++) UNOP(pre_decrement, --)

#undef BINOP
#undef UNOP

template<typename E, typename F> expr<comma<E, F> > operator,(expr<E>, expr<F>) { return 0; }
template<typename E> expr<postfix_incr<E> > operator++(expr<E> const &, int) { return 0; }
template<typename E> expr<postfix_decr<E> > operator--(expr<E> const &, int) { return 0; }

template<typename E> ostream & operator<<(ostream & o, expr<E> const &) { E::print(o); return o; }

} // namespace detail

detail::expr<detail::atom> const _(0);
  // We don't worry about defining this in a header, because this header has no use outside geordi anyway.

} // namespace precedence

#endif // Header guard.
