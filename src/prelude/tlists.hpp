#ifndef TLISTS_HPP
#define TLISTS_HPP

#include <string>

namespace tlists
{
  template <typename...> struct tlist;

  template <typename H, typename T> struct cons;
  template <typename H, typename... T> struct cons<H, tlist<T...> > { typedef tlist<H, T...> type; };

  template <size_t, typename> struct group;

  // group_successive

  template <typename, size_t, typename...> struct group_successive_impl;

  template <typename A, typename B, typename... T>
  struct group_successive_impl<A, 0, B, T...>
  { typedef typename group_successive_impl<B, 1, T...>::type type; };

  template <typename A, size_t N, typename... T>
  struct group_successive_impl<A, N, A, T...>
  { typedef typename group_successive_impl<A, N + 1, T...>::type type; };

  template <typename A, size_t N, typename B, typename... T>
  struct group_successive_impl<A, N, B, T...>
  { typedef typename cons<group<N, A>, typename group_successive_impl<B, 1, T...>::type>::type type; };

  template <typename A> struct group_successive_impl<A, 0> { typedef tlist<> type; };
  template <typename A, size_t N> struct group_successive_impl<A, N>
  { typedef tlist<group<N, A> > type; };

  template <typename... T> struct group_successive: group_successive_impl<void, 0, T...> {};
}

#endif // header guard
