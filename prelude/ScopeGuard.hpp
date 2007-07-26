
#ifndef SCOPEGUARD_HPP
#define SCOPEGUARD_HPP

#include <boost/function.hpp>
#include <boost/noncopyable.hpp>

struct ScopeGuard: boost::function<void ()>, boost::noncopyable
{
  typedef boost::function<void ()> F;
  ScopeGuard () {}
  template <typename T> explicit ScopeGuard (T const & f) : F (f) {}
  ~ScopeGuard () { try { if (*this) operator()(); } catch (...) {} }
};

#endif // header guard
