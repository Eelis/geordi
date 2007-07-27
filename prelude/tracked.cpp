
#include "tracked.hpp"

#include "foreach.hpp"

#include <iostream>
#include <iomanip>

#include <boost/static_assert.hpp>
#include <boost/io/ios_state.hpp>

namespace tracked
{
  namespace
  {
    template <typename T>
    std::ostream & prt (T const * const t) { return std::cout << ' ' << *t << "::"; }

    void print_addr (std::ostream & o, void const * const p)
    {
      boost::io::ios_flags_saver const fs (o);
      typedef unsigned long ulong;
      BOOST_STATIC_ASSERT(sizeof(ulong) == sizeof(void const *));
      o << std::hex << std::setfill('0') << std::setw(3)
        << (((ulong(p) & 0xf0000000ul) >> 20) | (ulong(p) & 0xfful));
    }
  }

  // B:

    B::B () { reg.s.insert(this); prt(this) << "B() "; }
    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B::B (B && b) { reg.s.insert(this); prt(this) << "mB(" << b << ") "; } // 'm' for "move"
    #endif
    B::B (int const i) { reg.s.insert(this); prt(this) << "B(" << i << ") "; }
    B::B (B const & b) { reg.s.insert(this); prt(this) << "B(" << b << ") "; }

    B & B::operator= (B const & b) { std::cout << ' ' << *this << '=' << b << ' '; return *this; }

    B & B::operator++ () { std::cout << " ++" << *this << ' '; return *this; }
    B B::operator++ (int) { B const r (*this); operator++(); return r;  }

    void B::f () const { prt(this) << "f() "; }
    void B::vf () const { prt(this) << "vf() "; }

    B::~B () { reg.s.erase(this); prt(this) << "~B() "; }

    B::Reg B::reg;

    B::Reg::~Reg ()
    {
      if (s.empty()) return;
      std::cout << " || leaked:";
      BOOST_FOREACH(B const * const b, s) std::cout << ' ' << *b;
    }

    std::ostream & operator<< (std::ostream & o, B const & b) { o << "B@"; print_addr(o, &b); return o; }

  // D:

    D::D () { prt(this) << "D() "; }
    D::D (D const & d): B(d) { prt(this) << "D(" << d << ") "; }
    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D::D (D && d) { prt(this) << "mD(" << d << ") "; }
    #endif

    D & D::operator= (D const & d)
    { B::operator=(d); std::cout << ' ' << *this << '=' << d << ' '; return *this; }

    void D::vf () const { prt(this) << "vf() "; }

    D::~D () { prt(this) << "~D() "; }

    std::ostream & operator<< (std::ostream & o, D const & d) { o << "D@"; print_addr(o, &d); return o; }
}
