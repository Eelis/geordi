
#include "tracked.hpp"

#include "foreach.hpp"

#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cstdio>

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
      ulong const i = ulong(p);

      ulong const bound =
        #ifdef __x86_64__
          0x1000000000;
        #else
          0x10000000;
        #endif

      o << (i < bound ? 'H' : 'S') << std::hex << std::setfill('0') << std::setw(2) << (i & 0xfful);
        // Stack/heap distinction based on pretty shameless assumption that just happens to hold on tested machines.
    }
  }

  // B:

    B::B (): pillaged(false) { reg.s.insert(this); prt(this) << "B() "; }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B::B (B && b): pillaged(false)
      {
        b.nopillage("move from");
        b.pillaged = true;
        reg.s.insert(this);
        prt(this) << "mB(" << b << ") "; // 'm' for "move"
      }
    #endif

    B::B (int const i): pillaged(false) { reg.s.insert(this); prt(this) << "B(" << i << ") "; }

    void B::nopillage (char const * const s) const
    {
      if (!pillaged) return;
      std::cout << " Error: Tried to " << s << " pillaged " << *this << '.' << std::flush;
      std::fclose(stdout); // Otherwise we get leak messages.
      std::exit(0);
    }

    B::B (B const & b): pillaged(false)
    {
      b.nopillage("copy");
      reg.s.insert(this); prt(this) << "B(" << b << ") ";
    }

    B & B::operator= (B const & b)
    {
      b.nopillage("assign from");
      pillaged = false;
      std::cout << ' ' << *this << '=' << b << ' '; return *this;
    }

    B & B::operator++ () { nopillage("pre-increment"); std::cout << " ++" << *this << ' '; return *this; }
    B B::operator++ (int) { nopillage("post-increment"); B const r (*this); operator++(); return r;  }

    void B::f () const { nopillage("call B::f() on"); prt(this) << "f() "; }
    void B::vf () const { nopillage("call B::vf() on"); prt(this) << "vf() "; }

    B::~B () { reg.s.erase(this); prt(this) << "~B() "; }

    void * B::operator new (size_t const s) { void * r = ::operator new(s); std::cout << " new"; return r; }
    void B::operator delete (void * p) { std::cout << "deleted "; ::operator delete(p); }

    void * B::operator new[] (size_t const s) { void * r = ::operator new[](s); std::cout << " new[]"; return r; }
    void B::operator delete[] (void * p) { std::cout << "delete[]d "; ::operator delete[](p); }

      // These new/delete operators are not called when B's are new'd as part of bigger objects, so B's ctors/dtor are still the appropriate place for reg insertion/erasure.

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
