
#ifndef TRACKED_HPP
#define TRACKED_HPP

#include <iostream>
#include <set>
#include <map>
#include <cstdlib>
#include <string>
#include <cstdio>
#include <cassert>
#include <utility>

namespace geordi { void abort(); }

namespace tracked
{
  namespace detail
  {
    extern bool muted;
      // Does not mute errors (such as calling functions on pillaged objects).

    typedef char name_t;

    struct Idd
    {
      protected:

        typedef unsigned int id_t;

        id_t const id;
        name_t const name; // of the most derived class

        explicit Idd (name_t);
        Idd (Idd const &, name_t);

        Idd & operator= (Idd const &);

        #ifdef __GXX_EXPERIMENTAL_CXX0X__
          Idd (Idd &&, name_t);
          Idd & operator= (Idd &&);
        #endif

        virtual ~Idd ();

        void assert_not_pillaged (std::string const & s) const;
        void assert_not_destructed (std::string const & s) const;

      private:

        enum { fresh, pillaged, destructed } status;

        static id_t new_id;

        struct Reg
        {
          typedef std::map<id_t, name_t> map;
            // This used to be a std::set<Idd const *>, but that broke when ~Reg tried to print the name and id of an Idd that was leaked and yet no longer around (for instance because it was placement-new'd into a stack-allocated buffer).
          map s;
          ~Reg ();
        };

        static Reg reg;
    };

    typedef std::map<std::pair<void const *, void const *>, std::set<unsigned int> > Allocations;
    extern Allocations allocations;

    struct U: virtual Idd
    {
      U(): Idd('X') {}
      void operator=(U const & u) { Idd::operator=(u); }
      #ifdef __GXX_EXPERIMENTAL_CXX0X__
        void operator=(U && u) { Idd::operator=(std::move(u)); }
      #endif
    };

    template <typename Derived, typename Base, name_t Name>
    struct T: Base
    {
      Derived & derived_this() { return *static_cast<Derived *>(this); }
      Derived const & derived_this() const { return *static_cast<Derived *>(this); }

      T (): Idd('X') { if (!muted) std::cout << ' ' << *this << "* "; }

      explicit T (int const i): Idd('X') { if (!muted) std::cout << ' ' << *this << "*(" << i << ") "; }

      T (T const & t): Idd('X'), Base(t) { if (!muted) std::cout << ' ' << *this << "*(" << t << ") "; }

      T & operator= (T const & t)
      { Base::operator=(t); if (!muted) std::cout << ' ' << *this << '=' << t << ' '; return *this; }

      #ifdef __GXX_EXPERIMENTAL_CXX0X__

        // Moves are displayed as =>, because -> and <= are operators.

        T (T && t): Idd('X'), Base(std::move(t)) { if (!muted) std::cout << ' ' << t << "=>" << *this << "* "; }

        Derived & operator= (T && t)
        {
          Base::operator=(std::move(t));
          if (!muted) std::cout << ' ' << t << "=>" << *this << ' ';
          return derived_this();
        }

      #endif

      Derived & operator++ ()
      {
        Base::assert_not_pillaged("pre-increment");
        if (!muted) std::cout << " ++" << *this << ' ';
        return derived_this();
      }

      Derived operator++ (int)
      {
        Base::assert_not_pillaged("post-increment");
        Derived const r(derived_this());
        operator++(); return r;
      }

      void f () const
      {
        Base::assert_not_pillaged(std::string("call ") + Name + "::f() on");
        if (!muted) std::cout << ' ' << *this << ".f() ";
      }

      virtual void vf () const
      {
        Base::assert_not_pillaged(std::string("call ") + Name + "::vf() on");
        if (!muted) std::cout << ' ' << *this << ".vf() ";
      }

      virtual ~T () = 0;

      // normal new:
      void * operator new (std::size_t const s) { return op_new(s, false, ::operator new(s)); }
      void * operator new[] (std::size_t const s) { return op_new(s, true, ::operator new(s)); }

      // placement new:
      void * operator new (std::size_t const, void * const p) throw () { return p; }
      void * operator new[] (std::size_t const, void * const p) throw () { return p; }

      // nothrow new:
      void * operator new (std::size_t const s, std::nothrow_t const & t) throw ()
      { return op_new(s, false, ::operator new (s, t)); }
      void * operator new[] (std::size_t const s, std::nothrow_t const & t) throw ()
      { return op_new(s, true, ::operator new (s, t)); }

      void operator delete (void * const p) throw () { op_delete(p, false); }
      void operator delete[] (void * const p) throw () { op_delete(p, true); }

      private:

        static void * op_new (std::size_t const s, bool const array, void * const r)
        {
          if (!r) return 0;
          allocations.insert(std::make_pair(std::make_pair(r, static_cast<char *>(r) + s), std::set<unsigned int>()));
          if (!muted) std::cout << " new(" << Name << (array ? "[]" : "") << ") ";
          return r;
        }

        static void op_delete (void * const p, bool const array)
        {
          Allocations::iterator i = allocations.begin();
          for (; i != allocations.end(); ++i) if (i->first.first == p) break;
          if (i == allocations.end()) {
            std::cout << " Error: Tried to delete" << (array ? "[]" : "") << " pointer not pointing to valid allocation.";
            geordi::abort();
          }

          if (!muted)
          {
            std::set<unsigned int> const & ids (i->second);
            std::cout << " delete";
            if (array) {
              typename std::set<unsigned int>::const_iterator b = ids.begin(), e = ids.end();
              std::cout << '[';
              if (b != e) { std::cout << Name << *b; while (++b != e) std::cout << ',' << Name << *b; }
              std::cout << ']';
            } else {
              assert(ids.size() == 1);
              std::cout << '(' << Name << *(ids.begin()) << ')';
            }
            std::cout << ' ';
          }

          allocations.erase(i);
          ::operator delete(p);
        }

        friend std::ostream & operator<< (std::ostream & o, T const & t) { return o << Name << t.id; }
    };

    template <typename Derived, typename Base, name_t Name>
    T<Derived, Base, Name>::~T() { if (!muted) std::cout << ' ' << *this << "~ "; }

  } // namespace detail

  inline void mute () { detail::muted = true; }
  inline void unmute () { detail::muted = false; }

  struct B: private virtual detail::Idd, detail::T<B, detail::U, 'B'>
  {
    typedef detail::T<B, detail::U, 'B'> Base;

    B();
    B(B const &);
    explicit B(int);

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B(B &&);
      B & operator=(B &&);
    #endif
  };

  struct D: private virtual detail::Idd, detail::T<D, B, 'D'>
  {
    typedef detail::T<D, B, 'D'> Base;

    D();
    D(D const & d);
    explicit D(int);

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D(D &&);
      D & operator=(D &&);
    #endif
  };

  // B and D used to be mere typedefs for detail::T<detail::Idd, 'B'> and detail::T<B, 'D'> (back when T did not yet use the CRTP). However, with that approach, b.~B() does not work.

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

    // This "three moves" swap implementation is most likely identical to what libstdc++'s std::swap will be using, so once libstdc++'s std::swap has been updated, the functions below can be removed.

    inline void swap(B & a, B & b) { B tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }
    inline void swap(D & a, D & b) { D tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }

  #endif

} // namespace tracked

#endif // header guard
