
#ifndef TRACKED_HPP
#define TRACKED_HPP

#include <iostream>
#include <set>
#include <map>
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <utility>

namespace tracked
{
  namespace detail
  {
    struct Idd
    {
      Idd ();
      Idd (Idd const &);
      Idd & operator= (Idd const &);

      #ifdef __GXX_EXPERIMENTAL_CXX0X__
        Idd (Idd &&);
        Idd & operator= (Idd &&);
      #endif

      virtual ~Idd ();
      unsigned int const id;

      protected:

        void nopillage (char const *) const;

      private:

        bool pillaged;

        virtual char name () const = 0;

        static unsigned int new_id;
        static struct Reg { std::set<Idd const *> s; ~Reg (); } reg;
    };

    void silent_exit ();

    typedef std::map<std::pair<void const *, void const *>, std::set<unsigned int> > Allocations;
    extern Allocations allocations;

    template <typename Base, char Name>
    struct T: Base
    {
      T () { std::cout << ' ' << *this << "* "; }

      explicit T (int const i) { std::cout << ' ' << *this << "*(" << i << ") "; }

      T (T const & t): Base(t) { std::cout << ' ' << *this << "*(" << t << ") "; }

      T & operator= (T const & t)
      { Base::operator=(t); std::cout << ' ' << *this << '=' << t << ' '; return *this; }

      #ifdef __GXX_EXPERIMENTAL_CXX0X__

        // Moves are displayed as =>, because -> and <= are operators.

        T (T && t): Base(std::move(t)) { std::cout << ' ' << t << "=>" << *this << "* "; }

        T & operator= (T && t)
        { Base::operator=(std::move(t)); std::cout << ' ' << t << "=>" << *this << ' '; return *this; }

      #endif

      T & operator++ () { Base::nopillage("pre-increment"); std::cout << " ++" << *this << ' '; return *this; }
      T operator++ (int) { Base::nopillage("post-increment"); T const r (*this); operator++(); return r;  }

      void f () const { Base::nopillage("call T::f() on"); std::cout << ' ' << *this << ".f() "; }
      virtual void vf () const { Base::nopillage("call T::vf() on"); std::cout << ' ' << *this << ".vf() "; }

      virtual ~T () { std::cout << ' ' << *this << "~ "; }

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
          std::cout << " new(" << Name << (array ? "[]" : "") << ") ";
          return r;
        }

        static void op_delete (void * const p, bool const array)
        {
          Allocations::iterator i = allocations.begin();
          for (; i != allocations.end(); ++i) if (i->first.first == p) break;
          if (i == allocations.end()) {
            std::cout << " Error: Tried to delete" << (array ? "[]" : "") << " pointer not pointing to valid allocation.";
            silent_exit();
          }

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
          allocations.erase(i);
          ::operator delete(p);
        }

        char name () const { return Name; }
    };

    template <typename B, char N>
    std::ostream & operator<< (std::ostream & o, T<B, N> const & t) { return o << N << t.id; }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      // This "three moves" swap implementation is most likely identical to what libstdc++'s std::swap will be using, so once libstdc++'s std::swap has been updated, the functions below can be removed.

      template <typename B, char N>
      void swap(T<B, N> & a, T<B, N> & b)
      { T<B, N> tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }

      template <typename B, char N> void swap(T<B, N> && a, T<B, N> & b) { b = std::move(a); }
      template <typename B, char N> void swap(T<B, N> & a, T<B, N> && b) { a = std::move(b); }

    #endif

  } // namespace detail

  typedef detail::T<detail::Idd, 'B'> B;
  typedef detail::T<B, 'D'> D;

} // namespace tracked

#endif // header guard
