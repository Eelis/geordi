
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
    typedef char name_t;

    struct Idd
    {
      typedef unsigned int id_t;

      id_t const id;
      name_t const name;

      protected:

        explicit Idd (name_t);
        Idd (Idd const &, name_t);

        Idd & operator= (Idd const &);

        #ifdef __GXX_EXPERIMENTAL_CXX0X__
          Idd (Idd &&, name_t);
          Idd & operator= (Idd &&);
        #endif

        virtual ~Idd ();

        void nopillage (char const *) const;

      private:

        bool pillaged;

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

    void silent_exit ();

    typedef std::map<std::pair<void const *, void const *>, std::set<unsigned int> > Allocations;
    extern Allocations allocations;

    template <typename Base, name_t Name>
    struct T: Base
    {
      explicit T (name_t const name = Name): Base(name) { std::cout << ' ' << *this << "* "; }

      explicit T (int const i, name_t const name = Name):
        Base(name) { std::cout << ' ' << *this << "*(" << i << ") "; }

      T (T const & t, name_t const name = Name):
        Base(t, name) { std::cout << ' ' << *this << "*(" << t << ") "; }

      T & operator= (T const & t)
      { Base::operator=(t); std::cout << ' ' << *this << '=' << t << ' '; return *this; }

      #ifdef __GXX_EXPERIMENTAL_CXX0X__

        // Moves are displayed as =>, because -> and <= are operators.

        T (T && t, name_t const name = Name):
          Base(std::move(t), name) { std::cout << ' ' << t << "=>" << *this << "* "; }

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
