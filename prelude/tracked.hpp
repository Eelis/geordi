#ifndef TRACKED_HPP
#define TRACKED_HPP

#include <iostream>
#include <string>
#include <boost/ref.hpp>

#include "type_strings.hpp"
#include "more_ostreaming.hpp"

namespace geordi { void abort(); }

namespace tracked
{
  namespace detail { extern bool muted; }
  inline void mute() { detail::muted = true; }
  inline void unmute() { detail::muted = false; }
    // Note: Terminal errors such as calling functions on pillaged objects are never muted.

  namespace detail
  {
    std::string unqualified(std::string const &);

    class Root
    {
      protected:

        unsigned int id() const;

        Root();
        template <typename T> explicit Root(T const &) { make_entry(); }
        Root(Root const &);
        Root & operator=(Root const &);
        virtual ~Root();

        #ifdef __GXX_EXPERIMENTAL_CXX0X__
          Root(Root &&);
          Root & operator=(Root &&);
        #endif

        enum Status { fresh, pillaged, destructed };
        void assert_status_below(Status, std::string const & s) const;

        template <typename T> void set_name() { entry()->name = unqualified(TYPE(T)); }

        static void op_delete(void * const p, bool const array, std::size_t const s, std::string const & name);

      private:

        struct Entry
        {
          void const * p;
          std::string name;
          Status status;
          explicit Entry(void const * const p): p(p), name("?"), status(fresh) {}
        };

        friend std::ostream & operator<<(std::ostream & o, Entry const & e)
        { return o << e.name << &e - &entries.front(); }

        typedef std::vector<Entry> Entries;
        static Entries entries;
          // Invariant: If multiple entries have identical p, then all but the last have status==destructed.
          // Invariant: Entry objects only ever exist as temporaries and in the entries variable.
          // Keeping track of names and IDs outside of the objects themselves allows us to give nice diagnostics for operations on objects that have already perished.

        void make_entry() const;
        Entry * entry() const;

        struct LeakReporter { ~LeakReporter(); };
        static LeakReporter leakReporter;
    };

    template <typename Derived, typename Base>
    struct Noisy: Base
    {
      static std::string const name;

      Derived & derived_this() { return *static_cast<Derived *>(this); }
      Derived const & derived_this() const { return *static_cast<Derived *>(this); }

      Noisy() { Base::template set_name<Derived>(); if (!muted) std::cout << ' ' << *this << "* "; }

      template <typename X>
      Noisy(X const & x): Base(x)
      { Base::template set_name<Derived>(); if (!muted) std::cout << ' ' << *this << "*(" << x << ") "; }

      Derived & operator=(Noisy const & t)
      { Base::operator=(t); if (!muted) std::cout << ' ' << *this << '=' << t << ' '; return derived_this(); }

      #ifdef __GXX_EXPERIMENTAL_CXX0X__

        // Moves are displayed as =>, because -> and <= are operators.

        Noisy(Noisy && n): Base(std::move<Base>(n))
        { if (!muted) std::cout << ' ' << n << "=>" << *this << "* "; }

        Derived & operator=(Noisy && n)
        {
          Base::operator=(std::move<Base>(n));
          if (!muted) std::cout << ' ' << n << "=>" << *this << ' ';
          return derived_this();
        }

      #endif

      Derived & operator++ ()
      {
        Base::assert_status_below(Base::pillaged, "pre-increment");
        if (!muted) std::cout << " ++" << *this << ' ';
        return derived_this();
      }

      Derived operator++ (int)
      {
        Base::assert_status_below(Base::pillaged, "post-increment");
        Derived const r(derived_this());
        operator++(); return r;
      }

      void f () const
      {
        Base::assert_status_below(Base::pillaged, "call " + name + "::f() on");
        if (!muted) std::cout << ' ' << *this << ".f() ";
      }

      virtual void vf () const
      {
        Base::assert_status_below(Base::pillaged, "call " + name + "::vf() on");
        if (!muted) std::cout << ' ' << *this << ".vf() ";
      }

      virtual ~Noisy() = 0;

      void * operator new (std::size_t const s) { return op_new(s, false, ::operator new(s)); }
      void * operator new[] (std::size_t const s) { return op_new(s, true, ::operator new[](s)); }

      void * operator new (std::size_t const, void * const p) throw () { return p; }
      void * operator new[] (std::size_t const, void * const p) throw () { return p; }

      void * operator new (std::size_t const s, std::nothrow_t const & t) throw ()
      { return op_new(s, false, ::operator new(s, t)); }
      void * operator new[] (std::size_t const s, std::nothrow_t const & t) throw ()
      { return op_new(s, true, ::operator new[](s, t)); }

      void operator delete (void * const p, std::size_t const s) throw ()
      { Base::op_delete(p, false, s, name); }
      void operator delete[] (void * const p, std::size_t const s) throw ()
      { Base::op_delete(p, true, s, name); }

      private:

        static void * op_new(std::size_t const s, bool const array, void * const r)
        {
          if (!r) return 0;
          if (!muted) std::cout << " new(" << name << (array ? "[]" : "") << ") ";
          return r;
        }

        friend std::ostream & operator<<(std::ostream & o, Noisy const & n) { return o << name << n.id(); }
    };

    template <typename Derived, typename Base>
    std::string const Noisy<Derived, Base>::name(unqualified(TYPE(Derived)));

    template <typename Derived, typename Base>
    Noisy<Derived, Base>::~Noisy() { if (!muted) std::cout << ' ' << *this << "~ "; }

    /* mute/unmute allow us to suppress boring messages that are not relevant to the issue under consideration. Their use can be a bit tedious though. If we only want to get messages for a few statements inside a statement block, we have to do something like:

      geordi { mute(); ..foo.. unmute(); ..bar.. mute(); ..bas.. }

    We therefore introduce some trickery to let us write the above as:

      geordi { ..foo.. TRACK{ ..bar.. } ..bas.. }

    */

    template <typename>
    class focus_t
    {
      struct mute_in_ctor { mute_in_ctor() { mute(); } };
      static mute_in_ctor m;
      public:
        focus_t() { &m; unmute(); }
        ~focus_t() { mute(); }
          // Taking m's address forces it to exist, but only if the focus_t template is ever instantiated. The effect is that if TRACK (which instantiates focus_t) is ever used, mute() will be called before main() is entered.
        operator bool() const { return false; }
    };

    template <typename T> typename focus_t<T>::mute_in_ctor focus_t<T>::m;

  } // namespace detail

  #define TRACK \
    if(::tracked::detail::focus_t<void> const & tracked_detail_focus = ::tracked::detail::focus_t<void>()) ; else

  // For B and D below, the implicitly declared/defined default ctor, copy ctor, assignment operator, and dtor are all symantically correct, but defining them ourselves gives us precious separate compilation. This is also the primary reason why B and D are not simply typedefs for detail::Noisy<detail::Root, 'B'> and detail::Noisy<B, 'D'>. Another problem with such typedefs is that b.~B() does not work.

  struct B: detail::Noisy<B, detail::Root>
  {
    typedef detail::Noisy<B, detail::Root> Base;

    B();
    B(B const &);
    explicit B(int);
    explicit B(char);
    explicit B(std::string const &);
      // We don't use a template for these because then we'd lose precious separate compilation.
    B & operator=(B const &);
    ~B();

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B(B &&);
      B & operator=(B &&);
    #endif
  };

  struct D: detail::Noisy<D, B>
  {
    typedef detail::Noisy<D, B> Base;

    D();
    D(D const &);
    explicit D(int);
    explicit D(char);
    explicit D(std::string const &);
    D & operator=(D const &);
    ~D();

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D(D &&);
      D & operator=(D &&);
    #endif
  };

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

    // This "three moves" swap implementation is most likely identical to what libstdc++'s std::swap will be using, so once libstdc++'s std::swap has been updated, the functions below can be removed.

    inline void swap(B & a, B & b) { B tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }
    inline void swap(D & a, D & b) { D tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }

  #endif

} // namespace tracked

#endif // header guard
