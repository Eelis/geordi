#ifndef TRACKED_HPP
#define TRACKED_HPP

#include <boost/noncopyable.hpp>
#include <iostream>
#include <string>

namespace tracked
{
  namespace detail
  {
    class Tracked
    {
      protected:

        Tracked();
        Tracked(Tracked const &);
        void operator=(Tracked const &);
        virtual ~Tracked();

        #ifdef __GXX_EXPERIMENTAL_CXX0X__
          Tracked(Tracked &&);
          void operator=(Tracked &&);
        #endif

        void set_name(char const *) const;
    };

    unsigned int id(Tracked const &);

    struct info: boost::noncopyable { info(); ~info(); std::ostream & operator()() const; };
  }

  void mute(); void unmute();

  namespace detail
  {
    /* mute/unmute allow us to suppress boring messages that are not relevant to the issue under consideration. Their use can be a bit tedious though. If we only want to get messages for a few statements inside a statement block, we have to do something like:

      geordi { mute(); ..foo.. unmute(); ..bar.. mute(); ..bas.. }

    We therefore introduce some trickery to let us write the above as:

      geordi { ..foo.. TRACK{ ..bar.. } ..bas.. }

    */

    template <typename>
    class focus_t {
      struct mute_in_ctor { mute_in_ctor() { mute(); } };
      static mute_in_ctor m;
      public:
        focus_t() { m; unmute(); }
          // Mentioning m forces it to exist, but only if the focus_t template is ever instantiated. The effect is that if TRACK (which instantiates focus_t) is ever used, mute() will be called before main() is entered.
        ~focus_t() { mute(); }
        operator bool() const { return false; }
    };

    template <typename T> typename focus_t<T>::mute_in_ctor focus_t<T>::m;

  } // namespace detail

  #define TRACK \
    if(::tracked::detail::focus_t<void> const & tracked_detail_focus = ::tracked::detail::focus_t<void>()) ; else

  struct B: protected detail::Tracked
  {
    B();
    B(B const &);

    explicit B(int const x) { set_name("B"); detail::info()() << *this << "*(" << x << ")"; }
    explicit B(char const x) { set_name("B"); detail::info()() << *this << "*(" << x << ")"; }
    explicit B(std::string const & x) { set_name("B"); detail::info()() << *this << "*(" << x << ")"; }
      // This used to be one ctor template, but that messed up snippets like: struct S { operator tracked::B(); }; S s; tracked::B b(s);

    B & operator=(B const &);
    virtual ~B();

    void * operator new(std::size_t);
    void * operator new[](std::size_t);
    void * operator new(std::size_t, std::nothrow_t const &) throw();
    void * operator new[](std::size_t, std::nothrow_t const &) throw();
    void * operator new(std::size_t const, void * const p) throw() { return p; }
    void * operator new[](std::size_t const, void * const p) throw() { return p; }
    void operator delete(void *, std::size_t) throw();
    void operator delete[](void *, std::size_t) throw();

    void f() const;
    virtual void vf() const;

    B & operator++();
    B operator++(int);

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B(B &&);
      B & operator=(B &&);
    #endif

    template<typename C, typename Tr>
    friend std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, B const & b)
    { return o << "B" << detail::id(b); }
  };

  struct D: B
  {
    D();
    D(D const &);

    explicit D(int const & x): B(x) { set_name("D"); detail::info()() << *this << "*(" << x << ")"; }
    explicit D(char const & x): B(x) { set_name("D"); detail::info()() << *this << "*(" << x << ")"; }
    explicit D(std::string const & x): B(x) { set_name("D"); detail::info()() << *this << "*(" << x << ")"; }

    D & operator=(D const &);
    ~D();

    void * operator new(std::size_t);
    void * operator new[](std::size_t);
    void * operator new(std::size_t, std::nothrow_t const &) throw();
    void * operator new[](std::size_t, std::nothrow_t const &) throw();
    void * operator new(std::size_t const, void * const p) throw() { return p; }
    void * operator new[](std::size_t const, void * const p) throw() { return p; }
    void operator delete(void *, std::size_t) throw();
    void operator delete[](void *, std::size_t) throw();

    void f() const;
    virtual void vf() const;

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D(D &&);
      D & operator=(D &&);
    #endif

    template<typename C, typename Tr>
    friend std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, D const & d)
    { return o << "D" << detail::id(d); }
  };

  #ifdef __GXX_EXPERIMENTAL_CXX0X__

    // This "three moves" swap implementation is most likely identical to what libstdc++'s std::swap will be using, so once libstdc++'s std::swap has been updated, the functions below can be removed.

    inline void swap(B & a, B & b) { B tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }
    inline void swap(D & a, D & b) { D tmp(std::move(b)); b = std::move(a); a = std::move(tmp); }

  #endif

} // namespace tracked

#endif // header guard
