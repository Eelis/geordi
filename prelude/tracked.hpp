#ifndef TRACKED_HPP
#define TRACKED_HPP

#include <iosfwd>
#include <new>

namespace tracked
{
  namespace detail
  {
    class Tracked
    {
      protected:

        Tracked();
        Tracked(Tracked const &);
        #if __cplusplus >= 201103
        Tracked(Tracked &&);
        #endif

        void operator=(Tracked const &);
        #if __cplusplus >= 201103
        void operator=(Tracked &&);
        #endif

        ~Tracked();

        void set_name(char const *) const;
    };
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
    #if __cplusplus >= 201103
    B(B &&);
    #endif

    B & operator=(B const &);
    #if __cplusplus >= 201103
    B & operator=(B &&);
    #endif
    virtual ~B();

    void * operator new(std::size_t);
    void * operator new[](std::size_t);
    #if __cplusplus >= 201103
    void * operator new(std::size_t, std::nothrow_t const &) throw();
    void * operator new[](std::size_t, std::nothrow_t const &) throw();
    #endif
    void * operator new(std::size_t const, void * const p) throw() { return p; }
    void * operator new[](std::size_t const, void * const p) throw() { return p; }
    void operator delete(void *, std::size_t) throw();
    void operator delete[](void *, std::size_t) throw();

    void f() const;
    virtual void vf() const;

    B & operator++();
    B operator++(int);

    void operator*() const;

    template<typename C, typename Tr>
    friend std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> &, B const &);

    private:
      template<typename C, typename Tr> void print(std::basic_ostream<C, Tr> &) const;
  };

  bool operator<(B const &, B const &);
  bool operator==(B const &, B const &);
  inline bool operator<=(B const & x, B const & y) { return x == y || x < y; }
  inline bool operator>(B const & x, B const & y) { return y < x; }
  inline bool operator>=(B const & x, B const & y) { return y <= x; }
  inline bool operator!=(B const & x, B const & y) { return !(x == y); }

  struct D: B
  {
    D();
    D(D const &);
    #if __cplusplus >= 201103
    D(D &&);
    #endif

    D & operator=(D const &);
    #if __cplusplus >= 201103
    D & operator=(D &&);
    #endif
    ~D();

    void * operator new(std::size_t);
    void * operator new[](std::size_t);
    #if __cplusplus >= 201103
    void * operator new(std::size_t, std::nothrow_t const &) throw();
    void * operator new[](std::size_t, std::nothrow_t const &) throw();
    #endif
    void * operator new(std::size_t const, void * const p) throw() { return p; }
    void * operator new[](std::size_t const, void * const p) throw() { return p; }
    void operator delete(void *, std::size_t) throw();
    void operator delete[](void *, std::size_t) throw();
    void operator delete(void *) throw() {}

    void f() const;
    virtual void vf() const;

    template<typename C, typename Tr>
    friend std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> &, D const &);

    private:
      template<typename C, typename Tr> void print(std::basic_ostream<C, Tr> &) const;
  };

} // namespace tracked

#endif // header guard
