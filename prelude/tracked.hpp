#ifndef TRACKED_HPP
#define TRACKED_HPP

#include <boost/noncopyable.hpp>
#include <iosfwd>
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

    explicit B(int);
    explicit B(char);
    explicit B(std::string const &);
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

    void operator*() const;

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B(B &&);
      B & operator=(B &&);
    #endif

    template<typename C, typename Tr>
    friend std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> &, B const &);

    private:
      template<typename C, typename Tr> void print(std::basic_ostream<C, Tr> &) const;
  };

  struct D: B
  {
    D();
    D(D const &);

    explicit D(int);
    explicit D(char);
    explicit D(std::string const &);

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
    friend std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> &, D const &);

    private:
      template<typename C, typename Tr> void print(std::basic_ostream<C, Tr> &) const;
  };

} // namespace tracked

#endif // header guard
