#include "tracked.hpp"
#include <vector>
#include <cassert>
#include <cstdlib>
#include <sstream>
#include <boost/implicit_cast.hpp>
#include <boost/ref.hpp>
#include <boost/noncopyable.hpp>
#include "more_ostreaming.hpp"
#include "geordi.hpp"

namespace tracked
{
  namespace detail
  {
    using geordi::error;
    using geordi::parsep;

    bool muted = false;

    struct info: boost::noncopyable
    {
      info() { if(!muted) std::cout << parsep; }
      ~info() { if(!muted) std::cout << parsep; }
      std::ostream & operator()() const
      {
        static std::ostream s(0); // used as null sink
        return muted ? s : std::cout;
      }
    };

    enum Status { fresh, pillaged, destructed };

    struct Entry {
      Tracked const * p;
      char const * name;
      Status status;
    };

    typedef std::vector<Entry> Entries;
    Entries entries;
      // Keeping track of Trackeds outside of the objects themselves allows us to give nice diagnostics for operations on objects that have already perished.
      // Invariant: If multiple entries have identical p, then all but the last have status==destructed.

    std::ostream & operator<<(std::ostream & o, Entry const & e)
    { return o << e.name << &e - &entries.front(); }

    Entry * entry(Tracked const * const r) {
      for (Entries::reverse_iterator i(entries.rbegin()); i != entries.rend(); ++i) if (i->p == r) return &*i;
      return 0;
    }

    void make_entry(Tracked const * const r) {
      if (Entry * const e = entry(r)) if (e->status != destructed) error()() << "leaked: " << *e << '.';
      Entry const e = { r, "?", fresh };
      entries.push_back(e);
    }

    void assert_status_below(Tracked const * const r, Status const st, std::string const & s) {
      Entry * const e = entry(r);
      if(!e) error()() << "tried to " << s << " non-existent object.";
      if (e->status < st) return;
      error()() << "tried to " << s << (e->status == pillaged ? " pillaged " : " destructed ") << *e << '.';
    }

    void * op_new(std::size_t const s, bool const array, void * const r, char const * const name) {
      if (!r) return 0;
      info()() << "new(" << name << (array ? "[]" : "") << ")";
      return r;
    }

    void op_delete(void * const p, bool const array, std::size_t const s) {
      if (array) ::operator delete[](p);
      else ::operator delete(p);
      std::vector<boost::reference_wrapper<Entry const> > v;
      for (Entries::const_iterator j = entries.begin(); j != entries.end(); ++j)
        if (p <= j->p && boost::implicit_cast<void const *>(j->p) <= static_cast<char *>(p) + s)
          v.push_back(boost::cref(*j));
      if (array) { info i; i() << "delete["; more_ostreaming::delimit(i(), v); i() << ']'; }
      else { assert(v.size() == 1); info()() << "delete(" << v.front() << ")"; }
    }

    void Tracked::set_name(char const * const s) const { entry(this)->name = s; }

    Tracked::Tracked() { make_entry(this); }

    Tracked::Tracked(Tag const & t): tag(t) { make_entry(this); }

    Tracked::Tracked(Tracked const & i): tag((assert_status_below(&i, pillaged, "copy"), i.tag)) { make_entry(this); }

    void Tracked::operator=(Tracked const & r) {
      assert_status_below(this, destructed, "assign to");
      assert_status_below(&r, pillaged, "assign from");
      entry(this)->status = fresh;
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      Tracked::Tracked(Tracked && r)
      { assert_status_below(&r, pillaged, "move"); make_entry(this); entry(&r)->status = pillaged; }

      void Tracked::operator=(Tracked && r) {
        assert_status_below(this, destructed, "move-assign to");
        assert_status_below(&r, pillaged, "move");
        entry(this)->status = fresh;
        entry(&r)->status = pillaged;
      }

    #endif

    Tracked::~Tracked()
    { assert_status_below(this, destructed, "re-destruct"); entry(this)->status = destructed; }

    struct LeakReporter {
      ~LeakReporter() {
        std::vector<boost::reference_wrapper<Entry const> > v;
        for (Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
          if (i->status != destructed) v.push_back(boost::cref(*i));
        if (!v.empty() && !muted)
        {
          std::ostringstream oss;
            // We don't use cout here because apparently it can be unavailable by the time this code runs (judging by the segfaults I observed when this code used cout).
          more_ostreaming::delimit(oss, v);
          std::printf("%sleaked: %s.", parsep, oss.str().c_str());
          abort();
        }
      }
    } leakReporter; // Must come after entries, so it will be destructed first.

    unsigned int id(Tracked const & t) { return entry(&t) - &entries.front(); }

  } // namespace detail

  void mute() { detail::muted = true; }
  void unmute() { detail::muted = false; }

  // B:

    B::B() { set_name("B"); detail::info const i; print(i()); i() << '*'; }
    B::B(B const & b): Tracked(b) { set_name("B"); detail::info const i; print(i()); i() << "*("; b.print(i()); i() << ')'; }
    B::B(detail::Tag const & t): Tracked(t) { set_name("B"); detail::info const i; print(i()); i() << "*(" << t << ')'; }
    B::B(int const x): Tracked(x) { set_name("B"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
    B::B(char const x): Tracked(x) { set_name("B"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
    B::B(std::string const & x): Tracked(x) { set_name("B"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
    B::B(char const * const x): Tracked(x) { set_name("B"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
      // todo: Delegating ctors should make this much cleaner.
    B & B::operator=(B const & b) { Tracked::operator=(b); detail::info const i; print(i()); i() << '='; b.print(i()); return *this; }
    B::~B() { detail::info const i; print(i()); i() << '~'; }

    void * B::operator new(std::size_t const s)
    { return detail::op_new(s, false, ::operator new(s), "B"); }
    void * B::operator new[](std::size_t const s)
    { return detail::op_new(s, true, ::operator new[](s), "B"); }
    void * B::operator new(std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, false, ::operator new(s, t), "B"); }
    void * B::operator new[](std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, true, ::operator new[](s, t), "B"); }
    void B::operator delete(void * const p, std::size_t const s) throw ()
    { detail::op_delete(p, false, s); }
    void B::operator delete[](void * const p, std::size_t const s) throw ()
    { detail::op_delete(p, true, s); }

    void B::f() const {
      assert_status_below(this, detail::pillaged, "call B::f() on");
      detail::info const i; print(i()); i() << ".f()";
    }

    void B::vf() const {
      assert_status_below(this, detail::pillaged, "call B::vf() on");
      detail::info const i; print(i()); i() << ".vf()";
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B::B(B && b): Tracked(std::move(b))
      { set_name("B"); detail::info const i; b.print(i()); i() << "=>"; print(i()); i() << '*'; }
      B & B::operator=(B && b) {
        Tracked::operator=(std::move(b));
        detail::info const i; b.print(i()); i() << "=>"; print(i());
        return *this;
      }
    #endif

    B & B::operator++() {
      assert_status_below(this, detail::pillaged, "pre-increment");
      detail::info const i; i() << "++"; print(i());
      return *this;
    }

    B B::operator++(int) {
      assert_status_below(this, detail::pillaged, "post-increment");
      B const r(*this); operator++(); return r;
    }

    void B::operator*() const
    {
      assert_status_below(this, detail::pillaged, "dereference");
      detail::info const i; i() << '*'; print(i());
    }

    detail::Tag const & B::tag() const
    { assert_status_below(this, detail::pillaged, "read"); return detail::Tracked::tag; }

    bool operator<(B const & x, B const & y) { return x.tag() < y.tag(); }
    bool operator==(B const & x, B const & y) { return y.tag() == x.tag(); }

    template<typename C, typename Tr>
    void B::print(std::basic_ostream<C, Tr> & o) const
    { o << 'B' << detail::id(*this); }

    template<typename C, typename Tr>
    std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, B const & b)
    { assert_status_below(&b, detail::pillaged, "read"); b.print(o); return o; }

    template std::ostream & operator<<<char, std::char_traits<char> >(std::ostream &, B const &);
    template std::wostream & operator<<<wchar_t, std::char_traits<wchar_t> >(std::wostream &, B const &);

  // D:

    D::D() { set_name("D"); detail::info const i; print(i()); i() << '*'; }
    D::D(D const & d): B(boost::implicit_cast<B const&>(d))
    { set_name("D"); detail::info const i; print(i()); i() << "*("; d.print(i()); i() << ')'; }
    D::D(detail::Tag const & t): B(t) { set_name("D"); detail::info const i; print(i()); i() << "*(" << t << ')'; }
    D::D(int const x): B(x) { set_name("D"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
    D::D(char const x): B(x) { set_name("D"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
    D::D(std::string const & x): B(x) { set_name("D"); detail::info const i; print(i()); i() << "*(" << x << ')'; }
    D::D(char const * const x): B(x) { set_name("D"); detail::info const i; print(i()); i() << "*(" << x << ')'; }

    D & D::operator=(D const & d) { B::operator=(d); detail::info const i; print(i()); i() << '='; d.print(i()); return *this; }
    D::~D() { detail::info const i; print(i()); i() << '~'; }

    void * D::operator new(std::size_t const s)
    { return detail::op_new(s, false, ::operator new(s), "D"); }
    void * D::operator new[](std::size_t const s)
    { return detail::op_new(s, true, ::operator new[](s), "D"); }
    void * D::operator new(std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, false, ::operator new(s, t), "D"); }
    void * D::operator new[](std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, true, ::operator new[](s, t), "D"); }
    void D::operator delete(void * const p, std::size_t const s) throw ()
    { detail::op_delete(p, false, s); }
    void D::operator delete[](void * const p, std::size_t const s) throw ()
    { detail::op_delete(p, true, s); }

    void D::f() const {
      assert_status_below(this, detail::pillaged, "call D::f() on");
      detail::info const i; print(i()); i()<< ".f()";
    }

    void D::vf() const {
      assert_status_below(this, detail::pillaged, "call D::vf() on");
      detail::info const i; print(i()); i() << ".vf()";
    }

    template<typename C, typename Tr>
    void D::print(std::basic_ostream<C, Tr> & o) const
    { o << 'D' << detail::id(*this); }

    template<typename C, typename Tr>
    std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, D const & d)
    { assert_status_below(&d, detail::pillaged, "read"); d.print(o); return o; }

    template std::ostream & operator<<<char, std::char_traits<char> >(std::ostream &, D const &);
    template std::wostream & operator<<<wchar_t, std::char_traits<wchar_t> >(std::wostream &, D const &);

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D::D(D && d): B(std::move(d))
      { set_name("D"); detail::info const i; d.print(i()); i() << "=>"; print(i()); i() << '*'; }
      D & D::operator=(D && d) {
        B::operator=(std::move(d));
        detail::info const i; d.print(i()); i() << "=>"; print(i());
        return *this;
      }
    #endif

  // In the above, it looks like there is a lot of code duplication for B and D. Previous implementations of these tracking facilities used clever CRTP helper templates to factor out as much of the common code as possible. However, to prevent the cleverness from showing through in gcc diagnostics, small delegators had to be put in B/D for all operations (in addition to the ones for the constructors which were always there, since constructors cannot be inherited (yet)). In the end, the hassle was not worth the gain, so I reverted back to the simple straightforward approach.

} // namespace tracked
