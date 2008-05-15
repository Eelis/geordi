#include "tracked.hpp"
#include <vector>
#include <cassert>
#include <fstream>
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

    std::ofstream nullstream; // We don't open this, so writes are no-ops.

    enum Status { fresh, pillaged, destructed };

    info::info() { if(!muted) std::cout << parsep; }
    info::~info() { if(!muted) std::cout << parsep; }
    std::ostream & info::operator()() const { return muted ? nullstream : std::cout; }

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
      if (Entry * const e = entry(r)) if (e->status != destructed) error()() << "leaked: " << *e;
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
      if (array) info()() << "delete" << v;
      else { assert(v.size() == 1); info()() << "delete(" << v.front() << ")"; }
    }

    void Tracked::set_name(char const * const s) const { entry(this)->name = s; }

    Tracked::Tracked() { make_entry(this); }

    Tracked::Tracked(Tracked const & i) { make_entry(this); assert_status_below(&i, pillaged, "copy"); }

    void Tracked::operator=(Tracked const & r) {
      assert_status_below(this, destructed, "assign to");
      assert_status_below(&r, pillaged, "assign from");
      entry(this)->status = fresh;
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      Tracked::Tracked(Tracked && r)
      { make_entry(this); assert_status_below(&r, pillaged, "move"); entry(&r)->status = pillaged; }

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
        if (!v.empty()) { info()() << "leaked: " << v; abort(); }
      }
    } leakReporter; // Must come after entries, so it will be destructed first.

    unsigned int id(Tracked const & t) { return entry(&t) - &entries.front(); }

  } // namespace detail

  void mute() { detail::muted = true; }
  void unmute() { detail::muted = false; }

  // B:

    B::B() { set_name("B"); detail::info()() << *this << "*"; }
    B::B(B const & b): Tracked(b) { set_name("B"); detail::info()() << *this << "*(" << b << ")"; }
    B & B::operator=(B const & b) { Tracked::operator=(b); detail::info()() << *this << "=" << b; return *this; }
    B::~B() { detail::info()() << *this << "~"; }

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
      detail::info()() << *this << ".f()";
    }

    void B::vf() const {
      assert_status_below(this, detail::pillaged, "call B::vf() on");
      detail::info()() << *this << ".vf()";
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B::B(B && b): Tracked(std::move<Tracked>(b))
      { set_name("B"); detail::info()() << b << "=>" << *this << "*"; }
      B & B::operator=(B && b) {
        Tracked::operator=(std::move<Tracked>(b));
        detail::info()() << b << "=>" << *this;
        return *this;
      }
    #endif

    B & B::operator++() {
      assert_status_below(this, detail::pillaged, "pre-increment");
      detail::info()() << "++" << *this;
      return *this;
    }

    B B::operator++(int) {
      assert_status_below(this, detail::pillaged, "post-increment");
      B const r(*this); operator++(); return r;
    }

  // D:

    D::D() { set_name("D"); detail::info()() << *this << "*"; }
    D::D(D const & d): B(boost::implicit_cast<B const&>(d))
    { set_name("D"); detail::info()() << *this << "*(" << d << ")"; }
    D & D::operator=(D const & d) { B::operator=(d); detail::info()() << *this << "=" << d; return *this; }
    D::~D() { detail::info()() << *this << "~"; }

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
      detail::info()() << *this << ".f()";
    }

    void D::vf() const {
      assert_status_below(this, detail::pillaged, "call D::vf() on");
      detail::info()() << *this << ".vf()";
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D::D(D && d): B(std::move<B>(d)) { set_name("D"); detail::info()() << d << "=>" << *this << "*"; }
      D & D::operator=(D && d) {
        B::operator=(std::move<B>(d));
        detail::info()() << d << "=>" << *this;
        return *this;
      }
    #endif

  // In the above, it looks like there is a lot of code duplication for B and D. Previous implementations of these tracking facilities used clever CRTP helper templates to factor out as much of the common code as possible. However, to prevent the cleverness from showing through in gcc diagnostics, small delegators had to be put in B/D for all operations (in addition to the ones for the constructors which were always there, since constructors cannot be inherited (yet)). In the end, the hassle was not worth the gain, so I reverted back to the simple straightforward approach.

} // namespace tracked
