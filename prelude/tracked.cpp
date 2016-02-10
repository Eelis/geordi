#include "tracked.hpp"
#include <vector>
#include <cassert>
#include <cstdlib>
#include "geordi.hpp"

namespace tracked
{
  namespace detail
  {
    using geordi::error;

    bool muted = false;

    struct info
    {
      info() { if (!muted) std::cout << geordi::parsep; }
      ~info() { if (!muted) std::cout << geordi::parsep; }

      std::ostream & operator()() const
      {
        static std::ostream s(0); // used as null sink
        return muted ? s : std::cout;
      }

      private:
        info(info const &);
        info & operator=(info const &);
    };

    enum Status { fresh, pillaged, destructed };

    struct Entry {
      Tracked const * p;
      char const * name;
      Status status;
    };

    typedef std::vector<Entry> Entries;

    Entries & entries() { static Entries * p = new Entries; return *p; }
      // Keeping track of Trackeds outside of the objects themselves allows us to give nice diagnostics for operations on objects that have already perished.
      // Invariant: If multiple entries have identical p, then all but the last have status==destructed.
      // Todo: not good enough

    std::ptrdiff_t id(Entry const & e) { return &e - &entries().front(); }


    void print(Entry const & e) { std::printf("%s%lu", e.name, id(e)); }

    Entry * entry(Tracked const * const r) {
      for (Entries::reverse_iterator i(entries().rbegin()); i != entries().rend(); ++i)
        if (i->p == r) return &*i;
      return 0;
    }

    std::ptrdiff_t id(Tracked const & t) { return id(*entry(&t)); }

    std::ostream & operator<<(std::ostream & o, Entry const & e)
    { return o << e.name << id(e); }

    void make_entry(Tracked const * const r) {
      if (Entry * const e = entry(r)) if (e->status != destructed) error()() << "leaked: " << *e << '.';
      Entry const e = { r, "?", fresh };
      entries().push_back(e);
    }

    void assert_status_below(Tracked const * const r, Status const st, std::string const & s) {
      Entry * const e = entry(r);
      if (!e) error()() << "tried to " << s << " non-existent object.";
      if (e->status < st) return;
      error()() << "tried to " << s << (e->status == pillaged ? " pillaged " : " destructed ") << *e << '.';
    }

    void * op_new(std::size_t, bool const array, void * const r, char const * const name) {
      if (!r) return 0;
      info()() << "new(" << name << (array ? "[]" : "") << ")";
      return r;
    }

    void op_delete(void * const p, std::size_t const s) {

      ::operator delete(p);

      for (Entries::const_iterator j = entries().begin(); j != entries().end(); ++j)
        if (p <= j->p && static_cast<void const *>(j->p) <= static_cast<char *>(p) + s)
        {
          info()() << "delete(" << *j << ")";
          return;
        }
    }

    void op_array_delete(void * const p, std::size_t const s) {
      ::operator delete[](p);
      info i;
      i() << "delete[";
      bool first = true;
      for (Entries::const_iterator j = entries().begin(); j != entries().end(); ++j)
        if (p <= j->p && static_cast<void const *>(j->p) <= static_cast<char *>(p) + s)
        {
          if (first) { first = false; }
          else i() << ", ";
          i() << *j;
        }
      i() << ']';
    }

    void Tracked::set_name(char const * const s) const { entry(this)->name = s; }

    Tracked::Tracked() { make_entry(this); }

    Tracked::Tracked(Tracked const & i) { assert_status_below(&i, pillaged, "copy"); make_entry(this); }

    void Tracked::operator=(Tracked const & r) {
      assert_status_below(this, destructed, "assign to");
      assert_status_below(&r, pillaged, "assign from");
      entry(this)->status = fresh;
    }

    #if __cplusplus >= 201103
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

  } // namespace detail

  void mute() { detail::muted = true; }
  void unmute() { detail::muted = false; }

  // B:

    B::B()
    {
      set_name("B");
      detail::info const i;
      print(i());
      i() << '*';
    }

    B::B(B const & b)
      : Tracked(b)
    {
      set_name("B");
      detail::info const i;
      print(i());
      i() << "*(";
      b.print(i());
      i() << ')';
    }

    B & B::operator=(B const & b) { Tracked::operator=(b); detail::info const i; print(i()); i() << '='; b.print(i()); return *this; }
    B::~B() {
      assert_status_below(this, detail::destructed, "destruct");
      detail::info const i; print(i()); i() << '~';
    }

    void * B::operator new(std::size_t const s)
    { return detail::op_new(s, false, ::operator new(s), "B"); }
    void * B::operator new[](std::size_t const s)
    { return detail::op_new(s, true, ::operator new[](s), "B"); }

    #if __cplusplus >= 201103
    void * B::operator new(std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, false, ::operator new(s, t), "B"); }
    void * B::operator new[](std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, true, ::operator new[](s, t), "B"); }
    #endif

    void B::operator delete(void * const p, std::size_t const s) throw ()
    { detail::op_delete(p, s); }
    void B::operator delete[](void * const p, std::size_t const s) throw ()
    { detail::op_array_delete(p, s); }

    void B::f() const {
      assert_status_below(this, detail::pillaged, "call B::f() on");
      detail::info const i; print(i()); i() << ".f()";
    }

    void B::vf() const {
      assert_status_below(this, detail::pillaged, "call B::vf() on");
      detail::info const i; print(i()); i() << ".vf()";
    }

    #if __cplusplus >= 201103
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

    template<typename C, typename Tr>
    void B::print(std::basic_ostream<C, Tr> & o) const
    { o << 'B' << id(*this); }

    template<typename C, typename Tr>
    std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, B const & b)
    { assert_status_below(&b, detail::pillaged, "read"); b.print(o); return o; }

    template std::ostream & operator<<<char, std::char_traits<char> >(std::ostream &, B const &);
    template std::wostream & operator<<<wchar_t, std::char_traits<wchar_t> >(std::wostream &, B const &);

  // D:

    D::D() { set_name("D"); detail::info const i; print(i()); i() << '*'; }
    D::D(D const & d): B(d)
    { set_name("D"); detail::info const i; print(i()); i() << "*("; d.print(i()); i() << ')'; }

    D & D::operator=(D const & d) { B::operator=(d); detail::info const i; print(i()); i() << '='; d.print(i()); return *this; }
    D::~D() {
      assert_status_below(this, detail::destructed, "destruct");
      detail::info const i; print(i()); i() << '~';
    }

    void * D::operator new(std::size_t const s)
    { return detail::op_new(s, false, ::operator new(s), "D"); }
    void * D::operator new[](std::size_t const s)
    { return detail::op_new(s, true, ::operator new[](s), "D"); }
    #if __cplusplus >= 201103
    void * D::operator new(std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, false, ::operator new(s, t), "D"); }
    void * D::operator new[](std::size_t const s, std::nothrow_t const & t) throw ()
    { return detail::op_new(s, true, ::operator new[](s, t), "D"); }
    #endif
    void D::operator delete(void * const p, std::size_t const s) throw ()
    { detail::op_delete(p, s); }
    void D::operator delete[](void * const p, std::size_t const s) throw ()
    { detail::op_array_delete(p, s); }

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
    { o << 'D' << id(*this); }

    template<typename C, typename Tr>
    std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, D const & d)
    { assert_status_below(&d, detail::pillaged, "read"); d.print(o); return o; }

    template std::ostream & operator<<<char, std::char_traits<char> >(std::ostream &, D const &);
    template std::wostream & operator<<<wchar_t, std::char_traits<wchar_t> >(std::wostream &, D const &);

    #if __cplusplus >= 201103
    D::D(D && d): B(std::move(d))
    { set_name("D"); detail::info const i; d.print(i()); i() << "=>"; print(i()); i() << '*'; }
    D & D::operator=(D && d) {
      B::operator=(std::move(d));
      detail::info const i; d.print(i()); i() << "=>"; print(i());
      return *this;
    }
    #endif

  // In the above, it looks like there is a lot of code duplication for B and D. Previous implementations of these tracking facilities used clever CRTP helper templates to factor out as much of the common code as possible. However, to prevent the cleverness from showing through in gcc diagnostics, small delegators had to be put in B/D for all operations (in addition to the ones for the constructors which were always there, since constructors cannot be inherited (yet)). In the end, the hassle was not worth the gain, so I reverted back to the simple straightforward approach.

  void atexit()
  {
    bool first = true;
    for (detail::Entries::const_iterator
        i = detail::entries().begin();
        i != detail::entries().end();
        ++i)
      if (i->status != detail::destructed)
      {
        if (first) { std::printf("%sleaked: ", geordi::parsep); first = false; }
        else std::printf(", ");

        print(*i);
      }

    if (!first)
    {
      std::printf(".");
      abort();
    }
  }

} // namespace tracked
