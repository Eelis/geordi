
#include "tracked.hpp"

#include <cassert>

namespace geordi { void abort (); }

namespace tracked
{
  namespace detail
  {
    bool muted = false;

    std::string unqualified(std::string const & s)
    {
      std::string::size_type const i = s.find_last_of(":");
      if (i == std::string::npos) return s;
      return s.substr(i + 1);
    }

    void Root::op_delete (void * const p, bool const array, std::size_t const s, std::string const & name)
    {
      if (array) ::operator delete[](p);
      else ::operator delete(p);
      if (muted) return;
      std::vector<boost::reference_wrapper<Entry const> > v;
      for (Entries::const_iterator j = entries.begin(); j != entries.end(); ++j)
        if (p <= j->p && j->p <= static_cast<char *>(p) + s) v.push_back(boost::cref(*j));
      if (array) std::cout << " delete" << v << ' ';
      else { assert(v.size() == 1); std::cout << " delete(" << v.front() << ") "; }
    }

    Root::Root() { make_entry(); }

    Root::Root(Root const & i) { make_entry(); i.assert_status_below(pillaged, "copy"); }

    Root & Root::operator=(Root const & r)
    {
      assert_status_below(destructed, "assign to");
      r.assert_status_below(pillaged, "assign from");
      entry()->status = fresh;
      return *this;
    }

    void Root::assert_status_below(Status const st, std::string const & s) const
    {
      if (Entry * const e = entry())
      {
        if (e->status < st) return;
        std::cout << " Error: Tried to " << s << (e->status == pillaged ? " pillaged " : " destructed ") << *e << '.';
        geordi::abort();
      }
      else { std::cout << " Error: Tried to " << s << " non-existent object."; geordi::abort(); }
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      Root::Root(Root && r)
      { make_entry(); r.assert_status_below(pillaged, "move"); r.entry()->status = pillaged; }

      Root & Root::operator=(Root && r)
      {
        assert_status_below(destructed, "move-assign to");
        r.assert_status_below(pillaged, "move");
        entry()->status = fresh;
        r.entry()->status = pillaged;
        return *this;
      }

    #endif

    Root::~Root () { assert_status_below(destructed, "re-destruct"); entry()->status = destructed; }

    Root::Entries Root::entries;
    Root::LeakReporter Root::leakReporter; // Must come after entries, so it will be destructed first.

    void Root::make_entry() const
    {
      if (Entry * const e = entry())
        if (e->status != destructed) { std::cout << "Error: Leaked: " << *e; geordi::abort(); }
      entries.push_back(Entry(this));
    }

    Root::Entry * Root::entry() const
    {
      for (Entries::reverse_iterator i(entries.rbegin()); i != entries.rend(); ++i)
        if (i->p == this) return &*i;
      return 0;
    }

    unsigned int Root::id() const { return entry() - &entries.front(); }

    Root::LeakReporter::~LeakReporter()
    {
      std::vector<boost::reference_wrapper<Entry const> > v;
      for (Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
        if (i->status != destructed) v.push_back(boost::cref(*i));
      if (!v.empty()) { std::cout << " Leaked: " << v; geordi::abort(); }
    }

  } // namespace detail

  // B:

    B::B() {}
    B::B(B const & b): Base(b) {}
    B::B(int const i): Base(i) {}
    B::B(char const c): Base(c) {}
    B::B(std::string const & s): Base(s) {}
    B & B::operator=(B const & b) { return Base::operator=(b); }
    B::~B() {}

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B::B(B && b): Base(std::move<Base>(b)) {}
      B & B::operator=(B && b) { return Base::operator=(std::move<Base>(b)); }
    #endif

  // D:

    D::D() {}
    D::D(D const & d): Base(d) {}
    D::D(int const i): Base(i) {}
    D::D(char const c): Base(c) {}
    D::D(std::string const & s): Base(s) {}
    D & D::operator=(D const & b) { return Base::operator=(b); }
    D::~D() {}

    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D::D(D && d): Base(std::move<Base>(d)) {}
      D & D::operator=(D && d) { return Base::operator=(std::move<Base>(d)); }
    #endif

} // namespace tracked
