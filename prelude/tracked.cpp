
#include "tracked.hpp"

#include "foreach.hpp"

namespace geordi { void abort (); }

namespace tracked
{
  namespace detail
  {
    bool muted = false;

    id_t Idd::new_id = 0;

    Idd::Idd (name_t const name): id(new_id++), name(name), status(fresh)
    {
      reg.s.insert(std::make_pair(id, name));
      for (Allocations::iterator i = allocations.begin(); i != allocations.end(); ++i)
        if (i->first.first <= this && this < i->first.second) { i->second.insert(id); break; }
    }

    Idd::Idd (Idd const & i, name_t const name): id(new_id++), name(name), status(fresh)
    { i.assert_not_pillaged("copy"); reg.s.insert(std::make_pair(id, name)); }

    Idd & Idd::operator= (Idd const & i)
    {
      assert_not_destructed("assign to");
      i.assert_not_pillaged("assign from");
      status = fresh;
      return *this;
    }

    void Idd::assert_not_pillaged (std::string const & s) const
    {
      if (status == fresh) return;
      std::cout << " Error: Tried to " << s << (status == pillaged ? " pillaged " : " destructed ") << name << id << '.';
      geordi::abort();
    }

    void Idd::assert_not_destructed (std::string const & s) const
    {
      if (status != destructed) return;
      std::cout << " Error: Tried to " << s << " destructed " << name << id << '.';
      geordi::abort();
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      Idd::Idd (Idd && i, name_t const name): id(new_id++), name(name), status(fresh)
      { i.assert_not_pillaged("move"); i.status = pillaged; reg.s.insert(std::make_pair(id, name)); }

      Idd & Idd::operator= (Idd && i)
      {
        assert_not_destructed("move-assign to");
        i.assert_not_pillaged("move");
        status = fresh;
        i.status = pillaged;
        return *this;
      }

    #endif

    Idd::~Idd () { assert_not_destructed("re-destruct"); status = destructed; reg.s.erase(id); }

    Allocations allocations;

    Idd::Reg Idd::reg;

    Idd::Reg::~Reg ()
    {
      if (s.empty()) return;
      std::cout << " || leaked:";
      BOOST_FOREACH(map::value_type const & p, s) std::cout << ' ' << p.second << p.first;
    }
  }
}
