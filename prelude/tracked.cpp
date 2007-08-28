
#include "tracked.hpp"

#include "foreach.hpp"

namespace tracked
{
  namespace detail
  {
    id_t Idd::new_id = 0;

    Idd::Idd (name_t const name): id(new_id++), name(name), pillaged(false)
    {
      reg.s.insert(std::make_pair(id, name));
      for (Allocations::iterator i = allocations.begin(); i != allocations.end(); ++i)
        if (i->first.first <= this && this < i->first.second) { i->second.insert(id); break; }
    }

    Idd::Idd (Idd const & i, name_t const name): id(new_id++), name(name), pillaged(false)
    { i.nopillage("copy"); reg.s.insert(std::make_pair(id, name)); }

    Idd & Idd::operator= (Idd const & i)
    { i.nopillage("assign from"); pillaged = false; return *this; }

    void silent_exit ()
    {
      std::flush(std::cout);
      std::fclose(stdout); // Otherwise we get leak messages.
      std::exit(0);
    }

    void Idd::nopillage (char const * const s) const
    {
      if (!pillaged) return;
      std::cout << " Error: Tried to " << s << " pillaged " << name << id << '.';
      silent_exit();
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      Idd::Idd (Idd && i, name_t const name): id(new_id++), name(name), pillaged(false)
      { i.nopillage("move"); i.pillaged = true; reg.s.insert(std::make_pair(id, name)); }

      Idd & Idd::operator= (Idd && i)
      { i.nopillage("move-assign from"); pillaged = false; i.pillaged = true; return *this; }

    #endif

    Idd::~Idd () { reg.s.erase(id); }

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
