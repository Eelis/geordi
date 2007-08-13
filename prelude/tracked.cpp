
#include "tracked.hpp"

#include "foreach.hpp"

namespace tracked
{
  namespace detail
  {
    unsigned int Idd::new_id = 0;

    Idd::Idd (): id(new_id++), pillaged(false)
    {
      reg.s.insert(this);
      for (Allocations::iterator i = allocations.begin(); i != allocations.end(); ++i)
        if (i->first.first <= this && this < i->first.second) { i->second.insert(id); break; }
    }

    Idd::Idd (Idd const & i): id(new_id++), pillaged(false)
    { i.nopillage("copy"); reg.s.insert(this); }

    Idd & Idd::operator= (Idd const & i)
    { i.nopillage("assign from"); pillaged = false; return *this; }

    void Idd::nopillage (char const * const s) const
    {
      if (!pillaged) return;
      std::cout << " Error: Tried to " << s << " pillaged " << name() << id << '.' << std::flush;
      std::fclose(stdout); // Otherwise we get leak messages.
      std::exit(0);
    }

    #ifdef __GXX_EXPERIMENTAL_CXX0X__

      Idd::Idd (Idd && i): id(new_id++), pillaged(false)
      { i.nopillage("move"); i.pillaged = true; reg.s.insert(this); }

      Idd & Idd::operator= (Idd && i)
      { i.nopillage("move-assign from"); pillaged = false; i.pillaged = true; return *this; }

    #endif

    Idd::~Idd () { reg.s.erase(this); }

    Allocations allocations;

    Idd::Reg Idd::reg;

    Idd::Reg::~Reg ()
    {
      if (s.empty()) return;
      std::cout << " || leaked:";
      BOOST_FOREACH(Idd const * const b, s) std::cout << ' ' << b->name() << b->id;
    }
  }
}
