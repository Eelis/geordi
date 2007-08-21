
/* Binary integral output manipulator

Implemented using a custom facet deriving from and replacing the std::num_put facet. Seems like a pretty horrible way to do it, but I don't know of a better way yet.

Usage:

  some_stream.imbue(std::locale(some_stream.getloc(), new bin_num_put));

  some_stream << bin << 123;

Our facet needs some kind of cue - a flag in ios_base - to know when to display integers in binary. The most legit way would probably be to use an iword. The problem with that is that our flag would be orthogonal to the ios_base::basefield flags; there would be no way to have std::{hex,dec,oct} unset our iword flag. For this reason we instead decide to assign ourselves a seemingly unused ios_base::basefields value, namely 0. Note that this is totally fragile, hackish, and unscalable - but it seems to work.

We follow the example set by std::hex and std::oct (using GNU libstdc++ as reference) in:
- converting signed integral arguments to unsigned ones;
- ignoring ios_base::showpos.

See ISO 14882 [22.2.2.2.2].

Todo: Grouping; more character types; use numpunct; padding.
*/

#ifndef BIN_IOMANIP
#define BIN_IOMANIP

#include <locale>
#include <iostream>
#include <limits>
#include <stdexcept>

class bin_num_put: public std::num_put<char>
{
  typedef std::num_put<char> base;

  typedef std::ios_base IB;

  iter_type put_integral (iter_type const i, IB & io, char_type const fill, long const & v) const
  {
    IB::fmtflags const f = io.flags();
    if (f & IB::basefield) return base::do_put(i, io, fill, v);
    unsigned long const w = v;
    return put_integral(i, io, fill, w);
  }

  iter_type put_integral (iter_type i, IB & io, char_type const fill, unsigned long const & v) const
  {
    IB::fmtflags const f = io.flags();
    if (f & IB::basefield) return base::do_put(i, io, fill, v);

    if (f & IB::showbase)
    {
      *i++ = '0';
      *i++ = f & IB::uppercase ? 'B' : 'b';
    }

    io.width(0);

    unsigned long m = 1ul;
    if (v) { m <<= std::numeric_limits<unsigned long>::digits - 1; while (!(m & v)) m >>= 1; }
    do *i++ = '0' + bool(m & v); while (m >>= 1);
    return i;
  }

  virtual iter_type do_put(iter_type const i, IB & io, char_type fill, unsigned long v) const
  { return put_integral(i, io, fill, v); }

  virtual iter_type do_put(iter_type const i, IB & io, char_type fill, long v) const
  { return put_integral(i, io, fill, v); }

  // The "proper" thing to do is to add a static std::locale::id const id, but that way bin_num_put does not replace the std::num_put facet in the locale.
};

inline std::ios_base & bin (std::ios_base & io)
{
  #if __GNUC_MINOR__ < 3 // 4.3's typeof is bugged.
  if (typeid(std::use_facet<std::num_put<char> >(io.getloc())) != typeid(bin_num_put))
    throw std::runtime_error("bin manipulator used on ios_base lacking bin_num_put facet");
      // Can't use std::has_facet because our facet has no id of its own.
  #endif
  io.setf(std::ios_base::fmtflags(0), std::ios_base::basefield);
  return io;
}

#endif // Header guard.
