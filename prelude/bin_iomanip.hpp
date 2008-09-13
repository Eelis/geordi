
/* Binary integral output manipulator

==== Usage ====

some_stream.imbue(std::locale(some_stream.getloc(), new bin_num_put<>));

some_stream << bin << 123;

Also see the testing section at the bottom of this file.

==== Implementation ====

Implemented using a custom facet deriving from and replacing the std::num_put facet. Seems like a pretty horrible way to do it, but I don't know of a better way yet.

Our facet needs some kind of cue - a flag in ios_base - to know when to display integers in binary. The most legit way would probably be to use an iword. The problem with that is that our flag would be orthogonal to the ios_base::basefield flags; there would be no way to have std::{hex,dec,oct} unset our iword flag. For this reason we instead decide to assign ourselves a seemingly unused ios_base::basefields value, namely 0. Note that this is totally fragile, hackish, and unscalable - but it seems to work.

Curiously, due to the way std::setbase is specified, std::setbase(2) also enables binary output. (Unfortunately, so does std::setbase(N) for any N that is not 8, 10, or 16.)

==== Display of negative values in non-decimal bases ====

Consider:

  std::cout << std::hex << short(-1) << ' ' << int(-1) << ' ' << long(-1) << std::endl;

On my system, this prints:

  ffff ffffffff ffffffffffffffff

This is surprising, because the num_put facet only has output functions for long integers and unsigned long integers. The expected output would be:

  ffffffffffffffff ffffffffffffffff ffffffffffffffff

In a stunning display of disregard for the design principle of separation of responsibilities, [27.6.2.6.2] paragraph 1 in N2369 (the latest standard draft at the time of this writing) achieves the former behavior by dictating that std::basic_ostream::operator<<(short) check to see if (flags()&basefield) equals oct or hex, and if so convert the value to an unsigned short before sending it to the num_put facet. That is, short(-1) is actually sent as the long integer value 0xffff to the num_put facet(!).

Since this special-case code is buried in std::basic_ostream::operator<<(short), there is no way for us to get the same behavior for our "bin" manipulator.

All of the above is also the case for plain int.

==== Misc notes ====

We follow the example set by std::hex and std::oct (using GNU libstdc++ as reference) in:
- converting signed integral arguments to unsigned ones;
- ignoring ios_base::showpos.

See ISO 14882 [22.2.2.2.2].

*/

#ifndef BIN_IOMANIP
#define BIN_IOMANIP

#include <ios>
#include <locale>
#include <limits>
#include <stdexcept>
#include <iterator>
#include <algorithm>
#include <climits>

template <typename Ch = char, typename Out = std::ostreambuf_iterator<Ch> >
class bin_num_put: public std::num_put<Ch, Out>
{
  typedef std::num_put<Ch, Out> base;

  typedef std::ios_base IB;

  #ifdef __GXX_EXPERIMENTAL_CXX0X__
    typedef unsigned long long I;

    virtual Out do_put(Out i, IB & io, Ch const fill, unsigned long const v) const
    { return io.flags() & IB::basefield ? base::do_put(i, io, fill, v) : do_put(i, io, fill, I(v)); }
    virtual Out do_put(Out const i, IB & io, Ch const fill, long long const v) const
    { return io.flags() & IB::basefield ? base::do_put(i, io, fill, v) : do_put(i, io, fill, I(v)); }
  #else
    typedef unsigned long I;
  #endif

  virtual Out do_put(Out i, IB & io, Ch const fill, I const v) const
  {
    IB::fmtflags const f = io.flags();
    if (f & IB::basefield) return base::do_put(i, io, fill, v);

    std::size_t const buf_size = sizeof(I) * CHAR_BIT * 2;
    Ch r[buf_size], * p = r + buf_size;

    {
      std::numpunct<char> const & numpunct = std::use_facet<std::numpunct<char> >(io.getloc());

      I m = 1ul;
      std::streamsize group_len = 0;
      std::string const & grouping = numpunct.grouping();
      std::string::const_iterator g = grouping.begin();

      do
      {
        if (g != grouping.end() && group_len++ == *g)
        {
          *--p = numpunct.thousands_sep(); group_len = 1;
          if (++g == grouping.end()) --g;
        }
        *--p = '0' + bool(v & m);
      }
      while ((m <<= 1) && v >= m);
    }

    std::streamsize const w = io.width(); io.width(0);
    std::streamsize d = r + buf_size - p; if (f & IB::showbase) d += 2;
    IB::fmtflags const adj = f & IB::adjustfield;
    if (adj != IB::left && adj != IB::internal) while (w > d) { *i++ = fill; ++d; }
    if (f & IB::showbase) { *i++ = '0'; *i++ = f & IB::uppercase ? 'B' : 'b'; }
    if (adj == IB::internal) while (w > d) { *i++ = fill; ++d; }
    i = std::copy(p, r + buf_size, i);
    if (adj == IB::left) while (w > d) { *i++ = fill; ++d; }
    return i;
  }

  virtual Out do_put(Out const i, IB & io, Ch const fill, long const v) const
  {
    unsigned long const w = v;
    return io.flags() & IB::basefield ? base::do_put(i, io, fill, v) : do_put(i, io, fill, w);
  }

  // The "proper" thing to do is to add a static std::locale::id const id, but that way bin_num_put does not replace the std::num_put facet in the locale.

  public:

    explicit bin_num_put(std::size_t const refs = 0): base(refs) {}
};

template <typename Ch, typename Tr>
std::basic_ios<Ch, Tr> & bin (std::basic_ios<Ch, Tr> & io)
{
  #if __GNUC_MINOR__ < 3 // 4.3's typeof is bugged.
  if (typeid(std::use_facet<std::num_put<Ch> >(io.getloc())) != typeid(bin_num_put<Ch>))
    throw std::runtime_error("bin manipulator used on basic_ios lacking bin_num_put facet");
      // Can't use std::has_facet because our facet has no id of its own.
  #endif

  io.setf(std::ios_base::fmtflags(0), std::ios_base::basefield);
  return io;
}

#endif // Header guard.

#ifdef BIN_IOMANIP_TEST

#include <iostream>
#include <iomanip>

template <typename Ch, typename Tr>
void test (std::basic_ostream<Ch, Tr> & o)
{
  o
    << "  grouping:  " << 253124118758 << '\n'
    << "  short(-1): " << short(-1) << '\n'
    << "  int(-1):   " << int(-1) << '\n'
    << "  long(-1):  " << long(-1) << '\n'
    << "  left:      [" << std::setw(13) << std::left << 46 << "]\n"
    << "  right:     [" << std::setw(13) << std::right << 46 << "]\n"
    << "  internal:  [" << std::setw(13) << std::internal << 46 << "]\n";
}

int main ()
{
  std::cout.imbue(std::locale(std::locale("dz_BT.utf8"), new bin_num_put<>));
  std::wcout.imbue(std::locale(std::locale("dz_BT.utf8"), new bin_num_put<wchar_t>));
    // dz_BT lets us test grouping and thousands_sep.

  std::cout << "Narrow:\n" << std::showbase << std::setfill('_');
  std::cout << " Hex:\n" << std::hex; test(std::cout);
  std::cout << " Bin:\n" << bin; test(std::cout);

  std::wcout << "\nWide:\n" << std::showbase << std::setfill(L'_');
  std::wcout << " Hex:\n" << std::hex; test(std::wcout);
  std::wcout << " Bin:\n" << bin; test(std::wcout);
}

#endif
