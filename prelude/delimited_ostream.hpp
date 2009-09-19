
#ifndef DELIMITED_OSTREAM_HPP
#define DELIMITED_OSTREAM_HPP

#include <iostream>
#include <ios>
#include <iomanip>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_enum.hpp>
#include <boost/type_traits/is_integral.hpp>

namespace del_ostream_detail
{
  template <typename Ch, typename Tr> std::basic_ostream<Ch, Tr> & default_stream ();
  template <> inline std::ostream & default_stream () { return std::cout; }
  template <> inline std::wostream & default_stream () { return std::wcout; }

  template <typename Ch> Ch const * default_delimiter ();
  template <> inline char const * default_delimiter () { return ", "; }
  template <> inline wchar_t const * default_delimiter () { return L", "; }

  template<typename T> struct take_by_value { enum { value = boost::is_integral<T>::value || boost::is_enum<T>::value }; };
}

template <typename Ch, typename Tr = std::char_traits<Ch> >
class basic_del_ostream // Immutable.
{
  typedef std::basic_ostream<Ch, Tr> Stream;

  typedef basic_del_ostream This;

  Stream * const streamp;
  Ch const * const delimiterp;
  bool const initial;

  Stream & stream () const { return streamp ? *streamp : del_ostream_detail::default_stream<Ch, Tr>(); }
  Ch const * delimiter () const { return delimiterp ? delimiterp : del_ostream_detail::default_delimiter<Ch>(); }

  template <typename Ch2, typename Tr2> friend class basic_del_ostream;

  basic_del_ostream (bool const initial, Stream * const streamp, Ch const * const delimiterp)
    : streamp(streamp), delimiterp(delimiterp), initial(initial) {}

  This set_stream (Stream & s) const { return This(true, &s, delimiterp); }

  template <typename Ch2, typename Tr2>
  basic_del_ostream<Ch2, Tr2> set_stream (std::basic_ostream<Ch2, Tr2> & s) const
  { return basic_del_ostream<Ch2, Tr2>(true, &s, 0 /* Changing to stream of different type loses custom delimiter. */); }

  public:

    basic_del_ostream (): streamp(0), delimiterp(0), initial(true) {}

    // Changing stream:

    template <typename Ch2, typename Tr2>
    friend basic_del_ostream<Ch2, Tr2> operator<< (std::basic_ostream<Ch2, Tr2> & s, This const & c)
    { return c.set_stream(s); }

    // Changing delimiter:

    This operator() (Ch const * const d) const { return This(initial, streamp, d); }

    template <typename Ch2> basic_del_ostream<Ch2> operator() (Ch2 const * const d) const
    { return basic_del_ostream<Ch2>(initial, 0 /* Changing to delimiter of different type loses custom stream. */, d); }

    // Operator<<'s:

    template <typename T>
    typename boost::disable_if<del_ostream_detail::take_by_value<T>, This>::type operator<<(T const & t) const
    { if (!initial) stream() << delimiter(); stream() << t; return This(false, streamp, delimiterp); }

    template <typename T>
    typename boost::enable_if<del_ostream_detail::take_by_value<T>, This>::type operator<<(T const t) const
    { if (!initial) stream() << delimiter(); stream() << t; return This(false, streamp, delimiterp); }

    #define IGNORE(T) This const & operator<< (T const & x) const { stream() << x; return *this; }

    typedef Stream & (* str_f) (Stream &); IGNORE(str_f) // For things like std::endl.
    typedef std::ios_base & (* ios_f) (std::ios_base &); IGNORE(ios_f) // For things like std::hex.
    IGNORE(__typeof__(std::setprecision(std::streamsize())))
    IGNORE(__typeof__(std::setbase(int())))

    #undef IGNORE

    #define NASTY_MANIP(x) \
      This operator<< (__typeof__(x) const & t) const { if (!initial) stream() << delimiter(); stream() << t; return This(true, streamp, delimiterp); }
        // For manipulators that are nasty in the sense that they (potentially) affect the display of the delimiter. These manipulators cannot be immediately sent through like above, but /must/ follow the delimiter (if any).
        // Todo: Change __typeof__ into decltype at some point. Also, the standard probably allows all of these manipulators to return objects of the same type, which would cause overload clashes. :-/
        // Note: In printing a delimiter, we basically assume that an actual field will follow the manipulator. The alternative is to queue up manipulators until a field is outputted, which would be a nasty hassle.

    NASTY_MANIP(std::setw(std::streamsize()))
    NASTY_MANIP(std::setfill(Ch()))
    NASTY_MANIP(std::setiosflags(std::ios_base::fmtflags()))

    #undef NASTY_MANIP

    // This special-casing of particular known manipulators is of course completely unscalable, but so be it.
};

typedef basic_del_ostream<char> del_ostream;
typedef basic_del_ostream<wchar_t> del_wostream;

del_ostream const del;

// Lighter-weight alternative:

template <typename Ch, typename Tr, typename T>
inline typename boost::disable_if<del_ostream_detail::take_by_value<T>, std::basic_ostream<Ch, Tr> >::type &
  operator,(std::basic_ostream<Ch, Tr> & o, T const & t)
{ return o << ", " << t; }

template <typename Ch, typename Tr, typename T>
inline typename boost::enable_if<del_ostream_detail::take_by_value<T>, std::basic_ostream<Ch, Tr> >::type &
  operator,(std::basic_ostream<Ch, Tr> & o, T const t)
{ return o << ", " << t; }

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator, (std::basic_ostream<Ch, Tr> & o, std::ios_base & (* const f) (std::ios_base &))
{ return o << f; }

template <typename Ch, typename Tr>
std::basic_ostream<Ch, Tr> & operator, (std::basic_ostream<Ch, Tr> & o, std::basic_ostream<Ch, Tr> & (* const f) (std::basic_ostream<Ch, Tr> &))
{ return o << f; }

#endif // header guard

#ifdef DELIMITED_OSTREAM_TEST

enum E { a };

struct X
{
  static int const i = 3;
  static E const e = a;
};

int main ()
{
  #define TEST(x) \
    x << "foo" << std::hex << std::showbase << std::uppercase << 45 << std::setw(5) << std::oct << std::setfill('0') << std::setiosflags(std::ios_base::internal) << 9 << std::setprecision(10) << (1/3.0) << std::endl;

  #define WTEST(x) \
    x << L"foo" << std::hex << std::showbase << std::uppercase << 45 << std::setw(5) << std::oct << std::setfill(L'0') << std::setiosflags(std::ios_base::internal) << 9 << std::setprecision(10) << (1/3.0) << std::endl;

  TEST(del) // Narrow: default delimiter and stream.
  TEST(del("; ")) // Narrow: custom delimiter, default stream.
  WTEST(del(L"; ")) // Wide: custom delimiter, default stream.
  std::endl(std::cout);
  TEST(std::cout << del) // Narrow: default delimiter, custom stream.
  TEST(std::cout << del("; ")) // Narrow: custom delimiter and stream.
  TEST(std::cout << del(L"; ")) // Narrow: binding to custom narrow std::cout loses custom wide delimiter.
  std::endl(std::cout);
  WTEST(std::wcout << del) // Wide: default delimiter and stream.
  WTEST(std::wcout << del("; ")) // Wide: binding to custom wide std::wcout loses custom narrow delimiter.
  WTEST(std::wcout << del(L"; ")) // Wide: custom delimiter and stream.

  del_ostream const md = del(" - ");
  std::cout << '\n' << std::dec << md << 1 << 2 << 3 << std::endl;

  std::cout << 3, "foo", std::oct, std::showbase, 50, X::i, X::e, std::endl;
}

#endif // test
