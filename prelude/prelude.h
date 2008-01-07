
#define _GLIBCXX_DEBUG
#define _GLIBCXX_DEBUG_PEDANTIC
  // These add a few seconds compile-time on slower machines.

#define _GLIBCXX_CONCEPT_CHECKS // adds approx 1 mbyte to pch

#include "tracked.hpp"
#include "using.hpp"
#include "more_stdlib_ostreaming.hpp"
#include "type_strings.hpp"
#include "delimited_ostream.hpp"
#include "bin_iomanip.hpp"
#include "lvalue_rvalue.hpp"

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  #include "unique_ptr.hpp"
#endif

#include <stdint.h>

#include <algorithm>
#include <bitset>
#include <complex>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <strstream>
#include <streambuf>
#include <string>
#include <typeinfo>
#include <utility>
#include <valarray>
#include <vector>

#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfloat>
#include <ciso646>
#include <climits>
#include <clocale>
#include <cmath>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cwchar>
#include <cwctype>

#include <tr1/array>
#include <tr1/memory>
#include <tr1/type_traits>
#include <tr1/utility>

/* these seem to increase the precompiled header size disproportionally:
#include <tr1/tuple>
#include <tr1/functional>
#include <tr1/unordered_map>
#include <tr1/unordered_set>
*/

#include <boost/version.hpp>
#include <boost/any.hpp>
#include <boost/array.hpp>
#include <boost/assert.hpp>
#include <boost/assign.hpp>
#include <boost/bind.hpp>
#include <boost/checked_delete.hpp>
#include <boost/format.hpp>
#include <boost/implicit_cast.hpp>
#include <boost/integer.hpp>
#include <boost/integer_traits.hpp>
#include <boost/iterator_adaptors.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/multi_array.hpp>
#include <boost/next_prior.hpp>
#include <boost/noncopyable.hpp>
#include <boost/optional.hpp>
#include <boost/range.hpp>
#include <boost/rational.hpp>
#include <boost/ref.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/tokenizer.hpp>
#include <boost/utility.hpp>
#include <boost/variant.hpp>

#if BOOST_VERSION >= 103400
  #include <boost/foreach.hpp>
#endif

char const help [] = "Mini-manual:  http://www.eelis.net/geordi/";

// Evil casts:

  // Consider   some_evil_cast<char const *>("foo")   . There are two possible interpretations for the result: either the array should decay into a pointer and be immediately returned, or the bytes making up the array are interpreted as the bytes making up the to-be-returned pointer. cast_dammit_cast does the former, while savage_cast does the latter.

  template <typename To, typename From> To savage_cast (From const & from)
  { return (To const &) from; }
    // Can even be used to check endianness:  geordi << hex << savage_cast<uint32_t>("\xef\xbe\xad\xde"))

  template <typename To, typename From> To cast_dammit_cast (From const from)
  {
    From const & r = from; // from itself is not a reference because we want arrays to decay, so that cast_dammit_cast<char*>("oi") works properly.
    return (To const &) r;
  }

typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned short ushort;
typedef long double ldouble;

#define RANGE(x) (::boost::begin(x)), (::boost::end(x))

#define GEORDI_STATEMENTS_PRE int main () {
#define GEORDI_STATEMENTS_POST }

#define GEORDI_PRINT_PRE GEORDI_STATEMENTS_PRE std::cout <<
#define GEORDI_PRINT_POST ; GEORDI_STATEMENTS_POST

namespace geordi
{
  std::string advice ();
  void abort ();
  struct initializer_t { initializer_t (); } const initializer;
    // Could theoretically be located in other TU, but our using of an .a for our .o's makes that painful.

  template <typename T> struct shower { T const & v; char const * const s; };

  template <typename C, typename Tr, typename T>
  std::basic_ostream<C, Tr> & operator<<(std::basic_ostream<C, Tr> & o, shower<T> const & s)
  { return o << s.s << " = " << s.v; }

  template <typename T>
  shower<T> show(T const & v, char const * const s) { shower<T> const r = { v, s }; return r; }
}

#define SHOW(x) ::geordi::show((x), #x)
  // The more obvious   #define SHOW(x) #x " = " << (x)   does not work in   cout << SHOW(x), SHOW(y);. The alternative   #define SHOW(x) (#x " = " + ::boost::lexical_cast<::std::string>(x))   does not use the stream's formatting flags.

#undef assert
#define assert(e) ((e) ? void() : (void(::std::cout << "Assertion `" #e "' fails."), ::geordi::abort()));

using namespace std;
using namespace boost::assign;
using geordi::advice;
