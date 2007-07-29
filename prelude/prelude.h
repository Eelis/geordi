
#define _GLIBCXX_DEBUG
#define _GLIBCXX_DEBUG_PEDANTIC
  // These add a few seconds compile-time on slower machines.

#define _GLIBCXX_CONCEPT_CHECKS // adds approx 1 mbyte to pch

#include "tracked.hpp"
#include "using.hpp"
#include "range_printing.hpp"
#include "foreach.hpp"
#include "type_strings.hpp"

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

using namespace std;
using namespace boost::assign;

char const
  help [] = "mini-manual:  http://www.eelis.net/geordi/",
  version [] = "2007-07-24";

std::string advice ();

template <typename T, size_t N> size_t array_size (T (&)[N]) { return N; }
  // Redundant. boost::size from Boost.Range handles these.

template <typename T, typename T2> T cast_dammit_cast (T2 const what) { return *((T*) &what); }
  // Doesn't actually work with strict aliasing rules enforced.

typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned long ulong;

#define GEORDI_STATEMENTS_PRE int main () { try {
#define GEORDI_STATEMENTS_POST \
  } catch (std::exception const & e) { std::cout << "exception: " << e.what(); } }

#define GEORDI_PRINT_PRE GEORDI_STATEMENTS_PRE std::cout << std::boolalpha <<
#define GEORDI_PRINT_POST ; GEORDI_STATEMENTS_POST

#undef assert
#define assert(e) ((e) ? void(::std::cout << "Assertion `" #e "' holds.") : (::std::cout << "Assertion `" #e "' fails.", exit(0)));
