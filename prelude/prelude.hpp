
#include "tracked.hpp"
#include "using.hpp"
#include "more_ostreaming.hpp"
#include "type_strings.hpp"
#include "delimited_ostream.hpp"
#include "bin_iomanip.hpp"
#include "lvalue_rvalue.hpp"
#include "show.hpp"
#include "evil_casts.hpp"
#include "geordi.hpp"

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

#define RANGE(x) (::boost::begin(x)), (::boost::end(x))

#define GEORDI_STATEMENTS_PRE int main () {
#define GEORDI_STATEMENTS_POST }

#define GEORDI_PRINT_PRE GEORDI_STATEMENTS_PRE std::cout <<
#define GEORDI_PRINT_POST ; GEORDI_STATEMENTS_POST

namespace geordi { geordi::initializer_t const initializer; }
  // Could theoretically be located in other TU, but our using of an .a for our .o's makes that painful.

#undef assert
#define assert(e) ((e) ? void() : (void(::std::cout << "Assertion `" #e "' fails."), ::geordi::abort()));

#define typeid(...) static_cast< ::type_strings_detail::type_info const &>(typeid(__VA_ARGS__))

using namespace std;
using namespace boost::assign;
using geordi::advice;
