
#include <debug/macros.h>

#ifdef _GLIBCXX_DEBUG_VERIFY
  #undef _GLIBCXX_DEBUG_VERIFY
  #define _GLIBCXX_DEBUG_VERIFY(_Condition,_ErrorMessage) \
    do if (! (_Condition)) ::__gnu_debug::_Error_formatter::_M_at("E7tKRJpMcGq574LY", 0)._ErrorMessage._M_error(); while (false)
#endif
  // "E7tKRJpMcGq574LY" is just a random string, recognized by the error filters, chosen to minimize the chance of false positives.

#include "tracked.hpp"
#include "using.hpp"
#include "more_ostreaming.hpp"
#include "literal_escape.hpp"
#include "type_strings.hpp"
#include "delimited_ostream.hpp"
#include "bin_iomanip.hpp"
#include "lvalue_rvalue.hpp"
#include "show.hpp"
#include "evil_casts.hpp"
#include "geordi.hpp"
#include "bark.hpp"

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

#ifdef GEORDI_USE_CHRONO
  #include <chrono>
#endif

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

#ifdef GEORDI_USE_EXTERN_TEMPLATE
  extern template class std::basic_ostream<char, std::char_traits<char>>;
#endif

char const help [] = "Mini-manual:  http://www.eelis.net/geordi/";

#define RANGE(x) (::boost::begin(x)), (::boost::end(x))
#define CRANGE(x) (::boost::const_begin(x)), (::boost::const_end(x))
#define RRANGE(x) (::boost::rbegin(x)), (::boost::rend(x))
#define CRRANGE(x) (::boost::const_rbegin(x)), (::boost::const_rend(x))

#define T(n) \
  namespace std \
  { template <typename C> \
    std::istreambuf_iterator<C> boost_range_begin(n<C> & i) { return std::istreambuf_iterator<C>(i); } \
    template <typename C> \
    std::istreambuf_iterator<C> boost_range_end(n<C> &) { return std::istreambuf_iterator<C>(); } \
  } \
  namespace boost \
  { template<typename C> \
    struct range_iterator<std::n<C> > { typedef std::istreambuf_iterator<C> type; }; \
    template<typename C> \
    struct range_const_iterator<std::n<C> >; \
  }

T(basic_istream)
T(basic_ifstream)
T(basic_istringstream)

#undef T

namespace geordi { geordi::initializer_t const initializer; }
  // Could theoretically be located in other TU, but our use of an .a for our .o's makes that painful.

#undef assert
#define assert(e) ((e) ? void() : (::std::printf("%s", "Assertion `" #e "' fails."), ::std::fclose(stdout), ::std::abort()))

#define typeid(...) ::type_strings_detail::type_info::from_std(typeid(__VA_ARGS__))

using namespace boost::assign;
