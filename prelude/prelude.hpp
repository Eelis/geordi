
#include <debug/macros.h>

#ifdef _GLIBCXX_DEBUG_VERIFY
  #undef _GLIBCXX_DEBUG_VERIFY
  #define _GLIBCXX_DEBUG_VERIFY(_Condition,_ErrorMessage) \
    do if (! (_Condition)) ::__gnu_debug::_Error_formatter::_M_at("E7tKRJpMcGq574LY", 0)._ErrorMessage._M_error(); while (false)
#endif
  // "E7tKRJpMcGq574LY" is just a random string, recognized by the error filters, chosen to minimize the chance of false positives.

#include "tracked.hpp"
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

#include <algorithm>
#include <atomic>
#include <bitset>
#include <chrono>
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
#include <thread>
#include <tuple>
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
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cwchar>
#include <cwctype>

#include <boost/version.hpp>
#include <boost/any.hpp>
#include <boost/checked_delete.hpp>
#include <boost/format.hpp>
#include <boost/implicit_cast.hpp>
#include <boost/integer.hpp>
#include <boost/integer_traits.hpp>
#include <boost/iterator_adaptors.hpp>
#include <boost/multi_array.hpp>
#include <boost/optional.hpp>
#include <boost/range.hpp>
#include <boost/rational.hpp>
#include <boost/utility.hpp>
#include <boost/variant.hpp>

extern template class std::basic_ostream<char>;

char const help [] = "Mini-manual:  http://www.eelis.net/geordi/";

#define RANGE(x) (::std::begin(x)), (::std::end(x))

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
