
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
#include <array>
#include <atomic>
#include <bitset>
#include <cfenv>
#include <chrono>
#include <complex>
#include <deque>
#include <exception>
#include <forward_list>
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
#include <mutex>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <random>
#include <ratio>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <strstream>
#include <streambuf>
#include <string>
#include <system_error>
#include <thread>
#include <tuple>
#include <typeindex>
#include <typeinfo>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
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

extern template class std::basic_ostream<char>;

char const help [] = "Mini-manual:  http://www.eelis.net/geordi/";

#define RANGE(x) (::std::begin(x)), (::std::end(x))

#define T(n) \
  namespace std \
  { template <typename C> \
    std::istreambuf_iterator<C> boost_range_begin(n<C> & i) { return std::istreambuf_iterator<C>(i); } \
    template <typename C> \
    std::istreambuf_iterator<C> boost_range_end(n<C> &) { return std::istreambuf_iterator<C>(); } \
  }

T(basic_istream)
T(basic_ifstream)
T(basic_istringstream)

#undef T

#undef assert
#define assert(e) ((e) ? void() : (::std::printf("%s", "Assertion `" #e "' fails."), ::std::fclose(stdout), ::std::abort()))

#define typeid(...) ::type_strings_detail::type_info::from_std(typeid(__VA_ARGS__))
