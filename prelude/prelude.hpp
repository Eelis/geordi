#include "patched_typeinfo.hpp"
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
#include <condition_variable>
#include <complex>
#include <deque>
#include <exception>
#include <forward_list>
#include <fstream>
#include <functional>
#include <future>
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
#include <shared_mutex>
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

#include <experimental/algorithm>
#ifndef __clang__
#include <experimental/any>
#endif
#include <experimental/chrono>
#ifndef __clang__
#include <experimental/filesystem>
#endif
#include <experimental/forward_list>
#include <experimental/functional>
#include <experimental/iterator>
#include <experimental/list>
#include <experimental/map>
#include <experimental/numeric>
#include <experimental/optional>
#include <experimental/ratio>
#include <experimental/set>
#include <experimental/string_view>
#include <experimental/system_error>
#include <experimental/tuple>
#include <experimental/type_traits>
#include <experimental/unordered_map>
#include <experimental/unordered_set>
#include <experimental/vector>

namespace std { using namespace experimental; }
extern template class std::basic_ostream<char>;

extern template class std::basic_regex<char>;
extern template std::string std::regex_replace<std::regex_traits<char>, char>(
  char const *, std::regex const &, char const *, std::regex_constants::match_flag_type);

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
