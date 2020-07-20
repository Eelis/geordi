#ifdef __clang__
  #include "libcxx_patched_typeinfo.hpp"
#else
  #include "libstdcxx_patched_typeinfo.hpp"
#endif

#if __cplusplus >= 201103 // maybe backport parts of these even further someday
  #include "lvalue_rvalue.hpp"
  #include "bin_iomanip.hpp"
  #include "type_strings.hpp"
#endif

#include "tracked.hpp"
#include "more_ostreaming.hpp"
#include "literal_escape.hpp"
#include "delimited_ostream.hpp"
#include "show.hpp"
#include "evil_casts.hpp"
#include "geordi.hpp"
#include "bark.hpp"

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

#if __cplusplus >= 201103
  #include <atomic>
  #include <codecvt>
  #include <thread>
  #include <regex>
  #include <condition_variable>
  #include <array>
  #include <chrono>
  #include <future>
  #include <mutex>
  #include <type_traits>
  #include <typeindex>
  #include <cfenv>
  #include <initializer_list>
  #include <tuple>
  #include <scoped_allocator>
  #include <random>
  #include <ratio>
  #include <cstdint>
  #include <cinttypes>
  #include <forward_list>
  #include <unordered_map>
  #include <unordered_set>
  #include <system_error>
#endif

#if __cplusplus >= 201703
  #include <shared_mutex>
  #include <any>
  #include <optional>
  #include <string_view>
  #include <variant>

  #if __has_include(<experimental/array>)
    #include <experimental/array>
  #endif
  #if __has_include(<experimental/algorithm>)
    #include <experimental/algorithm>
  #endif
  #if __has_include(<experimental/deque>)
    #include <experimental/deque>
  #endif
  #if __has_include(<experimental/filesystem>)
    #include <experimental/filesystem>
  #endif
  #if __has_include(<experimental/forward_list>)
    #include <experimental/forward_list>
  #endif
  #if __has_include(<experimental/functional>)
    #include <experimental/functional>
  #endif
  #if __has_include(<experimental/iterator>)
    #include <experimental/iterator>
  #endif
  #if __has_include(<experimental/list>)
    #include <experimental/list>
  #endif
  #if __has_include(<experimental/map>)
    #include <experimental/map>
  #endif
  #if __has_include(<experimental/memory>)
    #include <experimental/memory>
  #endif
  #if __has_include(<experimental/memory_resource>)
    #include <experimental/memory_resource>
  #endif
  #if __has_include(<experimental/propagate_const>)
    #include <experimental/propagate_const>
  #endif
  #if __has_include(<experimental/random>)
    #include <experimental/random>
  #endif
  #if __has_include(<experimental/regex>)
    #include <experimental/regex>
  #endif
  #if __has_include(<experimental/set>)
    #include <experimental/set>
  #endif
  #if __has_include(<experimental/string>)
    #include <experimental/string>
  #endif
  #if __has_include(<experimental/type_traits>)
    #include <experimental/type_traits>
  #endif
  #if __has_include(<experimental/unordered_map>)
    #include <experimental/unordered_map>
  #endif
  #if __has_include(<experimental/unordered_set>)
    #include <experimental/unordered_set>
  #endif
  #if __has_include(<experimental/utility>)
    #include <experimental/utility>
  #endif
  #if __has_include(<experimental/vector>)
    #include <experimental/vector>
  #endif
#endif

#if __cplusplus > 201703
  #include <numbers>
  #include <concepts>
  #include <version>
  #include <compare>
  #include <span>
  #include <ranges>
  #include <bit>
  #include <stop_token>
#endif

#if __cplusplus >= 201103
  extern template class std::basic_ostream<char>;

  extern template class std::basic_regex<char>;
  extern template std::string std::regex_replace<std::regex_traits<char>, char>(
    char const *, std::regex const &, char const *, std::regex_constants::match_flag_type);
#endif

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
