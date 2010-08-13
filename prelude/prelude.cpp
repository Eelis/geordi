
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <cerrno>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <ctime>
#include <cassert>
#include <ios>
#include <set>
#include <functional>
#include <clocale>
#include <stdlib.h>
#include <cxxabi.h>
#include <ext/malloc_allocator.h>
#include <boost/noncopyable.hpp>
#include "geordi.hpp"
#include "bin_iomanip.hpp"

namespace geordi
{
  namespace
  {
    bool is_prefix_of(char const * a, char const * b) { while(*a && *b == *a) { ++a; ++b; } return !*a; }

    void terminate_handler(bool const unexp)
    {
      // We use printf because cout/cerr may be dead.

      std::printf("%sterminated", parsep);

      if(std::type_info const * const t = abi::__cxa_current_exception_type())
      {
        int status = 0;
        char const * const name = abi::__cxa_demangle(t->name(), 0, 0, &status);

        // In OOM conditions, the above call to __cxa_demangle will fail (and name will be 0). Supplying a preallocated buffer using __cxa_demangle's second and third parameters does not help, because it performs additional internal allocations.

        std::printf(" by ");
        if(unexp) std::printf("unexpected ");
        try { throw; }
        catch(std::exception const & e)
        {
          char const * const what = e.what();
          if(!name) std::printf("exception: ");
          else if(!is_prefix_of(name, what)) std::printf("%s: ", name);
          std::printf("%s", what);
        }
        catch(char const * const s) { std::printf("exception: %s", s); }
        catch(int const i) { std::printf("exception: %d", i); }
        catch(...)
        {
          std::printf("exception");
          if(name) std::printf(" of type %s", name);
        }
      }

      std::fclose(stdout);
      std::abort();
    }

    void terminate_handler() { terminate_handler(false); }
    void unexpected_handler() { terminate_handler(true); }
  }

  initializer_t::initializer_t ()
  {
    std::ios_base::Init const i;

    std::boolalpha(std::cout);
    std::boolalpha(std::wcout);
    std::boolalpha(std::cerr);
    std::boolalpha(std::wcerr);
    std::boolalpha(std::clog);
    std::boolalpha(std::wclog);

    std::unitbuf(std::cout);
    std::unitbuf(std::wcout);
    std::unitbuf(std::cerr);
    std::unitbuf(std::wcerr);
    std::unitbuf(std::clog);
    std::unitbuf(std::wclog);

    static bin_num_put<> bnp(1);
    static bin_num_put<wchar_t> wbnp(1);
    std::cout.imbue(std::locale(std::cout.getloc(), &bnp));
    std::wcout.imbue(std::locale(std::wcout.getloc(), &wbnp));
    std::cerr.imbue(std::locale(std::cerr.getloc(), &bnp));
    std::wcerr.imbue(std::locale(std::wcerr.getloc(), &wbnp));
    std::clog.imbue(std::locale(std::clog.getloc(), &bnp));
    std::wclog.imbue(std::locale(std::wclog.getloc(), &wbnp));
      // Having this compiled separately saves more than a full second per request.

    std::set_terminate(terminate_handler);
    std::set_unexpected(unexpected_handler);

    std::setlocale(LC_ALL, "");
  }

  utsname uname()
  {
    utsname r;
    if (uname(&r)) throw std::runtime_error(std::strerror(errno));
    return r;
  }

  char const * demangle(char const * const name)
  {
    int st;
    char * const p = abi::__cxa_demangle(name, 0, 0, &st);

    switch (st)
    {
      case 0: return p;
      case -1: throw std::runtime_error("A memory allocation failure occurred.");
      case -2: throw std::runtime_error("Not a valid name under the GCC C++ ABI mangling rules.");
      case -3: throw std::runtime_error("One of the arguments is invalid.");
      default: assert(!"unexpected demangle status");
    }

    // We could return a std::string and free p, but since deallocation is not a concern (and is even a no-op) in geordi anyway, we don't bother.
  }

} // namespace geordi

std::ostream & operator<<(std::ostream & o, wchar_t const c)
{
  char buf[MB_LEN_MAX];
  int const i = wctomb(buf, c);
  if(i < 0) o << '?';
  else std::copy(buf, buf + i, std::ostreambuf_iterator<char>(o));
  return o;
}

std::ostream & operator<<(std::ostream & o, wchar_t const * s)
{ for(; *s; ++s) o << *s; return o; }

std::ostream & operator<<(std::ostream & o, std::wstring const & s)
{ for(std::wstring::const_iterator i = s.begin(); i != s.end(); ++i) o << *i; return o; }
