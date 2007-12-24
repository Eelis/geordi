
#include <cxxabi.h>
#include <string>
#include <stdexcept>
#include <cstdlib>
#include <boost/bind.hpp>
#include "ScopeGuard.hpp"

namespace type_strings_detail
{
  std::string cxa_demangle(const char * const name)
  {
    int st;
    char * const p = abi::__cxa_demangle(name, 0, 0, &st);

    switch (st)
    {
      case 0: { ScopeGuard const freeer (boost::bind(&std::free, p)); return p; }
      case -1: throw std::runtime_error("A memory allocation failure occurred.");
      case -2: throw std::runtime_error("Not a valid name under the GCC C++ ABI mangling rules.");
      case -3: throw std::runtime_error("One of the arguments is invalid.");
      default: assert(!"unexpected demangle status");
    }
  }
}
