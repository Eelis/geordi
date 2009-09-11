
#include <cxxabi.h>
#include <cassert>
#include <stdexcept>

namespace type_strings_detail
{
  struct type_info: std::type_info { char const * name() const; static type_info const & from_std(std::type_info const&); };

  char const * type_info::name() const
  {
    int st;
    char * const p = abi::__cxa_demangle(std::type_info::name(), 0, 0, &st);

    switch (st)
    {
      case 0: return p;
      case -1: throw std::runtime_error("A memory allocation failure occurred.");
      case -2: throw std::runtime_error("Not a valid name under the GCC C++ ABI mangling rules.");
      case -3: throw std::runtime_error("One of the arguments is invalid.");
      default: assert(!"unexpected demangle status");
    }

    // We could (and used to) return a std::string and free p, but since deallocation is not a concern (and is even a no-op) in geordi anyway, we might as well preserve name()'s return type.
  }

  type_info const & type_info::from_std(std::type_info const & i) { return static_cast<type_info const&>(i); }
}
