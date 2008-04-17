
#include <cxxabi.h>
#include <string>
#include <stdexcept>
#include <cstdlib>
#include <boost/shared_ptr.hpp>

namespace type_strings_detail
{
  std::string cxa_demangle(char const * const name)
  {
    int st;
    char * const p = abi::__cxa_demangle(name, 0, 0, &st);

    switch (st)
    {
      case 0: { return boost::shared_ptr<char>(p, &std::free).get(); }
        // Todo: Use unique_ptr once it's supported everywhere.
      case -1: throw std::runtime_error("A memory allocation failure occurred.");
      case -2: throw std::runtime_error("Not a valid name under the GCC C++ ABI mangling rules.");
      case -3: throw std::runtime_error("One of the arguments is invalid.");
      default: assert(!"unexpected demangle status");
    }
  }

  struct type_info: std::type_info
  { std::string name() const { return cxa_demangle(std::type_info::name()); } };
}
