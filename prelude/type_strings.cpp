
#include "type_strings.hpp"
#include <cxxabi.h>
#include <string>
#include <map>
#include <utility>
#include <typeinfo>
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

  std::type_info const & demangle_typeid(std::type_info const & t)
  {
    struct type_info: std::string, std::type_info
    {
      std::string const d;
      explicit type_info(char const * const s):
        std::string(cxa_demangle(s)), std::type_info(c_str()) {}
    };

    typedef std::map<std::type_info const *, std::type_info const *> M;
    static M m;
    M::const_iterator const i = m.find(&t);
    if(i != m.end()) return *i->second;
    std::type_info const * const r = new type_info(t.name());
    m.insert(std::make_pair(&t, r));
    return *r;
  }
}
