#include "more_ostreaming.hpp"

#if __cplusplus >= 201103

namespace more_ostreaming
{
  template void delimit<char, std::char_traits<char>, std::vector<int> >(std::ostream &, std::vector<int> const &);
}

#endif
