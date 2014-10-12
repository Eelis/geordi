#include "more_ostreaming.hpp"

namespace more_ostreaming
{
  template void delimit<char, std::char_traits<char>, std::vector<int> >(std::ostream &, std::vector<int> const &);
}
