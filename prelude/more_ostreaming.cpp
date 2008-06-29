#include "more_ostreaming.hpp"

namespace more_ostreaming_detail
{
  template void print_range<char, std::char_traits<char>, std::vector<int> >(std::ostream &, std::vector<int> const &);
}
