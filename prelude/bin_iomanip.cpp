
#include "bin_iomanip.hpp"

void imbue_bin_num_put (std::ostream & o)
  // Having this separate saves more than a full second per request.
{
  o.imbue(std::locale(o.getloc(), new bin_num_put));
}
