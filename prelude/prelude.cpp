
// Instantiating things in prelude.h makes compiling much slower, so doing it here is worthwhile.

#include <iostream>
#include <string>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <cerrno>
#include <string.h>
#include <cstdlib>
#include <ctime>

#include "bin_iomanip.hpp"

std::string advice ()
{
  std::srand(std::time(0));
  std::ifstream f ("advice.txt");
  if (!f) throw std::runtime_error(strerror(errno));
  std::string line;
  std::vector<std::string> lines;
  while (std::getline(f, line))
  {
    std::string::size_type const i = line.find("$");
    if (i != std::string::npos) line.erase(i);
    if (!line.empty() && line.size() <= 300) lines.push_back(line);
  }
  if (lines.empty()) throw std::runtime_error("no advice available");
  return lines.at(rand() % lines.size());
}

void geordi_init ()
{
  std::cout.imbue(std::locale(std::cout.getloc(), new bin_num_put));
    // Having this compiled separately saves more than a full second per request.
}
