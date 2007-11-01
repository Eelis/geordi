
#include <iostream>
#include <string>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <cerrno>
#include <string.h>
#include <cstdlib>
#include <cstdio>
#include <ctime>
#include <ios>
#include <cxxabi.h>

#include "bin_iomanip.hpp"

namespace geordi
{
  void abort () // std::abort() causes "Disallowed system call: gettid".
  {
    std::cout << "\nAborted." << std::flush; // The initial \n causes the message to be shown only in -i mode.
    std::fclose(stdout); // Prevents things like tracked reporting leaks.
    std::exit(0);
  }

  namespace
  {
    void terminate_handler ()
    {
      if (std::type_info const * const t = abi::__cxa_current_exception_type())
      {
        std::string name = t->name(); // mangled

        int status = -1;
        char * const dem = abi::__cxa_demangle(name.c_str(), 0, 0, &status);

        if (status == 0) { name = dem; free(dem); }

        try { throw; }
        catch (std::exception & e) { std::cout << name << ": " << e.what(); }
        catch (char const * const s) { std::cout << "char const* exception: " << s; }
        catch (...) { std::cout << "uncaught exception of type " << name; }
      }
      else std::cout << "terminate called without an active exception.";
        // Happens when terminate() is called explicitly, or "throw;" is called when there is no active exception.

      abort();
    }
  }

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

  struct initializer_t { initializer_t (); };

  initializer_t::initializer_t ()
  {
    std::ios_base::Init const i;

    std::boolalpha(std::cout);

    std::cout.imbue(std::locale(std::cout.getloc(), new bin_num_put<>));
    std::wcout.imbue(std::locale(std::wcout.getloc(), new bin_num_put<wchar_t>));
      // Having this compiled separately saves more than a full second per request.

    std::set_terminate(terminate_handler);
  }
}
