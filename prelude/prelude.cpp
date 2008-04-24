
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
#include <set>
#include <functional>
#include <clocale>
#include <cxxabi.h>
#include <ext/malloc_allocator.h>
#include <boost/noncopyable.hpp>
#include "geordi.hpp"
#include "bin_iomanip.hpp"

namespace geordi
{
  void abort () // std::abort() causes "Disallowed system call: gettid".
  {
    std::cout << "\nAborted." << std::flush; // The initial \n causes the message to be shown only in Local mode.
    std::fclose(stdout); // Prevents things like tracked reporting leaks.
    std::exit(0);
  }

  char const parsep[] = "\342\200\251";
    // UTF-8 encoding of PARAGRAPH SEPARATOR (U+2029). See "Output separators" in notes.txt.

  namespace
  {
    bool is_prefix_of(char const * a, char const * b) { while(*a && *b == *a) { ++a; ++b; } return !*a; }

    void terminate_handler(bool const unexp)
    {
      std::cout << parsep << "terminated";

      if(std::type_info const * const t = abi::__cxa_current_exception_type())
      {
        int status = 0;
        char const * const name = abi::__cxa_demangle(t->name(), 0, 0, &status);

        // In OOM conditions, the above call to __cxa_demangle will fail (and name will be 0). Supplying a preallocated buffer using __cxa_demangle's second and third parameters does not help, because it performs additional internal allocations.

        std::cout << " by ";
        if(unexp) std::cout << "unexpected ";
        try { throw; }
        catch(std::exception const & e)
        {
          char const * const what = e.what();
          if(!name) std::cout << "exception: ";
          else if(!is_prefix_of(name, what)) std::cout << name << ": ";
          std::cout << what;
        }
        catch(char const * const s) { std::cout << "exception: " << s; }
        catch(int const i) { std::cout << "exception: " << i; }
        catch(...)
        {
          std::cout << "exception";
          if(name) std::cout << " of type " << name;
        }
      }

      abort();
    }

    void terminate_handler() { terminate_handler(false); }
    void unexpected_handler() { terminate_handler(true); }
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

  initializer_t::initializer_t ()
  {
    std::ios_base::Init const i;

    std::boolalpha(std::cout);

    std::cout.imbue(std::locale(std::cout.getloc(), new bin_num_put<>));
    std::wcout.imbue(std::locale(std::wcout.getloc(), new bin_num_put<wchar_t>));
      // Having this compiled separately saves more than a full second per request.

    std::set_terminate(terminate_handler);
    std::set_unexpected(unexpected_handler);

    std::setlocale(LC_ALL, "");
  }

  typedef std::set<void *, std::less<void *>, __gnu_cxx::malloc_allocator<void *> > allocs;

  allocs & prev() { static allocs * r(0); if (!r) r = new (std::malloc(sizeof(allocs))) allocs; return *r; }
  allocs & plain_current() { static allocs * r(0); if (!r) r = new (std::malloc(sizeof(allocs))) allocs; return *r; }
  allocs & array_current() { static allocs * r(0); if (!r) r = new (std::malloc(sizeof(allocs))) allocs; return *r; }
    // Invariant: These three are disjoint.
    // We can't use ordinary variables because when the construction of an earlier static-storage variable uses dynamic storage, it would cause us to operate on not-yet-constructed containers. We use malloc to avoid infinite mutual recursion with plain_new/array_new. We don't ever destruct/deallocate the prev/plain_current/array_current containers, because there is no way to ensure that that happens after all other destruction (which could involve dynamic storage) has finished.

  void * plain_new(std::size_t const s) throw()
  { void * const r = std::malloc(s); if (r) { prev().erase(r); plain_current().insert(r); } return r; }
  void * array_new(std::size_t const s) throw()
  { void * const r = std::malloc(s); if (r) { prev().erase(r); array_current().insert(r); } return r; }
    // These can be called before the three allocs variables are even constructed.

} // namespace geordi

// Plain new:

void * operator new(size_t const i, std::nothrow_t const &) throw ()
{ return geordi::plain_new(i); }
void * operator new(size_t const i) throw (std::bad_alloc)
{ if (void * const r = geordi::plain_new(i)) return r; throw std::bad_alloc(); }
void operator delete(void * const p, std::nothrow_t const &) throw () { operator delete(p); }
void operator delete(void * const p) throw ()
{
  if(!p) return;
  using namespace geordi;
  if (prev().find(p) != prev().end()) error()() << "tried to delete already deleted pointer.";
  if (array_current().find(p) != array_current().end())
    error()() << "tried to apply non-array operator delete to pointer returned by new[].";
  allocs::iterator const i = plain_current().find(p);
  if (i == plain_current().end())
    error()() << "tried to delete pointer not returned by previous matching new invocation.";
  plain_current().erase(i);
  std::free(p);
  prev().insert(p);
}

// Array new[]:

void * operator new[](size_t const i, std::nothrow_t const &) throw ()
{ return geordi::array_new(i); }
void * operator new[](size_t const i) throw (std::bad_alloc)
{ if (void * const r = geordi::array_new(i)) return r; throw std::bad_alloc(); }
void operator delete[](void * const p, std::nothrow_t const &) throw () { operator delete[](p); }
void operator delete[](void * const p) throw ()
{
  if(!p) return;
  using namespace geordi;
  if (prev().find(p) != prev().end()) error()() << "tried to delete[] already deleted pointer.";
  if (plain_current().find(p) != plain_current().end())
    error()() << "tried to delete[] pointer returned by non-array operator new.";
  allocs::iterator const i = array_current().find(p);
  if (i == array_current().end())
    error()() << "tried to delete[] pointer not returned by previous new[] invocation.";
  array_current().erase(i);
  std::free(p);
  prev().insert(p);
}
