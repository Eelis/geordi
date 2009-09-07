#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <map>
#include <cstdlib>
#include <cstdio>
#include <ext/malloc_allocator.h>
#include <mcheck.h>
#include "geordi.hpp"

extern "C"
{
  void abort() throw() // default version causes "SYS_gettid: Operation not permitted".
  {
    std::printf("%s%s", geordi::parsep, strsignal(SIGABRT));
    std::fclose(stdout); // Prevents things like tracked reporting leaks.
    std::exit(0);
  }
}

namespace
{
  enum alloc_reg { ar_plain, ar_array, ar_deleted };

  typedef std::map<void *, alloc_reg, std::less<void *>, __gnu_cxx::malloc_allocator<std::pair<void * const, alloc_reg> > > Allocs;

  Allocs & allocs() { static Allocs * const r = new (std::malloc(sizeof(Allocs))) Allocs; return *r; }
    // We can't use an ordinary variable because when the construction of an earlier static-storage variable uses dynamic storage, it would cause us to operate on a not-yet-constructed container. We use malloc to avoid infinite mutual recursion with op_new. We don't ever destruct/deallocate r, because there is no way to ensure that that happens after all other destruction (which could involve dynamic storage) has finished.

  std::new_handler get_new_handler() throw()
  {
    std::new_handler const r = std::set_new_handler(0);
    std::set_new_handler(r);
    return r;
  }

  void * op_new(std::size_t const s, alloc_reg const ar) throw()
  {
    std::new_handler const h = get_new_handler();

    for(;;) // See 18.5.1.1 paragraph 4 (in N2691).
    {
      if(void * const r = std::malloc(s))
        for(;;)
        {
          try { allocs()[r] = ar; return r;  }
          catch (std::bad_alloc const &)
          { if(!h) { std::free(r); return 0; } h(); }
        }
      if(!h) return 0;
      h();
    }
  }

  void del(void * const p, bool const a)
  {
    if(!p) return;
    Allocs::iterator const i = allocs().find(p);
    if(i == allocs().end())
      geordi::error()() << "tried to delete pointer not returned by previous matching allocation.";
    if(i->second == ar_deleted)
      geordi::error()() << "tried to delete already deleted pointer.";
    if(a && i->second == ar_plain)
      geordi::error()() << "tried to delete[] pointer returned by non-array operator new.";
    if(!a && i->second == ar_array)
      geordi::error()() << "tried to apply non-array operator delete to pointer returned by new[].";
    i->second = ar_deleted;

    // We don't actually deallocate the memory, because then it could be reallocated, causing UB in { int * const p = new int; delete p; new int; delete p; } to go unnoticed if the second allocation returns the same address (which is not unlikely).

    mprobe(p);
  }
}

// Plain new/delete:

void * operator new(size_t const i, std::nothrow_t const &) throw ()
{ return op_new(i, ar_plain); }
void * operator new(size_t const i) throw (std::bad_alloc)
{ if (void * const r = op_new(i, ar_plain)) return r; throw std::bad_alloc(); }
void operator delete(void * const p, std::nothrow_t const &) throw () { operator delete(p); }
void operator delete(void * const p) throw() { del(p, false); }

// Array new[]/delete[]:

void * operator new[](size_t const i, std::nothrow_t const &) throw ()
{ return op_new(i, ar_array); }
void * operator new[](size_t const i) throw (std::bad_alloc)
{ if (void * const r = op_new(i, ar_array)) return r; throw std::bad_alloc(); }
void operator delete[](void * const p, std::nothrow_t const &) throw () { operator delete[](p); }
void operator delete[](void * const p) throw() { del(p, true); }
