#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <set>
#include <cstdlib>
#include <cstdio>
#include <ext/malloc_allocator.h>
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
}

// Plain new/delete:

void * operator new(size_t const i, std::nothrow_t const &) throw ()
{ return plain_new(i); }
void * operator new(size_t const i) throw (std::bad_alloc)
{ if (void * const r = plain_new(i)) return r; throw std::bad_alloc(); }
void operator delete(void * const p, std::nothrow_t const &) throw () { operator delete(p); }
void operator delete(void * const p) throw ()
{
  if(!p) return;
  if (prev().find(p) != prev().end()) geordi::error()() << "tried to delete already deleted pointer.";
  if (array_current().find(p) != array_current().end())
    geordi::error()() << "tried to apply non-array operator delete to pointer returned by new[].";
  allocs::iterator const i = plain_current().find(p);
  if (i == plain_current().end())
    geordi::error()() << "tried to delete pointer not returned by previous matching new invocation.";
  plain_current().erase(i);
  std::free(p);
  prev().insert(p);
}

// Array new[]/delete[]:

void * operator new[](size_t const i, std::nothrow_t const &) throw ()
{ return array_new(i); }
void * operator new[](size_t const i) throw (std::bad_alloc)
{ if (void * const r = array_new(i)) return r; throw std::bad_alloc(); }
void operator delete[](void * const p, std::nothrow_t const &) throw () { operator delete[](p); }
void operator delete[](void * const p) throw ()
{
  if(!p) return;
  if (prev().find(p) != prev().end()) geordi::error()() << "tried to delete[] already deleted pointer.";
  if (plain_current().find(p) != plain_current().end())
    geordi::error()() << "tried to delete[] pointer returned by non-array operator new.";
  allocs::iterator const i = array_current().find(p);
  if (i == array_current().end())
    geordi::error()() << "tried to delete[] pointer not returned by previous new[] invocation.";
  array_current().erase(i);
  std::free(p);
  prev().insert(p);
}
