
#include <iosfwd>
#include <set>

namespace tracked
{
  struct B
  {
    B ();
    explicit B (int);
    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      B (B &&);
    #endif
    B (B const &);
    B & operator= (B const &);
    virtual ~B ();

    B & operator++ ();
    B operator++ (int);
      // Allows for a nice demonstration of the canonical postfix operator++ semantics using { tracked::B s (2); (s++).f(); }

    void f () const;
    virtual void vf () const;

    void * operator new (size_t);
    void * operator new[] (size_t);
    void operator delete (void *);
    void operator delete[] (void *);

    private:

      bool pillaged;
      void nopillage (char const *) const;

      static struct Reg { std::set<B const *> s; ~Reg (); } reg;
  };

  std::ostream & operator<< (std::ostream &, B const &);

  struct D: B
  {
    D ();
    D (D const &);
    #ifdef __GXX_EXPERIMENTAL_CXX0X__
      D (D &&);
    #endif
    D & operator= (D const &);
    void vf () const;
    virtual ~D ();
  };

  std::ostream & operator<< (std::ostream &, D const &);
}
