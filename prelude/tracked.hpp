
#include <iosfwd>
#include <set>

namespace tracked
{
  struct B
  {
    B ();
    explicit B (int);
    B (B const &);
    B & operator= (B const &);
    virtual ~B ();

    B & operator++ ();
    B operator++ (int);
      // Allows for a nice demonstration of the canonical postfix operator++ semantics using { tracked::B s (2); (s++).f(); }

    void f () const;
    virtual void vf () const;

    private: static struct Reg { std::set<B const *> s; ~Reg (); } reg;
  };

  std::ostream & operator<< (std::ostream &, B const &);

  struct D: B
  {
    D ();
    D (D const &);
    D & operator= (D const &);
    void vf () const;
    virtual ~D ();
  };

  std::ostream & operator<< (std::ostream &, D const &);
}
