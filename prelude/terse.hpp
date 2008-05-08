
// Keywords:
#define au auto
#define brk break
#define cat catch
#define cls class
#define co const
#define cex constexpr
#define ccast const_cast
#define cnt continue
#define dtp decltype
#define def default
#define del delete
#define dcast dynamic_cast
#define el else
#define expl explicit
#define expo export
#define ext extern
#define ff false // We could define tt/ff as constants, but then they'd be lvalues.
#define inl inline
#define mut mutable
#define ns namespace
#define op operator
#define pvt private
#define prt protected
#define pub public
#define reg register
#define rcast reinterpret_cast
#define ret return
#define sgn signed
#define szof sizeof
#define sass static_assert
#define scast static_cast
#define stc static
#define st struct
#define sw switch
#define tmp template
#define tt true
#define tpd typedef
#define tpi typeid
#define tpn typename
#define uni union
#define use using
#define vrt virtual
#define vol volatile
#define wh while

typedef char ch;
typedef float flt;
typedef double dub;
typedef long double ldub;
typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned short ushort;
// C++0x:
  // typedef char16_t ch16
  // typedef char32_t ch32
  // typedef long long llong;
  // typedef unsigned long long ullong;

// Stdlib:
#define ass assert
#define iter iterator
typedef ::std::string str;
  // The following are to be replaced with template aliases at some point.
#define vec ::std::vector
#define umap ::std::unordered_map
#define uset ::std::unordered_set
#define numlim ::std::numeric_limits

// Boost:
#define opt ::boost::optional
#define lcast ::boost::lexical_cast
#define foreach BOOST_FOREACH
using boost::next;
using boost::prior;

// Misc:
#define M int main ()
