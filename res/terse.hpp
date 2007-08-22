
// Keywords:
#define au auto
#define brk break
#define cat catch
#define cexpr constexpr
#define cls class
#define co const
#define dlt delete
#define def default
#define dtp decltype
#define dub double
#define dcast dynamic_cast
#define expl explicit
#define ext extern
#define ff false
#define flt float
#define inl inline
#define tpd typedef
#define tpn typename
#define mut mutable
#define ns namespace
#define op operator
#define pub public
#define pvt private
#define prt protected
#define reg register
#define ret return
#define rcast reinterpret_cast
#define st struct
#define stc static
#define scast static_cast
#define sass static_assert
#define tmpl template
#define tid typeid
#define tt true
#define use using
#define vol volatile
#define vrt virtual
#define wh while
// No need for #define uns unsigned, because we have uint/ulong/uchar typedefs.

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
#define lcast ::boost::lexical_cast
#define foreach BOOST_FOREACH
using boost::next;
using boost::prior;
