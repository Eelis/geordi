#include "bark.hpp"

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/find.hpp>

namespace bark_detail
{
  char closer(char const c) {
    switch(c) {
      case '<': return '>';
      case '>': return '<';
      case '{': return '}';
      case '}': return '{';
      case '(': return ')';
      case ')': return '(';
      case '[': return ']';
      case ']': return '[';
      default: return 0;
  } }

  template <typename I>
  I find_toplevel(char const c, I i, I const e)
  {
    for(;;)
    {
      if(i == e) return e;
      else if(*i == c) return i;
      else if(char const cl = closer(*i)) i = find_toplevel(cl, i+1, e)+1;
      else ++i;
    }
  }

  string bark(string const pf, string const func) {

    string::size_type const with = pf.find(" [with ");
    if(with != string::npos)
      return bark(pf.substr(0, with), func) + pf.substr(with);

    string::const_iterator p = boost::ends_with(pf, ")>") ? pf.end() : boost::find_first(pf, func + '(').begin();
    if(p == pf.end() || (p != pf.begin() && *(p-1) == ':')) {
      std::reverse_iterator<string::const_iterator> j(p);
      p = find_toplevel(' ', j, pf.rend()).base();
    }

    return string(p, pf.end());
  }
}
