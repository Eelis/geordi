#ifndef BARK_HPP
#define BARK_HPP

namespace bark_detail
{
  void do_bark(char const * pf, char const * func);
}

#define BARK (::bark_detail::do_bark(__PRETTY_FUNCTION__, __func__))
  // We don't include a trailing semicolon, because a function body looking like { BARK } confuses geordi's C++ parser.

#endif
