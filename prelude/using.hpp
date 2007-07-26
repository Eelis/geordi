
#include <boost/preprocessor/seq/for_each.hpp>

#define USING(ns,syms) BOOST_PP_SEQ_FOR_EACH(USING_1,ns,syms)
#define USING_1(r,ns,sym) using ns :: sym;
