#!/bin/bash
. /geordi/compile-config
for std in 98 03 11 14 1z
do
  stdflag="-std=c++${std}"
  echo "compiling prelude with ${stdflag}"
  cp prelude.hpp /geordi/run/prelude-${std}.hpp
  $GCC $stdflag $GCC_COMPILE_FLAGS -c -x c++-header prelude.hpp -o /geordi/run/prelude-${std}.hpp.gch
  $CLANG $stdflag $CLANG_COMPILE_FLAGS -x c++-header prelude.hpp -o /geordi/run/prelude-${std}.hpp.pch
  $GCC $stdflag $GCC_COMPILE_FLAGS -fsanitize=undefined -fno-sanitize-recover=all -fPIC -shared -o /lib/libgeordi_preload-${std}.so preload.cpp
  $GCC $stdflag $GCC_COMPILE_FLAGS -fsanitize=undefined -fno-sanitize-recover=all -c {prelude,tracked,more_ostreaming,bark}.cpp
  ar -rsc /lib/libgeordi_prelude-${std}.a {prelude,bark,tracked,more_ostreaming}.o
done
