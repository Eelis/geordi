#!/bin/bash
. /geordi/compile-config
$GCC $GCC_COMPILE_FLAGS -c -x c++-header prelude.hpp -o /geordi/run/prelude.hpp.gch
$CLANG $CLANG_COMPILE_FLAGS -x c++-header prelude.hpp -o /geordi/run/prelude.hpp.pch
$GCC $GCC_COMPILE_FLAGS -fsanitize=undefined -fno-sanitize-recover=all -fPIC -shared -o /lib/libgeordi_preload.so preload.cpp
$GCC $GCC_COMPILE_FLAGS -fsanitize=undefined -fno-sanitize-recover=all -c {prelude,tracked,more_ostreaming,bark}.cpp
ar -rsc /lib/libgeordi_prelude.a {prelude,bark,tracked,more_ostreaming}.o
