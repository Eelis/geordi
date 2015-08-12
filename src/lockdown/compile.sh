#!/bin/sh
g++ -static-libstdc++ -static-libgcc -std=c++11 -O2 /geordi/src/lockdown/lockdown.cpp `pkg-config --cflags --libs libseccomp` -Wl,--rpath,/usr/local/lib64 -o /usr/bin/geordi-lockdown
