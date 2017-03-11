FROM ubuntu:16.04
RUN apt-get update && apt-get install -y libreadline-dev libboost-dev build-essential libgmp-dev pkg-config libseccomp-dev software-properties-common subversion libmpfr-dev libmpc-dev flex zlib1g-dev git libedit-dev ncurses-dev cmake bison libcap-dev python wget locales-all

COPY llvm-no-temp-files.patch /geordi/src/
COPY install-clang /geordi/src/
RUN /geordi/src/install-clang

#COPY install-klee /geordi/src/
#RUN /geordi/src/install-klee

COPY install-haskell /geordi/src/
RUN /geordi/src/install-haskell

COPY install-gcc-trunk /geordi/src/
RUN /geordi/src/install-gcc-trunk

RUN cabal update && cabal install --global cabal-install
RUN cabal install --global --reorder-goals --allow-newer=process process deepseq mtl syb utf8-string network containers readline parsec Diff regex-compat base-unicode-symbols setops streams semigroups regex-posix template-haskell transformers pointed distributive comonad contravariant profunctors semigroupoids irc setlocale snap
# (We could rely on the list in geordi.cabal, but having these here shortens the development cycle when I'm testing changes in geordi.)

COPY src /geordi/src
RUN cabal install --global /geordi/src --prefix=/usr

COPY compile-config /geordi/
WORKDIR /geordi/run

CMD ["/usr/bin/geordi-local"]
# Omitting the quotes and brackets changes the meaning to "shell form", which needs /bin/sh.

COPY prelude /geordi/src/prelude
COPY prep-image /geordi/src/
RUN /geordi/src/prep-image
