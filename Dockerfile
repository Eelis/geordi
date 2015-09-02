FROM ubuntu:15.04
RUN apt-get update && apt-get install -y libreadline-dev libboost-dev build-essential libgmp-dev pkg-config libseccomp-dev software-properties-common subversion libmpfr-dev libmpc-dev flex zlib1g-dev git libedit-dev ncurses-dev cmake bison libcap-dev python

COPY llvm-no-temp-files.patch /geordi/src/
COPY install-clang /geordi/src/
RUN /geordi/src/install-clang

#COPY install-klee /geordi/src/
#RUN /geordi/src/install-klee

ADD https://haskell.org/platform/download/7.10.2/haskell-platform-7.10.2-a-unknown-linux-deb7.tar.gz /

COPY install-gcc-trunk /geordi/src/
RUN /geordi/src/install-gcc-trunk

WORKDIR /
RUN tar xf haskell-platform-7.10.2-a-unknown-linux-deb7.tar.gz
RUN /install-haskell-platform.sh
 # 15.04's haskell-platform package is too old

RUN cabal update && cabal install --global cabal-install
RUN cabal install --global --reorder-goals --allow-newer=process filepath process deepseq mtl syb unix utf8-string network containers readline parsec Diff directory regex-compat base-unicode-symbols setops streams semigroups regex-posix template-haskell transformers pointed distributive comonad contravariant profunctors semigroupoids irc setlocale snap
# (We could rely on the list in geordi.cabal, but having these here shortens the development cycle when I'm testing changes in geordi.)

COPY src /geordi/src
RUN rm -rf /geordi/src/dist; cabal install --global /geordi/src --prefix=/usr

COPY compile-config /geordi/
WORKDIR /geordi/run
CMD ["/usr/bin/geordi-local"]

COPY prelude /geordi/src/prelude
COPY prep-image /geordi/src/
RUN /geordi/src/prep-image
