FROM ubuntu:14.04
RUN apt-get update && apt-get install -y libreadline-dev libboost-dev build-essential libgmp-dev pkg-config libseccomp-dev

#ADD https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz /
COPY haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz /
RUN tar xvf haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
RUN /usr/local/haskell/ghc-7.8.3-x86_64/bin/activate-hs
RUN rm /haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
# 14.04's haskell-platform package is too old

RUN cabal update && cabal install filepath process deepseq mtl syb unix utf8-string network containers readline parsec Diff directory regex-compat base-unicode-symbols setops streams semigroups regex-posix template-haskell transformers pointed distributive comonad contravariant profunctors semigroupoids irc setlocale
# (We could rely on the list in geordi.cabal, but having these here shortens the development cycle when I'm testing changes in geordi.)

COPY layer /
WORKDIR /geordi-src
RUN cabal install --prefix=/usr
RUN geordi-prep-image

# Let's clean up a bit (hopefully one day we can start from a less bloated image...):

ENV SUDO_FORCE_REMOVE yes
RUN apt-get remove -y vim-common vim-tiny python3 upstart perl less iputils-ping logrotate net-tools python3.4 keyboard-configuration sudo 2>&1 > /dev/null
RUN dpkg --force-all -r apt gnupg ntpdate python3-minimal python3.4-minimal whiptail iproute2 diffutils cron bzip2 bsdutils adduser apt-utils passwd libuuid1 isc-dhcp-client perl-base ubuntu-keyring sed netcat-openbsd login mawk gpgv cpio libtext-charwidth-perl libtext-wrapi18n-perl debconf-i18n liblocale-gettext-perl libtext-iconv-perl netbase libsemanage1 libpython3-stdlib libpython3.4-stdlib mount module-init-tools libgnutls-openssl27 libgcrypt11 libgnutls26 libgpg-error0 makedev tzdata e2fsprogs insserv patch xml-core make lockfile-progs manpages manpages-dev sgml-base ncurses-base ncurses-bin shared-mime-info libxml2 libplymouth2 libpng12-0 libss2 libprocps3 libgdbm3 libncursesw5 gzip fakeroot libfakeroot libjson0 busybox-initramfs hostname libexpat1 pkg-config libpopt0 e2fslibs libestr0 libmount1 klibc-utils libklibc xkb-data ucf isc-dhcp-common file libsqlite3-0 libusb-0.1-4 libp11-kit0 initramfs-tools-bin kmod 2>&1 > /dev/null
RUN rm -rf /home /mnt /opt /var /sbin /srv /boot /media /root /usr/sbin /bin /tmp

USER nobody

CMD ["/usr/bin/geordi-local"]
