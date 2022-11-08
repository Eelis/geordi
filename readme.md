# Geordi Readme

* * *

## Table of Contents

*   [1\. Installation](#installation)
*   [2\. Running geordi](#running)
*   [3\. Localization](#localization)
*   [4\. Known problems](#problems)

* * *

## 1\. Installation

### 1.1 Prerequisites

*   An i386 or x86-64 machine
*   GNU/Linux ≥ 2.6.34
*   GCC/G++ ≥ 4.6
*   [Boost](http://www.boost.org/) ≥ 1.33 (Only the headers are needed)
*   [GHC](http://www.haskell.org/ghc/) ≥ 7.4

In addition, geordi depends on a number of [Hackage](http://hackage.haskell.org/) packages, but these will be downloaded, built, and installed automatically by [cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall).

### 1.2 Installing geordi

The easiest way to build and install geordi is with [cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall), part of the [Haskell Platform](http://hackage.haskell.org/platform//).

In the geordi directory, say:

<pre>      cabal update
      cabal install</pre>

This will download, build, and install any missing dependencies from [Hackage](http://hackage.haskell.org/), and will then build and install geordi (by default into <tt>$HOME/.cabal/</tt>).

### 1.3 Setting up a chroot

To set up a directory for geordi to chroot into when it starts, do the following:

1.  Edit <kbd>$PREFIX/share/geordi-0/compile-config</kbd> to your liking (where <kbd>$PREFIX</kbd> is the location where Cabal installed geordi, which defaults to <kbd>$HOME/.cabal/</kbd>). Check that the g++ path refers to the actual g++ binary, not some wrapper that adds output coloring or something.

2.  On Debian and Ubuntu, replace

    <pre>  group = "nobody"</pre>

    with

    <pre>  group = "nogroup"</pre>

    in <kbd>$PREFIX/share/geordi-0/jail-config</kbd>.

3.  <kbd>geordi-mkrt</kbd>
    This creates <kbd>$PREFIX/share/geordi-0/rt</kbd>, which will be our chroot root, and copies various files into it that GCC needs in order to function.

    If you ever make further modifications to <kbd>compile-config</kbd>, throw the old <kbd>rt</kbd> away and build a new one with <kbd>geordi-mkrt</kbd> (followed by <kbd>geordi-compile-prelude</kbd>).

4.  <kbd>geordi-compile-prelude</kbd>
    This compiles the prelude files in <kbd>$PREFIX/share/geordi-0/prelude</kbd> to produce <kbd>$PREFIX/share/geordi-0/rt/{prelude.hpp.gch,prelude.a,libtpreload.so.0.0}</kbd>.

Now try running

<pre>  sudo geordi-local "<< 'x'"</pre>

If this complains about missing executables like <kbd>as</kbd> and <kbd>ld</kbd> which are present in a non-standard location in your <kbd>PATH</kbd>, try

<pre>  sudo env PATH=$PATH geordi-local "<< 'x'"</pre>

If you get errors complaining about missing libraries and such (this can happen due to differing versions or configurations of gcc), try locating those on your system and manually copying them into the corresponding place in <kbd>$PREFIX/share/geordi-0/rt</kbd>.

If your <kbd>compile-config</kbd> points to a custom-built gcc in a non-standard location <kbd>/foo/bar</kbd> and you get an error about <kbd>/t</kbd> missing some libraries, try adding <kbd>-Wl,-rlink,/foo/bar/lib</kbd> to <kbd>LINK_FLAGS</kbd> in <kbd>compile-config</kbd>.

### 1.4 Running the testsuite

A modest testsuite can now be run with <kbd>sudo geordi-testsuite</kbd> (recommended).

* * *

## 2\. Running geordi

### 2.1 <kbd>geordi-local</kbd>

<kbd>geordi-local</kbd> lets one test snippets locally (that is, without connecting to any IRC server).

Usage: <kbd>sudo geordi-local [option]... [request]...</kbd>

Request syntax is described in the [manual](http://www.eelis.net/geordi/#usage), except that here the nickname (and colon/comma) must be omitted.

Options:

<dl>
<dt><kbd>-h</kbd> / <kbd>--help</kbd></dt>

<dd>
Display help and exit.

</dd>

</dl>

If any requests are specified, they are evaluated and their results are shown. If the first request starts with a request option, interject “<kbd>--</kbd>” to prevent it from being interpreted as a command option.

If no requests are specified, the program goes into a Read-Eval-Print-Loop.

### 2.2 <kbd>geordi-irc</kbd>

Usage: <kbd>sudo geordi-irc [option]...</kbd>

Starting an IRC bot with root permissions should make one seriously nervous. Geordi requires root permissions only because [chroot(2)](http://linux.die.net/man/2/chroot) does. Geordi chroots into <kbd>rt</kbd> as one of the first things it does on startup, and changes to the user and group specified in jail-config before it starts responding to IRC messages.

Options:

<dl>
<dt><kbd>-c <file></kbd> / <kbd>--config <file></kbd></dt>

<dd>
Load bot configuration from <file> instead of <kbd>irc-config</kbd>.

</dd>

<dt><kbd>-h</kbd> / <kbd>--help</kbd></dt>

<dd>
Display help and exit.

</dd>

</dl>

#### NickServ identification

To have the bot identify to NickServ, change the following line in <kbd>irc-config</kbd>:

<pre>  , nick_pass = Nothing</pre>

into:

<pre>  , nick_pass = Just "mypassword"</pre>

#### Nickless requests

To have the bot respond to nickless requests (e.g. "<kbd><< 3</kbd>") in channels #foo, #bar, and #bas, use:

<pre>  , allow_nickless_requests_in = ["#foo", "#bar", "#bas"]</pre>

#### Auto-reconnect

Geordi does not auto-reconnect. For that, just use something like

<pre>  while true; do geordi-irc; sleep 120; done</pre>

#### Connecting to multiple networks

Make config files for the different networks. Then run one <kbd>geordi-irc</kbd> instance for each network, passing -c arguments pointing to the respective config files. The <kbd>rt</kbd> directory will be safely shared.

#### Censoring phrases

Some networks automatically kick or ban clients that utter certain phrases (like botnet commands). To prevent a geordi bot from uttering these, list regexes for them in irc-config. E.g.:

<pre>  , censor = ["some naughty phrase", "some wicked utterance"]</pre>

* * *

## 3\. Localization

To get compiler diagnostics (and some (but not all) other geordi messages) in a locale of your choice, copy the following into <kbd>rt</kbd> (preserving paths, for example using <kbd>cp --parents</kbd>):

*   /usr/lib/locale/foo_BAR.utf8/*
*   /usr/share/locale/foo/LC_MESSAGES/{cpplib,gcc,libc,libstdc++,opcodes}.mo

These paths might differ somewhat on your system.

You may need to install a separate GCC locales package or other kind of language pack for your distro, to get the files above.

Furthermore, not all of the files above may be available for your locale; copy those that are.

Additionally, the LC_ALL environment variable must be set to foo_BAR.utf8 when geordi is started. Note that sudo clears environment variables by default, so either use

<pre>  sudo -E geordi-...</pre>

or use

<pre>  sudo env LC_ALL=foo_BAR.utf8 geordi-...</pre>

* * *

## 4\. Known problems

*   Because <kbd>geordi-irc</kbd> connects as root, IRC servers that query ident may get a root response and may decide to reject the connection. (Quakenet is known to do this.)
