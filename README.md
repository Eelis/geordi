Geordi Readme
=============

* * * * *

Table of Contents
-----------------

-   [1. Installation](#installation)
-   [2. Running geordi](#running)
-   [3. Localization](#localization)
-   [4. Known problems](#problems)

* * * * *

1. Installation
---------------

geordi runs as a docker container.

To build the image from which the container will be instantiated, in the
directory where Dockerfile is, say:

      sudo docker build -t geordi .

Then, to run a local interactive geordi session, say:

      sudo docker run -it geordi

Or, to run the IRC bot (using the configuration in irc-config in the
image), say:

      sudo docker run geordi geordi-irc

#### NickServ identification {#nickserv}

To have the bot identify to NickServ, change the following line in
irc-config:

      , nick_pass = Nothing

into:

      , nick_pass = Just "mypassword"

#### Nickless requests

To have the bot respond to nickless requests (e.g. "\<\< 3") in channels
\#foo, \#bar, and \#bas, use:

      , allow_nickless_requests_in = ["#foo", "#bar", "#bas"]

#### Auto-reconnect {#reconnect}

Geordi does not auto-reconnect. For that, just use something like

      while true; do geordi-irc; sleep 120; done

#### Connecting to multiple networks {#multinetwork}

Make config files for the different networks. Then run one geordi-irc
instance for each network, passing -c arguments pointing to the
respective config files. The rt directory will be safely shared.

#### Censoring phrases

Some networks automatically kick or ban clients that utter certain
phrases (like botnet commands). To prevent a geordi bot from uttering
these, list regexes for them in irc-config. E.g.:

      , censor = ["some naughty phrase", "some wicked utterance"]

* * * * *

3. Localization
---------------

To get compiler diagnostics (and some (but not all) other geordi
messages) in a locale of your choice, copy the following into rt
(preserving paths, for example using cp --parents):

-   /usr/lib/locale/foo\_BAR.utf8/\*
-   /usr/share/locale/foo/LC\_MESSAGES/{cpplib,gcc,libc,libstdc++,opcodes}.mo

These paths might differ somewhat on your system.

You may need to install a separate GCC locales package or other kind of
language pack for your distro, to get the files above.

Furthermore, not all of the files above may be available for your
locale; copy those that are.

Additionally, the LC\_ALL environment variable must be set to
foo\_BAR.utf8 when geordi is started. Note that sudo clears environment
variables by default, so either use

      sudo -E geordi-...

or use

      sudo env LC_ALL=foo_BAR.utf8 geordi-...
