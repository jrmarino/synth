### Synth -  Custom package repository builder for FreeBSD and DragonFly

Synth is an advanced concurrent (parallel) ports building tool aimed at
regular users that prefer or require building their own packages from
source.  Synth will build packages in a clean environment that exactly
mirrors the system it builds on, creates a local repository, installs a
pkg repository configuration file that causes the local packages to be
used with the highest priority, and automatically upgrades the system
with a single command.

#### > synth version

The version screen appears when Synth is executed with no command, an
unknown command, or the "version" command.  It displays the current version,
the copyright, and a summary of valid commands along with the usage.


    ====================================================================
      Custom package repository builder for FreeBSD and DragonFly 2.02
    ====================================================================
                   Copyright (C) 2015-2017 John R. Marino
    
    
    Usage: synth [zero-parameter-option]
    -or-   synth [list-option] <list of port origins | filename>
    
    zero-parameter-option includes: help, configure, version, status,
                          upgrade-system, prepare-system, status-everything
                          everything, purge-distfiles, rebuild-repository
    list-option includes: status, build, just-build, install, force, test

#### > synth help

The help screen appears when "synth help" is entered from the command line.
It provides a slightly longer explanation of each command Synth recognizes.
For the most detailed explanation of the Synth commands, please refer the
manual page, e.g. "man synth".

    Summary of command line options - see synth.1 man page for more details
    ===============================================================================
    synth status              Dry-run: Shows what 'upgrade-system' would build
    synth configure           Brings up interactive configuration menu
    synth upgrade-system      Incremental rebuild of installed packages on system.
                              Afterwards, the local repository is rebuilt and the
                              system packages are automatically upgraded.
    synth prepare-system      Like 'upgrade-system' but ends when repo is rebuilt
    synth rebuild-repository  Rebuilds local Synth repository on command
    synth purge-distfiles     Deletes obsolete source distribution files
    synth status-everything   Dry-run: Shows what 'everything' would build
    synth everything          Builds entire ports tree and rebuilds repository
    synth version             Displays version, description and usage summary
    synth help                Displays this screen
    synth status [ports]      Dry-run: Shows what will be rebuilt with given list
    synth build [ports]       Incrementally build ports based on given list, but
                              asks before updating repository and system
    synth just-build [ports]  Like 'build', but skips post-build questions
    synth install [ports]     Like 'build', but upgrades system without asking
    synth force [ports]       Like 'build', but deletes existing packages first
    
    synth test [ports]        Just builds with DEVELOPER=yes; pre-deletes pkgs
    
    [ports] is a space-delimited list of origins, e.g. editors/joe editors/emacs.
    It may also be a path to a file containing one origin per line.

There are two command forms: commands with no arguments (e.g. help, version)
and that take a list of port origins (unlimited) or a single path to file
containing lists of port origins, one per line.  These are equivalent:

```
> synth just-build editors/joe editors/nano editors/libreoffice
```

```
> synth just-build /tmp/build.list   
```

(where the file /tmp/build.list contains:
```
editors/joe
editors/nano
editors/libreoffice
```

#### > synth configure

Synth uses an interactive menu to configure itself.  It is based on profiles
that can mix and match the parts of the build system.  Most people will use
the "LiveSystem" profile, but advanced users may have custom bases installed
elsewhere and prefer to create a profile that uses them.  The configuration
also covers items such as how many builders to spawn during building, whether
tmpfs should be used, etc.

    Synth configuration profile: LiveSystem
    ===============================================================================
       [A] Ports directory            /usr/ports
       [B] Packages directory         /var/synth/live_packages
       [C] Distfiles directory        /usr/ports/distfiles
       [D] Port options directory     /var/db/ports
       [E] Build logs directory       /var/log/synth
       [F] Build base directory       /usr/obj/synth-live
       [G] System root directory      /
       [H] Compiler cache directory   disabled
       [I] Num. concurrent builders   6
       [J] Max. jobs per builder      4
       [K] Use tmpfs for work area    true
       [L] Use tmpfs for localbase    true
       [M] Display using ncurses      true
       [N] Fetch prebuilt packages    false
    
       [>]   Switch/create profiles
       [RET] Exit
    
    Press key of selection:

Just press the letter of the item that needs configuring.  All changes will be
marked with an asterisk, and pressing the Enter key (carriage return) will
save the changes.

#### > synth status
```
> synth status
> synth status [ports]
> synth status-everything
```

The "Status" command is unique in that there are three versions of it, and it has
both the singular and ports-list form.  The purpose of the "Status" command is to
perform a "dry-run" of the intended command without actually changing anything on
the system.  It will show which packages will be deleted due to failed validity
checks, and which packages will be built as a result.  The "synth status" command
looks at the current list of installed packages and checks the given ports tree.
From that it calculates which ones are outdated, and how far the rebuild will
cascade.  The result is the number of packages that will be rebuilt to bring
the current packages up to date.  The "synth status [ports]" accepts a list of
ports and checks them against the local repository, and from that determines
what actually will be rebuilt.  The "synth status-everything" command returns
the incremental list of ports that would be built if the entire ports tree is
requested (this is obviously not a command for an average user).

    # synth status databases/postgresql96-server
    These are the ports that would be built ([N]ew, [R]ebuild, [U]pgrade):
      N => databases/postgresql96-client
      N => devel/icu
      N => databases/postgresql96-server
    Total packages that would be built: 3
    The complete build list can also be found at:
    /var/synth/synth_status_results.txt

#### > synth upgrade-system

This is a popular command.  It would be used after bringing the ports tree up
to date. Executing "synth update" will perform the same analysis as
"synth status", but then starting building all the required ports concurrently.
When that is finished, the local repository will be updated, and finally the
system's pkg(8) program will be commanded to update the system using the local
repository.

![synth status](www/synth_ncurses.png)

During the build process, the build status is shown and updated every second.
The user can see the results of recent builds (up to 50, but limited to screen
size), the status of each builder, and some statistics, include system load,
swap status, and the average number of packages built per hour.  The "impulse"
statistic is the package build rate over the last 500 seconds.  The "Lines"
column in the builder section is the current length of the build log, updated
each second.

#### > synth prepare-system

This command is similar to the "synth upgrade-system" command except that it
stops after rebuilding the local repository.  The sysadmin can then upgrade
the system using pkg(8) at their leisure.

#### > synth rebuild-repository

This command will do a sanity check on all built packages and remove the
invalid ones.  It will then (re)create a local repository comprised of the
packages that remain.

#### > synth just-build [ports]

This command will build the list of given ports and when it has finished, the
program will provide a final tally of the results and exit.

#### > synth build [ports]

This command will build the list of given ports, and when it has finished, it
will prompt the user to answer "T" (True) or "F" (False) if they want to
rebuild the local repository.  Just the pre-scanning can take a few minutes so
normally the answer would be "F" until the user believes the last package has
been built.  After the repository is rebuilt, the user will be asked another
T/F question to confirm they want to install the specifically listed ports on
to the system.

#### > synth install [ports]

This command is the same as "synth build [ports]" except it will not ask any
questions.  When the build is finished, it rebuilds the repository and
installs the ports automatically.

#### > synth force [ports]

This command is similar to "synth build [ports]" but the difference is that
any package that exists for the listed ports is deleted first, even if it is
a perfectly valid and up-to-date package.  The result of a "synth build"
command could be that nothing gets built, but "synth force" will always build
what it requested.

#### > synth test [ports]

This command is similar to "synth force [ports]", but the difference is that
the ports are built under DEVELOPER mode and have additional checks.  This
makes the build logs suitable for submitting to FreeBSD Bugzilla (as an
alternative to Poudriere)

#### > synth everything

The vast majority of people will not need the "synth everything" command.
It builds every package in the ports tree and rebuilds the local repository
(without asking) when it's done.

#### > synth purge-distfiles

The command will remove all previously fetched distfiles that are no longer
referenced by the ports tree.  It takes a few minutes to scan everything
and then deletes the files en-masse without asking.

### Miscellaneous

#### Custom make.conf

Any file that matches the path "/usr/local/etc/synth/[profile]-make.conf"
where [profile] is the name of the selected configuration profile will be
appended to the builders stock make.conf.  Note that the default profile
name is "LiveSystem", so that would make the path is
"/usr/local/etc/synth/LiveSystem-make.conf" for most users.

#### Graceful exits

Do not hit Control-C during the build!  If you want to stop building, hit
the Control-Q combination.  Synth will exit as soon as it can.  If it's hit
during the building process, it will finish the packages that are currently
building but it won't start any new ones.

#### Port Options

Synth uses cached options if they have been saved.  Synth will scan these
options file before starting a build, and if any are obsolete (number of
options don't match the current port, the option names are different, etc)
then it will print out the problematic ports and halt, recommending that
the cached options either be removed or re-saved to something valid.  To
build a package with non-default options, just run
"make -C /usr/ports/[category]/[portname] config" before staring a build.

#### Build logs

Every build produces a log.  By default they are located at
/var/logs/synth, but this location is configurable.  The log name is in the
format [category]___[portname].log.

#### Regular users

Right now Synth can only be executed by the root user

#### Non-curses mode

There is a text mode for the building phase.  It shows much less information
than the curses-based screen, but if curses is acting up, the text mode gets
the job done just fine.  Also, when cron launches Synth, it is required that
it be using a profile that has disabled the curses display.

#### Web interface

Starting with version 1.60, a dynamic web report is automatically generated
for every build.  The report is created in the _Report_ subdirectory of the
logs directory (/var/log/synth/Report/index.html by default).

![web report](http://downloads.dragonlace.net/misc/synth-img/synth_www_3.png)

The report is updated 10 times per minute.  The entire build history is
retained and searchable during the run; it's reset upon subsequent runs.
In addition to the standard search and navigation options, several areas
of the report are clickable.  Click on the *Built*, *Failed*, *Ignored*,
and *Skipped* fields to click filter for results.  Clicking on the *Total*
field will remove all search filters.  Additionally, every cell in the
*No.* column will filter the history for the port origin, which is
particularly useful for finding all the ports that the builders skipped
due to build failures or finding the port set to IGNORE.  The builder ID
column cells also trigger a quick filter for all work performed by the builder.

To view the report on a localhost, simply navigate the browser to the
reports directory.  To view it remotely, a web server has to be installed,
running and configured to expose the Synth logs directory for remote
browsers.

## Frequently Asked Questions

### Synth fails when I run it inside a jail.  What can I do?

First, there is no benefit to running Synth inside a jail.  It internally
creates jail-like environments for each builder and to add another layer
around Synth doesn't provide any extra protection.  It is recommended that
you don't even bother.

Wait, you don't care about that recommendation?  You demand to be able to
run Synth in a jail?  Okay, add the following to jail.conf:

```
enforce_statfs=0
allow.mount
allow.mount.nullfs
allow.mount.tmpfs
allow.mount.devfs
allow.chflags
```
(Courtesy of Dewayne Geraghty)

### Synth can't fetch because I use a proxy server.  How can I fix this?

This requires providing custom environment variables.  Create a file named
/usr/local/etc/synth/<profile>-environment (e.g. LiveSystem-environment) and define
one variable per line, e.g. 

```
HTTP_PROXY=http://proxyserver:8888
FTP_PROXY=http://proxyserver:8888

http_proxy=http://proxyserver:8888
ftp_proxy=http://proxyserver:8888
```
Change the urls and port numbers to match your actual proxy server port, of course.

### Synth is configured to prefetch suitable officially-built packages, but very few are actually retrieved.  What's wrong?

This only happens to FreeBSD users.  On FreeBSD, there are basically two official repositories:
_Quarterly_ and _Latest_.  The 10.2 Release and later have pkg(8) configured to use the 
_Quarterly_ packages by default.  Earlier releases are configured to use the _Latest_ packages.

Either repository is fine, but the provided ports tree has to match!  If you decided to continue
with the _Quarterly_ branch (a fine choice if you don't like constant rebuilding) then you need
to provide Synth with a SVN version of the ports tree set to the same _Quarterly_ branch. As could
be deduced from the name, a new SVN branch is created every three months, so Synth users that
choose the _Quarterly_ ports and packages need to remember to switch branches in January, April,
July, and October.

If you want the newest versions of software always and still leverage prebuilt packages, then ensure
pkg(8) is configured for the _Latest_ packages and the ports tree is as well.  Unlikely
_Quarterly_ users, _Latest_ users never have to change the ports tree configuration.

### Can I use Synth to show UPDATING information?

No, but you can use __pkg(8)__ to accomplish this.  Add something like this to your ~/.cshrc file:

```
alias pnotes    'date -v -4w +%Y%m%d | xargs pkg updating --date'
```

Then the next time you log in, the command "pnotes" will display the last 4 weeks worth of 
UPDATING entries based on what is installed on the system.  Note that most entries are for
portmaster and portupgrade users, and that there will be no action for Synth/pkg users for
the great majority of the entries.

(Courtesy of Matt Smith)

### Can redundant cached port options be removed en-masse?

Yes.  It is not uncommon for an existing port options cache to contain
over a 100 redundant files.  This happens when people build ports on a
live system and just hit "ok" when the options dialog appears.  The causes
the options settings to be saved, but this is unnecessary because the
settings are the same as the defaults.  Later this can cause issues when
the port maintainer updates the options and they no longer match the saved
values.  The redundant files can be listed like this:

```
/bin/sh /usr/ports/Tools/scripts/redundant-opt-files.sh
```

To delete all the redundant options, just pipe the output into *rm*

```
/bin/sh /usr/ports/Tools/scripts/redundant-opt-files.sh | xargs rm -rf
```

### How does one configure Synth to use ccache?

First, install ccache:

```
> pkg install ccache 
or
> cd /usr/ports/devel/ccache && make install
```

Check initial configuration:

```
> ccache -s
cache directory                     /root/.ccache
primary config                      /root/.ccache/ccache.conf
secondary config      (readonly)    /usr/local/etc/ccache.conf
cache hit (direct)                     0
cache hit (preprocessed)               0
cache miss                             0
files in cache                         0
cache size                           0.0 kB
max cache size                       5.0 GB
```

Update the maximum cache size:

```
> ccache --max-size=15G
Set cache size limit to 15.0 GB
```

Set the cache location where you want it (e.g. /var/cache/ccache):

```
> ccache --set-config=cache_dir=/var/cache/ccache
```

check configuration again:

```
> ccache -s
cache directory                     /var/cache/ccache
primary config                      /root/.ccache/ccache.conf
secondary config      (readonly)    /usr/local/etc/ccache.conf
cache hit (direct)                     0
cache hit (preprocessed)               0
cache miss                             0
files in cache                         0
cache size                           0.0 kB
max cache size                      15.0 GB
```

Now run __synth configure__, selection option [H], and enter the value of 
_cache directory_ (/var/cache/ccache in this example)

While synth is building, you can run __ccache -s__ command repeatedly in
another terminal to check if the statistics are changing during the build.
If they are, ccache is properly configured.

### How to configure Synth to crossbuild binaries

Synth can be configured to crossbuild packages with a bit of help from
`binmiscctl(1)` and the `qemu-user-static` port. Currently, synth supports
armv6 and aarch64 crossbuilds on FreeBSD. Note that synth also works
natively on FreeBSD/ARM64 (aarch64), but that option is not available for
FreeBSD/ARM (armv6).

The following example demonstrates how to
build crossbuild armv6 from an amd64 host, and uses [QemuUserModeHowTo](https://wiki.freebsd.org/QemuUserModeHowTo)
as a reference:

First, set up a armv6 sysroot that can be used with `chroot(1)`:
```
# SYSROOT=/crossbuild/armv6
# cd /usr/src
# make -j 6 TARGET=arm TARGET_ARCH=armv6 buildworld
# make DESTDIR=$SYSROOT TARGET=arm TARGET_ARCH=armv6 installworld
```

If this is a new sysroot, you will also need to:
```
# make DESTDIR=$SYSROOT TARGET=arm TARGET_ARCH=armv6 distribution
```

On the host environment, build `qemu-user-static` and copy the emulator binary
into a sysroot directory that will be mounted by synth builds, eg `/sbin`.
Register armv6 binaries to use the the emulator when chroot'd.
```
# synth install emulators/qemu-user-static
# cp /usr/local/bin/qemu-arm-static $SYSROOT/sbin
# binmiscctl add armelf \
  --interpreter /sbin/qemu-arm-static \
  --magic "\x7f\x45\x4c\x46\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28\x00" \
  --mask  "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff" \
  --size 20 --set-enabled
# mount -t devfs devfs ${DESTDIR}/dev
```

The sysroot is now ready to be used, and can be tested:
```
# chroot -u root $SYSROOT
root@helios:/ # file /usr/bin/file
/usr/bin/file: ELF 32-bit LSB executable, ARM, EABI5 version 1 (FreeBSD), dynamically linked, interpreter /libexec/ld-elf.so.1, for FreeBSD 11.0 (1100510), FreeBSD-style, stripped
```

On a new chroot environment, `/var/run/ld-elf.so.hints` needs to be generated:
```
root@helios:/ # service ldconfig start
```

Set up separate log directories and package repos for synth on the host:
```
# mkdir /var/synth/armv6
# mkdir /var/log/synth-armv6
```

Create a new profile, eg: *ArmBuild*, to use when crossbuilding. In
particular, change:
* Packages directory : `/var/synth/armv6`
* Build logs directory : `/var/log/synth-armv6`
* System root directory : `/crossbuild/armv6`

To run the crossbuilding synth from the host environment, you will need to
ensure that correct profile and package repository database is consulted, eg:
```
# env SYNTHPROFILE=ArmBuild PKG_DBDIR="${SYSROOT}/var/db/pkg" synth build x11/xorg
```

Current drawbacks:
* `synth upgrade-system` will not work, as it updates the host, not the crossbuild

(Courtesy of Jonathan Chen)

## Overview Diagrams

![Relationship with ports and pkg(8)](http://downloads.dragonlace.net/misc/synth-img/synth-arch.png)

![Synth configuration summary](http://downloads.dragonlace.net/misc/synth-img/synth-config2.png)

## Significant articles on the Internet

#### kaba1ah.org : [Managing ports for multiple FreeBSD servers](http://www.kaba1ah.org/2017/07/31/managing-ports-for-multiple-freebsd-servers-2/)

## Testimonials

##### Chris_H - FreeBSD forums member - 25 January 2016

> marino@ regarding: 0.98_5 -- smooth as silk!

> Just completed updating my box, using that version. Everything went flawlessly. Failures were handled gracefully, as well as all other events. Well; save the screen corruption, due to (kernel) messages being sent to it. But I didn't see anything in the Changelog, to indicate there were any changes in that regard -- wishful thinking on my part. ;)

> Thank you very much, John. My dev box thanks you, also. :)

##### garry - FreeBSD forums member - 29 January 2016

> I used synth-0.99_2 to build a new repository, reproducing what I had previously built with ports-mgmt/poudriere using the same general setup (3 jails, 3 jobs, tmpfs for the workdir and data). Synth built my 1800 packages in about 18 hours of wall time with no problems. It had taken more than 24 hours of wall time to build that repository with poudriere(8). It seems that Synth is very efficient in setting up and tearing down builders (chroot(8)s / jail(8)s). I noticed that small packages can be done in 20 seconds by Synth but take a minimum of two minutes with poudriere(8).

> Synth is very pleasant to use. Thanks again.

##### fernandel - 2 February 2016

> I am also using Synth from the first day and after all this years which I "spent" with portmaster I am so happy and thankful that is Synth in the ports and for Marinos help too. Thank you.

> BTW: I start synth-upgrade system before bad time and in the morning is everything done

##### Crivens -  Moderator FreeBSD forum - 1 Feb 2016

> I for one, would take the moment to thank you for some great software!

> This thread is turning into rope, sort of thing. Hopefully it will turn into some chapter of the handbook.

##### protocelt - Moderator FreeBSD forum - 2 Feb 2016

> I have 900+ ports installed on my workstation. Last repository rebuild I did, with the ccache(1) cache fully populated, Synth took around 3.5 hours to rebuild the entire repository. I don't know how long Poudriere took on the same machine but I know it was longer. I thought there was a mistake and ports were not getting rebuilt, but after checking, they were all in fact rebuilt. It is quite fast.

##### PacketMan - FreeBSD forums member - 31 Jan / 3 Feb 2016

> Really liking this synth. Thank you so much Marino. :beer:

> Yes [Synth] a great tool; I can't wait to roll it out to all my machines.

##### tankist02 - FreeBSD forums member - 7 March 2016

> @marino Thank a lot for the great tool and explanation what and why it does. I used to have mysterious crashes after running portmaster. These days I use synth exclusively and crashes don't happen anymore. I tried to use poudriere and it was just too much and slow-ish for my simple needs (home desktop). 
