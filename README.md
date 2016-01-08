### Synth -  Custom package repository builder for FreeBSD and DragonFly

Synth is an advanced concurrent (parallel) ports building tool aimed at regular users that prefer or require building their own packages from source.  Synth will build packages in a clean environment that exactly mirrors the system it builds on, creates a local repository, installs a pkg repository configuration file that causes the local packages to be used with the highest priority, and automatically upgrades the system with a single command.

#### > synth version

The version screen appears when Synth is executed with no command, an unknown command, or the “version” command.  It displays the current version, the copyright, and a summary of valid commands along with the usage.

![synth version](https://www.dragonflybsd.org/~marino/synth-img/version.png)

#### > synth help

The help screen appears when “synth help” is entered from the command line.  It provides a slightly longer explanation of each command Synth recognizes.  For the most detailed explanation of the Synth commands, please refer the manual page, e.g. “man synth”.

![synth help](https://www.dragonflybsd.org/~marino/synth-img/help.png)

There are two command forms: commands with no arguments (e.g. help, version) and that take a list of port origins (unlimited) or a single path to file containing lists of port origins, one per line.  These are equivalent:

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

Synth uses an interactive menu to configure itself.  It is based on profiles that can mix and match the parts of the build system.  Most people will use the “Live System”, but advanced users may have custom bases installed else and prefer to create a profile that uses them.  The configuration also covers  items such as how many builders to spawn during building, whether tmpfs should be used, etc.

![synth configure](https://www.dragonflybsd.org/~marino/synth-img/configure.png)

Just press the letter of the item that needs configuring.  All changes will be marked with an asterisk, and pressing the Enter key (carriage return) will save the changes.

#### > synth status
```
> synth status
> synth status [ports]
> synth status-everything
```

The “Status” command is unique in that there are three versions of it, and it has both the singular and ports-list form.  The purpose of the “Status” command is to perform a “dry-run” of the intended command without actually changing anything on the system.  It will show which packages will be deleted due to failed validity checks, and which packages will be built as a result.  The “synth status” command looks at the current list of installed packages and checks the given ports tree.  From that it calculates which ones are outdated, and how far the rebuild will cascade.  The result is the number of packages that will be rebuilt to bring the current packages up to date.  The “synth status [ports]” accepts a list of ports and checks them against the local repository, and from that determines what actually will be rebuilt.  The “synth status-everything” command returns the incremental list of ports that would be built if the entire ports tree is requested (this is obviously not a command for an average user).

![synth status](https://www.dragonflybsd.org/~marino/synth-img/status.png)

#### > synth upgrade-system

This is a popular command.  It would be used after bringing the ports tree up to date.  Executing “synth update” will perform the same analysis as “synth status”, but then starting building all the required ports concurrently.  When that is finished, the local repository will be updated, and finally the system’s pkg(8) program will be commanded to update the system using the local repository.

![synth status](https://www.dragonflybsd.org/~marino/synth-img/ncurses.png)

During the build process, the build status is shown and updated every second.  The user can see the results of recent builds (up to 50, but limited to screen size), the status of each builder, and some statistics, include system load, swap status, and the average number of packages built per hour.  The “impulse” statistic is the package build rate over the last 500 seconds.  The “Lines” column in the builder section is the current length of the build log, updated each second.

#### > synth rebuild-repository

This command is similar to the “synth upgrade-system” command except that it stops after rebuilding the local repository.  The sysadmin can then upgrade the system using pkg(8) at their leisure.

#### > synth just-build [ports]

This command will build the list of given ports and when it has finished, the program will provide a final tally of the results and exit.

#### > synth build [ports]

This command will build the list of given ports, and when it has finished, it will prompt the user to answer “T” (True) or “F” (False) if they want to rebuild the local repository.  Just the pre-scanning can take a few minutes so normally the answer would be “F” until the user believes the last package has been built.  After the repository is rebuilt, the user will be asked another T/F question to confirm they want to install the specifically listed ports on to the system.

#### > synth install [ports]

This command is the same as “synth build [ports]” except it will not ask any questions.  When the build is finished, it rebuilds the repository and installs the ports automatically.

#### > synth force [ports]

This command is similar to “synth build [ports]” but the difference is that any package that exists for the listed ports is deleted first, even if it is a perfectly valid and up-to-date package.  The result of a “synth build” command could be that nothing gets built, but “synth force” will always build what it requested.

#### > synth test [ports]

This command is similar to “synth build [ports]”, but the difference is that the ports are built under DEVELOPER mode and have additional checks.  This makes the build logs suitable for submitting to FreeBSD Bugzilla (as an alternative to Poudriere)

#### > synth everything

The vast majority of people will not need the “synth everything” command.  It builds every package in the ports tree and rebuilds the local repository (without asking) when it’s done.

#### > synth purge-distfiles

The command will remove all previously fetched distfiles that are no longer referenced by the ports tree.  It takes a few minutes to scan everything and then deletes the files en-masse without asking.

###	Miscellaneous

#### Custom make.conf

Any file that matches the path “/usr/local/etc/synth/<profile>-make.conf” where <profile> is the name of the selected configuration profile will be appended to the builders stock make.conf.  Note that the default profile name is “Live System” with a space, so that would me the path is “/usr/local/etc/synth/Live\ System-make.conf” for most users.

#### Graceful exits

Do not hit Control-C during the build!  If you want to stop building, hit the “Escape” key.  Synth will exit as soon as it can.  If it’s hit during the building process, it will finish the packages that are currently building but it won’t start any new ones.

#### Port Options

Synth uses cached options if they have been saved.  Synth will scan these options file before starting a build, and if any are obsolete (number of options don’t match the current port, the option names are different, etc) then it will print out the problematic ports and halt, recommending that the cached options either be removed or re-saved to something valid.  To build a package with non-default options, just run “> make –C /usr/ports/<category>/<portname> config” before staring a build.

#### Build logs

Every build produces a log.  By default they are located at /var/logs/synth, but this location is configurable.  The log name is in the format <category>___<portname>.log.

#### Regular users

Right now Synth can only be executed by the root user

#### “Test” mode

The test mode is not fully implemented yet.  It needs to check leftovers and filesystem violations (although for the latter, most of the jail is read-only which may cause a build failure)

#### Hanging processes

Right now, Synth does not have a “no-hang” watchdog.  Hanging processes have to be killed manually until one is added.

#### Non-curses mode

There is a text mode for the building phase.  It shows much less information than the curses-based screen, but if curses is acting up, the text mode gets the job done just fine.

