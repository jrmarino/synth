# synth
Next D/Ports build tool for live systems
(Alternative for Portmaster and Portupgrade tools)

Anyone that insists on building ports from source without much
consideration to changes in dependencies is pretty much doomed to
experience issues at some point.

The proper approach would be to build ports locally with Poudriere,
creating a local repository with a higher priority than the selected
official repository.  However, Poudriere takes some time to set up
and can be intimidating to casual users.

Synth hopes to address the need to build reliably on a live system
without having to pre-setup jails and porttrees.  It uses a similar
techniques to poudriere to properly rebuild strictly what is necessary.
It is intended to be user-friendly to use and much more informative
during the build.  Moreover, unlike the tools it replaces, it can
build in parallel like Poudriere.

The first (and standard) mode is to use a clean but identical base
as the current live system.  Later versions will be able to support
a ports compiler or a completely specified set of base tools.

This is a work in progress, it's not even close to usable.
