# Intro

The files in this directory exist only to help a user get started setting-up KeePassXC with my
choices for some preferences.  These files are inactive, not used, and are only effective if a
user manually chooses to apply them per the following:


## `./template.kdbx`

A KeePass-DB with my desired groups and settings, to use like a template.  A user must copy (or
merge) it to make their separate DB file, e.g. `~/.local/share/my/secrets.kdbx`.  The password for
this template DB, and so also for fresh copies of it, is "password", as needed to unlock a copy to
start working with it.

1. It is *critical* that each user changes the password for their new DB, via the `Change
   Password` button that is in the `Database > Database Security... > Database Credentials` menu.
   This should be changed to a high-entropy passphrase (which KeePassXC's Password Generator can
   create for you via the little dice icon that is in the end of a password field).

2. The sub groups in the new DB should be cloned from the originals, then the originals deleted,
   then the clones renamed to remove the " - Clone" suffix.  The `Root` group should have its ID
   changed via the `Refresh database root group ID` button that is in the `Database > Database
   Settings... > Browser Integration` menu.  These actions make each and every group have a unique
   UUID different than what the template has (unfortunately, KeePassXC doesn't provide an easier
   way).

3. The newly-cloned `Secret Service` group must be designated as the one to use for that service,
   via selecting that group via the `Expose entries under this group` radio button that is in the
   `Database > Database Settings... > Secret Service Integration` menu.

4. The name of the new DB should be changed from "Template" to e.g. "Secrets", via the `Database
   name` field that is in the `Database > Database Settings... > General` menu.


## `./keepassxc-browser_settings.json`

My desired KeePassXC-Browser settings, in exported form.

1. A user must import this into the KeePassXC-Browser extension (after installing that in a web
   browser) via its settings menu.  Note: you must refresh the settings-menu page, in order to see
   the newly-imported settings.
