# Useful locations to have in the "source path".  (Note that `dir` adds to the front (pushes), so
# these lines are in reverse order of precedence, but within a line the order is the precedence.)

# These work with my /etc/nixos/lib/source-code-of-package.nix that automatically manages these.
# Most Nixpkgs' source files are unpacked and prepared in their own /build/ directory, and
# sourceCodeOfPackage copies build directories to /run/current-system/sw/src/of-pkg-via-my/, or
# ~/.nix-profile/src/of-pkg-via-my/, or ./result/src/of-pkg-via-my/, etc.
dir ~/.nix-profile/src/of-pkg-via-my /run/current-system/sw/src/of-pkg-via-my

# These are managed by the user.
dir ~/tmp/src ~/src ~/.local/src /tmp/src /tmp /var/tmp/src


# How a program behaves under debugging

# Can be changed by user.
target native
# Requires `target native`.
set scheduler-locking step
set step-mode on


# User interface

set disassembly-flavor intel
