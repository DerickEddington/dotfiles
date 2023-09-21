set -e -u
[ "${VERBOSE:=0}" -ge 3 ] && set -x
[ "${VERBOSE:=0}" -ge 4 ] && set -v

selfDir=$(dirname "$0"); readonly selfDir

# shellcheck source=../../sh/helpers.sh
. "$XDG_DATA_HOME"/my/sh/helpers.sh

# Install Bash if not already
#
_my_install_bash_if_needed

# Use Bash for the rest, ASAP.
#
exec bash "$selfDir"/phase1.bash
