set -o errexit -o nounset
shopt -s assoc_expand_once extglob
(( "${VERBOSE:=0}" >= 3 )) && set -o xtrace
(( "${VERBOSE:=0}" >= 4 )) && set -o verbose

# Capture arguments, before anything else could mess with them.
#
readonly args=("$@")
readonly primaryDotfilesBranch=${1:?}
readonly currentLoginShell=${SHELL:-}
readonly HOME  # Guarantee this remains the same throughout.
readonly targetHome=${HOME:?}

# Might as well.
readonly XDG_DATA_HOME XDG_CONFIG_HOME XDG_RUNTIME_DIR XDG_STATE_HOME XDG_CACHE_HOME

# shellcheck source=../../bash/helpers.bash
source "${XDG_DATA_HOME:?}"/my/bash/helpers.bash


# TODO: Remember the various situations this can be run for:
# - From minimal bootstrap collection of files, when ~/.dotfiles does not exist yet.
# - From a separate temp checkout of the dotfiles repo, when ~/.dotfiles does not exist yet.
# - Any other(s) desired?


# Functions

function do-in-home {
    (( $# == 1 ))  # Else: errexit.
    # shellcheck disable=2217  # Redirecting stdin just to be extra safe.
    ( cd "$targetHome" < /dev/null > /dev/null || exit
      eval "$1"
    )
}

function has-dotfiles {
    [ -e "$targetHome"/.dotfiles ]
}

function failed-to-dotfiles {
    fail "Failed to $1 ~/.dotfiles repository!" "${@:2}"
}

function start-dotfiles-repo {
    local -r userName=$(std logname || std id -u -n || std echo unknown)
    local -r userEmail=${userName:?}@${HOSTNAME:-$(std uname -n)}
    do-in-home "
        git init --initial-branch=preexisting .
        git add --ignore-errors .
        git init --separate-git-dir=.dotfiles .  # After, to avoid adding .dotfiles/.
        git config user.name ${userName@Q}
        git config user.email ${userEmail@Q}
        if git status --porcelain | std grep -q -E -e '^A ' ; then
            git commit --message='As was.'
        else
            : # Nothing was added, so, to avoid error, do not try to commit.
        fi
    "
}

function merge-dotfiles {
    # shellcheck disable=SC2016
    do-in-home '
        export GIT_DIR=.dotfiles
        mergeBranch=merge-dotfiles-into-$(git branch --show-current)

        git checkout -b "$mergeBranch"

        if git merge --allow-unrelated-histories --no-edit '"${primaryDotfilesBranch@Q}"'
        then
            git fetch --no-write-fetch-head . "$mergeBranch":'"${primaryDotfilesBranch@Q}"'
            git checkout '"${primaryDotfilesBranch@Q}"'
            git branch --delete --force "$mergeBranch"
        else
            git merge --abort
            # Leave the $mergeBranch for the user to manually do the merge.
            exit 6
        fi
    '
}

function hide-dotfiles-gitdir {
    do-in-home 'std mv .git .git-hidden'
}

function setup-dotfiles
{
    local retCode=0

    # Check if we don't need to do anything.  Doing nothing is critical for being idempotent when
    # this script is applied to the same home multiple times (e.g. via a `vagrant up` trigger).
    #
    has-dotfiles && {
        println "Info: Already has \`.dotfiles\` in ${targetHome@Q}. Skipping."
        return 0  #  Considered a success.
    }

    # Install Git if not already
    #
    _my_install_git_if_needed

    # Create ~/.dotfiles repository, and track all preexisting files in ~/ in their own separate
    # branch.
    #
    start-dotfiles-repo || failed-to-dotfiles "start" 4

    # Fetch host user's designated branch
    #
    fetch-into-dotfiles-repo || failed-to-dotfiles "push to" 5

    # Merge the `preexisting` branch with the user's dotfiles branch, while keeping the
    # preexisting files checked-out.  Done this way so that ~/.ssh/authorized_keys never goes
    # missing and so that it retains whatever key(s) were originally provisioned, to prevent the
    # possibility of breaking SSH access even once briefly.  When this succeeds, the user's branch
    # is checked-out.
    #
    merge-dotfiles || {
        warn "Failed to merge ${primaryDotfilesBranch@Q} with preexisting! You should manually do."
        retCode=7
    }

    # Hide ~/.git so that the user's home directory is not seen by Git as a repository most of the
    # time.
    #
    hide-dotfiles-gitdir || failed-to-dotfiles "hide git-dir of" 8

    return $retCode
}

function change-gitignore {
    local -r hmComment='# Things managed by home-manager'

    if do-in-home "std grep -q -F -e ${hmComment@Q} .gitignore"
    then
        # This deletes (d) all lines starting from the comment one to the end of file ($).
        local -r sedScript="/$hmComment/,\$d"
        do-in-home "
            std sed -e ${sedScript@Q} .gitignore > .gitignore-changed
            std mv .gitignore-changed .gitignore
        "
    fi
}

function setup-home
{
    setup-dotfiles || warn "Failed to setup ~/.dotfiles completely."

    # Change .gitignore to not ignore files that are of interest in the remote home.
    #
    change-gitignore || warn 'Failed to change .gitignore.'

    # Ensure permissions on and in ~/ are good
    #
    do-in-home '
        std chmod o-rwx .
        std chmod -R go-rwx .ssh
    ' || warn 'Failed to chmod something(s) in ~/.'

    # Stage any of the changes to ~/ resulting from the above.
    #
    stage-changes-in-home "at end of setup-home" || true
}

function stage-changes-in-home {
    do-in-home 'git --git-dir=.dotfiles add --all --ignore-errors' || {
        warn "Failed to stage changes to ~/${1:+ $1}."
        return 11
    }
}






# Make user's login shell be Bash, if not already.
# Hopefully, this is portable enough? (https://en.wikipedia.org/wiki/Chsh)
#
if [ "$(std basename "$currentLoginShell")" != bash ]; then
    sudo chsh -s "$(command -v bash)" "$(logname)" || warn "Failed to change login shell to Bash."
fi


# TODO: invoke install-desired-packages, or something.
#       Must be done after setting-up the user's home,
#       because some packages need to install into home along with their metadata,
#       and we want these changes to home to be captured by the user's dotfiles branch.


# Operations

setup-home
setup-packages
setup-login
commit-staged-changes
