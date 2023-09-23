#! /usr/bin/env bash
readonly args=("$@")
readonly HOME SHELL  # Guarantee these remain the same throughout.
# shellcheck source=../../bash/helpers.bash
source "${XDG_DATA_HOME:-$HOME/.local/share}"/my/bash/helpers.bash
_my-script-prelude

# The top of our same instance of our facility's files.
#
# shellcheck disable=SC2154  # selfDirNorm is assigned by _my-script-prelude
topDir=$(norm_abs_path "$selfDirNorm"/../../../../..)
readonly topDir


# Defaults

readonly defaultBootstrapDotfilesRepo=$topDir/.git
readonly defaultDotfilesRefspecs=("HEAD:user/\$USER" "main")


# Functions

function process-vars
{
    process-env-vars
    process-args
}

function process-env-vars
{
    readonly currentLoginShell=${SHELL:-}

    userName=$(userName_given || print unknown)
    userEmail=${userName:?}@${HOSTNAME:-$(std uname -n || print unknown)}
    readonly userName userEmail
}

function process-args
{
    readonly targetHome=${args[0]}

    # Exporting these enables not needing to change our current working directory.
    #
    export GIT_DIR=$targetHome/.git GIT_WORK_TREE=$targetHome

    readonly bootstrapDotfilesRepo=${args[1]:-$defaultBootstrapDotfilesRepo}

    if (( ${#args[@]} >= 3 )); then
        readonly dotfilesRefspecs=("${args[@]:2}")
    else
        readonly dotfilesRefspecs=("${defaultDotfilesRefspecs[@]}")
    fi

    if (( ${#dotfilesRefspecs[@]} >= 1 )); then
        # The first refspec is considered the primary.
        readonly primaryDotfilesRef=${dotfilesRefspecs[0]%:*}
        userBranch=${dotfilesRefspecs[0]##*:}
        userBranch=${userBranch//\$USER/$userName}
        readonly userBranch
    else
        fail "No refspecs given!"
    fi
}

function has-repo-already {
    ( cd "$targetHome"
      [ -e .dotfiles ] || [ -e .git ] || [ -e .git-hidden ]
    )
}

function failed-to-dotfiles {
    fail "Failed to $1 ~/.dotfiles repository!" "${@:2}"
}

function start-dotfiles-repo
{
    git init --initial-branch=preexisting
    git add --all --ignore-errors
    git config user.name "$userName"
    git config user.email "$userEmail"
    if git status --porcelain | std grep -q -E -e '^A ' ; then
        git commit --message='As was.'
    else
        : # Nothing was added, so, to avoid error, do not try to commit.
    fi
}

function fetch-into-dotfiles-repo
{
    local refspecs=("${primaryDotfilesRef}:${userBranch}" "${dotfilesRefspecs[@]:1}") i
    for i in "${!refspecs[@]}"; do
        if ! [[ "${refspecs[i]}" = *:* ]]; then
            refspecs[i]="${refspecs[i]/*/&:&}"  # Construct refspec from ref.
        fi
    done

    # We don't change directory because $bootstrapDotfilesRepo might be (and often is) a relative
    # pathname.
    #
    git fetch "$bootstrapDotfilesRepo" "${refspecs[@]}"
}

function merge-dotfiles
{
    local -r mergeBranch=merge-dotfiles-into-$(git branch --show-current)

    git checkout -b "$mergeBranch"

    if git merge --allow-unrelated-histories --no-edit "$userBranch"
    then
        git fetch --no-write-fetch-head "$targetHome"/.git "$mergeBranch":"$userBranch"
        git checkout "$userBranch"
        git branch --delete --force "$mergeBranch"
    else
        git merge --abort
        # Leave the $mergeBranch for the user to manually do the merge.
        return 6
    fi
}

function hide-dotfiles-gitdir
{
    # After the previous operations, to avoid adding and tracking .dotfiles/, and to avoid having
    # .dotfiles/ until now.
    git init --separate-git-dir="$targetHome"/.dotfiles
    unset GIT_DIR GIT_WORK_TREE
    std mv "$targetHome"/.git{,-hidden}
}

function setup-dotfiles
{
    local retCode=0

    # Check if we don't need to do anything.  Doing nothing is critical for being idempotent when
    # this script is applied to the same home multiple times (e.g. via a `vagrant up` trigger).
    #
    has-repo-already && {
        println "Info: Already have a repository in ${targetHome@Q}. Skipping."
        exit 0  #  Considered a success.
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
    fetch-into-dotfiles-repo || failed-to-dotfiles "fetch into" 5

    # Merge the `preexisting` branch with the user's dotfiles branch, while keeping the
    # preexisting files checked-out.  Done this way so that ~/.ssh/authorized_keys never goes
    # missing and so that it retains whatever key(s) were originally provisioned, to prevent the
    # possibility of breaking SSH access even once briefly.  When this succeeds, the user's branch
    # is checked-out.
    #
    merge-dotfiles || {
        warn "Failed to merge ${userBranch@Q} with preexisting! You should manually do."
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

    if std grep -q -F -e "$hmComment" "$targetHome"/.gitignore
    then
        # This deletes (d) all lines starting from the comment one to the end of file ($).
        local -r sedScript="/$hmComment/,\$d"
        ( cd "$targetHome"
          std sed -e "$sedScript" .gitignore > .gitignore-changed
          std mv .gitignore-changed .gitignore
        )
    fi
}

function setup-home
{
    setup-dotfiles || warn "Failed to setup ~/.dotfiles completely."

    # Change .gitignore to not ignore files that are of interest in the target home.
    #
    change-gitignore || warn 'Failed to change .gitignore.'

    # Ensure permissions on and in ~/ are good
    #
    ( cd "$targetHome"
      std chmod o-rwx .
      std chmod -R go-rwx .ssh
    ) || warn 'Failed to chmod something(s) in ~/.'

    # Stage any of the changes to ~/ resulting from the above.
    #
    stage-changes-in-home "at end of setup-home" || true
}

function stage-changes-in-home
{
    git --git-dir="$targetHome"/.dotfiles --work-tree="$targetHome" add --all --ignore-errors || {
        warn "Failed to stage changes to ~/${1:+ $1}."
        return 11
    }
}


# Operations

process-vars
setup-home
fail "Unimplemented"
setup-packages
setup-login
commit-staged-changes





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
