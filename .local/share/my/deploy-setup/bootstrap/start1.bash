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
readonly defaultBootstrapDotfilesRefspecs=("HEAD:user/\$USER" "main")  # '$USER' is a placeholder.


# Functions

function process-vars
{
    process-env-vars
    process-args
}

function process-env-vars
{
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
        local -r bootstrapDotfilesRefspecs=("${args[@]:2}")
    else
        local -r bootstrapDotfilesRefspecs=("${defaultBootstrapDotfilesRefspecs[@]}")
    fi

    if (( ${#bootstrapDotfilesRefspecs[@]} >= 1 ))
    then
        local refspecs=("${bootstrapDotfilesRefspecs[@]}")
        local i
        for i in "${!refspecs[@]}"; do
            refspecs[i]=${refspecs[i]//\$USER/$userName}
            if ! [[ "${refspecs[i]}" = *:* ]]; then
                refspecs[i]="${refspecs[i]}:${refspecs[i]}"  # Construct refspec from ref.
            fi
        done
        readonly fetchDotfilesRefspecs=("${refspecs[@]}")

        # The first refspec is considered the primary and its <dst> determines which branch is
        # used for the user's home directory.
        #
        readonly userBranch=${fetchDotfilesRefspecs[0]##*:}
    else
        fail "No refspecs given!"
    fi
}

function has-repo-already
{
    if ( cd "$targetHome" ; [ -e .dotfiles ] || [ -e .git ] || [ -e .git-hidden ] )
    then
        println "Info: Already have a repository in ${targetHome@Q}. Skipping."
        return 0
    else
        return 1
    fi
}

function failed-to-dotfiles {
    fail "Failed to $1 ~/.dotfiles repository!" "${@:2}"
}

function commit-if-staged
{
    if git status --porcelain | std grep -q -E -e '^A ' ; then
        git commit --message="${1:?}"
    else
        : # Nothing was added, so, to avoid error, do not try to commit.
    fi
}

function start-dotfiles-repo
{
    git init --initial-branch=preexisting
    git add --all --ignore-errors

    git config user.name "$userName"
    git config user.email "$userEmail"

    commit-if-staged 'As was.'
}

function fetch-into-dotfiles-repo
{
    # We don't change directory because $bootstrapDotfilesRepo might be (and often is) a relative
    # pathname.
    #
    git fetch "$bootstrapDotfilesRepo" "${fetchDotfilesRefspecs[@]}"
}

function merge-dotfiles
{
    local -r mergeBranch=merge-dotfiles-into-$(git branch --show-current)

    git checkout -b "$mergeBranch"

    if git merge --allow-unrelated-histories --no-edit "$userBranch"
    then
        git fetch --no-write-fetch-head "$GIT_DIR" "$mergeBranch":"$userBranch"
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
    git init --separate-git-dir="${GIT_DIR%/.git}"/.dotfiles
    std mv "$GIT_DIR"{,-hidden}
    unset GIT_DIR GIT_WORK_TREE
}

function install-dotfiles
{
    local retCode=0

    # Extra check of this, to be safer.
    #
    has-repo-already && return 0  #  Considered a success.

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

function prepare-home
{
    install-dotfiles || warn "Failed to install ~/.dotfiles completely."

    # Ensure permissions on and in ~/ are good
    #
    ( cd "$targetHome"
      std chmod o-rwx .
      std chmod -R go-rwx .ssh
    ) || warn 'Failed to chmod something(s) in ~/.'

    # Delegate to the branch's approach for any further preparation of the target home.  Must only
    # be done after installing and checking-out the dotfiles.
    #
    run-hook prepare-home

    # Stage any of the changes to ~/ resulting from the above.
    #
    stage-changes-in-home "at end of prepare-home" || true
}

function stage-changes-in-home
{
    git --git-dir="$targetHome"/.dotfiles --work-tree="$targetHome" add --all --ignore-errors || {
        warn "Failed to stage changes to ~/${1:+ $1}."
        return 11
    }
}

function install-packages
{
    # Delegate to the branch's approach for installing any desired packages.  Must only be done
    # after setting-up the target home, because some packages need to install into home along with
    # their metadata, and we want these changes to home to be captured by the branch.
    #
    run-hook install-packages

    # Stage any changes to ~/ resulting from the above installing.  (E.g. `cargo install` might
    # add/update its metadata files.)
    #
    stage-changes-in-home "after install-packages" || true
}

function prepare-login
{
    # Delegate to the branch's approach for configuring logins for the target user.  Must only be
    # done after installing and checking-out the dotfiles.  Is done after the `install-packages`
    # hook, in case the branch's `prepare-login` hook wants to use some package from that.
    #
    run-hook prepare-login

    # Stage any of the changes to ~/ resulting from the above.
    #
    stage-changes-in-home "after prepare-login" || true
}

function run-hook
{
    local -r hook="$targetHome"/.config/my/deploy-setup/hooks/"${1:?}"

    ( cd "$targetHome"  # The hooks expect this.

      if [ -x "$hook" ]; then
          # Use the target home's instance of the facility's files, because they should be
          # mutually coherent with the versions of these hooks, and so that anything run (perhaps
          # transitively) by these hooks sees the target home as HOME.  We allow XDG_CACHE_HOME
          # XDG_RUNTIME_DIR to remain as they are (usually set to special temporary locations by
          # start0.sh) because that seems to make sense because running these hooks is still part
          # of the bootstrap.  (We use `env` in case any of these variables are read-only in our
          # script.)
          #
          local -r envVars=(
              HOME="$targetHome"
              XDG_CONFIG_HOME="$targetHome"/.config
              XDG_DATA_HOME="$targetHome"/.local/share
              XDG_STATE_HOME="$targetHome"/.local/state
              VERBOSE="${VERBOSE:-}"
          )
          std env "${envVars[@]}" "$hook"
      else
          :  # We don't care if it's not provided.
      fi
    )
}

function commit-staged-changes
{
    ( export GIT_DIR=$targetHome/.dotfiles GIT_WORK_TREE=$targetHome

      # shellcheck disable=SC2154  # selfBase is assigned by _my-script-prelude
      commit-if-staged "Changes made via \`$selfBase\`."
    )
}


# Operations

process-vars

# Check if we should not do anything.  Doing nothing is critical for being idempotent when this
# script is applied to the same home multiple times (e.g. via a `vagrant up` trigger).
#
if has-repo-already ; then
    exit 0  #  Considered a success.
else
    prepare-home
    install-packages
    prepare-login
    commit-staged-changes
fi
