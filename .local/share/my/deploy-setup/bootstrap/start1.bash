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
# '$USER' is a placeholder that will be substituted with the target user's name.
readonly defaultBootstrapDotfilesRefspecs=("main:user/\$USER" "main")


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
        info "Already have a repository in ${targetHome@Q}. Skipping."
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
    if git status --porcelain --untracked-files=no --ignored=no | std grep -q -E -e '^[^ ?!] '
    then
        git commit --message="${1:?}"
    else
        : # Nothing was added, so, to avoid error, do not try to commit.
    fi
}

function has-branch {
    git show-ref --verify --quiet -- refs/heads/"${1:?}"
}

function has-tag {
    git show-ref --verify --quiet -- refs/tags/"${1:?}"
}

function start-dotfiles-repo
{
    git init
    git add --all --ignore-errors

    git config user.name "$userName"
    git config user.email "$userEmail"

    commit-if-staged 'As was.'
    git branch --move preexisting

    git tag preexisting || warn "Failed to tag \`preexisting\`"
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
    function sleep-print-dot { std sleep 5 || true ; print "." ;}

    local state='initial' recovery='none' loopCount
    local -r specRecoveryFile=$topDir/recover-merge-dotfiles
    local -r mergeBranch=merge-dotfiles-into-$(git branch --show-current)

    git checkout -b "$mergeBranch"
    has-tag preexisting && git branch --delete preexisting
    git branch deployed-from "$userBranch"

    while [ "$state" != 'done' ]
    do
        if git merge --allow-unrelated-histories --no-edit deployed-from
        then
            state='done'

        elif [ "$state" = 'initial' ]
        then
            # Abort to undo merge conflicts, to ensure they can't corrupt files that are needed
            # for logging-in as the user (e.g. ~/.profile).
            git merge --abort

            error "Failed to merge ${userBranch@Q} with preexisting!"

            info "The attempted merge was aborted, and so any merge conflicts were undone."
            info "First, you should login to the target host as the target user."
            info "There, to specify how to recover, you will write a word to the file named:"
            info "    ${specRecoveryFile@Q}"
            info "Write 'reattempt', to reattempt the same merge to resolve conflicts."
            info "Write 'raw', to do something else, if you know what you are doing."

            print "Waiting for ${specRecoveryFile@Q} ..."
            until [ -e "$specRecoveryFile" ]; do
                sleep-print-dot
            done
            println

            for (( loopCount=0 ; ; ++loopCount )) ; do
                case $(< "$specRecoveryFile") in
                    (reattempt)
                        # Allow our outer loop to do the merge again, which is expected to fail
                        # again, but handle that failure differently by allowing manual
                        # resolution.
                        state='reattempt' recovery='reattempt'
                        break
                        ;;
                    (raw)
                        # Leave the $mergeBranch (at the same commit as the `preexisting` branch)
                        # for the user to manually do the merge or otherwise rearrange.  The user
                        # wants to do something differently than the merge-fetch-checkout-delete
                        # approach this function would've done, which is why we don't do that when
                        # continuing.
                        if (( loopCount == 0 )); then
                            state='raw' recovery='raw'
                            info "Write 'done' to the file, when finished with raw recovery."
                            print "Waiting for ${specRecoveryFile@Q} ..."
                        else
                            sleep-print-dot
                        fi
                        ;;
                    (done)
                        println
                        state='done'
                        break
                        ;;
                    (*)
                        error "Unknown recovery method! Try again by writing a recognized word."
                        ;;
                esac
            done

        elif [ "$state" = 'reattempt' ]
        then
            info "Commit the merge, when finished with reattempt recovery."

            print "Waiting for clean \`git status\` ..."
            while [ "$(git status --porcelain --untracked-files=no --ignored=no | std wc -l)" \
                    -ge 1 ]; do
                sleep-print-dot
            done
            println

            state='done'

        else
            fail "Unreachable"
        fi
    done

    if [ "$recovery" != 'none' ]; then
        info "Continuing after presumed recovery of merge."
    fi

    if has-branch deployed-from ; then  # Conditional, in case raw recovery deleted it.
        if git tag deployed-from deployed-from ; then
            git branch --delete --force deployed-from
        else
            warn "Failed to tag \`deployed-from\`."
        fi
    fi

    case "$recovery" in
        (none|reattempt)
            git fetch "$GIT_DIR" "$mergeBranch":"$userBranch"
            git checkout "$userBranch"
            git branch --delete --force "$mergeBranch"
            ;;
        (failed)
            failed-to-dotfiles "recover merge into" 6
            ;;
    esac

    std rm -f "$specRecoveryFile" || true
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
    # possibility of breaking SSH access.  When this succeeds, the user's branch
    # is checked-out.
    #
    merge-dotfiles || failed-to-dotfiles "merge into" 7

    # Hide ~/.git so that the user's home directory is not seen by Git as a repository most of the
    # time.
    #
    hide-dotfiles-gitdir || failed-to-dotfiles "hide git-dir of" 8
}

function prepare-home
{
    install-dotfiles

    # These are important enough to always ensure their permissions are good, regardless of what
    # the branch's hooks do for other things.
    local -r privateThings=(
        .ssh
        .gnupg .local/share/my/emacs/elpa/gnupg
    )

    # Ensure permissions on and in ~/ are good
    #
    ( cd "$targetHome"

      function chmod-or-warn { std chmod "$@" || warn "Failed to \`chmod $*\`." ;}

      chmod-or-warn o-rwx .

      for X in "${privateThings[@]}" ; do
          if [ -e "$X" ]; then
              chmod-or-warn -R go-rwx "$X"
          fi
      done
    )

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
