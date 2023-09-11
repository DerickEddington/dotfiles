# Aspects that a user might want to customize.


# Don't expand "!".
set +o histexpand

# Save multi-line commands as multi-line (instead of as a single line converted to have ";").
shopt -s lithist


# Practically unlimited, but limited against madness.
HISTFILESIZE=10000000

# Limit a mad session's history to be small enough to not clobber much of the `combined` file.
HISTSIZE=$((HISTFILESIZE / 100))

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Don't put commands matching these in the history.
HISTIGNORE=\\:  # The `:` command by itself.
