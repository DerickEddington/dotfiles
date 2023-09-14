# Aspects that a user might want to customize.


# Shell options
shopt -s assoc_expand_once  # Critical for correctness of associative-array keys! Insane!
shopt -s autocd
shopt -s cdspell
shopt -s checkhash
shopt -s checkjobs
shopt -s dirspell
#shopt -s dotglob
shopt -s extglob

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar


# For the `time` builtin command.  Like the default but with the CPU% added.
TIMEFORMAT=$'\nreal\t%3lR\nuser\t%3lU\nsys\t%3lS\nCPU%%\t%P'
