# The GNU utilities are available in FreeBSD via packages but are prefixed with "g" or "gnu".
# This file provides a helper to populate a directory to have them without the prefix.
# This helper is intended to only be used very rarely when (re)creating ./bin/.


function _my_freebsd_populate_gnu_utils_unprefixed
{
    local destDir=${1:-./gnu/bin}

    ( shopt -s extglob nullglob

      # Setup the GNU utilities for use by their normal non-prefixed names.

      # These are all installed with each of their utils prefixed with "g" or "gnu".
      local -r gnuUtilsPkgs=(coreutils gsed gtar gnugrep)
      sudo pkg install --yes "${gnuUtilsPkgs[@]}" || exit

      # Must exclude others that happen to also start with "g" (e.g. git, gettext).  At a
      # very-early stage of setting-up a fresh host, there shouldn't be any others in
      # /usr/local/bin/ that start with "g" (e.g. gcc, gdb) that should be omitted.  Later, there
      # will be, but those won't be captured if this function is run at a very-early stage.
      # TODO: If this hack is kept, it might be better to instead explicitly list only those that
      # we need, but that would require figuring-out what those are.
      #
      local -r gPrefixed=(/usr/local/bin/g!(nu?(stat)|it*|et@(opt|text)*|rep*))
      # These are actually prefixed with "gnu" (to avoid conflict with other FreeBSD utils).
      local -r gnuPrefixed=(/usr/local/bin/gnustat)

      # A directory to have them without the prefix.
      mkdir -p "$destDir" || exit
      ( cd "$destDir" || exit
        println "Setting-up $destDir"

        # Symlink them from our dir as their non-prefixed names.
        local gUtil
        for gUtil in "${gPrefixed[@]}"; do
            ln -s "$gUtil" "${gUtil#/usr/local/bin/g}" || exit
        done
        for gUtil in "${gnuPrefixed[@]}"; do
            ln -s "$gUtil" "${gUtil#/usr/local/bin/gnu}" || exit
        done

        local -r exclude=(
          # env  # If destDir is added to PATH, this causes problems with pkg.
        )
        for gUtil in "${exclude[@]}"; do
            rm "$gUtil" || exit
        done
      ) || exit
    ) || return
}
