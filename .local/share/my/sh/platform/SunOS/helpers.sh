# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# Avoid source'ing this file more than once.
#
[ "${_MY_SH_SOURCED_ALREADY__PLATFORM_OS_HELPERS+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive source'ing.
_MY_SH_SOURCED_ALREADY__PLATFORM_OS_HELPERS=true


# Platform-specific identification

MY_PLATFORM_ARCH=$(std isainfo -n)  # The correct value (versus the inaccurate `uname -m`).

if [ -e /etc/release ]
then
    if std grep -q -i -F -e OpenIndiana /etc/release ; then
        MY_PLATFORM_VARIANT=OpenIndiana
    fi
fi

MY_PLATFORM_VERSION=$(std uname -r)

readonly MY_PLATFORM_ARCH MY_PLATFORM_VARIANT MY_PLATFORM_VERSION

if ! [ "${MY_PLATFORM_VARIANT-}" ]
then
    warn "Don't know how to further identify this $MY_PLATFORM_OS platform!"
fi
