import os, subprocess


# This is defined globally so that it's reusable by other Python snippets in GDB and so that it's
# computed only once.
[my_platform_os_variant, my_platform_variant, *my_platform_ids] = subprocess.check_output(
    ['bash', '-c',
     'source "${XDG_DATA_HOME:-${HOME:?}/.local/share}"/my/bash/helpers.bash > /dev/null'
     '  || exit \n'
     'println "${MY_PLATFORM_OS_VARIANT:?}" \n'
     'println "${MY_PLATFORM_VARIANT-}" \n'
     'for id in "${MY_PLATFORM_IDS[@]:?}"; do println "$id"; done'],
    text=True
).splitlines()


def makeDirs(dirs):
    for d in dirs:
        os.makedirs(os.path.expanduser(d), exist_ok=True)


def sourceDirs_prepend(*dirs):
    makeDirs(dirs)
    gdb.execute('dir ' + ' '.join(dirs))


def debugInfoDirs_get():
    dirs = gdb.parameter('debug-file-directory')
    return dirs.split(':') if dirs else []

def debugInfoDirs_add(*dirs):
    prev = debugInfoDirs_get()
    dirs = [d for d in dirs if not d in prev]
    makeDirs(dirs)
    gdb.set_parameter('debug-file-directory', ':'.join(dirs + prev))

def debugInfoDirs_prepend(*dirs):
    prev = debugInfoDirs_get()
    prev = [p for p in prev if not p in dirs]
    makeDirs(dirs)
    gdb.set_parameter('debug-file-directory', ':'.join(list(dirs) + prev))


# The above are defined before doing the next, so that the above may be used in the next
# source'ing.


def source_platform_specific_init():
    for platform_id in my_platform_ids:
        subinit_file = os.path.join('my/platform', platform_id, 'init')
        gdb.execute('source-subinit-if-exists ' + subinit_file)

source_platform_specific_init()

del source_platform_specific_init


# This is done after the above, so it prepends to any values set by the above.
if my_platform_variant != 'NixOS':
    # Useful locations to have in the "source path".  Source-code is often not platform-specific
    # and so it's alright to place such in the generic ~/tmp/src/ directory, even when $HOME is
    # shared across multiple hosts of different platforms (alternatively, there is also a
    # platform-specific directory).  It's assumed that /tmp/ and /var/tmp/ are per-host (i.e. not
    # shared) and so can be used to limit the visibility of source-code placed under those to be
    # per-host.  (Note that `dir` prepends to the previous value, and that it moves to the front
    # any previous elements that are the same, and the left-to-right order is the precedence.)
    sourceDirs_prepend('~/tmp/src', '/tmp/src', '/var/tmp/src')
    # Useful locations to have in the "debug-info path".  There isn't a ~/tmp/debug/ because
    # debug-info is usually platform-specific.  It's assumed that /tmp/ and /var/tmp/ are per-host
    # (i.e. not shared) and therefore already limited to being platform-specific.
    debugInfoDirs_prepend('/tmp/debug', '/var/tmp/debug')

# This is done after the above, so it prepends to any values set by the above.
sourceDirs_prepend(os.path.join('~/tmp/src/platform', my_platform_os_variant))
debugInfoDirs_prepend(os.path.join('~/tmp/debug/platform', my_platform_os_variant))
