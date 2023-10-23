# Nushell Environment Config File
#
# version = "0.86.0"

# TODO: delete
def create_right_prompt [] {
    # create a right prompt in magenta with green separators and am/pm underlined
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | format date '%x %X %p') # try to respect user's locale
    ] | str join | str replace --regex --all "([/:])" $"(ansi green)${1}(ansi magenta)" |
        str replace --regex --all "([AP]M)" $"(ansi magenta_underline)${1}")

    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code, (char space), $time_segment] | str join)
}


# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}

# TODO: Re-think how I'd use these.
# Directories to search for scripts when calling source or use
# $env.NU_LIB_DIRS = [
#     # FIXME: This default is not implemented in rust code as of 2023-09-06.
#     ($nu.default-config-dir | path join 'scripts') # add <nushell-config-dir>/scripts
# ]
# 
# # Directories to search for plugin binaries when calling register
# $env.NU_PLUGIN_DIRS = [
#     # FIXME: This default is not implemented in rust code as of 2023-09-06.
#     ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
# ]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')
