do --env {

    # Compute these only once
    #
    let color = {
        sepline: {
            success: (ansi white)  # Usually renders as gray.
            failure: (ansi red)
        }
        user_at_host: (if (is-admin) { (ansi magenta) } else { (ansi green) })
        dir: (ansi blue)
        indicator: (if (is-admin) { (ansi red) } else { (ansi yellow) })
    }
    let sepline = {
        char: '⏤' # '-' # '─'
        datetime_format: '%F %T %Z'
    }
    let user = try { $env.USER } catch { ^id -u -n }  # TODO: Could also support non-POSIX.
    let host = (sys).host.hostname
    let user_at_host = $color.user_at_host + $user + "@" + $host
    let indicator = {
        char: (if (is-admin) { "◼" } else { "⏵" })
    }

    def print-sepline [--show-exit-code] {
        let last_exit_code = $env.LAST_EXIT_CODE  # Capture this immediately before it changes.
        let sepline = do {
            let width = (term size).columns
            # TODO: Nushell fails to capture `$color` & `$sepline` in the closure.
            # let status = (
            #     if (not $show_exit_code) or ($last_exit_code == 0) {
            #         { color: $color.sepline.success, prefix: "" }
            #     } else {
            #         { color: $color.sepline.failure,
            #           prefix: ($"!($sepline.char)($last_exit_code)") }
            #     })
            # let suffix = do {
            #     let datetime = date now | format date $sepline.datetime_format
            #     let width = $width - ($status.prefix | str length --grapheme-clusters)
            #     (($datetime + $sepline.char)
            #      | fill --width $width --alignment right --character $sepline.char)
            # }
            let status = (
                if (not $show_exit_code) or ($last_exit_code == 0) {
                    { color: (ansi white), prefix: "" }
                } else {
                    { color: (ansi red),
                      prefix: ($"!⏤($last_exit_code)") }
                })
            let suffix = do {
                let datetime = date now | format date '%F %T %Z'
                let width = $width - ($status.prefix | str length --grapheme-clusters)
                (($datetime + '⏤')
                 | fill --width $width --alignment right --character '⏤')
            }
            ($status.color + $status.prefix + $suffix)
        }
        print $sepline
    }

    # Perform tilde substitution on dir.  To determine if the prefix of the path matches the home
    # dir, we split the current path into segments, and compare those with the segments of the
    # home dir. In cases where the current dir is a parent of the home dir (e.g. `/home`, homedir
    # is `/home/user`), this comparison will also evaluate to true. Inside the condition, we
    # attempt to str replace `$home` with `~`.  Inside the condition, either:
    # 1. The home prefix will be replaced.
    # 2. The current dir is a parent of the home dir, so it will be unaffected by the str replace.
    def substitute-tilde [dir: string] {
        let home = try { $env.HOME } catch { $nu.home-path }
        if ($dir | path split | zip ($home | path split) | all { $in.0 == $in.1 }) {
            $dir | str replace $home "~"
        } else {
            $dir
        }
    }

    def create-left-prompt [] {
        let dir = $color.dir + (substitute-tilde $env.PWD)
        # (One trailing newline is removed later automatically by Nushell's internal handling.)
        ($user_at_host + " " + $dir + "\n\n")
    }

    $env.config.hooks.pre_prompt = ( # Prepend to ensure it captures $env.LAST_EXIT_CODE.
        $env.config.hooks.pre_prompt | prepend { print-sepline --show-exit-code })
    $env.config.hooks.pre_execution = ( # Append so it runs immediately before the REPL command.
        $env.config.hooks.pre_execution | append { print-sepline })

    load-env {
        PROMPT_COMMAND: { create-left-prompt }
        PROMPT_COMMAND_RIGHT: { null }
        # These may be closures instead, if needed to dynamically compute them.
        PROMPT_INDICATOR: ($color.indicator + $indicator.char + " ")
        PROMPT_INDICATOR_VI_INSERT: ": "
        PROMPT_INDICATOR_VI_NORMAL: "> "
        PROMPT_MULTILINE_INDICATOR: "::: "

        # If you want previously entered commands to have a different prompt from the usual one,
        # you can uncomment one or more of the following lines.
        # This can be useful if you have a 2-line prompt and it's taking up a lot of space
        # because every command entered takes up 2 lines instead of 1. You can then uncomment
        # the line below so that previously entered commands show with a single `🚀`.
        # TRANSIENT_PROMPT_COMMAND: "🚀 "
        # TRANSIENT_PROMPT_INDICATOR: ""
        # TRANSIENT_PROMPT_INDICATOR_VI_INSERT: ""
        # TRANSIENT_PROMPT_INDICATOR_VI_NORMAL: ""
        # TRANSIENT_PROMPT_MULTILINE_INDICATOR: ""
        # TRANSIENT_PROMPT_COMMAND_RIGHT: ""
    }
}
