# My User Home Directory Configuration

As used for my personal laptop.

## Noteworthy Aspects

- Incorporates [Home Manager](https://github.com/nix-community/home-manager),
  with configuration for MATE Desktop and Firefox.

- Integrates with, and uses some of the options of, my [NixOS
  configuration](https://github.com/DerickEddington/nixos-config).  Provides the
  "skeleton" for new user's home directories, giving reproducible consistent
  user environments that can be tailored per-user.

- Substantial Emacs configuration with workarounds for prominent bugs in TRAMP,
  LSP, etc.  A little configuration for other apps.  (Not managed by Home
  Manager.)

- Custom Bash history handling that archives each session's separately while
  still starting each session with its initial history being the combined
  histories of all sessions.  The grouping of each session's history is
  preserved in the combined.  Removal of duplicates and of user-specified
  ignores is done in the combined.  Extra effort is made to be robust with
  multiple concurrent sessions and with other aspects.

- Custom Bash prompt.

- Extra support for debugging of binaries (executables & libraries) installed by
  Nix packages (pre-built w/o rebuilding or locally-built), and also for
  arbitrary binaries via dedicated per-user temporary directories, according to
  your choice.  Automatic setup of GDB to find debug-info and source-code for
  all these.  Options for Home Manager to configure all this.

- GDB commands for generating LLVM (Clang, Rust, etc.) code-coverage reports
  multiple times before a process exits, which is useful to see the current
  coverage of a program before something else happens to it, and which is useful
  when injecting function calls (and other alterations) via GDB to conduct
  spontaneous tests and iteratively exploring the coverage effects.

- Enables both: sharing changes between multiple users (via the `main` branch),
  and tracking private per-user dot-files (in private per-user branches).

- [Companion support
  script](https://github.com/DerickEddington/nixos-config/blob/main/users/setup-home)
  for easily setting this up for new users, with hidden `~/.git` so that home
  directories normally do not appear as repositories (which is normally
  undesirable).  The dot-files remain a working-tree checkout of each user's
  repository.

- [Companion
  command](https://github.com/DerickEddington/nixos-config/blob/main/users/with-unhidden-gitdir.nix)
  for temporarily unhiding `~/.git`, for working on dot-files changes.
