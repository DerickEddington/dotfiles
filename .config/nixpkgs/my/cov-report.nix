# A package/derivation so users can choose whether to install this via Home Manager or not.
# See the included file for its documentation about what it is.

{ pkgs }:

let
  inherit (builtins) readFile;
  inherit (pkgs) writeShellScriptBin;
in

writeShellScriptBin "my-cov-report" (readFile ./cov-report.bash)
