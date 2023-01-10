# Provide the evaluated value of the host's NixOS configuration.

let
  inherit (builtins) pathExists tryEval;
in

let
  exprFile = (tryEval <nixpkgs/nixos>).value;
  nixosEvaled =
    if exprFile != false && pathExists exprFile
    then (tryEval (import exprFile {})).value
    else false;
in
if nixosEvaled != false then nixosEvaled else null
