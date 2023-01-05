let
  inherit (builtins) isPath pathExists tryEval;
  isDirWithDefault = path: pathExists (path + "/default.nix");
in

rec {
  fileName = (tryEval <nixos-config>).value;  # `false` when the eval failed.

  isDefined = isPath fileName;  # False when the eval failed.

  dirName = if isDefined
            then (if isDirWithDefault fileName
                  then fileName
                  else dirOf fileName)
            else false;
}
