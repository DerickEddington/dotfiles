# Provide the location of the host's NixOS configuration.

let
  inherit (builtins) getEnv isPath isString pathExists substring tryEval;

  # Redefine these, instead of using `myLib`, to avoid infinite recursion that would occur with
  # `myLib.sys.myLib`; and redefine these, instead of using `lib`, to avoid depending on `lib`
  # because that argument to the ./default.nix expression-file's function sometimes needs to be
  # `null`.

  maybeEnv = name: default:
    let value = getEnv name; in
    if value == "" then default else value;

  isAbsolutePath = strOrPath: "/" == (substring 0 1 "${toString strOrPath}");

  isDirWithDefault = path: pathExists (path + "/default.nix");
in

rec {
  fileName = let
    def = maybeEnv "NIXOS_CONFIG"  # Try this standard env-var, like <nixpkgs/nixos> does.
            # Usually the env-var is undefined and so the below is the usual value.
            (tryEval <nixos-config>).value;  # `false` when the eval failed.
  in
    if isPath def then
      def
    else if def == false then
      null
    else
      assert isString def; assert isAbsolutePath def;
      /. + def;  # (Convert to path type.)

  isDefined = isPath fileName;  # False when the eval failed.

  dirName = if isDefined
            then (if isDirWithDefault fileName
                  then fileName
                  else dirOf fileName)
            else null;
}
