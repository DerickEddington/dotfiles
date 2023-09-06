{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib fetchFromGitHub rustPlatform;
in

rustPlatform.buildRustPackage rec {
  pname = "my_bash_history_combiner";
  version = "0.0.2";

  src = fetchFromGitHub {
    owner = "DerickEddington";
    repo = pname;
    rev = version;
    hash = "sha256-Ox7+TlP5v1uEsbC5Vb+J+/cS/Q2V2ZVU1hPfr1jJsbA=";
  };

  cargoHash = "sha256-++MXtIg7iVkYHYpQ4GtOJlmF8OU8oHth6yF/NXDR/nE=";

  meta = with lib; {
    description = "Combines a Bash session history with a single history with dups and ignores erased.";
    homepage = "https://github.com/DerickEddington/my_bash_history_combiner";
    license = licenses.unlicense;
    maintainers = [ maintainers.DerickEddington ];
  };
}
