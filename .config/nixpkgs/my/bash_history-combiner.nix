{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib fetchFromGitHub rustPlatform;
in

rustPlatform.buildRustPackage rec {
  pname = "my_bash_history_combiner";
  version = "0.0.3";

  src = fetchFromGitHub {
    owner = "DerickEddington";
    repo = pname;
    rev = version;
    hash = "sha256-+Rrb7K9xrNihF/krpaG33z7cpbUH8g4Rb77Ee89voyo=";
  };

  cargoHash = "sha256-1kIF8DtLsgb0CD2ysQv2yf/RA/2pu5LDFMm0q2Xcx4E=";

  meta = with lib; {
    description = "Combines a Bash session history with a single history with dups and ignores erased.";
    homepage = "https://github.com/DerickEddington/my_bash_history_combiner";
    license = licenses.unlicense;
    maintainers = [ maintainers.DerickEddington ];
  };
}
