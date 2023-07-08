{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib fetchFromGitHub rustPlatform;
in

rustPlatform.buildRustPackage rec {
  pname = "my_bash_history_combiner";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "DerickEddington";
    repo = pname;
    rev = version;
    hash = "sha256-EbZXdZw/8GlS5O/QuaNZEYFL72o1oBuw6BxrgfaPDdE=";
  };

  cargoHash = "sha256-pSH+IR2b81TAmpmjd94l9o8dFIDBb5mN7cK7ya9A6wc=";

  meta = with lib; {
    description = "Combines a Bash session history with a single history with dups and ignores erased.";
    homepage = "https://github.com/DerickEddington/my_bash_history_combiner";
    license = licenses.unlicense;
    maintainers = [ maintainers.DerickEddington ];
  };
}
