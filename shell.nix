let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "21.11";
    sha256 = "sha256-AjhmbT4UBlJWqxY0ea8a6GU2C2HdKUREkG43oRr3TZg=";
  };
  pkgs = import releasedPkgs {};
  stdenv = pkgs.stdenv;
  extraInputs = sysPkg.lib.optionals stdenv.isDarwin (with sysPkg.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices]);

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ pkgs.gnumake
                  pkgs.erlangR24
                  pkgs.wget
                  pkgs.python39
                  pkgs.python39Packages.virtualenv

                  pkgs.nodejs

                ] ++ extraInputs;
  shellHook = ''
      cd ui
      npm install
      cd ..
  '';

}
