let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "21.11";
    sha256 = "sha256-AjhmbT4UBlJWqxY0ea8a6GU2C2HdKUREkG43oRr3TZg=";
  };
  stdenv = released_pkgs.stdenv;
  released_pkgs = import releasedPkgs {};

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ released_pkgs.gnumake
                  released_pkgs.erlangR24
                  released_pkgs.wget
                  released_pkgs.python39
                  released_pkgs.python39Packages.virtualenv
                ];
  shellHook = ''
  '';

}
