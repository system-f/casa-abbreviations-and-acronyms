{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  tasty-hedgehog-github = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "tasty-hedgehog";
    rev = "5da389f5534943b430300a213c5ffb5d0e13459e";
    sha256 = "04pmr9q70gakd327sywpxr7qp8jnl3b0y2sqxxxcj6zj2q45q38m";
  }) {};

  these-github = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "isomorphism";
    repo = "these";
    rev = "1bbf84ea9dd6d44d5fae520aaf9c8d3f016a2054";
    sha256 = "17gbi2c5d8mkvbfxxp3hq738p5cb5vcwzhpmcvfa9qawp7dsy5qc";
  }) {};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty-hedgehog =
        if super ? tasty-hedgehog
        then super.tasty-hedgehog
        else tasty-hedgehog-github;
      these =
         if super ? these
         then super.these
         else these-github;
    };
  };

  casa-abbreviations-and-acronyms = modifiedHaskellPackages.callPackage ./casa-abbreviations-and-acronyms.nix {};
in
  casa-abbreviations-and-acronyms
