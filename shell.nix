{ sources ? import ./nix/sources.nix { } }:

let

  nixpkgs = import sources.nixpkgs {};

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  inherit (pkgs) nodePackages;

  azimuth = import ./release.nix;

in

  pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = azimuth.buildInputs ++ [
      haskellPackages.cabal-install
      haskellPackages.ghc
      nodePackages.npm
      pkgs.secp256k1
      pkgs.zlib.dev
      pkgs.pkg-config
    ];
  }

