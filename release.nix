let
  sources = import ./nix/sources.nix { };
  pkgs    = import sources.nixpkgs {};
  azimuth = pkgs.haskellPackages.callPackage ./default.nix { };
in
  azimuth
