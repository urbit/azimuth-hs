let

  pkgs = import ./nix/nixpkgs.nix { };

  azimuth = pkgs.haskellPackages.callPackage ./default.nix { };

in

  azimuth
