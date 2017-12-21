let
  pkgs = import ./pkgs.nix { inherit config; };
  projectPackages = (import ./dependencies.nix { }).externalPackages;

  # Configure project packages
  config = {
      allowUnfree = true;
      packageOverrides = pkgs: rec {
        haskellPackages = projectPackages;
      };
  };

in
  { project0 = pkgs.haskellPackages.callPackage ./default.nix { };
  }
