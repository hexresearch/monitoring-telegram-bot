{ }:
let
  pkgs = import ./pkgs.nix { config = { allowUnfree = true; }; };
  # Utilities to modify haskell packages
  justStaticExecutables = pkgs.haskell.lib.justStaticExecutables;
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  dontCheck = pkgs.haskell.lib.dontCheck;
  # Filter to exclude garbage from sources of derivations
  filterHaskell = src:
    let f = name: type:
      let base = builtins.baseNameOf name;
      in pkgs.lib.cleanSourceFilter name type &&
        (type != "directory" || base != "dist");
    in builtins.filterSource f src;
  addSrcFilter = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
      src = filterHaskell drv.src;
    });

  # Package set for external deps
  externalPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld:
      let
        call = haskellPackagesNew.callPackage;
      in rec {
        servant        = dontCheck (dontHaddock (call ./nixdeps/servant.nix {}));
        servant-client = dontCheck (dontHaddock (call ./nixdeps/servant-client.nix {}));
      };
    };
in { inherit externalPackages; }
