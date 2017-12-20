{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, alarmclock, async, base, http-client
      , http-client-tls, interpolatedstring-perl6, megaparsec
      , optparse-applicative, servant, servant-client, stdenv
      , telegram-api, text, time, yaml
      }:
      mkDerivation {
        pname = "monitoring-telegram-bot";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson alarmclock async base http-client http-client-tls
          interpolatedstring-perl6 megaparsec servant servant-client
          telegram-api text time yaml
        ];
        executableHaskellDepends = [ base optparse-applicative ];
        homepage = "https://github.com/hexresearch/monitoring-telegram-bot#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
