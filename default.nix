{ mkDerivation, aeson, alarmclock, async, base, http-client
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
}
