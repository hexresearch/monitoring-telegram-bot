{ mkDerivation, aeson, attoparsec, base, base-compat
, base64-bytestring, bytestring, deepseq, exceptions, generics-sop
, hspec, http-api-data, http-client, http-client-tls, http-media
, http-types, HUnit, monad-control, mtl, network, network-uri
, QuickCheck, safe, semigroupoids, servant, servant-server, stdenv
, string-conversions, text, transformers, transformers-base
, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.10";
  sha256 = "1aiyz6731fjgmjsppql1f5xnmqwv6qh26g4dgnvw399qgsn13r2m";
  revision = "2";
  editedCabalFile = "0j5ws3zjz748kmd7sn9vgdwp4mqdyzw26qnl46jdcf838b6klhl1";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat base64-bytestring bytestring
    exceptions generics-sop http-api-data http-client http-client-tls
    http-media http-types monad-control mtl network-uri safe
    semigroupoids servant string-conversions text transformers
    transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring deepseq generics-sop hspec
    http-api-data http-client http-media http-types HUnit mtl network
    QuickCheck servant servant-server text transformers
    transformers-compat wai warp
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
