{ mkDerivation
, lib
, base
, base64-bytestring
, binary
, bytestring
, containers
, hnix-store-core
, mtl
, network
, pipes
, pretty-simple
, stdenv
, text
, time
, unix
, unordered-containers
}:
mkDerivation {
  pname = "hnix-store-remote";
  version = "0.2.0.0";
  src = lib.sourceByRegex ./hnix-store-remote [
    "src(/.*)*(\.hs)?"
    "tests(/.*)*(\.hs)?"
    ".*\.cabal"
    "LICENSE"
    "Setup\.hs"
  ];
  libraryHaskellDepends = [
    base
    base64-bytestring
    binary
    bytestring
    containers
    hnix-store-core
    mtl
    network
    pipes
    pretty-simple
    text
    time
    unix
    unordered-containers
  ];
  homepage = "https://github.com/haskell-nix/hnix-store";
  description = "Remote hnix store";
  license = stdenv.lib.licenses.asl20;
}
