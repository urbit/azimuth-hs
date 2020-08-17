{ mkDerivation, base, data-default-class, exceptions, haskoin-core
, hspec, memory, mtl, shelly, stdenv, text, urbit-hob, web3
}:
mkDerivation {
  pname = "azimuth-hs";
  version = "0.2.1";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base data-default-class exceptions haskoin-core memory mtl text
    urbit-hob web3
  ];
  testHaskellDepends = [ base hspec shelly text urbit-hob ];
  homepage = "https://github.com/urbit/azimuth-hs";
  description = "Interact with Azimuth from Haskell";
  license = stdenv.lib.licenses.mpl20;
}
