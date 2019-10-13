{ mkDerivation, aeson, base, bytestring, containers, eccrypto
, ghcjs-base, ghcjs-dom, ghcjs-prim, jsaddle, lens, reflex
, reflex-dom-core, stdenv, text, time
}:
mkDerivation {
  pname = "js4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers eccrypto ghcjs-base ghcjs-dom
    ghcjs-prim jsaddle lens reflex reflex-dom-core text time
  ];
  license = stdenv.lib.licenses.gpl3;
}
