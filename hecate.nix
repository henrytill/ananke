{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base64-bytestring, bytestring, cassava, directory, filepath, gnupg1compat
, hlint, htoml, memory, mtl, optparse-applicative, parsec, process
, process-extras, QuickCheck, quickcheck-text, SHA, sqlite-simple
, stdenv, text, time, transformers, unix, unordered-containers
, vector
}:
mkDerivation {
  pname = "hecate";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base base64-bytestring bytestring cassava directory
    filepath htoml memory mtl optparse-applicative parsec process
    process-extras SHA sqlite-simple text time transformers unix
    unordered-containers vector
  ];
  librarySystemDepends = [ gnupg1compat ];
  executableHaskellDepends = [
    ansi-terminal ansi-wl-pprint base directory mtl sqlite-simple
  ];
  executableSystemDepends = [ gnupg1compat ];
  testHaskellDepends = [
    base hlint mtl QuickCheck quickcheck-text sqlite-simple unix
  ];
  testSystemDepends = [ gnupg1compat ];
  preCheck = ''
    export GNUPGHOME="$PWD/example/gnupg"
  '';
  description = "A simple password manager";
  license = stdenv.lib.licenses.asl20;
}
