{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base64-bytestring, bytestring, cassava, directory, filepath
, hlint, htoml, mtl, optparse-applicative, process, process-extras
, QuickCheck, quickcheck-text, SHA, sqlite-simple, stdenv, text
, time, unix, unordered-containers, vector
, gnupg1compat, sqlite
}:
mkDerivation {
  pname = "hecate";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  doHaddock = false;
  libraryHaskellDepends = [
    ansi-wl-pprint base base64-bytestring bytestring cassava directory
    filepath htoml mtl optparse-applicative process process-extras SHA
    sqlite-simple text time unix unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal ansi-wl-pprint base directory mtl sqlite-simple
  ];
  executableSystemDepends = [ sqlite gnupg1compat ];
  testHaskellDepends = [
    base directory hlint mtl QuickCheck quickcheck-text sqlite-simple
    unix
  ];
  testSystemDepends = [ sqlite gnupg1compat ];
  preCheck = ''
    export GNUPGHOME="$PWD/example/gnupg"
  '';
  postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
  description = "A simple password manager";
  license = stdenv.lib.licenses.asl20;
}
