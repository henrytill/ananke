{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base64-bytestring, bytestring, cassava, directory, filepath
, hlint, mtl, optparse-applicative, process, process-extras
, QuickCheck, quickcheck-text, SHA, sqlite-simple, stdenv, text
, time, toml-parser, unix, vector
, gnupg1compat, sqlite
}:
mkDerivation {
  pname = "hecate";
  version = "0.4.1.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  doHaddock = false;
  libraryHaskellDepends = [
    ansi-wl-pprint base base64-bytestring bytestring cassava directory
    filepath mtl optparse-applicative process process-extras SHA
    sqlite-simple text time toml-parser unix vector
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
  description = "A minimal password manager";
  license = stdenv.lib.licenses.asl20;
}
