{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base64-bytestring, bytestring, cassava, directory, exceptions
, filepath, hlint, lens-family, lens-toml-parser, mtl
, optparse-applicative, process, process-extras, QuickCheck
, quickcheck-text, SHA, sqlite-simple, stdenv, text, time
, toml-parser, transformers, unix, vector
, gnupg, sqlite
}:
mkDerivation {
  pname = "hecate";
  version = "0.6.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  doHaddock = false;
  libraryHaskellDepends = [
    ansi-wl-pprint base base64-bytestring bytestring cassava directory
    exceptions filepath lens-family lens-toml-parser mtl
    optparse-applicative process process-extras SHA sqlite-simple text
    time toml-parser transformers unix vector
  ];
  executableHaskellDepends = [
    ansi-terminal ansi-wl-pprint base directory mtl transformers
  ];
  executableSystemDepends = [ sqlite gnupg ];
  testHaskellDepends = [
    base directory exceptions hlint lens-family mtl QuickCheck
    quickcheck-text sqlite-simple text time transformers unix
  ];
  testSystemDepends = [ sqlite gnupg ];
  preCheck = ''
    export GNUPGHOME="$PWD/example/gnupg"
  '';
  postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
  description = "A minimal password manager";
  license = stdenv.lib.licenses.asl20;
}
