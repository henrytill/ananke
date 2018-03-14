{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822"
, doCheck ? ! nixpkgs.stdenv.isDarwin
}:

let

  inherit (nixpkgs) pkgs;

  hecateRaw = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate" ./. {};

  hecate = pkgs.haskell.lib.overrideCabal hecateRaw (oldAttrs: {
    inherit doCheck;
    isLibrary               = false;
    enableSharedExecutables = false;
    doHaddock               = false;
    executableSystemDepends = [ pkgs.sqlite pkgs.gnupg ];
    testSystemDepends       = [ pkgs.sqlite pkgs.gnupg ];
    preCheck = ''
      export GNUPGHOME="$PWD/example/gnupg"
    '';
    postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
  });

in

  hecate
