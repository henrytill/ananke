let
  pkgs = import <nixpkgs> {};

  jobs = rec {

    hecate =
      { compiler ? "ghc822"
      , doCheck ? ! pkgs.stdenv.isDarwin
      }:

      let
        hecateRaw = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate" ./. {};
      in
        pkgs.haskell.lib.overrideCabal hecateRaw (oldAttrs: {
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
    };
in
  jobs
