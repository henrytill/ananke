let
  pkgs = import <nixpkgs> { config.allowBroken = true; };

  jobs = {

    hecate =
      { compiler ? "ghc924"
      , doCheck ? ! pkgs.stdenv.isDarwin
      }:

      let
        _hecate = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate" ./. {};

        extDeps = [ pkgs.gnupg ];
      in
        pkgs.haskell.lib.overrideCabal _hecate (oldAttrs: {
          inherit doCheck;
          isLibrary               = false;
          enableSharedExecutables = false;
          doHaddock               = false;
          executableSystemDepends = extDeps;
          testSystemDepends       = extDeps;
          preCheck = ''
            export GNUPGHOME="$PWD/example/gnupg"
          '';
          postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
        });
    };
in
  jobs
