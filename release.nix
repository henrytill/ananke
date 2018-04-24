let
  pkgs = import <nixpkgs> {};

  jobs = {

    hecate =
      { compiler ? "ghc822"
      , doCheck ? ! pkgs.stdenv.isDarwin
      }:

      let
        ltpSrc = pkgs.fetchFromGitHub {
          owner           = "xngns";
          repo            = "lens-toml-parser";
          rev             = "d22e4782e1c10ec244cee5dfed6c7bf7b9375726";
          sha256          = "1zn6micmqhf6462cnb19lk1mbxilfjgdx52z0bcvbgj8bnkhdpmf";
          fetchSubmodules = true;
        };
        lens-toml-parser = pkgs.haskell.packages.${compiler}.callCabal2nix "lens-toml-parser" ltpSrc {};
        hecateRaw        = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate" ./. { inherit lens-toml-parser; };
        extDeps          = [ pkgs.sqlite pkgs.gnupg ];
      in
        pkgs.haskell.lib.overrideCabal hecateRaw (oldAttrs: {
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
