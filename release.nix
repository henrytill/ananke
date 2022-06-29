let
  pkgs = import <nixpkgs> { config.allowBroken = true; };

  jobs = {

    hecate =
      { compiler ? "ghc8107"
      , doCheck ? ! pkgs.stdenv.isDarwin
      }:

      let
        tpSrc  = pkgs.fetchFromGitHub {
          owner  = "henrytill";
          repo   = "toml-parser";
          rev    = "2f9000030f98e8bb79ddba1826e8def583754d3c";
          sha256 = "sha256-pvJK2eSq/Bke4PzhjToZarxMP8XGOgKMHa+JxVMjVUg=";
        };
        ltpSrc = pkgs.fetchFromGitHub {
          owner  = "henrytill";
          repo   = "lens-toml-parser";
          rev    = "cf6ad080a2da24d1b9a3548d949c21a138e4df62";
          sha256 = "sha256-seywzGUn27+IYkg6l4QTMKdBFzN0tP1JAXGBGdxN+70=";
        };

        toml-parser      = pkgs.haskell.packages.${compiler}.callCabal2nix "toml-parser"      tpSrc  {};
        lens-toml-parser = pkgs.haskell.packages.${compiler}.callCabal2nix "lens-toml-parser" ltpSrc { inherit toml-parser; };
        hecateRaw        = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate"           ./.    { inherit lens-toml-parser toml-parser; };

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
