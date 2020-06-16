let
  pkgs = import <nixpkgs> {};

  jobs = {

    hecate =
      { compiler ? "ghc883"
      , doCheck ? ! pkgs.stdenv.isDarwin
      }:

      let
        hlint-version       = "2.2.11";
        lens-family-version = "1.2.3";

        hlint            = pkgs.haskell.packages.${compiler}.callHackage "hlint"            hlint-version       {};
        lens-family-core = pkgs.haskell.packages.${compiler}.callHackage "lens-family-core" lens-family-version {};
        lens-family      = pkgs.haskell.packages.${compiler}.callHackage "lens-family"      lens-family-version { inherit lens-family-core; };

        dweSrc = pkgs.fetchFromGitHub {
          owner  = "xngns";
          repo   = "dwergaz";
          rev    = "v0.2.0.4";
          sha256 = "1cfm1k2683p0aylavgghirajk1zwzvh6wkqq7lf2ssyk26mav2pi";
        };
        ltpSrc = pkgs.fetchFromGitHub {
          owner  = "xngns";
          repo   = "lens-toml-parser";
          rev    = "v0.1.0.4";
          sha256 = "11xi8gn3zknza87l1plnrgk48z7v7ya18wr45jsr3x5w56ini585";
        };

        dwergaz          = pkgs.haskell.packages.${compiler}.callCabal2nix "dwergaz"          dweSrc {};
        lens-toml-parser = pkgs.haskell.packages.${compiler}.callCabal2nix "lens-toml-parser" ltpSrc { inherit dwergaz hlint lens-family; };
        hecateRaw        = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate"           ./.    { inherit hlint lens-family lens-toml-parser; };

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
