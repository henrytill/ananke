let
  pkgs = import <nixpkgs> {};

  jobs = {

    hecate =
      { compiler ? "ghc862"
      , doCheck ? ! pkgs.stdenv.isDarwin
      }:

      let
        ltpSrc = pkgs.fetchFromGitHub {
          owner  = "xngns";
          repo   = "lens-toml-parser";
          rev    = "v0.1.0.3";
          sha256 = "1km96lxc5bjcny5gjccjgcaw05ln7rr9kw1hbvbgpd1rjp0jj23x";
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
