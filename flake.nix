{
  description = "A minimal password manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    let makeHecate = system:
      { compiler ? "ghc946"
      , doCheck ? true
      , static ? false
      }:
      let
        pkgs = if static
               then nixpkgs.legacyPackages.${system}.pkgsMusl
               else nixpkgs.legacyPackages.${system};
        call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
        flags = "-fbackend-json";
        src = builtins.path { path = ./.; name = "hecate-src"; };
        hecate_ = call compiler "hecate" src flags {};
        extDeps = [ nixpkgs.legacyPackages.${system}.gnupg ];
      in
      pkgs.haskell.lib.overrideCabal hecate_ (_: {
        inherit doCheck;
        isExecutable = true;
        isLibrary = false;
        doHaddock = false;
        configureFlags = pkgs.lib.optionals (static) [
          "--enable-executable-static"
          "--extra-lib-dirs=${pkgs.gmp.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
        ];
        executableSystemDepends = extDeps;
        testSystemDepends = extDeps;
        preCheck = ''
          export GNUPGHOME="$src/example/gnupg"
        '';
        postFixup = "rm -rf $out/lib";
      });
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        hecate = makeHecate system;
      in {
        packages.hecate = hecate {};
        packages.hecate-static = hecate { static = true; };
        packages.default = self.packages.${system}.hecate;
      });
}
