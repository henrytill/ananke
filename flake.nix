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
    flake-utils.lib.eachDefaultSystem (system:
      let
        hecate =
          { compiler ? "ghc944"
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
            enableLibraryProfiling = false;
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags = pkgs.lib.optionals (static) [
              "--enable-executable-static"
              "--extra-lib-dirs=${pkgs.gmp.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
            ];
            executableSystemDepends = extDeps;
            testSystemDepends = extDeps;
            preCheck = ''
              export GNUPGHOME="$PWD/example/gnupg"
            '';
            postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
          });
      in {
        packages.hecate = hecate {};
        packages.hecate-static = hecate { static = true; };
        packages.default = self.packages.${system}.hecate;
      });
}
