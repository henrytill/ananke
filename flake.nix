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
        pkgs = nixpkgs.legacyPackages.${system};
        call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
        hecate =
          { compiler ? "ghc944"
          , doCheck ? true
          }:
          let
            flags = "-fbackend-json";
            hecate_ = call compiler "hecate" ./. flags {};
            extDeps = [ pkgs.gnupg ];
          in
          pkgs.haskell.lib.overrideCabal hecate_ (_: {
            inherit doCheck;
            isExecutable = true;
            isLibrary = false;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableSharedLibraries = false;
            executableSystemDepends = extDeps;
            testSystemDepends = extDeps;
            preCheck = ''
              export GNUPGHOME="$PWD/example/gnupg"
            '';
            postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
          });
      in {
        packages.hecate = hecate {};
        packages.default = self.packages.${system}.hecate;
      });
}
