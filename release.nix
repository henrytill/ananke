{ pkgs ? import <nixpkgs> {} }:
{
  hecate =
    { compiler ? "ghc943"
    , doCheck ? ! pkgs.stdenv.isDarwin
    }:
    let
      hecate_ = pkgs.haskell.packages.${compiler}.callCabal2nix "hecate" ./. {};
      extDeps = [ pkgs.gnupg ];
    in
    pkgs.haskell.lib.overrideCabal hecate_ (oldAttrs: {
      inherit doCheck;
      isExecutable            = true;
      isLibrary               = false;
      doHaddock               = false;
      enableLibraryProfiling  = false;
      enableSharedLibraries   = false;
      executableSystemDepends = extDeps;
      testSystemDepends       = extDeps;
      preCheck = ''
        export GNUPGHOME="$PWD/example/gnupg"
      '';
      postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
    });
}
