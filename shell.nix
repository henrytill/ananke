{ compiler ? "ghc924"
, doCheck ? true
}:

((import ./release.nix).hecate { inherit compiler doCheck; }).env
