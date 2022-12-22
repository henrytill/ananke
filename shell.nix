{ compiler ? "ghc943"
, doCheck ? true
}:

((import ./release.nix {}).hecate { inherit compiler doCheck; }).env
