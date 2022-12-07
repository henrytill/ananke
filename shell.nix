{ compiler ? "ghc942"
, doCheck ? true
}:

((import ./release.nix {}).hecate { inherit compiler doCheck; }).env
