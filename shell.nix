{ compiler ? "ghc822"
, doCheck ? true
}:

((import ./release.nix).hecate { inherit compiler doCheck; }).env
