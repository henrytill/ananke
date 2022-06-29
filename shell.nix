{ compiler ? "ghc8107"
, doCheck ? true
}:

((import ./release.nix).hecate { inherit compiler doCheck; }).env
