{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822"
, doCheck ? true
}:

(import ./default.nix { inherit nixpkgs compiler doCheck; }).env
