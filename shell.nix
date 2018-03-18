{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822"
, doCheck ? true
}:

(import ./release.nix { inherit nixpkgs compiler doCheck; }).hecate.env
