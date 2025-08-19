{
  description = "A password manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  nixConfig = {
    extra-substituters = [ "https://henrytill.cachix.org" ];
    extra-trusted-public-keys = [
      "henrytill.cachix.org-1:EOoUIk8e9627viyFmT6mfqghh/xtfnpzEtqT4jnyn1M="
    ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      overlay = final: prev: {
        ananke = final.rustPlatform.buildRustPackage {
          name = "ananke";
          pname = "ananke";
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          buildInputs = with final; [ sqlite ];
          nativeCheckInputs = with final; [ gnupg ];
          src = builtins.path {
            path = ./.;
            name = "ananke-src";
          };
          env = {
            ANANKE_COMMIT_HASH = "${self.rev or self.dirtyRev}";
            ANANKE_COMMIT_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";
          };
        };
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages.ananke = pkgs.ananke;
        packages.default = self.packages.${system}.ananke;
      }
    );
}
