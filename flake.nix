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
      cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
      mkAnanke =
        pkgs:
        pkgs.rustPlatform.buildRustPackage {
          pname = "ananke";
          version = "${cargoToml.package.version}-${self.shortRev or self.dirtyShortRev}";
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          nativeBuildInputs = with pkgs; [ pkg-config ];
          buildInputs = with pkgs; [ sqlite ];
          nativeCheckInputs = with pkgs; [ gnupg ];
          src = builtins.path {
            path = ./.;
            name = "ananke-src";
          };
          env = {
            ANANKE_COMMIT_HASH = "${self.rev or self.dirtyRev}";
            ANANKE_COMMIT_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";
          };
        };
      overlay = final: prev: {
        ananke = mkAnanke final;
        ananke-static = mkAnanke final.pkgsStatic;
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
        packages = {
          ananke = pkgs.ananke;
          ananke-static = pkgs.ananke-static;
          default = self.packages.${system}.ananke;
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ pkgs.ananke ];
          packages = with pkgs; [
            rust-analyzer
            rustfmt
            clippy
            cargo-deny
          ];
        };
      }
    );
}
