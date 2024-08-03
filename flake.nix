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

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      makeAnanke =
        pkgs:
        pkgs.rustPlatform.buildRustPackage {
          name = "ananke";
          pname = "ananke";
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          buildInputs = with pkgs; [ sqlite ];
          nativeCheckInputs = with pkgs; [ gnupg ];
          src = builtins.path {
            path = ./.;
            name = "ananke-src";
          };
          ANANKE_COMMIT_HASH = "${self.rev or self.dirtyRev}";
          ANANKE_COMMIT_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";
        };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.ananke = makeAnanke pkgs;
        packages.default = self.packages.${system}.ananke;
      }
    );
}
