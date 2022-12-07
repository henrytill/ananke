{
  description = "A minimal password manager";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
    {
      packages.x86_64-linux.hecate  = (import ./release.nix { inherit pkgs; }).hecate {};
      packages.x86_64-linux.default = self.packages.x86_64-linux.hecate;
    };
}
