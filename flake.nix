{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {
    lib.cron = import ./default.nix { inherit (nixpkgs) lib; };
  };
}
