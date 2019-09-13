huper: helf: {
  hnix-store-core =
    helf.callPackage ./hnix-store-core.nix {};
  hnix-store-remote =
    helf.callPackage ./hnix-store-remote.nix {};
}
