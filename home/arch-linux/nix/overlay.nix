# Overlay for nixpkgs that sets `nix` to the latest version.
# Match with Arch repo as closely as we can.
final: prev: {
  nix = prev.nixVersions.latest;
}
