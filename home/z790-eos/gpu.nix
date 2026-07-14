# Settings for GPU on host `z790-eos`.
{ config, lib, ... }:
let
  # Whether to enable GPU integration.
  useGPU = config.features.desktop.enable;
  # NVIDIA driver info that must match the one installed on host OS.
  # To get the hash of a newer version, run `./scripts/fetch-nvidia-driver.sh`
  # with the version string as an argument.
  # ex) ./scripts/fetch-nvidia-driver.sh 580.105.08
  nvidia = {
    version = "610.43.03";
    sha256 = "sha256-ReLUwTSiPDXlDyU6SqY+fl6NF+PRhdSgfIpY6WEu05I=";
  };
in
{
  config = lib.mkIf useGPU {
    targets.genericLinux.gpu = {
      enable = true;
      nvidia = {
        enable = true;
        inherit (nvidia) version sha256;
      };
    };
    nixpkgs.config = {
      allowUnfree = true;
      nvidia.acceptLicense = true;
    };
  };
}
