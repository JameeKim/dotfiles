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
    version = "595.58.03";
    sha256 = "sha256-jA1Plnt5MsSrVxQnKu6BAzkrCnAskq+lVRdtNiBYKfk=";
  };

  gpuCfg = config.targets.genericLinux.gpu;
  setupPackage = gpuCfg.packages.callPackage ./setup {
    nonNixosGpu = gpuCfg.setupPackage;
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

    home.packages = [ setupPackage ];
    home.activation.notifyEosGpuSetup =
      let
        setupPath = "${lib.getExe setupPackage}";
      in
      lib.hm.dag.entryAfter [ "checkExistingGpuDrivers" ] ''
        warnEcho "If you see a message to run a script for GPU drivers,"
        warnEcho "run this instead:"
        warnEcho "  sudo ${setupPath}"
      '';
  };
}
