# Settings related to studying.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.features.study;
in
{
  config = lib.mkIf cfg.enable {
    # Bibliography manager.
    programs.papis = {
      enable = true;

      # [settings] section
      settings = {
        # General
        use-git = false;
        doc-paths-extra-chars = "_";
        # Tools
        opentool = "xdg-open";
        browser = "floorp";
        picktool = "papis";
        editor = "nvim";
        # BibTex
        bibtex-unicode = true;
      };

      libraries = {
        programming = {
          isDefault = true;
          settings = {
            dir = "~/study/programming";
          };
        };
      };

      # Temporary fix for papis test failing for version 0.14.1 at `test_git_cli`.
      package = pkgs.papis.overrideAttrs (
        f: p: {
          disabledTests = p.disabledTests ++ [ "test_git_cli" ];
        }
      );
    };
  };
}
