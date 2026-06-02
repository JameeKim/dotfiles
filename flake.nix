{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      flake-parts,
      home-manager,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      {
        imports = [
          home-manager.flakeModules.home-manager
        ];

        flake = {
          homeConfigurations =
            let
              commonModules = builtins.attrValues self.homeModules ++ [
                {
                  _class = "homeManager";
                  _file = self;
                  config._module.args.flake = self;
                }
              ];
              toModule = n: v: {
                _class = "homeManager";
                _file = "${builtins.toString self}#homeConfigurations.${n}";
                imports = [ v ];
              };
            in
            {
              jameekim = withSystem "x86_64-linux" (
                { pkgs, ... }:
                home-manager.lib.homeManagerConfiguration {
                  inherit pkgs;
                  modules = commonModules ++ [
                    ./home/jameekim
                    ./home/z790-eos
                    (toModule "jameekim" {
                      features = {
                        dev = {
                          enable = true;
                          game-assets.enable = true;
                          llm.enable = true;
                        };
                        desktop.enable = true;
                        gaming.enable = true;
                        study.enable = true;
                      };
                    })
                  ];
                }
              );
            };

          homeModules = {
            arch-linux = ./home/arch-linux;
            features = ./home/features;
          };
        };

        systems = [ "x86_64-linux" ];
        perSystem =
          {
            self',
            pkgs,
            system,
            ...
          }:
          {
            formatter = pkgs.treefmt.withConfig {
              runtimeInputs = [ pkgs.nixfmt ];
              configFile = ./treefmt.toml;
            };

            devShells.default = self'.devShells.dev;
            devShells.dev = pkgs.mkShell {
              packages = [
                self'.formatter
                pkgs.nixd
                home-manager.packages.${system}.home-manager
              ];
            };
          };
      }
    );
}
