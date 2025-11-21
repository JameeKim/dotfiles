{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      formatter.${system} = pkgs.treefmt.withConfig {
        runtimeInputs = [ pkgs.nixfmt ];
        settings = nixpkgs.lib.importTOML ./treefmt.toml;
      };

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
          jameekim = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = commonModules ++ [
              ./home/jameekim
              ./home/z790-eos
              (toModule "jameekim" {
                features = {
                  dev.enable = true;
                  desktop.enable = true;
                  gaming.enable = true;
                  study.enable = true;
                };
              })
            ];
          };
        };

      homeModules =
        let
          toModule = n: v: {
            _class = "homeManager";
            _file = "${builtins.toString self}#homeModules.${n}";
            imports = [ v ];
          };
        in
        builtins.mapAttrs toModule {
          arch-linux = ./home/arch-linux;
          features = ./home/features;
        };

      devShells.${system} = {
        default = self.devShells.${system}.dev;
        dev = pkgs.mkShell {
          packages = [
            self.formatter.${system}
            pkgs.nixd
            home-manager.packages.${system}.home-manager
          ];
        };
      };
    };
}
