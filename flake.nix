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
        settings = pkgs.lib.importTOML ./treefmt.toml;
      };

      homeConfigurations.jameekim = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./users/jameekim/home.nix
          ./hosts/z790-eos/home.nix
          ./modules/home
        ];
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
