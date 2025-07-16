{
  description = "BitTorrent Haskell development environment";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.stack
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.gmp
            pkgs.ncurses
            pkgs.haskell.packages.ghc947.haskell-language-server
          ];
          
          # Tell Stack to use Nix
          STACK_IN_NIX_SHELL = "1";
          
          # Make libraries visible
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.gmp
            pkgs.zlib
            pkgs.ncurses
          ];
          
          shellHook = ''
            echo "BitTorrent dev environment ready"
            export STACK_NIX_PATH=nixpkgs=${pkgs.path}
          '';
        };
      });
}
