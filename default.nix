{
  nixpkgs ? import <nixpkgs> {
    overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        rev = "ef220b4a0990fd0f5dd25c588c24b2c1ad88c8fc";
        ref = "master";
      }))
    ];
  }
}:
let
  inherit (nixpkgs) pkgs;
  localPackage = pkgs.callPackage ./builder.nix;
in with pkgs;
  emacsWithPackagesFromUsePackage {
    config = builtins.readFile ./src/remote-packages.el;
    alwaysEnsure = true;
    override = epkgs: epkgs // {
      fandango = localPackage {
        name = "emacs-fandango";
        src = ./fandango;
        buildInputs = with epkgs; [ dash s evil ];
      };
    };
  }
