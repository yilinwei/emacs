{
  nixpkgs ? import <nixpkgs> {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  }
}:
let
  inherit (nixpkgs) pkgs;
  site = with pkgs;
    stdenv.mkDerivation {
      name = "emacs-site";
      src = lib.cleanSource ./src;
      buildInputs = [ emacs mu offlineimap ];
      buildPhase = ''
        emacs --batch --eval "(byte-recompile-file \"$(pwd)/melpa-mirror-packages.el\" t 0)"
      '';
      installPhase = ''
        mkdir -p $out/share/emacs/site-lisp
        cp -r . $out/share/emacs/site-lisp/
      '';
    };
  emacs = with pkgs;
    emacsWithPackagesFromUsePackage {
      config = builtins.readFile ./src/melpa-mirror-packages.el;
      alwaysEnsure = true;
    };
  run-emacs = with pkgs;
    writeScriptBin "run-emacs"
      ''
        #!${stdenv.shell}
        export FONTCONFIG_FILE=${fontconfig.out}/etc/fonts/fonts.conf
        ${emacs.out}/bin/emacs
      '';
in {
  inherit site;
  inherit emacs;
  inherit run-emacs;
}
