let
  pkgs = import <nixpkgs> {};
  emacs = (import ./default.nix) {};
in
pkgs.mkShell {
  buildInputs = [ emacs ];
}
