{ compiler ? "ghcjs86"
, nixpkgs ? <nixpkgs>
}:
let
  config = {
    allowBroken = true;
  };

  pkgs = (import nixpkgs) { inherit config; };

  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = (import ./nix/overrides.nix) pkgs;
  };

  hpkgs' = if pkgs.lib.inNixShell then pkgs.haskellPackages else hpkgs;
in {
  js4 = hpkgs'.callPackage ./default.nix { };
}

