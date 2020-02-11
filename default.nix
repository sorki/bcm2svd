{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:
nixpkgs.haskell.packages.${compiler}.callCabal2nix "bcm2svd" ./. { }
