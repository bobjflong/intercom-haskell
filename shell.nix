{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, hspec
      , lens, stdenv, text, wreq
      }:
      mkDerivation {
        pname = "intercom-haskell";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ aeson base bytestring containers lens text wreq ];
        testDepends = [ aeson base bytestring containers hspec lens text ];
        description = "Haskell bindings to Intercom";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
