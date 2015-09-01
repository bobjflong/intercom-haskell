{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, hspec
      , lens, mtl, stdenv, text, transformers, unordered-containers, wreq
      }:
      mkDerivation {
        pname = "intercom-haskell";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring containers lens mtl text transformers
          unordered-containers wreq
        ];
        testHaskellDepends = [
          aeson base bytestring hspec lens text unordered-containers
        ];
        description = "Haskell bindings to Intercom";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
