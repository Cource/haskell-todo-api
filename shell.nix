{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, http-types, lib, monad-logger
      , persistent-sqlite, resourcet, text, yesod, yesod-core
      }:
      mkDerivation {
        pname = "haskell-nix-test";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base http-types monad-logger persistent-sqlite resourcet text
          yesod yesod-core
        ];
        license = lib.licenses.gpl3Only;
        mainProgram = "haskell-nix-test";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
