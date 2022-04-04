{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, binary, binary-conduit
      , bytestring, conduit, conduit-extra, containers, cryptonite
      , exceptions, hashable, haskeline, hpack, lib, mtl, text
      , transformers, unordered-containers, utf8-string, vector
      }:
      mkDerivation {
        pname = "tictactoe-online";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base binary binary-conduit bytestring conduit conduit-extra
          containers cryptonite exceptions hashable haskeline mtl text
          transformers unordered-containers utf8-string vector
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          async base binary binary-conduit bytestring conduit conduit-extra
          containers cryptonite exceptions hashable haskeline mtl text
          transformers unordered-containers utf8-string vector
        ];
        prePatch = "hpack";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
