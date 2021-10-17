let
  pkgs = import <nixpkgs> {};

  ghcEnv = pkgs.ghc.withPackages (haskellPkgs: with haskellPkgs; [
    HUnit
    brittany
    cabal-install
    hlint
    tasty
    tasty-hunit
    tasty-quickcheck
  ]);
in

pkgs.mkShell {
  packages = with pkgs; [
    ghcEnv
  ];

  shellHook =''
  '';
}
