let
  pkgs = import <nixpkgs> {};

  ghcEnv = pkgs.ghc.withPackages (haskellPkgs: with haskellPkgs; [
    tasty
    tasty-hunit
    tasty-quickcheck
  ]);
in

pkgs.mkShell {
  packages = with pkgs; [
    cabal-install
    ghcEnv
    hlint
    ormolu
  ];

  shellHook =''
    export PATH="${toString ./scripts}:$PATH"
  '';
}
