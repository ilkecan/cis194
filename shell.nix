let
  pkgs = import <nixpkgs> {};

  ghcEnv = pkgs.ghc.withPackages (haskellPkgs: with haskellPkgs; [
    stan
    tasty
    tasty-hunit
    tasty-quickcheck
    weeder
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
