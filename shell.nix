let
  pkgs = import <nixpkgs> { };

  ghcEnv = pkgs.ghc.withPackages (haskellPkgs: with haskellPkgs; [
    tasty
    tasty-bench
    tasty-hunit
    tasty-quickcheck
  ]);
in

pkgs.mkShell {
  packages = with pkgs; with haskellPackages; [
    cabal-install
    ghcEnv
    hlint
    nixpkgs-fmt
    ormolu
    reuse
    shellcheck
    stan
    weeder
  ];

  shellHook = ''
    export PATH="${toString ./scripts}:$PATH"
  '';
}
