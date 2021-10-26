let
  pkgs = import <nixpkgs> { };

  ghcEnv = pkgs.ghc.withPackages (haskellPkgs: with haskellPkgs; [
    range
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
    scan
    shellcheck
    stan
    weeder
  ];

  shellHook = ''
    export PATH="${toString ./scripts}:$PATH"
  '';
}
