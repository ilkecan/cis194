{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs)
    fetchFromGitHub
    ghc
    haskell
    haskellPackages
    mkShell
    ;

  # https://github.com/nick8325/quickcheck/pull/336
  myQC = haskell.lib.overrideSrc haskellPackages.QuickCheck {
    src = fetchFromGitHub {
      owner = "ilkecan";
      repo = "quickcheck";
      rev = "add-complete-pragma";
      sha256 = "sha256-AY/lvhc9mtNQdyT1BYy7cWcElIZVKhE2L3PaL83ywv0=";
    };
  };

  ghcEnv = ghc.withPackages (haskellPkgs: with haskellPkgs; [
    range
    tasty
    tasty-bench
    tasty-hunit
    (tasty-quickcheck.override {
      QuickCheck = myQC;
    })
  ]);
in

mkShell {
  packages = with pkgs; with haskellPackages; [
    cabal-install
    ghcEnv
    hlint
    nix-linter
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
