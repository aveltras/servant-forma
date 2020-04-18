let

  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
  pkgs = import nixpkgs {
    inherit config;
    overlays = [
      (_: _: { inherit sources; })
    ];
  };
  
  ghcVersion = "ghc865";
  # https://github.com/cachix/ghcide-nix/blob/master/nix/sources.json
  # nixpkgsRev = "2d149fcaf3b794947eeb3899859a371d10d38f9f";
  
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };
  
  ghcide = import (githubTarball "cachix" "ghcide-nix" "master") {};
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  extra-deps = with pkgs.haskell.lib; (super: {

  });
  
  packages = {
    servant-forma = ./servant-forma;
    servant-forma-server = ./servant-forma-server;
  };
  
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages."${ghcVersion}".override {
        overrides = self: super: (extra-deps super) // builtins.mapAttrs (name: path: super.callCabal2nix name (gitignore path) {}) packages;
      };
    };
  };

  shell = with pkgs; haskellPackages.shellFor {
    packages = p: builtins.attrValues (builtins.mapAttrs (name: path: builtins.getAttr name haskellPackages) packages); 
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.niv
      ghcide."ghcide-${ghcVersion}"
      
      (writeShellScriptBin "testLib" '' 
        ${haskellPackages.cabal-install}/bin/cabal test --enable-library-profiling --test-show-details=direct $1 ''${@:2}
      '')

      (writeShellScriptBin "watchLib" ''
        ${ghcid}/bin/ghcid -c "cabal v2-repl $1" -W ''${@:2}
      '')

    ];
  };
  
in {
  inherit nixpkgs;
  inherit pkgs;
  inherit shell;
}
