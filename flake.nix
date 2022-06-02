{
  description = "Haxible";
  nixConfig.bash-prompt = "[nix(haxible)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/ed014c27f4d0ca772fb57d3b8985b772b0503bbd";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        packageName = "haxible";

        haskellOverrides = {
          overrides = hpFinal: hpPrev: {
            ginger = let
              src = builtins.fetchGit {
                url = "https://github.com/tdammers/ginger";
                ref = "master";
                rev = "eb33b2bdb481d9fcc08e1d59722c52a79687e665";
              };
              base = pkgs.haskell.lib.dontCheck
                (hpPrev.callCabal2nix "ginger" src { });
            in base;
            haxl = let
              src = builtins.fetchGit {
                url = "https://github.com/TristanCacqueray/haxl";
                ref = "update-hashable";
                rev = "ff52ed1301c83c1d937c5889c58b9a0a8f4ca768";
              };
              base = pkgs.haskell.lib.dontCheck
                (hpPrev.callCabal2nix "haxl" src { });
            in base;
            resource-pool = let
              src = builtins.fetchGit {
                url = "https://github.com/scrive/pool";
                ref = "master";
                rev = "e270972c3f92c7658fab764d791d28edac26b950";
              };
              base = hpPrev.callCabal2nix "resource-pool" src { };
            in base;
          };
        };

        python = pkgs.python310.withPackages (ps: with ps; [ ansible ansible-core ]);

        haskellPackages =
          pkgs.haskell.packages.ghc922.override haskellOverrides;
        drv = (haskellPackages.callCabal2nix packageName self { }).overrideAttrs
          (_: { GIT_COMMIT = self.rev or "dirty"; });

        exe = pkgs.haskell.lib.justStaticExecutables drv;

        mkApp = script: {
          type = "app";
          program =
            builtins.toString (pkgs.writers.writeBash "app-wrapper.sh" script);
        };

      in {
        apps.default = exe;
        packages.default = drv;
        devShell = haskellPackages.shellFor {
          packages = p: [ drv ];
          GIT_COMMIT = self.rev or "dirty";

          buildInputs = with haskellPackages; [
            python
            ormolu
            ghcid
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];
        };

        devShells.hoogle = haskellPackages.shellFor {
          packages = p: [ drv ];
          withHoogle = true;
        };
      });
}
