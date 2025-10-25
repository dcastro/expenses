/* Template from: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#scaffolding

   List of supported systems for cross-compilation: https://github.com/NixOS/nixpkgs/blob/master/lib/systems/examples.nix

   Purescript:
    * Used `mkSpagoDerivation`: https://github.com/jeslie0/mkSpagoDerivation
    * The spago repo (https://github.com/purescript/spago)
        links to `purescript-overlay` (https://github.com/thomashoneyman/purescript-overlay),
        which links to `mkSpagoDerivation`.
    * Alternatives:
      * Spago2nix: https://github.com/justinwoo/spago2nix
        Only supports spago-legacy (https://github.com/purescript/spago-legacy),
        does not support spago@next (https://github.com/purescript/spago)
      * easy-purescript-nix: https://github.com/justinwoo/easy-purescript-nix
        I think this also would have worked, it's just more bare-bones.
        It provides the basic tools (purs, spago, esbuild) to build the project. Only thing it's missing is `parcel`.
      * Purs-nix: https://github.com/purs-nix/purs-nix
        It's essentially equivalent to `spago bundle`, it just builds the index.js bundle.
        We'd just need to run `parcel` afterwards.
        It's meant to _replace_ spago (and not work on top of it), which means we'd have to re-declare all our purescript package dependencies here, in the nix flake.

    nix flake metadata
    nix flake show --allow-import-from-derivation
*/

{
  description = "A very basic flake";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    nix-filter.url = "github:numtide/nix-filter";

    # haskell
    haskellNix.url = "github:input-output-hk/haskell.nix";

    # purescript
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, mkSpagoDerivation
    , ps-overlay, nix-filter }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        filter = nix-filter.lib;

        overlays = [
          # https://github.com/jeslie0/mkSpagoDerivation
          mkSpagoDerivation.overlays.default
          ps-overlay.overlays.default

          haskellNix.overlay
          (final: _prev: {
            # This overlay adds our project to pkgs
            expensesProject = final.haskell-nix.project' {

              # We explicitly list out every file needed to build the project
              # to avoid rebuilds when files like `README.md` or `justfile` change.
              # Related:
              #   https://github.com/numtide/nix-filter
              #   https://discourse.nixos.org/t/how-to-make-src-in-a-flake-nix-not-change-a-lot/15129
              #   https://discourse.nixos.org/t/excluding-a-subdirectory-when-using-local-paths/3954
              #   https://unix.stackexchange.com/q/720616/98391
              src = filter {
                root = ./.;
                include = [
                  "src"
                  "server"
                  ./package.yaml
                  ./stack.yaml
                  ./stack.yaml.lock
                ];
              };

              # Use stack.yaml instead of cabal.project
              projectFileName = "stack.yaml";

              # Had to upgrade to GHC 9.10.2 to workaround an issue compiling `th-orphans`, something happens with `iserv`
              # Seems to be related to this, but not 100% sure:
              # https://github.com/NixOS/nixpkgs/issues/275304
              # https://github.com/NixOS/nixpkgs/pull/383165
              # Nikolay Yakimov also ran into the same issue in 2023, while working on the morley-debugger, and fix it with this:
              # https://gitlab.com/morley-framework/morley-debugger/-/blob/b387801d59bc43d62afde0f48d956fbfcf8d7b91/flake.nix#L27
              # compiler-nix-name = "ghc967";

              # Had to upgrade to GHC 9.12.2 to workaround a TH issue compiling `jose`:
              # https://github.com/input-output-hk/haskell.nix/issues/2314#issuecomment-3191436890
              # compiler-nix-name = "ghc9102";

              compiler-nix-name = "ghc9122";

              # Workaround for: https://github.com/input-output-hk/haskell.nix/issues/2423
              modules = [{
                packages.directory.flags.os-string = true;
                packages.unix.flags.os-string = true;
              }];

              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = { };
                # hlint = {};
                # haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        # Bundle the frontend with the given backend derivation.
        mkBundle = name: server:
          pkgs.stdenv.mkDerivation {
            inherit name;
            phases = [ "installPhase" ];
            installPhase = ''
              mkdir --parents $out/bin/resources
              cp -r "${server}"/* $out
              cp -r "${
                self.packages.${system}.expenses-manager-frontend
              }"/* $out/bin/resources
            '';
          };

      in {
        packages = {
          # Build the frontend
          expenses-manager-frontend = pkgs.mkSpagoDerivation {
            spagoYaml = ./frontend/spago.yaml;
            spagoLock = ./frontend/spago.lock;
            src = ./frontend;
            nativeBuildInputs = [
              pkgs.purs-unstable
              pkgs.spago-unstable
              pkgs.esbuild
              # We need NodeJS to run `parcel`
              pkgs.nodejs_22
            ];
            version = "0.1.0";
            buildPhase = ''
              spago bundle --outfile bundle/index.js
              cp dev/* bundle/
              npm run parcel-build -- bundle/index.html --dist-dir out
              cp -r node_modules/@dvsl/zoomcharts/lib/assets/ out
            '';
            installPhase = "mkdir $out; cp -r out/* $out";
            buildNodeModulesArgs = {
              npmRoot = ./frontend;
              nodejs = pkgs.nodejs;
            };
          };

          # Build the server
          expenses-manager-server-native = (pkgs.expensesProject.flake
            { }).packages."expenses:exe:expenses-manager-server";

          # Build a dinamically linked binary for 64bit ARM
          expenses-manager-server-rpi =
            (pkgs.pkgsCross.aarch64-multiplatform.expensesProject.flake
              { }).packages."expenses:exe:expenses-manager-server";

          # Build a static binary for 64bit ARM
          # This still doesn't work,
          # I can't build the `jose` package due to this issue: https://github.com/input-output-hk/haskell.nix/issues/2362
          expenses-manager-server-rpi-static =
            (pkgs.pkgsCross.aarch64-multiplatform-musl.expensesProject.flake
              { }).packages."expenses:exe:expenses-manager-server";

          # Bundles with frontend + backend
          expenses-manager-bundle-native =
            mkBundle "expenses-manager-bundle-native"
            self.packages.${system}.expenses-manager-server-native;
          expenses-manager-bundle-rpi = mkBundle "expenses-manager-bundle-rpi"
            self.packages.${system}.expenses-manager-server-rpi;
        };

        apps = {
          expenses-manager-service = {
            type = "app";
            program = "${
                self.packages.${system}.expenses-manager-bundle-native
              }/bin/expenses-manager-server";
          };
        };

      });
}
