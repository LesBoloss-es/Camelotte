{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    topiary = {
      url = "github:tweag/topiary";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## NOTE: We use submodules to vendor things (eg. TestU01), so Nix needs to
    ## grab them too as part of `self`.
    self.submodules = true;
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          self',
          pkgs,
          lib,
          inputs',
          ...
        }:
        let
          opkgs = pkgs.ocamlPackages;

          inherit (builtins)
            readDir
            ;
          inherit (lib)
            attrValues
            concatMapAttrs
            mapAttrs
            mapAttrs'
            filterAttrs
            hasSuffix
            removeSuffix
            callPackageWith
            ;

          inherit (inputs'.topiary.lib) gitHookBinFor gitHookFor;
          myTopiaryConfig = {
            includeLanguages = [
              "ocaml"
              "ocaml_interface"
            ];
          };

          ## Helper to make tighter `src` arguments to the derivations.
          ##
          thisSubdirAsDuneSource =
            path:
            with lib.fileset;
            toSource {
              root = ./.;
              fileset = unions [
                ./dune-project
                (gitTracked path)
              ];
            };

          ## Look for files <dir>/<name>.nix and produce an attribute set <name>
          ## = callPackage. The callPackage is instrumented to provide regular
          ## packages, OCaml packages, and also camelottePackages themselves.
          ##
          camelottePackages =
            mapAttrs
              (
                _: packageFile:
                callPackageWith (pkgs.ocamlPackages // camelottePackages) packageFile {
                  inherit thisSubdirAsDuneSource;
                }
              )
              (
                concatMapAttrs (
                  dir: _:
                  mapAttrs' (packageName: _: {
                    name = removeSuffix ".nix" packageName;
                    value = ./. + "/${dir}/${packageName}";
                  }) (filterAttrs (packageName: _: hasSuffix ".nix" packageName) (readDir (./. + "/${dir}")))
                ) (filterAttrs (_: kind: kind == "directory") (readDir ./.))
              );

        in
        {
          packages = camelottePackages // {
            ## Expose the Attic client such that the CI can grab it without having
            ## to pull a different nixpkgs.
            attic = pkgs.attic-client;
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = attrValues self'.packages;
            inherit (self'.checks.git-hooks) shellHook;
            buildInputs = self'.checks.git-hooks.enabledPackages ++ [
              (gitHookBinFor myTopiaryConfig)
              opkgs.ocaml-lsp
            ];
          };

          checks.git-hooks = inputs'.git-hooks.lib.run {
            src = ./.;
            hooks = {
              deadnix.enable = true;
              dune-fmt.enable = true;
              dune-opam-sync.enable = true;
              nixfmt-rfc-style.enable = true;
              topiary-latest = gitHookFor myTopiaryConfig // {
                enable = true;
                ## Topiary of course does not support `val` statements in `.ml` files.
                excludes = [ "valet/test/examples/.*\\.ml" ];
              };
            };
          };
        };

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute.
      ##
      perInput = system: flake: if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://nix-cache.niols.fr/camelotte"
    ];
    extra-trusted-public-keys = [
      "camelotte:xTmn7pS8c2C2afYebd64iwzO1HaoJWiwZzQTS0Y1TMw="
    ];
  };
}
