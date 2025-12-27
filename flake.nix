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
    flake-parts.lib.mkFlake { inherit inputs; } (
      { self, lib, ... }:
      let
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

        ## Look for files <dir>/<name>.nix and produce an attribute set mapping
        ## package names to the file defining them, eg. "monadise-lwt" =
        ## /path/to/monadise/monadise-lwt.nix.
        ##
        camelottePackages = concatMapAttrs (
          dir: _:
          mapAttrs' (packageName: _: {
            name = removeSuffix ".nix" packageName;
            value = ./. + "/${dir}/${packageName}";
          }) (filterAttrs (packageName: _: hasSuffix ".nix" packageName) (readDir (./. + "/${dir}")))
        ) (filterAttrs (_: kind: kind == "directory") (readDir ./.));

      in

      {
        systems = [
          "x86_64-linux"
          "aarch64-linux"
        ];

        ## An overlay for nixpkgs that adds all of the Camelotte packages to the
        ## `ocamlPackages` scope. This is the preferred way of consuming this
        ## flake, because it allows overriding dependencies of Camelotte
        ## packages rather than having them come with their own nixpkgs.
        ##
        flake.overlays.default = _: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope (
            finalScope: _:
            mapAttrs (
              _: packageFile:
              callPackageWith finalScope packageFile {
                inherit thisSubdirAsDuneSource;
              }
            ) camelottePackages
          );
        };

        perSystem =
          {
            self',
            pkgs,
            inputs',
            system,
            ...
          }:
          let
            inherit (inputs'.topiary.lib) gitHookBinFor gitHookFor;
            myTopiaryConfig = {
              includeLanguages = [
                "ocaml"
                "ocaml_interface"
              ];
            };
          in
          {
            ## Inject our own overlay into the `pkgs` argument. This will make
            ## all our packages available in `pkgs.ocamlPackages`, which will in
            ## turn be injected in the `callPackage` calls of the packages, so
            ## they can refer to one another (as long as there is no recursion).
            ##
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };

            ## Since our packages are already in `pkgs`, we only grab them from
            ## there to expose them as an output of the flake. We also expose
            ## the Attic client such that the CI can grab it without having to
            ## pull a different nixpkgs.
            ##
            packages = mapAttrs (packageName: _: pkgs.ocamlPackages.${packageName}) camelottePackages // {
              attic = pkgs.attic-client;
            };

            ## Our devShell provides all the inputs of our own packages, as well
            ## as Git hooks and some development packages.
            ##
            devShells.default = pkgs.mkShell {
              inputsFrom = attrValues self'.packages;
              inherit (self'.checks.git-hooks) shellHook;
              buildInputs = self'.checks.git-hooks.enabledPackages ++ [
                (gitHookBinFor myTopiaryConfig)
                pkgs.ocamlPackages.ocaml-lsp
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
      }
    );

  nixConfig = {
    extra-trusted-substituters = [
      "https://nix-cache.niols.fr/camelotte"
    ];
    extra-trusted-public-keys = [
      "camelotte:xTmn7pS8c2C2afYebd64iwzO1HaoJWiwZzQTS0Y1TMw="
    ];
  };
}
