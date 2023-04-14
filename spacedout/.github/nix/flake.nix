{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.pre-commit-hooks.flakeModule ];

      systems = [ "x86_64-linux" ];

      perSystem = { pkgs, self', inputs', config, ... }: {
        formatter = pkgs.nixfmt;

        packages.default = let
          scope =
            inputs'.opam-nix.lib.buildOpamProject { inherit pkgs; } "spacedout"
            ../../. {
              merlin = "*";
              ocaml-base-compiler = "*";
              ocaml-lsp-server = "*";
              ocp-indent = "*";
              utop = "*";
            };
        in (scope.spacedout // { inherit scope; });

        devShells.default = pkgs.mkShell {
          buildInputs = with self'.packages.default.scope; [
            merlin
            ocaml-lsp-server
            ocp-indent
            utop
          ];
          inputsFrom = [ self'.packages.default ];
          shellHook = config.pre-commit.installationScript;
        };

        pre-commit.settings.hooks = {
          deadnix.enable = true;
          dune-opam-sync.enable = true;
          nixfmt.enable = true;
          opam-lint.enable = true;
          prettier.enable = true;
          topiary.enable = true;
        };
      };

      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };
}
