{
  perSystem =
    { pkgs, utils, ... }:
    let
      opkgs = pkgs.ocamlPackages;

      biniou-patched = opkgs.biniou.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or [ ]) ++ [ ./biniou-1.2.2.patch ];
      });

    in
    {
      packages.ppx_deriving_biniou = opkgs.buildDunePackage {
        pname = "ppx_deriving_biniou";
        version = "dev";
        src = utils.thisSubdirAsDuneSource ./.;
        buildInputs = with opkgs; [
          ppxlib
          ppx_import
        ];
        propagatedBuildInputs = [ biniou-patched ];
        doCheck = true;
        checkInputs = with opkgs; [
          ppx_deriving_qcheck
          qcheck-alcotest
          js_of_ocaml
          js_of_ocaml-compiler
        ];
        nativeCheckInputs = with opkgs; [
          js_of_ocaml
          js_of_ocaml-compiler
          pkgs.nodejs
        ];
      };
    };
}
