{
  perSystem =
    { pkgs, utils, ... }:
    let
      opkgs = pkgs.ocamlPackages;
    in
    {
      packages.ppx_deriving_biniou = opkgs.buildDunePackage {
        pname = "ppx_deriving_biniou";
        version = "dev";
        src = utils.thisSubdirAsDuneSource ./.;
        buildInputs = with opkgs; [
          biniou
          ppxlib
          ppx_import
        ];
        doCheck = true;
        checkInputs = with opkgs; [
          ppx_deriving_qcheck
          qcheck-alcotest
        ];
      };
    };
}
