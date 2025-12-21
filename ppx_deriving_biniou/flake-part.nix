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
          ppxlib
          ppx_import
        ];
        propagatedBuildInputs = [ opkgs.biniou ];
        doCheck = true;
        checkInputs = with opkgs; [
          ppx_deriving_qcheck
          qcheck-alcotest
        ];
      };
    };
}
