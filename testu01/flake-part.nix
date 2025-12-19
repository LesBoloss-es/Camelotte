{
  perSystem =
    { pkgs, utils, ... }:
    let
      opkgs = pkgs.ocamlPackages;
    in
    {
      packages.testu01 = opkgs.buildDunePackage {
        pname = "testu01";
        version = "dev";
        src = utils.thisSubdirAsDuneSource ./.;
        doCheck = true;
      };
    };
}
