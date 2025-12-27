{
  buildDunePackage,
  thisSubdirAsDuneSource,

  ppxlib,
  ppx_import,
  biniou,
  ppx_deriving_qcheck,
  qcheck-alcotest,
}:

buildDunePackage {
  pname = "ppx_deriving_biniou";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  buildInputs = [
    ppxlib
    ppx_import
  ];
  propagatedBuildInputs = [ biniou ];
  doCheck = true;
  checkInputs = [
    ppx_deriving_qcheck
    qcheck-alcotest
  ];
}
