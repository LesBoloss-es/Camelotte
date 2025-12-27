{
  buildDunePackage,
  thisSubdirAsDuneSource,

  alcotest,
  ppx_inline_test,
}:

buildDunePackage {
  pname = "next";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  buildInputs = [ ppx_inline_test ];
  doCheck = true;
  checkInputs = [ alcotest ];
}
