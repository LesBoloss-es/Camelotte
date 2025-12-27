{
  buildDunePackage,
  thisSubdirAsDuneSource,

  alcotest,
}:

buildDunePackage {
  pname = "monadise";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  doCheck = true;
  checkInputs = [ alcotest ];
}
