{
  buildDunePackage,
  thisSubdirAsDuneSource,
}:

buildDunePackage {
  pname = "testu01";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  doCheck = true;
}
