{
  buildDunePackage,
  thisSubdirAsDuneSource,

  ppxlib,
}:

buildDunePackage {
  pname = "valet";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  doCheck = true;
  propagatedBuildInputs = [ ppxlib ];
}
