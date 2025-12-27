{
  buildDunePackage,
  thisSubdirAsDuneSource,

  ppxlib,
  ppx_deriving,
}:

buildDunePackage {
  pname = "ppx_deriving_madcast";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  propagatedBuildInputs = [
    ppxlib
    ppx_deriving
  ];
}
