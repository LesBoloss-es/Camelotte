{
  buildDunePackage,
  thisSubdirAsDuneSource,

  ppxlib,
}:

buildDunePackage {
  pname = "ppx_monad";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  propagatedBuildInputs = [ ppxlib ];
}
