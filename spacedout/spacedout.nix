{
  buildDunePackage,
  thisSubdirAsDuneSource,

  ppx_inline_test,
}:

buildDunePackage {
  pname = "spacedout";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  buildInputs = [ ppx_inline_test ];
}
