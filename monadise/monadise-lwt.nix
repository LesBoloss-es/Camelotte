{
  buildDunePackage,
  thisSubdirAsDuneSource,

  monadise,

  lwt,
}:

buildDunePackage {
  pname = "monadise-lwt";
  version = "dev";
  src = thisSubdirAsDuneSource ./.;
  buildInputs = [
    monadise
    lwt
  ];
}
