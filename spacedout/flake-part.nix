{
  perSystem =
    { pkgs, ... }:
    let
      opkgs = pkgs.ocamlPackages;
    in
    {
      packages.spacedout = opkgs.buildDunePackage {
        pname = "spacedout";
        version = "dev";
        src = ./..;
        buildInputs = [ opkgs.ppx_inline_test ];
      };
    };
}
