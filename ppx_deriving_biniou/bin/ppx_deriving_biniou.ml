(* Force the ppx_deriving_biniou module to be loaded to register the transformation *)
let () = ignore Ppx_deriving_biniou.type_decl_str

let () = Ppxlib.Driver.standalone ()
