open Ppxlib
open Ast_builder.Default

let some x = Some x
let longident ~loc s = {txt = Longident.parse s; loc}

let map_longident f = function
  | Lident s -> Lident (f s)
  | Ldot (l, s) -> Ldot (l, f s)
  | Lapply _ -> failwith "map_longident"

(** Like [Ldot] but on the left. *)
let rec ldotl x = function
  | Lident y -> Ldot (Lident x, y)
  | Ldot (l, s) -> Ldot (ldotl x l, s)
  | Lapply _ -> failwith "ldotl"

type functions = {
  to_biniou: expression;
  of_biniou: expression;
}

type direction = To_biniou | Of_biniou

let direction_to_string = function
  | To_biniou -> "to_biniou"
  | Of_biniou -> "of_biniou"

let mangle_lid dir lid =
  Ppx_deriving.mangle_lid (`Suffix (direction_to_string dir)) lid

let mangle_type_decl dir td =
  Ppx_deriving.mangle_type_decl (`Suffix (direction_to_string dir)) td

(** Given a core_type, return Parsetree expressions that are functions to
    convert to and from biniou. *)
let rec core_type_to_functions (core_type : core_type) : functions =
  let loc = core_type.ptyp_loc in
  match core_type.ptyp_desc with
  (* | Ptyp_any *)
  (* | Ptyp_var of string *)
  (* | Ptyp_arrow of Asttypes.arg_label * core_type * core_type *)
  (* | Ptyp_tuple of core_type list *)
  (* | Ptyp_object of object_field list * Asttypes.closed_flag *)
  (* | Ptyp_class of Longident.t Asttypes.loc * core_type list *)
  (* | Ptyp_alias of core_type * string Asttypes.loc *)
  (* | Ptyp_variant of row_field list * Asttypes.closed_flag * Asttypes.label list option *)
  (* | Ptyp_poly of string Asttypes.loc list * core_type *)
  (* | Ptyp_package of package_type *)
  (* | Ptyp_open of Longident.t Asttypes.loc * core_type *)
  (* | Ptyp_extension of extension *)
  (* | Ptyp_constr of Longident.t Asttypes.loc * core_type list *)
  | Ptyp_constr (ty, args) ->
    (
      let in_runtime_lib =
        List.map (fun x -> Lident x) ["int"; "int32"; "int64"; "float"; "array"; "list"]
      in
      let ty_txt =
        if List.mem ty.txt in_runtime_lib then
          ldotl "Ppx_deriving_biniou_runtime" ty.txt
        else ty.txt
      in
      let fns = {
        to_biniou = pexp_ident ~loc {txt = mangle_lid To_biniou ty_txt; loc = ty.loc};
        of_biniou = pexp_ident ~loc {txt = mangle_lid Of_biniou ty_txt; loc = ty.loc};
      }
      in
      match args with
      | [] -> fns
      | args ->
        let args_fns = List.map core_type_to_functions args in
        {
          to_biniou = pexp_apply ~loc fns.to_biniou (List.map (fun arg_fns -> (Nolabel, arg_fns.to_biniou)) args_fns);
          of_biniou = pexp_apply ~loc fns.of_biniou (List.map (fun arg_fns -> (Nolabel, arg_fns.of_biniou)) args_fns);
        }
    )
  | _ -> Location.raise_errorf ~loc "core_type: cannot convert to/from Biniou"

(** Given a type_decl, return Parsetree expressions that are functions to
    convert to and from biniou. *)
let type_decl_to_functions (type_decl : type_declaration) : functions =
  let loc = type_decl.ptype_loc in
  match type_decl.ptype_kind with
  | Ptype_record lab_decls ->
    let to_biniou =
      pexp_fun ~loc Nolabel None (pvar ~loc "x") @@
      pexp_variant ~loc "Record" @@
      some @@
      pexp_array ~loc @@
      List.map
        (fun lab_decl ->
          let loc = lab_decl.pld_loc in
          let name = lab_decl.pld_name.txt in
          let pexp_name = let loc = lab_decl.pld_name.loc in pexp_constant ~loc (Pconst_string (name, loc, None)) in
          pexp_tuple ~loc [
            pexp_construct ~loc (longident ~loc "Some") (Some pexp_name);
            pexp_apply ~loc (pexp_ident ~loc @@ longident ~loc "Bi_io.hash_name") [(Nolabel, pexp_name)];
            pexp_apply
              ~loc
              (core_type_to_functions lab_decl.pld_type).to_biniou
              [(Nolabel, pexp_field ~loc (pexp_ident ~loc (longident ~loc "x")) (longident ~loc name))]
          ]
        )
        lab_decls
    in
    let of_biniou_name = mangle_type_decl Of_biniou type_decl in
    let of_biniou =
      pexp_function_cases ~loc [
        (
          case
            ~lhs: (
              ppat_variant ~loc "Record" (Some (pvar ~loc "r"))
            )
            ~guard: None
            ~rhs: (
              pexp_record
                ~loc
                (
                  List.map
                    (fun lab_decl ->
                      let loc = lab_decl.pld_loc in
                      let name = lab_decl.pld_name.txt in
                      (
                        longident ~loc name,
                        pexp_apply
                          ~loc
                          (core_type_to_functions lab_decl.pld_type).of_biniou
                          [
                            (
                              Nolabel,
                              pexp_apply
                                ~loc
                                (pexp_ident ~loc @@ longident ~loc "Ppx_deriving_biniou_runtime.record_find")
                                [
                                  (Labelled "name", pexp_constant ~loc (Pconst_string (of_biniou_name, loc, None)));
                                  (Nolabel, pexp_constant ~loc (Pconst_string (name, loc, None)));
                                  (Nolabel, pexp_ident ~loc (longident ~loc "r"));
                                ]
                            )
                          ]
                      )
                    )
                    lab_decls
                )
                None
            )
        );
        (
          case
            ~lhs: (pvar ~loc "t")
            ~guard: None
            ~rhs: (
              pexp_apply
                ~loc
                (pexp_ident ~loc @@ longident ~loc "Ppx_deriving_biniou_runtime.could_not_convert")
                [
                  (Nolabel, pexp_constant ~loc (Pconst_string (of_biniou_name, loc, None)));
                  (Nolabel, pexp_ident ~loc (longident ~loc "t"));
                ]
            )
        );
      ]
    in
      {to_biniou; of_biniou}
  | _ -> Location.raise_errorf ~loc "type_decl"

let type_decl_str
    ~(options : (string * expression) list)
    ~(path : string list)
    (type_decls : type_declaration list)
    : structure
  =
  ignore options;
  ignore path;
  let type_decls = List.map (fun type_decl -> (type_decl, type_decl_to_functions type_decl)) type_decls in
  (* FIXME: loc *)
  pstr_value_list ~loc: Location.none Nonrecursive (
    List.map
      (fun (type_decl, functions) ->
        let loc = type_decl.ptype_loc in
        value_binding
          ~loc
          ~pat: (ppat_var ~loc {txt = mangle_type_decl To_biniou type_decl; loc})
          ~expr: functions.to_biniou
      )
      type_decls
  ) @
    pstr_value_list ~loc: Location.none Nonrecursive (
      List.map
        (fun (type_decl, functions) ->
          let loc = type_decl.ptype_loc in
          value_binding
            ~loc
            ~pat: (ppat_var ~loc {txt = mangle_type_decl Of_biniou type_decl; loc})
            ~expr: functions.of_biniou
        )
        type_decls
    )

let () = Ppx_deriving.(register (create "biniou" ~type_decl_str ()))
