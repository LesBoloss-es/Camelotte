open Ppxlib
open Ast_builder.Default

let some x = Some x

let map_location f {txt; loc} = {txt = f txt; loc}

let lident x = Lident x

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

type direction = To_biniou | Of_biniou

(** Convert a {!direction} to a string. The optional argument [?exn], true by
    default, adds a suffix [_exn] to the [Of_biniou] string only. *)
let direction_to_string ?(exn = true) dir =
  match dir, exn with
  | To_biniou, _ -> "to_biniou"
  | Of_biniou, true -> "of_biniou_exn"
  | Of_biniou, false -> "of_biniou"

let mangle_lid ?exn dir lid =
  Ppx_deriving.mangle_lid (`Suffix (direction_to_string ?exn dir)) lid

let mangle_type_decl ?exn dir td =
  Ppx_deriving.mangle_type_decl (`Suffix (direction_to_string ?exn dir)) td

let mangle_type_decl' ~loc ?exn dir td =
  {txt = mangle_type_decl ?exn dir td; loc}

type options = {
  alias: bool;
}

let parse_options (options : (string * expression) list) : options =
  let alias =
    match List.assoc_opt "alias" options with
    | None | Some[%expr true] -> true
    | Some[%expr false] -> false
    | Some expr -> Location.raise_errorf ~loc: expr.pexp_loc "The `alias` option only supports boolean values."
  in
    {alias}

let type_decl_to_serialiser_type ~dir ?(exn = true) type_decl : core_type =
  let loc = type_decl.ptype_loc in
  let name = map_location lident type_decl.ptype_name in
  match dir, exn with
  | To_biniou, _ -> [%type: [%t ptyp_constr ~loc name []] -> Bi_io.tree]
  | Of_biniou, true -> [%type: Bi_io.tree -> [%t ptyp_constr ~loc name []]]
  | Of_biniou, false -> [%type: Bi_io.tree -> ([%t ptyp_constr ~loc name []], (string * Bi_io.tree)) Stdlib.Result.t]

module Core_type_to_serialiser : sig
    (** Given a core_type, return a Parsetree expression that is a function
        converting to or from biniou, depending on the direction argument. *)
    val core_type_to_serialiser : dir: direction -> core_type -> expression
  end
= struct
  let rec core_type_to_serialiser ~dir core_type =
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
    | Ptyp_constr (ty, args) -> ptyp_constr_to_serialiser ~loc ~dir ty args
    | _ -> Location.raise_errorf ~loc "core_type: cannot convert to/from Biniou"

  and ptyp_constr_to_serialiser ~loc ~dir ty args =
    let in_runtime_lib =
      List.map (fun x -> Lident x) ["int"; "int32"; "int64"; "float"; "array"; "list"]
    in
    let ty_txt =
      if List.mem ty.txt in_runtime_lib then
        ldotl "Ppx_deriving_biniou_runtime" ty.txt
      else ty.txt
    in
    let fn = pexp_ident ~loc {txt = mangle_lid dir ty_txt; loc = ty.loc} in
    match args with
    | [] -> fn
    | args ->
      let args_fns = List.map (core_type_to_serialiser ~dir) args in
      pexp_apply ~loc fn (List.map (fun arg_fn -> (Nolabel, arg_fn)) args_fns)
end
include Core_type_to_serialiser

module Type_decl_to_serialiser : sig
    (** Given a type_decl, return a Parsetree expression that is a function
        converting to or from biniou, depending on the direction argument. *)
    val type_decl_to_serialiser : dir: direction -> type_declaration -> expression
  end
= struct
  let ptype_record ~loc ~dir type_decl lab_decls =
    match dir with
    | To_biniou ->
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
              (core_type_to_serialiser ~dir lab_decl.pld_type)
              [(Nolabel, pexp_field ~loc (pexp_ident ~loc (longident ~loc "x")) (longident ~loc name))]
          ]
        )
        lab_decls
    | Of_biniou ->
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
                          (core_type_to_serialiser ~dir lab_decl.pld_type)
                          [
                            (
                              Nolabel,
                              pexp_apply
                                ~loc
                                (pexp_ident ~loc @@ longident ~loc "Ppx_deriving_biniou_runtime.record_find")
                                [
                                  (Labelled "name", pexp_constant ~loc (Pconst_string (mangle_type_decl Of_biniou type_decl, loc, None)));
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
                  (Nolabel, pexp_constant ~loc (Pconst_string (mangle_type_decl Of_biniou type_decl, loc, None)));
                  (Nolabel, pexp_ident ~loc (longident ~loc "t"));
                ]
            )
        );
      ]

  let type_decl_to_serialiser ~dir type_decl =
    let loc = type_decl.ptype_loc in
    pexp_constraint
      ~loc
      (
        match type_decl.ptype_kind with
        | Ptype_record lab_decls -> ptype_record ~loc ~dir type_decl lab_decls
        | _ -> Location.raise_errorf ~loc "type_decl"
      )
      (type_decl_to_serialiser_type ~dir type_decl)
end
include Type_decl_to_serialiser

let type_decls_to_serialiser_str ~dir type_decls : structure =
  (* FIXME: loc *)
  pstr_value_list ~loc: Location.none Nonrecursive (
    List.map
      (fun type_decl ->
        let loc = type_decl.ptype_loc in
        value_binding
          ~loc
          ~pat: (ppat_var ~loc {txt = mangle_type_decl dir type_decl; loc})
          ~expr: (type_decl_to_serialiser ~dir type_decl)
      )
      type_decls
  )

let type_decls_to_aliases_str ~options type_decls : structure =
  if options.alias then
    List.map
      (fun type_decl ->
        let loc = type_decl.ptype_loc in
        pstr_value ~loc Nonrecursive [
          value_binding
            ~loc
            ~pat: (ppat_var ~loc {txt = mangle_type_decl ~exn: false Of_biniou type_decl; loc})
            ~expr: [%expr fun x ->
              try
                Ok ([%e pexp_ident ~loc (longident ~loc (mangle_type_decl ~exn: true Of_biniou type_decl))] x)
              with
                | Ppx_deriving_biniou_runtime.Could_not_convert (where, what) ->
                  Error (where, what)
            ]
        ]
      )
      type_decls
  else []

let type_decl_to_serialiser_vd ~loc ~dir ?(exn = true) type_decl : value_description =
  value_description ~loc ~name: (mangle_type_decl' ~loc dir ~exn type_decl) ~type_: (type_decl_to_serialiser_type ~dir ~exn type_decl) ~prim: []

let type_decl_to_serialiser_vds ~options ~loc type_decl : value_description list =
  [type_decl_to_serialiser_vd ~loc ~dir: To_biniou type_decl;
  type_decl_to_serialiser_vd ~loc ~dir: Of_biniou ~exn: true type_decl;
  ] @
    (if options.alias then [type_decl_to_serialiser_vd ~loc ~dir: Of_biniou ~exn: false type_decl] else [])

let type_decl_str ~options ~path (type_decls : type_declaration list) : structure =
  ignore path;
  let options = parse_options options in
  List.concat_map (fun dir -> type_decls_to_serialiser_str ~dir type_decls) [To_biniou; Of_biniou] @
    type_decls_to_aliases_str ~options type_decls

let type_decl_sig ~options ~path (type_decls : type_declaration list) : signature =
  ignore path;
  let options = parse_options options in
  List.concat_map
    (fun type_decl ->
      let loc = type_decl.ptype_loc in
      List.map (psig_value ~loc) (type_decl_to_serialiser_vds ~options ~loc type_decl)
    )
    type_decls

let () = Ppx_deriving.(register (create "biniou" ~type_decl_str ~type_decl_sig ()))
