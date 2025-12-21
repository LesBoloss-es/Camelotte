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

let mangle_lid' ~loc ?exn dir lid =
  {txt = mangle_lid ?exn dir lid; loc}

let mangle_type_decl ?exn dir td =
  Ppx_deriving.mangle_type_decl (`Suffix (direction_to_string ?exn dir)) td

let mangle_type_decl' ~loc ?exn dir td =
  {txt = mangle_type_decl ?exn dir td; loc}

(** Given a type variable, produce the name of its serialiser. *)
(* FIXME: It would be cleaner to carry a quoter around to ensure that the
   variables we are using for type variables are fresh. For now, we will rely on
   something deterministically derived from the variable name. *)
let mangle_type_var ~dir var =
  match mangle_lid dir (lident @@ "_tvar_" ^ var) with
  | Lident s -> s
  | _ -> assert false

let mangle_type_var' ~loc ~dir var = {txt = mangle_type_var ~dir var; loc}

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

let core_type_to_serialiser_type ~dir core_type : core_type =
  ignore dir;
  let loc = core_type.ptyp_loc in
  match core_type.ptyp_desc with
  | Ptyp_any -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_any in core_type_to_serialiser_type"
  | Ptyp_var _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_var in core_type_to_serialiser_type"
  | Ptyp_arrow _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_arrow in core_type_to_serialiser_type"
  | Ptyp_tuple _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_tuple in core_type_to_serialiser_type"
  | Ptyp_constr _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_constr in core_type_to_serialiser_type"
  | Ptyp_object _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_object in core_type_to_serialiser_type"
  | Ptyp_class _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_class in core_type_to_serialiser_type"
  | Ptyp_alias _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_alias in core_type_to_serialiser_type"
  | Ptyp_variant _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_variant in core_type_to_serialiser_type"
  | Ptyp_poly _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_poly in core_type_to_serialiser_type"
  | Ptyp_package _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_package in core_type_to_serialiser_type"
  | Ptyp_open _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_open in core_type_to_serialiser_type"
  | Ptyp_extension _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_extension in core_type_to_serialiser_type"

let type_decl_to_serialiser_type ~dir ?(exn = true) type_decl : core_type =
  let loc = type_decl.ptype_loc in
  let name = map_location lident type_decl.ptype_name in
  let params = List.map fst type_decl.ptype_params in
  let grab_fun_params type_ =
    (* Given an type, wrap it in an arrow (type) that grabs the serialising arguments. *)
    List.fold_right
      (fun param type_ ->
        let loc = param.ptyp_loc in
        ptyp_arrow
          ~loc
          Nolabel
          (
            match param with
            | {ptyp_desc = Ptyp_var var; _} ->
              (
                match dir with
                | To_biniou -> [%type: [%t ptyp_var ~loc var] -> Bi_io.tree]
                | Of_biniou -> [%type: Bi_io.tree -> [%t ptyp_var ~loc var]]
              )
            | _ -> assert false
          )
          type_
      )
      params
      type_
  in
  grab_fun_params @@
    match dir, exn with
    | To_biniou, _ -> [%type: [%t ptyp_constr ~loc name params] -> Bi_io.tree]
    | Of_biniou, true -> [%type: Bi_io.tree -> [%t ptyp_constr ~loc name params]]
    | Of_biniou, false -> [%type: Bi_io.tree -> ([%t ptyp_constr ~loc name params], (string * Bi_io.tree)) Stdlib.Result.t]

(** For pattern matching, provide a catch-all that raises
    {!Ppx_deriving_biniou_runtime.Could_not_convert}. *)
let catchall ~loc name =
  case
    ~lhs: (pvar ~loc "t")
    ~guard: None
    ~rhs: (
      pexp_apply
        ~loc
        (pexp_ident ~loc @@ longident ~loc "Ppx_deriving_biniou_runtime.could_not_convert")
        [
          (Nolabel, pexp_constant ~loc (Pconst_string (name, loc, None)));
          (Nolabel, pexp_ident ~loc (longident ~loc "t"));
        ]
    )

module Core_type_to_serialiser : sig
    (** Given a core_type, return a Parsetree expression that is a function
        converting to or from biniou, depending on the direction argument. *)
    val core_type_to_serialiser : dir: direction -> core_type -> expression
  end
= struct
  let rec core_type_to_serialiser ~dir core_type =
    let loc = core_type.ptyp_loc in
    match core_type.ptyp_desc with
    | Ptyp_any -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_any"
    | Ptyp_var var -> ptyp_var_to_serialiser ~loc ~dir var
    | Ptyp_arrow _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_arrow"
    | Ptyp_tuple core_types -> ptyp_tuple_to_serialiser ~loc ~dir core_types
    | Ptyp_object _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_object"
    | Ptyp_class _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_class"
    | Ptyp_alias _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_alias"
    | Ptyp_variant _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_variant"
    | Ptyp_poly _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_poly"
    | Ptyp_package _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_package"
    | Ptyp_open _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_open"
    | Ptyp_extension _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support Ptyp_extension"
    | Ptyp_constr (ty, args) -> ptyp_constr_to_serialiser ~loc ~dir ty args

  and ptyp_var_to_serialiser ~loc ~dir var =
    pexp_ident ~loc @@ map_location lident @@ mangle_type_var' ~loc ~dir var

  and ptyp_tuple_to_serialiser ~loc ~dir core_types =
    let vars = List.mapi (fun i _ -> ("x" ^ string_of_int i)) core_types in
    let var_patterns = List.map2 (fun var core_type -> pvar ~loc: core_type.ptyp_loc var) vars core_types in
    let var_serialised = List.map2 (fun var core_type -> pexp_apply ~loc (core_type_to_serialiser ~dir core_type) [Nolabel, pexp_ident ~loc (longident ~loc var)]) vars core_types in
    match dir with
    | To_biniou -> pexp_fun ~loc Nolabel None (ppat_tuple ~loc var_patterns) @@ pexp_variant ~loc "Tuple" @@ some @@ pexp_array ~loc @@ var_serialised
    | Of_biniou ->
      pexp_function_cases ~loc [
        case ~lhs: (ppat_variant ~loc "Tuple" @@ some @@ ppat_array ~loc var_patterns) ~guard: None ~rhs: (pexp_tuple ~loc var_serialised);
        catchall ~loc "FIXME";
      ]

  and ptyp_constr_to_serialiser ~loc ~dir ty args =
    let in_runtime_lib =
      List.map lident ["unit"; "bool"; "string"; "int"; "int32"; "int64"; "float"; "option"; "array"; "list"]
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
  let ptype_variant_to_serialiser ~loc ~dir type_decl constr_decls =
    let nums = List.mapi (fun i _ -> Pconst_integer (string_of_int i, None)) constr_decls in
    let constr_decl_to_pieces constr_decl =
      (* Given a constructor declaration, produce: the pattern and expression
         for To_biniou, and the pattern and expression for Of_biniou. *)
      match constr_decl.pcd_args with
      | Pcstr_tuple [] ->
        (
          None,
          [%expr None],
          [%pat? None],
          None
        )
      | Pcstr_tuple [arg] ->
        (
          Some [%pat? arg],
          [%expr Some ([%e core_type_to_serialiser ~dir arg] arg)],
          [%pat? Some arg],
          Some [%expr [%e core_type_to_serialiser ~dir arg] arg]
        )
      | Pcstr_tuple args ->
        let arg_names = List.mapi (fun i arg -> {txt = "arg" ^ string_of_int i; loc = arg.ptyp_loc}) args in
        let arg_names' = List.map2 (fun arg_name arg -> pexp_ident ~loc: arg.ptyp_loc @@ map_location lident arg_name) arg_names args in
        (
          Some (ppat_tuple ~loc @@ List.map (ppat_var ~loc) arg_names),
          [%expr Some (`Tuple [%e pexp_array ~loc (List.map2 (fun arg_name arg -> [%expr [%e core_type_to_serialiser ~dir arg] [%e arg_name]]) arg_names' args)])],
          [%pat? Some (`Tuple[%p ppat_array ~loc @@ List.map (ppat_var ~loc) arg_names])],
          Some (pexp_tuple ~loc (List.map2 (fun arg_name arg -> [%expr [%e core_type_to_serialiser ~dir arg] [%e arg_name]]) arg_names' args))
        )
      | Pcstr_record _ -> Location.raise_errorf ~loc "Ppx_deriving_runtime does not support records in constructors"
    in
    match dir with
    | To_biniou ->
      pexp_function_cases ~loc (
        List.map2
          (fun num constr_decl ->
            let loc = constr_decl.pcd_loc in
            let (pattern_arguments, constructor_arguments, _, _) = constr_decl_to_pieces constr_decl in
            case
              ~lhs: (ppat_construct ~loc (map_location lident constr_decl.pcd_name) pattern_arguments)
              ~guard: None
              ~rhs: (pexp_variant ~loc "Num_variant" @@ some @@ pexp_tuple ~loc [pexp_constant ~loc num; constructor_arguments])
          )
          nums
          constr_decls
      )
    | Of_biniou ->
      pexp_function_cases ~loc @@
        List.flatten [
          (
            List.map2
              (fun num constr_decl ->
                let loc = constr_decl.pcd_loc in
                let (_, _, pattern_arguments, constructor_arguments) = constr_decl_to_pieces constr_decl in
                case
                  ~lhs: [%pat? `Num_variant ([%p ppat_constant ~loc num], [%p pattern_arguments])]
                  ~guard: None
                  ~rhs: (
                    pexp_construct ~loc (map_location lident constr_decl.pcd_name) constructor_arguments
                  )
              )
              nums
              constr_decls
          );
          [catchall ~loc (mangle_type_decl dir type_decl)];
        ]

  let ptype_record_to_serialiser ~loc ~dir type_decl lab_decls =
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
        (catchall ~loc (mangle_type_decl dir type_decl));
      ]

  let type_decl_to_serialiser ~dir type_decl =
    let loc = type_decl.ptype_loc in
    let grab_fun_params expr =
      (* Given an expression, wrap it in a function (expression) that grabs the
         [_tvar_a_<dir>_biniou] arguments, if needed. *)
      match type_decl.ptype_params with
      | [] -> expr
      | params ->
        pexp_function
          ~loc
          (
            List.map
              (fun (param, _) ->
                let loc = param.ptyp_loc in
                match param with
                | {ptyp_desc = Ptyp_var var; _} ->
                  pparam_val ~loc Nolabel None (pvar ~loc @@ mangle_type_var ~dir var)
                | _ -> Location.raise_errorf ~loc "Ppx_deriving_biniou: unexpected type parameter that is not Ptyp_var"
              )
              params
          )
          None @@
          Pfunction_body expr
    in
    pexp_constraint
      ~loc
      (
        grab_fun_params @@
          match type_decl.ptype_kind with
          | Ptype_abstract ->
            (
              match type_decl.ptype_manifest with
              | None -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support abstract types without manifest"
              | Some core_type -> core_type_to_serialiser ~dir core_type
            )
          | Ptype_open -> Location.raise_errorf ~loc "Ppx_deriving_biniou does not support open types"
          | Ptype_variant constr_decls -> ptype_variant_to_serialiser ~loc ~dir type_decl constr_decls
          | Ptype_record lab_decls -> ptype_record_to_serialiser ~loc ~dir type_decl lab_decls
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
          ~pat: (ppat_var ~loc @@ mangle_type_decl' ~loc dir type_decl)
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
            ~pat: (ppat_var ~loc @@ mangle_type_decl' ~loc ~exn: false Of_biniou type_decl)
            ~expr: [%expr fun x ->
              try
                Ok ([%e pexp_ident ~loc (longident ~loc (mangle_type_decl ~exn: true Of_biniou type_decl))] x)
              with
                | Ppx_deriving_biniou_runtime.Could_not_convert (where, what) -> Error (where, what)
            ]
        ]
      )
      type_decls
  else []

let type_decl_to_serialiser_vd ~loc ~dir ?(exn = true) type_decl : value_description =
  value_description
    ~loc
    ~name: (mangle_type_decl' ~loc dir ~exn type_decl)
    ~type_: (type_decl_to_serialiser_type ~dir ~exn type_decl)
    ~prim: []

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
