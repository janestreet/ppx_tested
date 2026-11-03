open! Ppxlib

let expression_desc expression =
  Ppxlib_jane.Shim.Expression_desc.of_parsetree
    expression.pexp_desc
    ~loc:expression.pexp_loc
;;

let with_expression_desc expression desc =
  { expression with
    pexp_desc =
      Ppxlib_jane.Shim.Expression_desc.to_parsetree ~loc:expression.pexp_loc desc
  }
;;

(* [exclave_ body] is encoded in the parsetree as

   {[
     Pexp_apply (exclave, [ Nolabel, body ])
   ]}

   where [exclave] is the [extension.exclave] extension node. *)
let is_exclave_extension function_ =
  match function_.pexp_desc with
  | Pexp_extension ({ txt = "extension.exclave"; _ }, PStr []) -> true
  | _ -> false
;;

let with_exclave_body expression exclave body =
  with_expression_desc expression (Pexp_apply (exclave, [ Nolabel, body ]))
;;

let tested_value_binding_attribute =
  Attribute.declare_flag "tested" Attribute.Context.value_binding
;;

let tested_expression_attribute =
  Attribute.declare_flag "tested" Attribute.Context.expression
;;

let tested_floating_attribute =
  Attribute.Floating.declare
    "tested"
    Attribute.Floating.Context.structure_item
    Ast_pattern.(pstr nil)
    ()
;;

let untested_expression_attribute =
  Attribute.declare_flag "untested" Attribute.Context.expression
;;

let untested_value_binding_attribute =
  Attribute.declare_flag "untested" Attribute.Context.value_binding
;;

let untested_floating_attribute =
  Attribute.Floating.declare
    "untested"
    Attribute.Floating.Context.structure_item
    Ast_pattern.(pstr nil)
    ()
;;

let extract_tested_payload ~loc payload =
  match payload with
  | PStr [] -> None
  | PStr [%str [%e? expression]] -> Some expression
  | _ ->
    Location.raise_errorf ~loc "[%%tested] payload must be empty or a single expression"
;;

type instrumentation =
  | Disabled
  | Enabled of { parent : Point_collector.Point.t option }
  | Covered_by of Point_collector.Point.t

let is_floating item attr =
  match item.pstr_desc with
  | Pstr_attribute _ ->
    (match Attribute.Floating.convert [ attr ] item with
     | Some () -> true
     | None -> false)
  | _ -> false
;;

let is_floating_tested item = is_floating item tested_floating_attribute
let is_floating_untested item = is_floating item untested_floating_attribute

let is_untested expression =
  Attribute.has_flag untested_expression_attribute ~mark_as_seen:false expression
;;

exception Untested

let check_not_untested expression = if is_untested expression then raise Untested

let check_not_untested_binding vb =
  if Attribute.has_flag untested_value_binding_attribute ~mark_as_seen:false vb
  then raise Untested
;;

(* Insert a probe before the last subexpression to begin execution within an expression,
   ignoring any subexpressions marked as untested. *)
let rec insert_tail_probe ~library point expression =
  check_not_untested expression;
  try
    match expression_desc expression with
    | Pexp_sequence (first, second) ->
      (match insert_tail_probe ~library point second with
       | second -> with_expression_desc expression (Pexp_sequence (first, second))
       | exception Untested ->
         let first = insert_tail_probe ~library point first in
         with_expression_desc expression (Pexp_sequence (first, second)))
    | Pexp_let (mutable_flag, rec_flag, bindings, body) ->
      (match insert_tail_probe ~library point body with
       | body ->
         with_expression_desc
           expression
           (Pexp_let (mutable_flag, rec_flag, bindings, body))
       | exception Untested ->
         let bindings = insert_tail_probe_bindings ~library point bindings in
         with_expression_desc
           expression
           (Pexp_let (mutable_flag, rec_flag, bindings, body)))
    | Pexp_letmodule (name, module_expr, body) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_letmodule (name, module_expr, body))
    | Pexp_letexception (extension_constructor, body) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_letexception (extension_constructor, body))
    | Pexp_open (open_declaration, body) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_open (open_declaration, body))
    | Pexp_constraint (body, core_type, modes) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_constraint (body, core_type, modes))
    | Pexp_coerce (body, from, to_) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_coerce (body, from, to_))
    | Pexp_poly (body, core_type) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_poly (body, core_type))
    | Pexp_ifthenelse (condition, then_, else_) ->
      let condition = insert_tail_probe ~library point condition in
      with_expression_desc expression (Pexp_ifthenelse (condition, then_, else_))
    | Pexp_match (scrutinee, cases) ->
      let scrutinee = insert_tail_probe ~library point scrutinee in
      with_expression_desc expression (Pexp_match (scrutinee, cases))
    | Pexp_try (body, cases) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_try (body, cases))
    | Pexp_while (condition, body) ->
      let condition = insert_tail_probe ~library point condition in
      with_expression_desc expression (Pexp_while (condition, body))
    | Pexp_for (pattern, start, stop, direction, body) ->
      (match insert_tail_probe ~library point stop with
       | stop ->
         with_expression_desc
           expression
           (Pexp_for (pattern, start, stop, direction, body))
       | exception Untested ->
         let start = insert_tail_probe ~library point start in
         with_expression_desc
           expression
           (Pexp_for (pattern, start, stop, direction, body)))
    | Pexp_apply (f, [ (Nolabel, body) ]) when is_exclave_extension f ->
      let body = insert_tail_probe ~library point body in
      with_exclave_body expression f body
    | Pexp_apply (function_, args) ->
      (match expression_desc function_ with
       | Pexp_ident _ ->
         (* Applications of identifiers get special treatment in the type-checker, which
            isn't preserved if a probe is inserted, so we don't consider applied
            identifiers when inserting the probe. *)
         let args = insert_tail_probe_labelled_expressions ~library point args in
         with_expression_desc expression (Pexp_apply (function_, args))
       | _ ->
         (match insert_tail_probe ~library point function_ with
          | function_ -> with_expression_desc expression (Pexp_apply (function_, args))
          | exception Untested ->
            let args = insert_tail_probe_labelled_expressions ~library point args in
            with_expression_desc expression (Pexp_apply (function_, args))))
    | Pexp_tuple labeled_expressions ->
      let labeled_expressions =
        insert_tail_probe_labelled_expressions ~library point labeled_expressions
      in
      with_expression_desc expression (Pexp_tuple labeled_expressions)
    | Pexp_unboxed_tuple labeled_expressions ->
      let labeled_expressions =
        insert_tail_probe_labelled_expressions ~library point labeled_expressions
      in
      with_expression_desc expression (Pexp_unboxed_tuple labeled_expressions)
    | Pexp_construct (constructor, argument) ->
      (match argument with
       | None -> Codegen.prepend_probe_call ~library point expression
       | Some argument ->
         let argument = insert_tail_probe ~library point argument in
         with_expression_desc expression (Pexp_construct (constructor, Some argument)))
    | Pexp_variant (tag, argument) ->
      (match argument with
       | None -> Codegen.prepend_probe_call ~library point expression
       | Some argument ->
         let argument = insert_tail_probe ~library point argument in
         with_expression_desc expression (Pexp_variant (tag, Some argument)))
    | Pexp_record (fields, base) ->
      (match insert_tail_probe_labelled_expressions ~library point fields with
       | fields -> with_expression_desc expression (Pexp_record (fields, base))
       | exception Untested ->
         (match base with
          | None -> Codegen.prepend_probe_call ~library point expression
          | Some base ->
            let base = insert_tail_probe ~library point base in
            with_expression_desc expression (Pexp_record (fields, Some base))))
    | Pexp_record_unboxed_product (fields, base) ->
      (match insert_tail_probe_labelled_expressions ~library point fields with
       | fields ->
         with_expression_desc expression (Pexp_record_unboxed_product (fields, base))
       | exception Untested ->
         (match base with
          | None -> Codegen.prepend_probe_call ~library point expression
          | Some base ->
            let base = insert_tail_probe ~library point base in
            with_expression_desc
              expression
              (Pexp_record_unboxed_product (fields, Some base))))
    | Pexp_field (record, field) ->
      let record = insert_tail_probe ~library point record in
      with_expression_desc expression (Pexp_field (record, field))
    | Pexp_unboxed_field (record, field) ->
      let record = insert_tail_probe ~library point record in
      with_expression_desc expression (Pexp_unboxed_field (record, field))
    | Pexp_setfield (record, field, value) ->
      (match insert_tail_probe ~library point record with
       | record -> with_expression_desc expression (Pexp_setfield (record, field, value))
       | exception Untested ->
         let value = insert_tail_probe ~library point value in
         with_expression_desc expression (Pexp_setfield (record, field, value)))
    | Pexp_array (mutable_flag, expressions) ->
      let expressions = insert_tail_probe_expressions ~library point expressions in
      with_expression_desc expression (Pexp_array (mutable_flag, expressions))
    | Pexp_send (object_, method_) ->
      let object_ = insert_tail_probe ~library point object_ in
      with_expression_desc expression (Pexp_send (object_, method_))
    | Pexp_setvar (name, value) ->
      let value = insert_tail_probe ~library point value in
      with_expression_desc expression (Pexp_setvar (name, value))
    | Pexp_override fields ->
      let fields = insert_tail_probe_labelled_expressions ~library point fields in
      with_expression_desc expression (Pexp_override fields)
    | Pexp_assert body ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_assert body)
    | Pexp_newtype (name, jkind_annotation, body) ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_newtype (name, jkind_annotation, body))
    | Pexp_stack body ->
      let body = insert_tail_probe ~library point body in
      with_expression_desc expression (Pexp_stack body)
    | Pexp_overwrite (overwritten, replacement) ->
      (match insert_tail_probe ~library point replacement with
       | replacement ->
         with_expression_desc expression (Pexp_overwrite (overwritten, replacement))
       | exception Untested ->
         let overwritten = insert_tail_probe ~library point overwritten in
         with_expression_desc expression (Pexp_overwrite (overwritten, replacement)))
    | Pexp_idx _
    | Pexp_splice _
    | Pexp_borrow _
    | Pexp_letop _
    | Pexp_ident _
    | Pexp_constant _
    | Pexp_unboxed_unit
    | Pexp_unboxed_bool _
    | Pexp_new _
    | Pexp_extension _
    | Pexp_unreachable
    | Pexp_hole
    | Pexp_function _
    | Pexp_lazy _
    | Pexp_object _
    | Pexp_pack _
    | Pexp_quote _
    | Pexp_comprehension _ -> Codegen.prepend_probe_call ~library point expression
  with
  | Untested -> Codegen.prepend_probe_call ~library point expression

and insert_tail_probe_bindings ~library point bindings =
  let rec loop = function
    | [] -> raise Untested
    | vb :: rest ->
      (match
         check_not_untested_binding vb;
         insert_tail_probe ~library point vb.pvb_expr
       with
       | expression ->
         let vb = { vb with pvb_expr = expression } in
         vb :: rest
       | exception Untested ->
         let rest = loop rest in
         vb :: rest)
  in
  List.rev (loop (List.rev bindings))

and insert_tail_probe_expressions ~library point expressions =
  let rec loop = function
    | [] -> raise Untested
    | expression :: rest ->
      (match insert_tail_probe ~library point expression with
       | expression -> expression :: rest
       | exception Untested ->
         let rest = loop rest in
         expression :: rest)
  in
  loop expressions

and insert_tail_probe_labelled_expressions
  : 'a. library:_ -> _ -> ('a * _) list -> ('a * _) list
  =
  fun ~library point pairs ->
  let rec loop = function
    | [] -> raise Untested
    | (key, expression) :: rest ->
      (match insert_tail_probe ~library point expression with
       | expression -> (key, expression) :: rest
       | exception Untested ->
         let rest = loop rest in
         (key, expression) :: rest)
  in
  loop pairs
;;

class mapper ~library ~check_library_set ~collector instrumentation =
  object (self)
    val instrumentation = instrumentation
    inherit Ast_traverse.map as super

    (* Attribute payloads are metadata parsed syntactically by other ppxes (e.g.
       [@@deriving], [@modality]), so never rewrite inside them. *)
    method! attribute attribute = attribute

    method private with_instrumentation =
      match instrumentation with
      | Disabled -> {<instrumentation = Enabled { parent = None }>}
      | Enabled _ | Covered_by _ -> self

    method private without_instrumentation = {<instrumentation = Disabled>}

    method private with_nested =
      match instrumentation with
      | Disabled | Enabled _ -> self
      | Covered_by point -> {<instrumentation = Enabled { parent = Some point }>}

    method private parent =
      match instrumentation with
      | Disabled | Enabled { parent = None } -> None
      | Enabled { parent = Some parent } | Covered_by parent -> Some parent

    method private exclude_from_parent loc =
      match self#parent with
      | None -> ()
      | Some parent -> Point_collector.Point.exclude_location parent loc

    method private nested_function_params params =
      List.map
        (fun (param : Ppxlib_jane.Shim.Pexp_function.function_param) ->
          match param.pparam_desc with
          | Pparam_val (label, Some default, pattern) ->
            let default = self#with_nested#expression default in
            { param with pparam_desc = Pparam_val (label, Some default, pattern) }
          | Pparam_val (_, None, _) | Pparam_newtype _ -> param)
        params

    method private cover expression =
      self#exclude_from_parent expression.pexp_loc;
      let point =
        Point_collector.fresh
          collector
          ~here:expression.pexp_loc
          ~condition:None
          ~parent:self#parent
          ~cover:expression.pexp_loc
      in
      {<instrumentation = Covered_by point>}, insert_tail_probe ~library point expression

    method! case case =
      match case.pc_guard with
      | Some guard when not (is_untested guard) ->
        (match instrumentation with
         | Enabled _ ->
           let self, guard = self#cover guard in
           self#case { case with pc_guard = Some guard }
         | Disabled | Covered_by _ ->
           let pc_guard = Option.map self#expression case.pc_guard in
           let pc_rhs = self#with_nested#expression case.pc_rhs in
           { case with pc_guard; pc_rhs })
      | _ -> super#case case

    method private instrument_tested expression payload =
      let loc = expression.pexp_loc in
      check_library_set ~loc;
      self#exclude_from_parent loc;
      let payload = extract_tested_payload ~loc payload in
      let condition = Option.map Pprintast.string_of_expression payload in
      let point =
        Point_collector.fresh
          collector
          ~here:loc
          ~condition
          ~parent:self#parent
          ~cover:loc
      in
      Codegen.probe_call ~loc ~library point payload

    method! expression expression =
      match Attribute.consume tested_expression_attribute expression with
      | Some (expression, ()) ->
        check_library_set ~loc:expression.pexp_loc;
        self#with_instrumentation#expression expression
      | None ->
        (match Attribute.consume untested_expression_attribute expression with
         | Some (expression, ()) ->
           self#exclude_from_parent expression.pexp_loc;
           self#without_instrumentation#expression expression
         | None ->
           (match expression_desc expression with
            | Pexp_extension ({ txt = "tested"; _ }, payload) ->
              self#instrument_tested expression payload
            | Pexp_function (params, type_constraint, function_body) ->
              let params = self#nested_function_params params in
              let function_body =
                match function_body with
                | Pfunction_body body -> Pfunction_body (self#with_nested#expression body)
                | Pfunction_cases (cases, loc, attrs) ->
                  Pfunction_cases (List.map self#with_nested#case cases, loc, attrs)
              in
              with_expression_desc
                expression
                (Pexp_function (params, type_constraint, function_body))
            | Pexp_lazy body ->
              let body = self#with_nested#expression body in
              with_expression_desc expression (Pexp_lazy body)
            (* These wrappers are transparent at runtime: they have a single child which
               is the value of the whole expression, so we rewrite that child in place
               rather than covering the wrapper.

               In particular, a wrapped function (e.g. [let f : type a. ... = fun ...]) is
               instrumented exactly like a bare function: probes land inside its body and
               the binding stays syntactically a function, so binding attributes such as
               [@tail_mod_cons] or [@inline] still apply. *)
            | Pexp_newtype (name, jkind_annotation, body) ->
              let body = self#expression body in
              with_expression_desc
                expression
                (Pexp_newtype (name, jkind_annotation, body))
            | Pexp_constraint (body, core_type, modes) ->
              let body = self#expression body in
              with_expression_desc expression (Pexp_constraint (body, core_type, modes))
            | Pexp_coerce (body, from, to_) ->
              let body = self#expression body in
              with_expression_desc expression (Pexp_coerce (body, from, to_))
            | Pexp_stack body ->
              let body = self#expression body in
              with_expression_desc expression (Pexp_stack body)
            | Pexp_apply (f, [ (Nolabel, body) ]) when is_exclave_extension f ->
              let body = self#expression body in
              with_exclave_body expression f body
            | Pexp_poly (body, core_type) ->
              let body = self#expression body in
              with_expression_desc expression (Pexp_poly (body, core_type))
            | _ ->
              (match instrumentation with
               | Enabled _ ->
                 let self, expression = self#cover expression in
                 self#expression expression
               | Disabled | Covered_by _ ->
                 (match expression_desc expression with
                  | Pexp_extension ({ txt = "probe"; _ }, _) -> expression
                  | Pexp_ifthenelse (condition, then_, else_) ->
                    let condition = self#expression condition in
                    let then_ = self#with_nested#expression then_ in
                    let else_ =
                      match else_ with
                      | Some else_ -> Some (self#with_nested#expression else_)
                      | None ->
                        (match instrumentation with
                         | Disabled | Enabled _ -> None
                         | Covered_by parent ->
                           let loc = expression.pexp_loc in
                           let point =
                             Point_collector.fresh_implicit_else
                               collector
                               ~here:loc
                               ~parent:(Some parent)
                           in
                           Some (Codegen.probe_call ~loc ~library point None))
                    in
                    with_expression_desc
                      expression
                      (Pexp_ifthenelse (condition, then_, else_))
                  | Pexp_match (scrutinee, cases) ->
                    let scrutinee = self#expression scrutinee in
                    let cases = List.map self#with_nested#case cases in
                    with_expression_desc expression (Pexp_match (scrutinee, cases))
                  | Pexp_try (body, cases) ->
                    let body = self#expression body in
                    let cases = List.map self#with_nested#case cases in
                    with_expression_desc expression (Pexp_try (body, cases))
                  | Pexp_while (condition, body) ->
                    let condition = self#expression condition in
                    let body = self#with_nested#expression body in
                    with_expression_desc expression (Pexp_while (condition, body))
                  | Pexp_for (pattern, start, stop, direction, body) ->
                    let start = self#expression start in
                    let stop = self#expression stop in
                    let body = self#with_nested#expression body in
                    with_expression_desc
                      expression
                      (Pexp_for (pattern, start, stop, direction, body))
                  | _ -> super#expression expression))))

    method! value_binding vb =
      match Attribute.consume tested_value_binding_attribute vb with
      | Some (vb, ()) ->
        check_library_set ~loc:vb.pvb_loc;
        self#with_instrumentation#value_binding vb
      | None ->
        (match Attribute.consume untested_value_binding_attribute vb with
         | Some (vb, ()) ->
           self#exclude_from_parent vb.pvb_loc;
           self#without_instrumentation#value_binding vb
         | None -> super#value_binding vb)

    method structure_items items =
      match items with
      | [] -> []
      | item :: rest ->
        if is_floating_tested item
        then (
          check_library_set ~loc:item.pstr_loc;
          self#with_instrumentation#structure_items rest)
        else if is_floating_untested item
        then self#without_instrumentation#structure_items rest
        else (
          let item = self#structure_item item in
          item :: self#structure_items rest)

    method! structure items = self#structure_items items
  end

let rewrite_structure_and_points ~library structure =
  let check_library_set ~loc =
    if String.equal library ""
    then
      Location.raise_errorf
        ~loc
        "ppx_tested annotation used without [-library=<name>]; either configure the \
         library name in the jbuild or remove the annotation"
  in
  let collector = Point_collector.create () in
  let mapper = new mapper ~library ~check_library_set ~collector Disabled in
  let structure = mapper#structure structure in
  let points = Point_collector.registered collector in
  let structure =
    if List.is_empty points
    then structure
    else (
      let loc =
        match structure with
        | { pstr_loc = { loc_start; _ }; _ } :: _ ->
          { loc_start; loc_end = loc_start; loc_ghost = true }
        | [] -> Location.none
      in
      let preamble =
        Codegen.preamble_structure_items
          ~loc
          ~library
          ~filenames:(Point_collector.filenames collector)
          points
      in
      preamble @ structure)
  in
  structure, points
;;

let rewrite_structure ~library structure =
  let structure, (_ : Point_collector.Point.t list) =
    rewrite_structure_and_points ~library structure
  in
  structure
;;
