open! Ppxlib

let release_expansion ~loc payload =
  let loc = { loc with loc_ghost = true } in
  match payload with
  | None -> [%expr ()]
  | Some e -> [%expr if false then ignore ([%e e] : bool) else ()]
;;

let consume_attr attr x =
  match Attribute.consume attr x with
  | Some (x, ()) -> x
  | None -> x
;;

let rewrite_structure structure =
  let mapper =
    object
      inherit Ast_traverse.map as super

      (* Attribute payloads are metadata parsed syntactically by other ppxes (e.g.
         [@@deriving], [@modality]), so never rewrite inside them.
      *)
      method! attribute attribute = attribute

      method! expression e =
        match e.pexp_desc with
        | Pexp_extension ({ txt = "tested"; _ }, payload) ->
          let loc = e.pexp_loc in
          let payload_expr = Rewrite_dev.extract_tested_payload ~loc payload in
          release_expansion ~loc payload_expr
        | _ ->
          let e = super#expression e in
          e
          |> consume_attr Rewrite_dev.tested_expression_attribute
          |> consume_attr Rewrite_dev.untested_expression_attribute

      method! value_binding vb =
        let vb = super#value_binding vb in
        vb
        |> consume_attr Rewrite_dev.tested_value_binding_attribute
        |> consume_attr Rewrite_dev.untested_value_binding_attribute

      method! structure items =
        let items = super#structure items in
        let is_floating_attribute attr item =
          Option.is_some (Attribute.Floating.convert [ attr ] item)
        in
        let is_floating_tested_or_untested item =
          match item.pstr_desc with
          | Pstr_attribute _ ->
            is_floating_attribute Rewrite_dev.tested_floating_attribute item
            || is_floating_attribute Rewrite_dev.untested_floating_attribute item
          | _ -> false
        in
        List.filter (fun item -> not (is_floating_tested_or_untested item)) items
    end
  in
  mapper#structure structure
;;
