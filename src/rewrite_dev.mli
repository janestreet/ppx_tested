(** [Rewrite_dev] is the dev-build expansion.

    This pass preserves [tested]/[untested] annotations as runtime coverage probes and
    emits metadata describing the collected points. *)

open! Ppxlib

val rewrite_structure : library:string -> structure -> structure

(** Like [rewrite_structure], but also returns the collected coverage points. Used by
    tests to check point structure, e.g. parenting, without the full expansion. *)
val rewrite_structure_and_points
  :  library:string
  -> structure
  -> structure * Point_collector.Point.t list

val extract_tested_payload : loc:Location.t -> payload -> expression option

(** {2 Attributes} *)

(** [let f x = ... [@@tested]] *)
val tested_value_binding_attribute : (value_binding, unit) Attribute.t

(** [(expr [@tested])] *)
val tested_expression_attribute : (expression, unit) Attribute.t

(** [@@@tested] *)
val tested_floating_attribute : (structure_item, unit) Attribute.Floating.t

val untested_value_binding_attribute : (value_binding, unit) Attribute.t
val untested_expression_attribute : (expression, unit) Attribute.t
val untested_floating_attribute : (structure_item, unit) Attribute.Floating.t
