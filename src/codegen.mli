(** [Codegen] contains constructors for the AST fragments inserted by the dev rewriter.

    They turn collected point metadata into the file-level registration preamble, and
    individual [Point_collector.Point.t]s into runtime probe calls. *)

open! Ppxlib

val preamble_structure_items
  :  loc:Location.t
  -> library:string
  -> filenames:Point_collector.Filename.t list
  -> Point_collector.Point.t list
  -> structure_item list

val probe_call
  :  loc:Location.t
  -> library:string
  -> Point_collector.Point.t
  -> expression option
  -> expression

val prepend_probe_call
  :  library:string
  -> Point_collector.Point.t
  -> expression
  -> expression
