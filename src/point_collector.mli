(** [Point_collector] is a per-source-file accumulator for the coverage points discovered
    while rewriting.

    The dev rewriter creates one collector, registers a point whenever it inserts a probe,
    then passes the collected points and interned filenames to [Codegen] to emit the
    file-level runtime metadata. *)

open! Ppxlib

type t

val create : unit -> t

(** [Filename] is an interned source filename.

    The collector assigns each distinct filename a unique. Almost all files have a single
    filename, but [#] line directives can introduce more. *)
module Filename : sig
  type t

  val id : t -> int
  val name : t -> string
end

module Point : sig
  (** See [Ppx_tested_runtime.Coverage.Latest.Point.Kind]. *)
  module Kind : sig
    type t =
      | Code
      | Implicit_else
      (** [Implicit_else] points track the implicit [else ()] branch of an [if] statement
          with no [else]; they cover no source locations and their [here] is the location
          of the [if] expression. *)
  end

  type t

  val id : t -> int
  val here : t -> Location.t

  (** The interned filename of [here]. *)
  val filename : t -> Filename.t

  (** For a user-written [[%tested e]], a pretty-printed copy of [e].

      This is reporting metadata; the actual runtime predicate is generated from the
      original payload expression. *)
  val condition : t -> string option

  val kind : t -> Kind.t
  val parent : t -> t option
  val exclude_location : t -> Location.t -> unit
  val covers : t -> (Lexing.position * Lexing.position) list
end

val fresh
  :  t
  -> here:Location.t
  -> condition:string option
  -> parent:Point.t option
  -> cover:Location.t
  -> Point.t

val fresh_implicit_else : t -> here:Location.t -> parent:Point.t option -> Point.t
val registered : t -> Point.t list

(** Distinct filenames of all registered points, in [Filename.id] order. *)
val filenames : t -> Filename.t list
