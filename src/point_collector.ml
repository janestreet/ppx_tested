open! Ppxlib

module Filename = struct
  type t =
    { id : int
    ; name : string
    }

  let id t = t.id
  let name t = t.name
end

module Point = struct
  module Kind = struct
    type t =
      | Code
      | Implicit_else
  end

  module Covers = struct
    type t =
      | Code of
          { cover : Location.t
          ; mutable exclusions : Location.t list
          }
      | Implicit_else

    let kind t : Kind.t =
      match t with
      | Implicit_else -> Implicit_else
      | Code _ -> Code
    ;;

    let exclude_location t loc =
      match t with
      | Implicit_else -> ()
      | Code r -> r.exclusions <- loc :: r.exclusions
    ;;
  end

  type t =
    { id : int
    ; here : Location.t
    ; filename : Filename.t
    ; condition : string option
    ; covers : Covers.t
    ; parent : t option
    }

  let id t = t.id
  let here t = t.here
  let filename t = t.filename
  let condition t = t.condition
  let kind t = Covers.kind t.covers
  let exclude_location t loc = Covers.exclude_location t.covers loc
  let parent t = t.parent
  let pos_lt (a : position) (b : position) = a.pos_cnum < b.pos_cnum
  let pos_max a b = if pos_lt a b then b else a
  let pos_min a b = if pos_lt a b then a else b
  let same_file (a : position) (b : position) = String.equal a.pos_fname b.pos_fname

  let covers t =
    match t.covers with
    | Implicit_else -> []
    | Code { cover; exclusions } ->
      let ps = cover.loc_start in
      let pe = cover.loc_end in
      let exclusions =
        List.filter_map
          (fun claim ->
            let ss = claim.loc_start in
            let se = claim.loc_end in
            if not (same_file ps ss)
            then None
            else (
              let ss = pos_max ss ps in
              let se = pos_min se pe in
              if pos_lt ss se then Some (ss, se) else None))
          exclusions
      in
      let exclusions =
        List.sort (fun (sa, _) (sb, _) -> compare sa.pos_cnum sb.pos_cnum) exclusions
      in
      let cursor, covers =
        List.fold_left
          (fun (cursor, covers) (ss, se) ->
            let covers = if pos_lt cursor ss then (cursor, ss) :: covers else covers in
            pos_max cursor se, covers)
          (ps, [])
          exclusions
      in
      let covers = if pos_lt cursor pe then (cursor, pe) :: covers else covers in
      List.rev covers
  ;;
end

type t =
  { mutable next_id : int
  ; mutable points : Point.t list
  ; (* Distinct filenames in first-use order. Almost always a single entry, but [#] line
       directives mean a file can contain locations with several filenames, so appending
       is cheap in practice. *)
    mutable filenames : Filename.t list
  }

let create () = { next_id = 0; points = []; filenames = [] }

let intern_filename t name =
  let rec loop n = function
    | (other : Filename.t) :: rest ->
      if String.equal other.name name then other else loop (n + 1) rest
    | [] ->
      let filename = { Filename.id = n; name } in
      t.filenames <- t.filenames @ [ filename ];
      filename
  in
  loop 0 t.filenames
;;

let fresh_point t ~(here : Location.t) ~condition ~covers ~parent =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  (* [expand_filename] prepends the [-dirname] the build rules pass to the driver, so
     coverage metadata records paths relative to the repo root rather than basenames. *)
  let filename =
    intern_filename t (Ppx_here_expander.expand_filename here.loc_start.pos_fname)
  in
  let point = { Point.id; here; filename; condition; covers; parent } in
  t.points <- point :: t.points;
  point
;;

let fresh t ~here ~condition ~parent ~cover =
  let covers = Point.Covers.Code { cover; exclusions = [] } in
  fresh_point t ~here ~condition ~covers ~parent
;;

let fresh_implicit_else t ~here ~parent =
  fresh_point t ~here ~condition:None ~covers:Implicit_else ~parent
;;

let registered t = List.sort (fun a b -> compare a.Point.id b.Point.id) t.points
let filenames t = t.filenames
