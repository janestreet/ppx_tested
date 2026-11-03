open! Ppxlib

let is_dev = ref Build_env.dev_build
let library_name_from_arg = ref None
let library_name_from_cookie = ref None

let () =
  Driver.add_arg
    "-library"
    (String (fun lib -> library_name_from_arg := Some lib))
    ~doc:"<name> Set the library name used to enable coverage tests."
;;

let library_cookie_name = "dune-build-unit"

let () =
  Driver.Cookies.add_handler (fun cookies ->
    match Driver.Cookies.get_res cookies library_cookie_name Ast_pattern.(estring __) with
    | Ok lib -> library_name_from_cookie := lib
    | Error _ -> ())
;;

let library_name () =
  match !library_name_from_arg with
  | Some lib -> lib
  | None -> Option.value !library_name_from_cookie ~default:""
;;

let rewrite_structure (_ : Expansion_context.Base.t) structure =
  if !is_dev
  then Rewrite_dev.rewrite_structure ~library:(library_name ()) structure
  else Rewrite_release.rewrite_structure structure
;;

let () =
  let instrument = Driver.Instrument.V2.make rewrite_structure ~position:Before in
  Driver.register_transformation "ppx_tested" ~instrument
;;

module For_testing = struct
  module Point = Point_collector.Point

  let rewrite_dev_structure = Rewrite_dev.rewrite_structure

  let dev_points ~library structure =
    let (_ : Ppxlib.structure), points =
      Rewrite_dev.rewrite_structure_and_points ~library structure
    in
    points
  ;;

  let rewrite_release_structure = Rewrite_release.rewrite_structure
end
