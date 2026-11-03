open! Ppxlib

let marks_ident = "__ppx_tested_marks"
let probe_name_for_library lib = "ppx_tested__" ^ lib

let source_position_record ~loc { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } =
  let eint = Ast_builder.Default.eint ~loc in
  [%expr
    { Ppx_tested_runtime.Coverage.Latest.Source_position.pos_lnum = [%e eint pos_lnum]
    ; pos_bol = [%e eint pos_bol]
    ; pos_cnum = [%e eint pos_cnum]
    }]
;;

let source_range_of_loc ~loc { loc_start; loc_end; loc_ghost = _ } =
  let start_pos = source_position_record ~loc loc_start in
  let end_pos = source_position_record ~loc loc_end in
  [%expr
    { Ppx_tested_runtime.Coverage.Latest.Source_range.start_pos = [%e start_pos]
    ; end_pos = [%e end_pos]
    }]
;;

let source_range_record ~loc start_pos end_pos =
  let start_e = source_position_record ~loc start_pos in
  let end_e = source_position_record ~loc end_pos in
  [%expr
    { Ppx_tested_runtime.Coverage.Latest.Source_range.start_pos = [%e start_e]
    ; end_pos = [%e end_e]
    }]
;;

let filename_id ~loc filename =
  let int_e = Ast_builder.Default.eint ~loc (Point_collector.Filename.id filename) in
  [%expr Ppx_tested_runtime.Coverage.Latest.Filename_id.of_int [%e int_e]]
;;

let filename_name ~loc filename =
  Ast_builder.Default.estring ~loc (Point_collector.Filename.name filename)
;;

let point_id ~loc point =
  let int_e = Ast_builder.Default.eint ~loc (Point_collector.Point.id point) in
  [%expr Ppx_tested_runtime.Coverage.Latest.Probe_id.of_int [%e int_e]]
;;

let point_record ~loc point =
  let here = Point_collector.Point.here point in
  let filename_e = filename_id ~loc (Point_collector.Point.filename point) in
  let here_e = source_range_of_loc ~loc here in
  let condition_e =
    match Point_collector.Point.condition point with
    | None -> [%expr None]
    | Some condition -> [%expr Some [%e Ast_builder.Default.estring ~loc condition]]
  in
  let kind_e =
    match Point_collector.Point.kind point with
    | Code -> [%expr Ppx_tested_runtime.Point.Kind.Code]
    | Implicit_else -> [%expr Ppx_tested_runtime.Point.Kind.Implicit_else]
  in
  let covers_e =
    Ast_builder.Default.pexp_array
      ~loc
      (List.map
         (fun (start_pos, end_pos) -> source_range_record ~loc start_pos end_pos)
         (Point_collector.Point.covers point))
  in
  let parent_e =
    match Point_collector.Point.parent point with
    | None -> [%expr None]
    | Some parent -> [%expr Some [%e point_id ~loc parent]]
  in
  [%expr
    { Ppx_tested_runtime.Point.filename = [%e filename_e]
    ; here = [%e here_e]
    ; condition = [%e condition_e]
    ; kind = [%e kind_e]
    ; covers = [%e covers_e]
    ; parent = [%e parent_e]
    }]
;;

let preamble_structure_items ~loc ~library ~filenames points =
  let loc = { loc with loc_ghost = true } in
  let probe_name_lit =
    Ast_builder.Default.estring ~loc (probe_name_for_library library)
  in
  let library_name_lit = Ast_builder.Default.estring ~loc library in
  let filenames_iarray =
    Ast_builder.Default.pexp_array ~loc (List.map (filename_name ~loc) filenames)
  in
  let points_iarray =
    Ast_builder.Default.pexp_array ~loc (List.map (point_record ~loc) points)
  in
  let len_int = Ast_builder.Default.eint ~loc (List.length points) in
  let marks_pat = Ast_builder.Default.pvar ~loc marks_ident in
  [ [%stri
      let [%p marks_pat] : Ppx_tested_runtime.Marks.Or_null.t =
        if [%probe_is_enabled [%e probe_name_lit]]
        then (
          let marks = Ppx_tested_runtime.Marks.create [%e len_int] in
          Ppx_tested_runtime.register
            ~library_name:[%e library_name_lit]
            ~module_name:Ppx_tested_runtime.__MODULE__
            ~filenames:[%e filenames_iarray]
            ~points:[%e points_iarray]
            marks;
          This marks)
        else Null
      ;;]
  ]
;;

let probe_call ~loc ~library point payload =
  let loc = { loc with loc_ghost = true } in
  let probe_name_lit =
    Ast_builder.Default.estring ~loc (probe_name_for_library library)
  in
  let index_lit = Ast_builder.Default.eint ~loc (Point_collector.Point.id point) in
  let marks_id = Ast_builder.Default.evar ~loc marks_ident in
  let predicate =
    match payload with
    | None -> [%expr true]
    | Some expression -> expression
  in
  let handler =
    [%expr Ppx_tested_runtime.mark [%e marks_id] [%e index_lit] ~when_:[%e predicate]]
  in
  let extension_payload =
    Ast_builder.Default.pstr_eval
      ~loc
      (Ast_builder.Default.eapply ~loc probe_name_lit [ handler ])
      []
  in
  Ast_builder.Default.pexp_extension
    ~loc
    (Loc.make ~loc "probe", PStr [ extension_payload ])
;;

let prepend_probe_call ~library point expression =
  let loc = { expression.pexp_loc with loc_ghost = true } in
  let probe = probe_call ~loc ~library point None in
  [%expr
    [%e probe];
    [%e expression]]
;;
