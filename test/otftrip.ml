(*---------------------------------------------------------------------------
   Copyright (c) 2013 The otfm programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

module Fmt = struct
  let pf = Format.fprintf
  let pr = Format.printf
  let epr = Format.eprintf
  let str = Format.asprintf
  let list = Format.pp_print_list
  let sp = Format.pp_print_space
  let cut = Format.pp_print_cut
  let nop ppf _ = ()
  let string = Format.pp_print_string
  let list ?(empty = nop) ?sep:pp_sep pp_elt ppf = function
  | [] -> empty ppf ()
  | l -> Format.pp_print_list ?pp_sep pp_elt ppf l
end

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let finally () = if inf <> "-" then close_in_noerr ic else () in
    Fun.protect ~finally @@ fun () ->
    let buf_size = 65536 in
    let b = Buffer.create buf_size in
    try while true do Buffer.add_channel b ic buf_size done; assert false with
    | End_of_file -> Ok (Buffer.contents b)
  with
  | Failure _ -> Error (Fmt.str "%s: input file too large" inf)
  | Sys_error e -> (Error e)

let exec = Filename.basename Sys.executable_name
let log msg = Fmt.epr ("%s: " ^^ msg ^^ "@.") exec
let log_if_error ~use = function Ok v -> v | Error e -> log "%s" e; use

let file_err file = Result.map_error (Fmt.str "%s: %a" file Otfm.pp_error)

(* Convenience decodes *)

let cmap_src_to_string (pid, eid, fid) =
  Fmt.str " platform-id:%d encoding-id:%d format:%d" pid eid fid

let get_postscript_name d =
  let* name = Otfm.postscript_name d in
  Ok (Option.value ~default:"<unknown>" name)

module Umap = Map.Make (Uchar)
module Cp_map = Map.Make (Int)
module Gid_map = Map.Make (Int)

let get_cmap_table d =
  let uchar = Umap.add and surrogate = Cp_map.add in
  Otfm.cmap_fold_uchars d ~uchar ~surrogate Umap.empty Cp_map.empty

let get_hmtx_table d =
  let add acc gid adv lsb = Gid_map.add gid (adv, lsb) acc in
  Otfm.hmtx d add Gid_map.empty

let get_glyph_id_of_cp file name d spec =
  let* src, (umap, cmap) = get_cmap_table d |> file_err file in
  let u, gid = match spec with
  | `Uchar u -> Uchar.to_int u, Umap.find_opt u umap
  | `Surrogate u ->u, Cp_map.find_opt u cmap
  in
  match gid with
  | Some gid -> Ok (src, gid)
  | None ->
      let src = cmap_src_to_string src in
      Error (Fmt.str "%s: %s: no binding for U+%04X in%s" file name u src)

let get_glyph_id_of_spec file name d = function
| `Gid gid -> Ok gid
| `Uchar _ | `Surrogate _ as spec ->
    let* _, gid = get_glyph_id_of_cp file name d spec in Ok gid

(* Font and file iterators *)

let process_file_fonts ~keep_going ~fonts process file =
  let* data = string_of_file file in
  let* ds = Otfm.decoder_collection (`String data) |> file_err file in
  let rec loop errs = function
  | [] -> errs
  | d :: ds ->
      let process file d =
        if fonts = [] then process file d else
        let* name = get_postscript_name d |> file_err file in
        if List.mem name fonts then process file d else Ok ()
      in
      match process file d with
      | Ok () -> loop errs ds
      | Error e ->
          if e <> "" then Fmt.epr "%s@." e;
          if keep_going then loop (errs + 1) ds else errs + 1
  in
  let errs = loop 0 ds in
  if errs <> 0 then Error "" else Ok ()

let process_files ~keep_going ~fonts process files =
  let rec loop errs = function
  | [] -> errs
  | f :: fs ->
      match process_file_fonts ~keep_going ~fonts process f with
      | Ok () -> loop errs fs
      | Error e ->
          if e <> "" then Fmt.epr "%s@." e;
          if keep_going then loop (errs + 1) fs else errs + 1
  in
  let errs = loop 0 files in
  if errs <> 0
  then Cmdliner.Cmd.Exit.some_error
  else Cmdliner.Cmd.Exit.ok

(* Table pretty printers

   XXX these would be good to rewrite with a few nice combinators *)

let pp_flavour ppf f =
  Fmt.string ppf (match f with `CFF -> "CFF" | `TTF -> "TTF")

let pp_cmap ppf d =
  let pp_map ppf u gid = Fmt.pf ppf "@,(%a %d)" Otfm.pp_cp u gid in
  let pp_binding ppf () k (u0, u1) gid = match k with
  | `Glyph -> for u = u0 to u1 do pp_map ppf u gid done
  | `Glyph_range -> for i = 0 to (u1 - u0) do pp_map ppf (u0 + i) (gid + i)done
  in
  Fmt.pf ppf "@,@[<v1>(cmap";
  match Otfm.cmap d (pp_binding ppf) () with
  | Error _ as e -> e
  | Ok ((pid, eid, fmt), _) ->
      Fmt.pf ppf "@,@[<1>(source@ (platform-id %d)@ (encoding-id %d)\
                  @ (format %d))@])@]" pid eid fmt;
      Ok ()

let pp_glyf ppf has_glyf d =
  if not has_glyf then Ok () else
  let pp_bbox ppf (minx, miny, maxx, maxy) =
    Fmt.pf ppf "@[(bbox@ %d@ %d@ %d@ %d)@]" minx miny maxx maxy
  in
  let pp_contour ppf pts =
    let rec pp_pts ppf = function
    | [] -> Fmt.pf ppf ")@]"
    | (b, x, y) :: pts ->
        Fmt.pf ppf "@,@[<1>(pt@ %b %d %d)@]" b x y; pp_pts ppf pts
    in
    Fmt.pf ppf "@,@[<v1>(contour%a" pp_pts pts
  in
  let pp_simple ppf gid cs bbox =
    Fmt.pf ppf "@,@[<v1>(glyph-simple %d %a" gid pp_bbox bbox;
    List.iter (pp_contour ppf) cs;
    Fmt.pf ppf ")@]"
  in
  let pp_matrix ppf m = match m with
  | None -> ()
  | Some (a, b, c, d) -> Fmt.pf ppf "@ @[<1>(ltr %g %g %g %g)@]" a b c d
  in
  let pp_component ppf (gid, (dx, dy), m) =
    Fmt.pf ppf "@,@[(component %d @[(move@ %d %d)@]%a)@]" gid dx dy pp_matrix m
  in
  let pp_composite ppf gid cs bbox =
    Fmt.pf ppf "@,@[<v1>(glyph-composite %d %a" gid pp_bbox bbox;
    List.iter (pp_component ppf) cs;
    Fmt.pf ppf ")@]"
  in
  match Otfm.glyph_count d with
  | Error _ as e -> e
  | Ok gc ->
      Fmt.pf ppf "@,@[<v1>(glyf";
      let rec loop gid =
        if gid >= gc then (Fmt.pf ppf "@]"; Ok ()) else
        match Otfm.loca d gid with
        | Error _ as e -> e
        | Ok None ->
            Fmt.pf ppf "@,@[(glyph-no-outline %d)@]" gid; loop (gid + 1)
        | Ok (Some gloc) ->
            match Otfm.glyf d gloc with
            | Error _ as e -> e
            | Ok (`Simple cs, bb) -> pp_simple ppf gid cs bb; loop (gid + 1)
            | Ok (`Composite cs, bb) ->
                pp_composite ppf gid cs bb; loop (gid + 1)
      in
      loop 0

let pp_head ppf d =
  Fmt.pf ppf "@,@[<v1>(head";
  match Otfm.head d with
  | Error _ as e -> e
  | Ok h ->
      Fmt.pf ppf "@,(font-revision 0x%08lX)" h.Otfm.head_font_revision;
      Fmt.pf ppf "@,(flags 0x%04X)" h.Otfm.head_flags;
      Fmt.pf ppf "@,(units-per-em %d)" h.Otfm.head_units_per_em;
      Fmt.pf ppf "@,(created %f)" h.Otfm.head_created;
      Fmt.pf ppf "@,(modified %f)" h.Otfm.head_modified;
      Fmt.pf ppf "@,(xmin %d)" h.Otfm.head_xmin;
      Fmt.pf ppf "@,(ymin %d)" h.Otfm.head_ymin;
      Fmt.pf ppf "@,(xmax %d)" h.Otfm.head_xmax;
      Fmt.pf ppf "@,(ymax %d)" h.Otfm.head_ymax;
      Fmt.pf ppf "@,(mac-style 0x%04X)" h.Otfm.head_mac_style;
      Fmt.pf ppf "@,(lowest_rec_ppem %d)" h.Otfm.head_lowest_rec_ppem;
      Fmt.pf ppf "@,(index_to_loc_format %d)" h.Otfm.head_index_to_loc_format;
      Fmt.pf ppf ")@]";
      Ok ()

let pp_hhea ppf d = match Otfm.hhea d with
| Error _ as e -> e
| Ok h ->
    Fmt.pf ppf "@,@[<v1>(hhea";
    Fmt.pf ppf "@,(ascender %d)" h.Otfm.hhea_ascender;
    Fmt.pf ppf "@,(descender %d)" h.Otfm.hhea_descender;
    Fmt.pf ppf "@,(line-gap %d)" h.Otfm.hhea_line_gap;
    Fmt.pf ppf "@,(advance-width-max %d)" h.Otfm.hhea_advance_width_max;
    Fmt.pf ppf "@,(min-left-side-bearing %d)" h.Otfm.hhea_min_left_side_bearing;
    Fmt.pf ppf "@,(min-right-side-bearing %d)"
      h.Otfm.hhea_min_right_side_bearing;
    Fmt.pf ppf "@,(xmax-extent %d)" h.Otfm.hhea_xmax_extent;
    Fmt.pf ppf "@,(caret-slope-rise %d)" h.Otfm.hhea_caret_slope_rise;
    Fmt.pf ppf "@,(caret-slope-run %d)" h.Otfm.hhea_caret_slope_run;
    Fmt.pf ppf "@,(caret-offset %d)" h.Otfm.hhea_caret_offset;
    Fmt.pf ppf ")@]";
    Ok ()

let pp_hmtx ppf d =
  let pp_hm ppf () id adv lsb =
    Fmt.pf ppf "@,(%d (adv %d) (lsb %d))" id adv lsb
  in
  Fmt.pf ppf "@,@[<v1>(hmtx";
  match Otfm.hmtx d (pp_hm ppf) () with
  | Error _ as e -> e
  | Ok () -> Fmt.pf ppf ")@]"; Ok ()

let pp_name ppf d =
  let pp_n ppf () id lang string =
    Fmt.pf ppf "@,(%d %s \"%s\")" id lang string
  in
  Fmt.pf ppf "@,@[<v1>(name";
  match Otfm.name d (pp_n ppf) () with
  | Error _ as e -> e
  | Ok () -> Fmt.pf ppf ")@]"; Ok ()

let pp_os2 ppf d =
  let pp_opt pp_v ppf = function
  | None -> Fmt.pf ppf "None" | Some v -> pp_v ppf v
  in
  let pp_ouint32 ppf v = pp_opt (fun ppf v -> Fmt.pf ppf "%lX" v) ppf v in
  let pp_oint = pp_opt Format.pp_print_int in
  match Otfm.os2 d with
  | Error _ as e -> e
  | Ok o ->
      Fmt.pf ppf "@,@[<v1>(os2";
      Fmt.pf ppf "@,(x-avg-char-width %d)" o.Otfm.os2_x_avg_char_width;
      Fmt.pf ppf "@,(us-weight-class %d)" o.Otfm.os2_us_weight_class;
      Fmt.pf ppf "@,(us-width-class %d)" o.Otfm.os2_us_width_class;
      Fmt.pf ppf "@,(fs-type %X)" o.Otfm.os2_fs_type;
      Fmt.pf ppf "@,(y-subscript-x-size %d)" o.Otfm.os2_y_subscript_x_size;
      Fmt.pf ppf "@,(y-subscript-y-size %d)" o.Otfm.os2_y_subscript_y_size;
      Fmt.pf ppf "@,(y-subscript-x-offset %d)" o.Otfm.os2_y_subscript_x_offset;
      Fmt.pf ppf "@,(y-subscript-y-offset %d)" o.Otfm.os2_y_subscript_y_offset;
      Fmt.pf ppf "@,(y-superscript-x-size %d)" o.Otfm.os2_y_superscript_x_size;
      Fmt.pf ppf "@,(y-superscript-y-size %d)" o.Otfm.os2_y_superscript_y_size;
      Fmt.pf ppf "@,(y-superscript-x-offset %d)"
        o.Otfm.os2_y_superscript_x_offset;
      Fmt.pf ppf "@,(y-superscript-y-offset %d)"
        o.Otfm.os2_y_superscript_y_offset;
      Fmt.pf ppf "@,(y-strikeout-size %d)" o.Otfm.os2_y_strikeout_size;
      Fmt.pf ppf "@,(y-strikeout-position %d)" o.Otfm.os2_y_strikeout_position;
      Fmt.pf ppf "@,(family-class %d)" o.Otfm.os2_family_class;
      Fmt.pf ppf "@,(panose \"%s\")" (String.escaped o.Otfm.os2_panose);
      Fmt.pf ppf "@,(ul-unicode-range1 %lX)" o.Otfm.os2_ul_unicode_range1;
      Fmt.pf ppf "@,(ul-unicode-range2 %lX)" o.Otfm.os2_ul_unicode_range2;
      Fmt.pf ppf "@,(ul-unicode-range3 %lX)" o.Otfm.os2_ul_unicode_range3;
      Fmt.pf ppf "@,(ul-unicode-range4 %lX)" o.Otfm.os2_ul_unicode_range4;
      Fmt.pf ppf "@,(ach-vend-id %a)"
        Otfm.Tag.pp (Otfm.Tag.of_int32 o.Otfm.os2_ach_vend_id);
      Fmt.pf ppf "@,(fs-selection %X)" o.Otfm.os2_fs_selection;
      Fmt.pf ppf "@,(us-first-char-index %d)" o.Otfm.os2_us_first_char_index;
      Fmt.pf ppf "@,(us-last-char-index %d)" o.Otfm.os2_us_last_char_index;
      Fmt.pf ppf "@,(s-typo-ascender %d)" o.Otfm.os2_s_typo_ascender;
      Fmt.pf ppf "@,(s-type-descender %d)" o.Otfm.os2_s_type_descender;
      Fmt.pf ppf "@,(s-typo-linegap %d)" o.Otfm.os2_s_typo_linegap;
      Fmt.pf ppf "@,(us-win-ascent %d)" o.Otfm.os2_us_win_ascent;
      Fmt.pf ppf "@,(us-win-descent %d)" o.Otfm.os2_us_win_descent;
      Fmt.pf ppf "@,(ul-code-page-range-1 %a)"
        pp_ouint32 o.Otfm.os2_ul_code_page_range_1;
      Fmt.pf ppf "@,(ul-code-page-range-2 %a)"
        pp_ouint32 o.Otfm.os2_ul_code_page_range_2;
      Fmt.pf ppf "@,(s-x-height %a)" pp_oint o.Otfm.os2_s_x_height;
      Fmt.pf ppf "@,(s-cap-height %a)" pp_oint o.Otfm.os2_s_cap_height;
      Fmt.pf ppf "@,(us-default-char %a)" pp_oint o.Otfm.os2_us_default_char;
      Fmt.pf ppf "@,(us-break-char %a)" pp_oint o.Otfm.os2_us_break_char;
      Fmt.pf ppf "@,(us-max-context %a)" pp_oint o.Otfm.os2_us_max_context;
      Fmt.pf ppf ")@]";
      Ok ()

let pp_kern ppf has_kern d =
  if not has_kern then Ok () else
  let dir = function `H -> "H" | `V -> "V" in
  let kind = function `Kern -> "kerning" | `Min -> "minimal" in
  let pp_kinfo ppf first i =
    if not first then Fmt.pf ppf ")@]";
    Fmt.pf ppf "@,@[<v1>((dir %s)@,(kind %s)@,(cross-stream %b)"
      (dir i.Otfm.kern_dir) (kind i.Otfm.kern_kind)
      (i.Otfm.kern_cross_stream);
    `Fold, false
  in
  let pp_pair ppf first l r v = Fmt.pf ppf "@,(%d %d %d)" l r v; first in
  Fmt.pf ppf "@,@[<v1>(kern";
  match Otfm.kern d (pp_kinfo ppf) (pp_pair ppf) true with
  | Error _ as e -> e
  | Ok _ -> Fmt.pf ppf ")@])@]"; Ok ()

let pp_tables ppf file ts d =
  let err = ref false in
  let log_if_error = function
  | Ok () -> ()
  | Error _ as r -> err := true; log_if_error ~use:() (file_err file r)
  in
  let has_glyf = List.mem Otfm.Tag.glyf ts in
  let has_kern = List.mem Otfm.Tag.kern ts in
  pp_name ppf d |> log_if_error;
  pp_head ppf d |> log_if_error;
  pp_hhea ppf d |> log_if_error;
  pp_os2  ppf d |> log_if_error;
  pp_cmap ppf d |> log_if_error;
  pp_hmtx ppf d |> log_if_error;
  pp_glyf ppf has_glyf d |> log_if_error;
  pp_kern ppf has_kern d |> log_if_error;
  if !err then (Error "") else Ok ()

(* cmap *)

let cmap_dump_font output_format show_path show_source file d =
  let* name = get_postscript_name d |> file_err file in
  let* src, (umap, cmap) =
    let uchar u gid acc = Cp_map.add (Uchar.to_int u) gid acc in
    let surrogate = Cp_map.add in
    let init = Cp_map.empty in
    Otfm.cmap_fold_uchars d ~uchar ~surrogate init init |> file_err file
  in
  let cp_map = Cp_map.union (fun _ -> assert false) umap cmap in
  let file = if show_path then Fmt.str "%s: " file else "" in
  let src = if show_source then cmap_src_to_string src else "" in
  begin match output_format with
  | `Short when file = "" && src = "" -> ()
  | `Short -> Fmt.pr "@[<h>%s%s@]@." file src
  | `Normal | `Long -> Fmt.pr "@[<h>%sname: %s%s@]@." file name src
  end;
  begin match output_format with
  | `Short | `Normal | `Long ->
      Cp_map.iter (fun u gid -> Fmt.pr "@[<h>U+%04X %d@]@." u gid) cp_map
  end;
  Ok ()

let cmap_get_font output_format show_path show_source cp file d =
  let* name = get_postscript_name d |> file_err file in
  let* src, gid = get_glyph_id_of_cp file name d cp in
  let file = if show_path then Fmt.str "%s: " file else "" in
  let src = if show_source then cmap_src_to_string src else "" in
  begin match output_format with
  | `Short -> Fmt.pr "@[<h>%s%d%s@]@." file gid src
  | `Normal | `Long -> Fmt.pr "@[<h>%s%s: %d%s@]@." file name gid src
  end;
  Ok ()

let cmap_dump keep_going fonts output_format show_path show_source files =
  let process = cmap_dump_font output_format show_path show_source in
  process_files ~keep_going ~fonts process files

let cmap_get keep_going fonts output_format show_path show_source cp files =
  let process = cmap_get_font output_format show_path show_source cp in
  process_files ~keep_going ~fonts process files

(* Dump *)

let dump ppf file d =
  let* f = Otfm.flavour d |> file_err file in
  Fmt.pf ppf "@[<v1>(@[<1>(file %S)@]" file;
  let fs = match f with `TTF -> "TTF" | `CFF -> "CFF" in
  Fmt.pf ppf "@,@[<1>(flavor %s)@]" fs;
  let* name = get_postscript_name d |> file_err file in
  Fmt.pf ppf "@,@[<1>(postscript-name %s)@]" name;
  let* glyph_count = Otfm.glyph_count d |> file_err file in
  Fmt.pf ppf "@,@[<1>(glyph-count %d)@]" glyph_count;
  let* ts = Otfm.table_list d |> file_err file in
  Fmt.pf ppf "@,@[<1>(tables ";
  List.iter (fun t -> Fmt.pf ppf "@ %a" Otfm.Tag.pp t) ts;
  Fmt.pf ppf ")@]";
  let* () = pp_tables ppf file ts d in
  Fmt.pf ppf ")@]@.";
  Ok ()

let decode file d =
  let err = ref false in
  let log_if_error = function
  | Ok _ -> ()
  | Error _ as r -> err := true; log_if_error ~use:() (file_err file r)
  in
  let kern_nop () _ = `Fold, () in
  let nop4 _ _ _ _ = () in
  Otfm.flavour d |> log_if_error;
  Otfm.table_list d |> log_if_error;
  Otfm.cmap d nop4 () |> log_if_error;
  Otfm.head d |> log_if_error;
  Otfm.hhea d |> log_if_error;
  Otfm.hmtx d nop4 () |> log_if_error;
  Otfm.name d nop4 () |> log_if_error;
  Otfm.os2 d |> log_if_error;
  Otfm.kern d kern_nop nop4 () |> log_if_error;
  if !err then Error "" else Ok ()

let dump_font output_format decode_only file d = match decode_only with
| true -> decode file d
| false -> dump Format.std_formatter file d

let dump keep_going fonts output_format decode_only files =
  let process = dump_font output_format decode_only in
  process_files ~keep_going ~fonts process files

(* Glyph *)

let glyph_get_font output_format show_path gid file d =
  failwith "TODO"

let glyph_get keep_going fonts output_format show_path gid files =
  let process = glyph_get_font output_format show_path gid in
  process_files ~keep_going ~fonts process files

(* hmtx *)

let hmtx_dump_font output_format show_path file d =
  let* name = get_postscript_name d |> file_err file in
  let* gmap = get_hmtx_table d |> file_err file in
  let file = if show_path then Fmt.str "%s: " file else "" in
  begin match output_format with
  | `Short when file = "" -> ()
  | `Short -> Fmt.pr "@[<h>%s@]@." file
  | `Normal | `Long -> Fmt.pr "@[<h>%sname: %s@]@." file name
  end;
  begin match output_format with
  | `Short | `Normal | `Long ->
      let pr gid (adv, lsb) = Fmt.pr "@[<h>%d adv:%d lsb:%d@]@." gid adv lsb in
      Gid_map.iter pr gmap
  end;
  Ok ()

let hmtx_get_font output_format show_path gid file d =
  let* name = get_postscript_name d |> file_err file in
  let* gid = get_glyph_id_of_spec file name d gid in
  let* gmap = get_hmtx_table d |> file_err file in
  match Gid_map.find_opt gid gmap with
  | None ->
      Error
        (Fmt.str "%s: %s: not horizontal metrics for glyph %d" file name gid)
  | Some (adv, lsb) ->
      let file = if show_path then Fmt.str "%s: " file else "" in
      begin match output_format with
      | `Short -> Fmt.pr "@[<h>%sadv:%d lsb:%d@]@." file adv lsb
      | `Normal | `Long ->
          Fmt.pr "@[<h>%s%s: adv:%d lsb:%d@]@." file name adv lsb
      end;
      Ok ()

let hmtx_dump keep_going fonts output_format show_path files =
  let process = hmtx_dump_font output_format show_path in
  process_files ~keep_going ~fonts process files

let hmtx_get keep_going fonts output_format show_path gid files =
  let process = hmtx_get_font output_format show_path gid in
  process_files ~keep_going ~fonts process files

(* Sniff *)

let sniff_font output_format show_path file d =
  file_err file @@
  let* flavour = Otfm.flavour d in
  let* glyph_count = Otfm.glyph_count d in
  let* name = get_postscript_name d in
  let* tables = Otfm.table_list d in
  let tables = List.sort Otfm.Tag.compare tables in
  let pp_tables = Fmt.(list ~sep:sp Otfm.Tag.pp) in
  begin match output_format with
  | `Short ->
      let pp_path ppf p = if show_path then Fmt.pf ppf "%s: " p else () in
      Fmt.pr "@[<h>%a%s %a glyphs:%d @[<h>%a@]@]@."
        pp_path file name pp_flavour flavour glyph_count pp_tables tables;
  | `Normal | `Long ->
      let pp_path ppf p = if show_path then Fmt.pf ppf "%s:@," p else () in
      Fmt.pr "@[<v>%aname: %s@,flavour: %a@,glyphs: %d@,tables: @[%a@]@]@."
        pp_path file name pp_flavour flavour glyph_count pp_tables tables
  end;
  Ok ()

let sniff keep_going fonts output_format show_path files =
  let process = sniff_font output_format show_path in
  process_files ~keep_going ~fonts process files

(* Command line interface *)

open Cmdliner

(* Converters *)

let uchar_or_surrogate u =
  if Uchar.is_valid u then Some (`Uchar (Uchar.of_int u)) else
  if 0 <= u && u <= 0x10FFFF then Some (`Surrogate u) else
  None

let pp_uchar_or_surrogate ppf cp =
  let cp = match cp with `Uchar u -> Uchar.to_int u | `Surrogate u -> u in
  Fmt.pf ppf "U+%04X" cp

let parse_uchar_utf_8 s = (* [uU]:UTF8 *)
  let ret _ s =
    let u acc _ = function
    | `Malformed _ -> Some (Error ())
    | `Uchar u ->
        match acc with
        | Some _ -> Some (Error ())
        | None -> Some (Ok (`Uchar u))
    in
    match Uutf.String.fold_utf_8 u None s with
    | Some (Ok v) -> Some v | _ -> None
  in
  try Scanf.sscanf s "%1[Uu]:%s%!" ret with
  | Scanf.Scan_failure _ | End_of_file -> None

let parse_uchar_spec s = (* [uU]+%X *)
  let ret _ u = uchar_or_surrogate u in
  try Scanf.sscanf s "%1[Uu]+%x%!" ret with
  | Scanf.Scan_failure _ | End_of_file -> None

let cp_conv =
  let parse s = match parse_uchar_utf_8 s with
  | Some u -> Ok u
  | None ->
      match parse_uchar_spec s with
      | Some u -> Ok u
      | None ->
          Error (Fmt.str "Could not parse an Unicode character from %S" s)
  in
  Cmdliner.Arg.conv' (parse, pp_uchar_or_surrogate) ~docv:"UCHAR"

let gid_spec_conv =
  let parse s = match parse_uchar_utf_8 s with
  | Some u -> Ok u
  | None ->
      match parse_uchar_spec s with
      | Some u -> Ok u
      | None ->
          match int_of_string_opt s with
          | None ->
              Error (Fmt.str "Could not parse an glyph identifier from %S" s)
          | Some gid ->
              if gid < 0 || gid > 65535
              then Error (Fmt.str "%d: not a valid glyph identifier." gid)
              else Ok (`Gid gid)
  in
  let print ppf = function
  | `Gid gid -> Fmt.pf ppf "%d" gid
  | `Uchar _ | `Surrogate _ as cp -> pp_uchar_or_surrogate ppf cp
  in
  Cmdliner.Arg.conv' (parse, print) ~docv:"GID"

(* Common options *)

let s_output_format_options = "OUTPUT FORMAT OPTIONS"
let output_format =
  let docs = s_output_format_options in
  let short =
    let doc = "Short output. Line based output with only relevant data." in
    Arg.info ["s"; "short"] ~doc ~docs
  in
  let long =
    let doc = "Long output. Outputs as much information as possible." in
    Arg.info ["l"; "long"] ~doc ~docs
  in
  Arg.(value & vflag `Normal [`Short, short; `Long, long])

let keep_going =
  let doc = "Do not stop at the first lookup or decoding error." in
  Arg.(value & flag & info ["k"; "keep-going"] ~doc)

let files ?after:(r = -1) () =
  let doc = "Decode OpenType file $(docv). Use $(b,-) for stdin. Repeatable." in
  let docv = "FILE" in
  Arg.(non_empty & pos_right r string [] & info [] ~doc ~docv)

let show_path =
  let doc = "Show font file paths in the output." in
  Arg.(value & flag & info ["p"; "show-path"] ~doc)

let show_source =
  let doc = "Show tables from which the data was drawn in the output." in
  Arg.(value & flag & info ["w"; "show-source"] ~doc)

let fonts =
  let doc = "Only show output for fonts whose name matches $(docv). \
             Repeatable."
  in
  Arg.(value & opt_all string [] & info ["f";"font"] ~doc ~docv:"FONT")

let gid =
  let doc =
    "The glyph identifier to lookup. This can be specified as an integer in \
     decimal or hexadecimal $(b,0x)$(i,H…) notation. Specifying an \
     an Unicode code point as $(b,u:)$(i,UTF8) with $(i,UTF8) a \
     single UTF-8 encoded character or via the $(b,U+)$(i,H…) code point \
     notation directly finds the glyph identifier via the cmap table."
  in
  let docv = "GID" in
  Arg.(required & pos 0 (some gid_spec_conv) None & info [] ~doc ~docv)

(* Commands and sub commands *)

let cmap_dump_cmd =
  let doc = "Dump the cmap table" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) dumps the cmap table of fonts.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  Cmd.v (Cmd.info "dump" ~man ~doc)
    Term.(const cmap_dump $ keep_going $ fonts $ output_format $ show_path $
          show_source $ files ())

let cmap_get_cmd =
  let doc = "Output glyph id of an Unicode character" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) determines the glyph id of a given Unicode \
          character by looking up the cmap table of fonts";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  let cp =
    let doc =
      "The Unicode character to lookup. This can specified as $(b,u:)$(i,UTF8)
       with $(i,UTF8) a single UTF-8 encoded character or as an Unicode \
       code point in $(b,U+)$(i,H…) notation. Since cmaps can map \
       them, surrogate code points can be specified."
    in
    let docv = "UCHAR" in
    Arg.(required & pos 0 (some cp_conv) None & info [] ~doc ~docv)
  in
  Cmd.v (Cmd.info "get" ~man ~doc)
    Term.(const cmap_get $ keep_going $ fonts $ output_format $ show_path $
          show_source $ cp $ files ~after:0 ())

let cmap_cmd =
  let doc = "cmap (character map) table lookups" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname)'s commands provides various lookups for the cmap table.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  Cmd.group (Cmd.info "cmap" ~man ~doc) [cmap_dump_cmd; cmap_get_cmd]

let dump_cmd =
  let doc = "Output font information and all decodable tables" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) outputs font information and all decodable tables.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  let only =
    let doc = "Decode only. Do not output data." in
    Arg.(value & flag & info ["d"; "decode-only"] ~doc)
  in
  Cmd.v (Cmd.info "dump" ~man ~doc)
    Term.(const dump $ keep_going $ fonts $ output_format $ only $ files ())

let glyph_get_cmd =
  let doc = "Output glyph information" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) outputs information about a glyph.";
      `S Manpage.s_options;
      `S s_output_format_options;
    ]
  in
  Cmd.v (Cmd.info "get" ~man ~doc)
    Term.(const glyph_get $ keep_going $ fonts $ output_format $ show_path $
          gid $ files ~after:0 ())

let glyph_cmd =
  let doc = "Glyph lookups" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname)'s commands provides various lookups for glyphs.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  Cmd.group (Cmd.info "glyph" ~man ~doc) [glyph_get_cmd]

let hmtx_get_cmd =
  let doc = "Output glyph information" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) outputs information about a glyph.";
      `S Manpage.s_options;
      `S s_output_format_options;
    ]
  in
  Cmd.v (Cmd.info "get" ~man ~doc)
    Term.(const hmtx_get $ keep_going $ fonts $ output_format $ show_path $
          gid $ files ~after:0 ())

let hmtx_get_dump =
  let doc = "Output glyph information" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) outputs information about a glyph.";
      `S Manpage.s_options;
      `S s_output_format_options;
    ]
  in
  Cmd.v (Cmd.info "get" ~man ~doc)
    Term.(const hmtx_get $ keep_going $ fonts $ output_format $ show_path $
          gid $ files ~after:0 ())

let hmtx_dump_cmd =
  let doc = "Dump the hmtx table" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) dumps the $(b,hmtx) table of fonts.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  Cmd.v (Cmd.info "dump" ~man ~doc)
    Term.(const hmtx_dump $ keep_going $ fonts $ output_format $ show_path $
          files ())

let hmtx_cmd =
  let doc = "hmtx (horizontal metrics) table lookups" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname)'s commands provides various lookups for the hmtx table.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  Cmd.group (Cmd.info "hmtx" ~man ~doc) [hmtx_dump_cmd; hmtx_get_cmd]

let sniff_cmd =
  let doc = "Output basic font information" in
  let man =
    [ `S Manpage.s_description;
      `P "$(tname) outputs the PostScript name, flavour, glyph count and \
          table names of fonts.";
      `S Manpage.s_options;
      `S s_output_format_options; ]
  in
  Cmd.v (Cmd.info "sniff" ~man ~doc)
    Term.(const sniff $ keep_going $ fonts $ output_format $ show_path $
          files ())

let cmds = [cmap_cmd; dump_cmd; glyph_cmd; hmtx_cmd; sniff_cmd]
let cmd =
  let doc = "OpenType file inspector" in
  let man =
    [ `S Manpage.s_description;
      `P "$(mname) decodes OpenType files ($(b,.otf), $(b,.ttf), $(b,.otc), \
          $(b,.ttc)) and outputs information about their fonts in various \
          ways.";
      `S Manpage.s_bugs;
      `P "This program is distributed with the Otfm OCaml library. \
          See https:/erratique.ch/software/otfm for contact information"; ]
  in
  Cmd.group Cmd.(info "otftrip" ~man ~doc ~version:"%%VERSION%%") cmds


let main () = exit (Cmd.eval' cmd)
let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The otfm programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
