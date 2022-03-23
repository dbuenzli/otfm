(*---------------------------------------------------------------------------
   Copyright (c) 2013 The otfm programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind
let pp = Format.fprintf
let strf = Format.asprintf
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
  | Failure _ -> Error (strf "%s: input file too large" inf)
  | Sys_error e -> (Error e)

let exec = Filename.basename Sys.executable_name
let log msg = Format.eprintf ("%s: " ^^ msg ^^ "@.") exec
let log_err inf e =
  Format.eprintf "@[<2>%s:%s:@ %a@]@." exec inf Otfm.pp_error e

let log_if_error ~use inf = function
| Ok v -> v
| Error e -> Format.eprintf "@[<2>%s:%s:@ %s@]@." exec inf e; use

let file_err file = Result.map_error (strf "%s: %a" file Otfm.pp_error)

let process_file_fonts ~keep_going process file =
  let* data = string_of_file file in
  let* ds = Otfm.decoder_collection (`String data) |> file_err file in
  let rec loop errs = function
  | [] -> errs
  | d :: ds ->
      match process file d with
      | Ok () -> loop errs ds
      | Error e ->
          if e <> "" then Format.eprintf "%s@." e;
          if keep_going then loop (errs + 1) ds else errs + 1
  in
  let errs = loop 0 ds in
  if errs <> 0 then Error "" else Ok ()

let process_files ~keep_going process files =
  let rec loop errs = function
  | [] -> errs
  | f :: fs ->
      match process_file_fonts ~keep_going process f with
      | Ok () -> loop errs fs
      | Error e ->
          if e <> "" then Format.eprintf "%s@." e;
          if keep_going then loop (errs + 1) fs else errs + 1
  in
  let errs = loop 0 files in
  if errs <> 0
  then Cmdliner.Cmd.Exit.some_error
  else Cmdliner.Cmd.Exit.ok


(* Table pretty printers *)

let pp_cmap ppf d =
  let pp_map ppf u gid = pp ppf "@,(%a %d)" Otfm.pp_cp u gid in
  let pp_binding ppf () k (u0, u1) gid = match k with
  | `Glyph -> for u = u0 to u1 do pp_map ppf u gid done
  | `Glyph_range -> for i = 0 to (u1 - u0) do pp_map ppf (u0 + i) (gid + i)done
  in
  pp ppf "@,@[<v1>(cmap";
  match Otfm.cmap d (pp_binding ppf) () with
  | Error _ as e -> e
  | Ok ((pid, eid, fmt), _) ->
      pp ppf "@,@[<1>(source@ (platform-id %d)@ (encoding-id %d)\
              @ (format %d))@])@]" pid eid fmt;
      Ok ()

let pp_glyf ppf has_glyf d =
  if not has_glyf then Ok () else
  let pp_bbox ppf (minx, miny, maxx, maxy) =
    pp ppf "@[(bbox@ %d@ %d@ %d@ %d)@]" minx miny maxx maxy
  in
  let pp_contour ppf pts =
    let rec pp_pts ppf = function
    | [] -> pp ppf ")@]"
    | (b, x, y) :: pts -> pp ppf "@,@[<1>(pt@ %b %d %d)@]" b x y; pp_pts ppf pts
    in
    pp ppf "@,@[<v1>(contour%a" pp_pts pts
  in
  let pp_simple ppf gid cs bbox =
    pp ppf "@,@[<v1>(glyph-simple %d %a" gid pp_bbox bbox;
    List.iter (pp_contour ppf) cs;
    pp ppf ")@]"
  in
  let pp_matrix ppf m = match m with
  | None -> ()
  | Some (a, b, c, d) -> pp ppf "@ @[<1>(ltr %g %g %g %g)@]" a b c d
  in
  let pp_component ppf (gid, (dx, dy), m) =
    pp ppf "@,@[(component %d @[(move@ %d %d)@]%a)@]" gid dx dy pp_matrix m
  in
  let pp_composite ppf gid cs bbox =
    pp ppf "@,@[<v1>(glyph-composite %d %a" gid pp_bbox bbox;
    List.iter (pp_component ppf) cs;
    pp ppf ")@]"
  in
  match Otfm.glyph_count d with
  | Error _ as e -> e
  | Ok gc ->
      pp ppf "@,@[<v1>(glyf";
      let rec loop gid =
        if gid >= gc then (pp ppf "@]"; Ok ()) else
        match Otfm.loca d gid with
        | Error _ as e -> e
        | Ok None -> pp ppf "@,@[(glyph-no-outline %d)@]" gid; loop (gid + 1)
        | Ok (Some gloc) ->
            match Otfm.glyf d gloc with
            | Error _ as e -> e
            | Ok (`Simple cs, bb) -> pp_simple ppf gid cs bb; loop (gid + 1)
            | Ok (`Composite cs, bb) ->
                pp_composite ppf gid cs bb; loop (gid + 1)
      in
      loop 0

let pp_head ppf d =
  pp ppf "@,@[<v1>(head";
  match Otfm.head d with
  | Error _ as e -> e
  | Ok h ->
      pp ppf "@,(font-revision 0x%08lX)" h.Otfm.head_font_revision;
      pp ppf "@,(flags 0x%04X)" h.Otfm.head_flags;
      pp ppf "@,(units-per-em %d)" h.Otfm.head_units_per_em;
      pp ppf "@,(created %f)" h.Otfm.head_created;
      pp ppf "@,(modified %f)" h.Otfm.head_modified;
      pp ppf "@,(xmin %d)" h.Otfm.head_xmin;
      pp ppf "@,(ymin %d)" h.Otfm.head_ymin;
      pp ppf "@,(xmax %d)" h.Otfm.head_xmax;
      pp ppf "@,(ymax %d)" h.Otfm.head_ymax;
      pp ppf "@,(mac-style 0x%04X)" h.Otfm.head_mac_style;
      pp ppf "@,(lowest_rec_ppem %d)" h.Otfm.head_lowest_rec_ppem;
      pp ppf "@,(index_to_loc_format %d)" h.Otfm.head_index_to_loc_format;
      pp ppf ")@]";
      Ok ()

let pp_hhea ppf d = match Otfm.hhea d with
| Error _ as e -> e
| Ok h ->
    pp ppf "@,@[<v1>(hhea";
    pp ppf "@,(ascender %d)" h.Otfm.hhea_ascender;
    pp ppf "@,(descender %d)" h.Otfm.hhea_descender;
    pp ppf "@,(line-gap %d)" h.Otfm.hhea_line_gap;
    pp ppf "@,(advance-width-max %d)" h.Otfm.hhea_advance_width_max;
    pp ppf "@,(min-left-side-bearing %d)" h.Otfm.hhea_min_left_side_bearing;
    pp ppf "@,(min-right-side-bearing %d)" h.Otfm.hhea_min_right_side_bearing;
    pp ppf "@,(xmax-extent %d)" h.Otfm.hhea_xmax_extent;
    pp ppf "@,(caret-slope-rise %d)" h.Otfm.hhea_caret_slope_rise;
    pp ppf "@,(caret-slope-run %d)" h.Otfm.hhea_caret_slope_run;
    pp ppf "@,(caret-offset %d)" h.Otfm.hhea_caret_offset;
    pp ppf ")@]";
    Ok ()

let pp_hmtx ppf d =
  let pp_hm ppf () id adv lsb = pp ppf "@,(%d (adv %d) (lsb %d))" id adv lsb in
  pp ppf "@,@[<v1>(hmtx";
  match Otfm.hmtx d (pp_hm ppf) () with
  | Error _ as e -> e
  | Ok () -> pp ppf ")@]"; Ok ()

let pp_name ppf d =
  let pp_n ppf () id lang string = pp ppf "@,(%d %s \"%s\")" id lang string in
  pp ppf "@,@[<v1>(name";
  match Otfm.name d (pp_n ppf) () with
  | Error _ as e -> e
  | Ok () -> pp ppf ")@]"; Ok ()

let pp_os2 ppf d =
  let pp_opt pp_v ppf = function None -> pp ppf "None" | Some v -> pp_v ppf v in
  let pp_ouint32 ppf v = pp_opt (fun ppf v -> pp ppf "%lX" v) ppf v in
  let pp_oint = pp_opt Format.pp_print_int in
  match Otfm.os2 d with
  | Error _ as e -> e
  | Ok o ->
      pp ppf "@,@[<v1>(os2";
      pp ppf "@,(x-avg-char-width %d)" o.Otfm.os2_x_avg_char_width;
      pp ppf "@,(us-weight-class %d)" o.Otfm.os2_us_weight_class;
      pp ppf "@,(us-width-class %d)" o.Otfm.os2_us_width_class;
      pp ppf "@,(fs-type %X)" o.Otfm.os2_fs_type;
      pp ppf "@,(y-subscript-x-size %d)" o.Otfm.os2_y_subscript_x_size;
      pp ppf "@,(y-subscript-y-size %d)" o.Otfm.os2_y_subscript_y_size;
      pp ppf "@,(y-subscript-x-offset %d)" o.Otfm.os2_y_subscript_x_offset;
      pp ppf "@,(y-subscript-y-offset %d)" o.Otfm.os2_y_subscript_y_offset;
      pp ppf "@,(y-superscript-x-size %d)" o.Otfm.os2_y_superscript_x_size;
      pp ppf "@,(y-superscript-y-size %d)" o.Otfm.os2_y_superscript_y_size;
      pp ppf "@,(y-superscript-x-offset %d)" o.Otfm.os2_y_superscript_x_offset;
      pp ppf "@,(y-superscript-y-offset %d)" o.Otfm.os2_y_superscript_y_offset;
      pp ppf "@,(y-strikeout-size %d)" o.Otfm.os2_y_strikeout_size;
      pp ppf "@,(y-strikeout-position %d)" o.Otfm.os2_y_strikeout_position;
      pp ppf "@,(family-class %d)" o.Otfm.os2_family_class;
      pp ppf "@,(panose \"%s\")" (String.escaped o.Otfm.os2_panose);
      pp ppf "@,(ul-unicode-range1 %lX)" o.Otfm.os2_ul_unicode_range1;
      pp ppf "@,(ul-unicode-range2 %lX)" o.Otfm.os2_ul_unicode_range2;
      pp ppf "@,(ul-unicode-range3 %lX)" o.Otfm.os2_ul_unicode_range3;
      pp ppf "@,(ul-unicode-range4 %lX)" o.Otfm.os2_ul_unicode_range4;
      pp ppf "@,(ach-vend-id %a)"
        Otfm.Tag.pp (Otfm.Tag.of_int32 o.Otfm.os2_ach_vend_id);
      pp ppf "@,(fs-selection %X)" o.Otfm.os2_fs_selection;
      pp ppf "@,(us-first-char-index %d)" o.Otfm.os2_us_first_char_index;
      pp ppf "@,(us-last-char-index %d)" o.Otfm.os2_us_last_char_index;
      pp ppf "@,(s-typo-ascender %d)" o.Otfm.os2_s_typo_ascender;
      pp ppf "@,(s-type-descender %d)" o.Otfm.os2_s_type_descender;
      pp ppf "@,(s-typo-linegap %d)" o.Otfm.os2_s_typo_linegap;
      pp ppf "@,(us-win-ascent %d)" o.Otfm.os2_us_win_ascent;
      pp ppf "@,(us-win-descent %d)" o.Otfm.os2_us_win_descent;
      pp ppf "@,(ul-code-page-range-1 %a)"
        pp_ouint32 o.Otfm.os2_ul_code_page_range_1;
      pp ppf "@,(ul-code-page-range-2 %a)"
        pp_ouint32 o.Otfm.os2_ul_code_page_range_2;
      pp ppf "@,(s-x-height %a)" pp_oint o.Otfm.os2_s_x_height;
      pp ppf "@,(s-cap-height %a)" pp_oint o.Otfm.os2_s_cap_height;
      pp ppf "@,(us-default-char %a)" pp_oint o.Otfm.os2_us_default_char;
      pp ppf "@,(us-break-char %a)" pp_oint o.Otfm.os2_us_break_char;
      pp ppf "@,(us-max-context %a)" pp_oint o.Otfm.os2_us_max_context;
      pp ppf ")@]";
      Ok ()

let pp_kern ppf has_kern d =
  if not has_kern then Ok () else
  let dir = function `H -> "H" | `V -> "V" in
  let kind = function `Kern -> "kerning" | `Min -> "minimal" in
  let pp_kinfo ppf first i =
    if not first then pp ppf ")@]";
    pp ppf "@,@[<v1>((dir %s)@,(kind %s)@,(cross-stream %b)"
      (dir i.Otfm.kern_dir) (kind i.Otfm.kern_kind)
      (i.Otfm.kern_cross_stream);
    `Fold, false
  in
  let pp_pair ppf first l r v = pp ppf "@,(%d %d %d)" l r v; first in
  pp ppf "@,@[<v1>(kern";
  match Otfm.kern d (pp_kinfo ppf) (pp_pair ppf) true with
  | Error _ as e -> e
  | Ok _ -> pp ppf ")@])@]"; Ok ()

let pp_tables ppf inf ts d =
  let err = ref false in
  let log_if_error = function
  | Ok () -> ()
  | Error e -> log_err inf e; err := true
  in
  log_if_error @@ pp_name ppf d;
  log_if_error @@ pp_head ppf d;
  log_if_error @@ pp_hhea ppf d;
  log_if_error @@ pp_os2  ppf d;
  log_if_error @@ pp_cmap ppf d;
  log_if_error @@ pp_hmtx ppf d;
  log_if_error @@ pp_glyf ppf (List.mem Otfm.Tag.glyf ts) d;
  log_if_error @@ pp_kern ppf (List.mem Otfm.Tag.kern ts) d;
  if !err then (Error "") else Ok ()

(* Decode *)

let decode_pp ppf inf d =
  let err r =
    Result.map_error (fun e -> strf "%s: %a" inf Otfm.pp_error e) r
  in
  let* f = Otfm.flavour d |> err in
  pp ppf "@[<v1>(@[<1>(file %S)@]" inf;
  let fs = match f with `TTF -> "TTF" | `CFF -> "CFF" in
  pp ppf "@,@[<1>(flavor %s)@]" fs;
  let* name = Otfm.postscript_name d |> err in
  let oname = match name with None -> "<none>" | Some n -> n in
  pp ppf "@,@[<1>(postscript-name %s)@]" oname;
  let* glyph_count = Otfm.glyph_count d |> err in
  pp ppf "@,@[<1>(glyph-count %d)@]" glyph_count;
  let* ts = Otfm.table_list d |> err in
  pp ppf "@,@[<1>(tables ";
  List.iter (fun t -> pp ppf "@ %a" Otfm.Tag.pp t) ts;
  pp ppf ")@]";
  let* () = pp_tables ppf inf ts d in
  pp ppf ")@]@.";
  Ok ()

let decode_only file d =
  let err = ref false in
  let log_if_error = function
  | Ok _ -> () | Error e -> log_err file e; err := true
  in
  let kern_nop () _ = `Fold, () in
  let nop4 _ _ _ _ = () in
  log_if_error @@ Otfm.flavour d;
  log_if_error @@ Otfm.table_list d;
  log_if_error @@ Otfm.cmap d nop4 ();
  log_if_error @@ Otfm.head d;
  log_if_error @@ Otfm.hhea d;
  log_if_error @@ Otfm.hmtx d nop4 ();
  log_if_error @@ Otfm.name d nop4 ();
  log_if_error @@ Otfm.os2 d;
  log_if_error @@ Otfm.kern d kern_nop nop4 ();
  if !err then Error "" else Ok ()

(* Decode *)

let decode_font output_format only file d = match only with
| true -> decode_only file d
| false -> decode_pp Format.std_formatter file d

let decode keep_going output_format only files =
  process_files ~keep_going (decode_font output_format only) files

(* Sniff *)

let pp_flavour ppf f =
  Format.pp_print_string ppf (match f with `CFF -> "cff" | `TTF -> "ttf")

let sniff_font output_format show_path file d =
  file_err file @@
  let* flavour = Otfm.flavour d in
  let* glyph_count = Otfm.glyph_count d in
  let* name = Otfm.postscript_name d in
  let name = Option.value ~default:"<unknown>" name in
  let* tables = Otfm.table_list d in
  let tables = String.concat " " (List.map (strf "%a" Otfm.Tag.pp) tables) in
  let file = if show_path then strf "%s: " file else "" in
  Format.printf "@[<h>%s%s %a glyphs:%d %s@]@."
    file name pp_flavour flavour glyph_count tables;
  Ok ()

let sniff keep_going output_format show_path files =
  process_files ~keep_going (sniff_font output_format show_path) files

(* Command line interface *)

open Cmdliner

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
  let doc = "Do not stop if a file or a font in a collection fails decoding." in
  Arg.(value & flag & info ["k"; "keep-going"] ~doc)

let files =
  let doc = "Decode OpenType file $(docv). Use $(b,-) for stdin." in
  let docv = "FILE" in
  Arg.(non_empty & pos_all string [] & info [] ~doc ~docv)

let decode_cmd =
  let doc = "Output font information and all decodable tables" in
  let man =
    [ `S Manpage.s_description;
      `P "$(mname) $(tname) outputs font information and all decodable tables.";
      `S Manpage.s_options;
      `S s_output_format_options;
    ]
  in
  let only =
    let doc = "Decode only. Do not output data." in
    Arg.(value & flag & info ["d"; "only"] ~doc)
  in
  Cmd.v (Cmd.info "decode" ~man ~doc)
    Term.(const decode $ keep_going $ output_format $ only $ files)

let sniff_cmd =
  let doc = "Output basic OpenType information about fonts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(mname) $(tname)y outputs the PostScript name, flavour, glyph
          count and table names of one or more file's fonts.";
      `S Manpage.s_options;
      `S s_output_format_options;
    ]
  in
  let show_path =
    let doc = "Show font file path in output." in
    Arg.(value & flag & info ["p"; "show-path"] ~doc)
  in
  Cmd.v (Cmd.info "sniff" ~man ~doc)
    Term.(const sniff $ keep_going $ output_format $ show_path $ files)

let cmds = [sniff_cmd; decode_cmd]
let cmd =
  let doc = "OpenType font file decoder" in
  let man =
    [ `S Manpage.s_description;
      `P "$(mname) decodes OpenType files ($(b,.otf), $(b,.ttf), $(b,.otc), \
          $(b,.ttc)) and outputs their font data in various ways.";
      `S Manpage.s_bugs;
      `P "This program is distributed with the Otfm OCaml library. \
          See https:/erratique.ch/software/otfm for contact information"; ]
  in
  Cmd.group Cmd.(info "otftrip" ~man ~doc ~version:"%%VERSION%%") cmds


let main () = exit (Cmd.eval' cmd)
let () = if !Sys.interactive then () else main ()

(*
let main () =
  let usage = Printf.sprintf
    "Usage: %s [OPTION]... [OTFFILE]...\n\
     Print human readable OpenType file font information on stdout.\n\
    Options:" exec
  in
  let cmd = ref `Pp in
  let set_cmd v () = cmd := v in
  let files = ref [] in
  let add_file f = files := f :: !files in
  let options = [
    "-d", Arg.Unit (set_cmd `Dec), "Decode only";
    "-p", Arg.Unit (set_cmd `Ps), "Only output postscript name";
  ]
  in
  Arg.parse (Arg.align options) add_file usage;
  let files = match List.rev ! files with [] -> ["-"] | fs -> fs in
  let cmd = match !cmd with
  | `Pp -> pp_file Format.std_formatter
  | `Dec -> dec_file
  | `Ps -> ps_file
  in
  let fold_cmd cmd err fn = match cmd fn with
  | Error `Reported -> true
  | Error (`Msg e) -> log "%s" e; true
  | Error (#Otfm.error as e) -> log_err fn e; true
  | Ok () -> err
  in
  let err = List.fold_left (fold_cmd cmd) false files in
  if err then exit 1 else exit 0

let () = main ()
*)

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
