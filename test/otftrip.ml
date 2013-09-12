(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf 
let str = Format.sprintf

let exec = Filename.basename Sys.executable_name 
let log msg = Format.eprintf ("%s: " ^^ msg ^^ "@.") exec
let log_err inf e = 
  Format.eprintf "@[<2>%s:%s:@ %a@]@." exec inf Otfm.pp_error e

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in 
    let s = String.create buf_size in 
    try
      while true do
        let c = input ic s 0 buf_size in 
        if c = 0 then raise Exit else
        Buffer.add_substring b s 0 c
      done;
      assert false
    with
    | Exit -> close ic; `Ok (Buffer.contents b)
    | Failure _ -> close ic; `Error (`Msg (str "%s: input file too large" inf))
    | Sys_error e -> close ic; (`Error (`Msg e));
  with
  | Sys_error e -> (`Error (`Msg e))

(* Table pretty printers *)     
               
let pp_cmap ppf d =
  let pp_map ppf u gid = pp ppf "@,(%a %d)" Otfm.pp_cp u gid in
  let pp_binding ppf () k (u0, u1) gid = match k with 
  | `Glyph -> for u = u0 to u1 do pp_map ppf u gid done
  | `Glyph_range -> for i = 0 to (u1 - u0) do pp_map ppf (u0 + i) (gid + i)done 
  in
  pp ppf "@,@[<v1>(cmap";
  match Otfm.cmap d (pp_binding ppf) () with
  | `Error _ as e -> e
  | `Ok ((pid, eid, fmt), _) ->
      pp ppf "@,@[<1>(source@ (platform-id %d)@ (encoding-id %d)\
              @ (format %d))@])@]" pid eid fmt; 
      `Ok ()
        
let pp_head ppf d = 
  pp ppf "@,@[<v1>(head"; 
  match Otfm.head d with 
  | `Error _ as e -> e
  | `Ok h ->
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
      `Ok ()

let pp_hhea ppf d = match Otfm.hhea d with 
| `Error _ as e -> e
| `Ok h ->
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
    `Ok ()

let pp_hmtx ppf d = 
  let pp_hm ppf () id adv lsb = pp ppf "@,(%d (adv %d) (lsb %d))" id adv lsb in
  pp ppf "@,@[<v1>(hmtx";
  match Otfm.hmtx d (pp_hm ppf) () with 
  | `Error _ as e -> e
  | `Ok () -> pp ppf ")@]"; `Ok ()
  
let pp_name ppf d = 
  let pp_n ppf () id lang string = pp ppf "@,(%d %s \"%s\")" id lang string in
  pp ppf "@,@[<v1>(name"; 
  match Otfm.name d (pp_n ppf) () with 
  | `Error _ as e -> e
  | `Ok () -> pp ppf ")@]"; `Ok ()

let pp_os2 ppf d = 
  let pp_opt pp_v ppf = function None -> pp ppf "None" | Some v -> pp_v ppf v in
  let pp_ouint32 ppf v = pp_opt (fun ppf v -> pp ppf "%lX" v) ppf v in
  let pp_oint = pp_opt Format.pp_print_int in
  match Otfm.os2 d with
  | `Error _ as e -> e
  | `Ok o ->
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
      `Ok ()

let pp_kern ppf has_kern d =
  if not has_kern then `Ok () else
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
  | `Error _ as e -> e
  | `Ok _ -> pp ppf ")@]"; `Ok ()
  
let pp_tables ppf inf ts d =
  let err = ref false in
  let ( >>= ) x f = match x with 
  | `Ok () -> f ()
  | `Error e -> log_err inf e; err := true; f ()
  in
  pp_name ppf d >>= fun () ->
  pp_head ppf d >>= fun () ->
  pp_hhea ppf d >>= fun () ->
  pp_os2  ppf d >>= fun () ->
  pp_cmap ppf d >>= fun () ->
  pp_hmtx ppf d >>= fun () ->
  pp_kern ppf (List.mem Otfm.Tag.t_kern ts) d >>= fun () ->
  if !err then `Ok () else (`Error `Reported)
                           
(* Commands *) 
                           
let pp_file ppf inf = match string_of_file inf with
| `Error _ as e -> e
| `Ok s ->
    let ( >>= ) x f = match x with 
    | `Ok v -> f v 
    | `Error e -> `Error (e :> [ Otfm.error | `Reported | `Msg of string])
    in
    let d = Otfm.decoder (`String s) in
    Otfm.flavour d >>= fun f -> 
    pp ppf "@[<v1>(@[<1>(file %S)@]" inf;
    let fs = match f with `TTF -> "TTF" | `CFF -> "CFF" in
    pp ppf "@,@[<1>(flavor %s)@]" fs;
    Otfm.postscript_name d >>= fun name -> 
    let oname = match name with None -> "<none>" | Some n -> n in
    pp ppf "@,@[<1>(postscript-name %s)@]" oname; 
    Otfm.glyph_count d >>= fun glyph_count -> 
    pp ppf "@,@[<1>(glyph-count %d)@]" glyph_count;
    Otfm.table_list d >>= fun ts -> 
    pp ppf "@,@[<1>(tables ";
    List.iter (fun t -> pp ppf "@ %a" Otfm.Tag.pp t) ts; 
    pp ppf ")@]";
    pp_tables ppf inf ts d >>= fun () -> 
    pp ppf ")@]@."; 
    `Ok ()
      
let dec_file inf = match string_of_file inf with 
| `Error _ as e -> e
| `Ok s -> 
    let err = ref false in
    let ( >>= ) x f = match x with 
    | `Ok _ -> f () 
    | `Error e -> log_err inf e; err := true; f ()
    in 
    let kern_nop () _ = `Fold, () in
    let nop4 _ _ _ _ = () in
    let d = Otfm.decoder (`String s) in
    Otfm.flavour d      >>= fun () -> 
    Otfm.table_list d   >>= fun () ->
    Otfm.cmap d nop4 () >>= fun () -> 
    Otfm.head d         >>= fun () -> 
    Otfm.hhea d         >>= fun () -> 
    Otfm.hmtx d nop4 () >>= fun () -> 
    Otfm.name d nop4 () >>= fun () ->
    Otfm.os2  d         >>= fun () -> 
    Otfm.kern d kern_nop nop4 () >>= fun () ->
    if !err then (`Error `Reported) else `Ok ()
        
let ps_file inf = match string_of_file inf with
| `Error _ as e -> e
| `Ok s -> 
    let d = Otfm.decoder (`String s) in 
    match Otfm.postscript_name d with 
    | `Error e -> `Error (e :> [ Otfm.error | `Reported | `Msg of string])
    | `Ok None -> Printf.printf "%s: <none>\n" inf; `Ok ()
    | `Ok (Some n) -> Printf.printf "%s: %s\n" inf n; `Ok ()

(* otftrip *)

let main () = 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... [OTFFILE]...\n\
    \ Print human readable OpenType file font information on stdout.\n\
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
  | `Error `Reported -> true 
  | `Error (`Msg e) -> log "%s" e; true 
  | `Error (#Otfm.error as e) -> log_err fn e; true
  | `Ok () -> err
  in
  let err = List.fold_left (fold_cmd cmd) false files in 
  if err then exit 1 else exit 0 

let () = main ()

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
