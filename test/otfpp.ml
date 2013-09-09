(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf 
let exec = Filename.basename Sys.executable_name 
let err = ref false
let log msg = err := true; Format.eprintf ("%s: " ^^ msg ^^ "@.") exec
let log_err inf e =
  err := true; Format.eprintf "@[<2>%s:%s:@ %a@]@." exec inf Otfm.pp_error e
  
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
    | Exit -> close ic; Some (Buffer.contents b)
    | Failure _ -> close ic; log "%s: input file too large" inf; None
    | Sys_error e -> close ic; log "%s" e; None
  with
  | Sys_error e -> log "%s" e; None

let pp_cmap ppf inf d =
  let pp_map ppf u gid = pp ppf "@,(%a %d)" Otfm.pp_cp u gid in
  let pp_binding ppf () k (u0, u1) gid = match k with 
  | `Glyph -> for u = u0 to u1 do pp_map ppf u gid done
  | `Glyph_range -> for i = 0 to (u1 - u0) do pp_map ppf (u0 + i) (gid + i)done 
  in
  pp ppf "@,@[<v1>(cmap@,@[<v1>(";
  match Otfm.cmap d (pp_binding ppf) () with
  | `Error e -> log_err inf e
  | `Ok ((pid, eid, fmt), _) -> 
      pp ppf ")@]"; 
      pp ppf "@,@[<1>(source@ (platform-id %d)@ (encoding-id %d)\
              @ (format %d))@])@]" pid eid fmt

let pp_head ppf inf d = 
  pp ppf "@,@[<v1>(head"; 
  match Otfm.head d with 
  | `Error e -> log_err inf e
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
      pp ppf ")@]"

let pp_hhea ppf inf d = 
  pp ppf "@,@[<v1>(hhea"; 
  match Otfm.hhea d with 
  | `Error e -> log_err inf e
  | `Ok h ->
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
      pp ppf ")@]"

let pp_hmtx ppf inf d = 
  let pp_hm ppf () id adv lsb = pp ppf "@,(%d (adv %d) (lsb %d))" id adv lsb in
  pp ppf "@,@[<v1>(hmtx";
  match Otfm.hmtx d (pp_hm ppf) () with 
  | `Error e -> log_err inf e 
  | `Ok () -> pp ppf ")@]"
  
let pp_tables ppf inf d =
  pp_cmap ppf inf d; 
  pp_head ppf inf d;
  pp_hhea ppf inf d;
  pp_hmtx ppf inf d
 
let pp_file ppf inf = match string_of_file inf with
| None -> () 
| Some s -> 
    let d = Otfm.decoder (`String s) in 
    match Otfm.flavour d with 
    | `Error e -> log_err inf e 
    | `Ok f -> 
        pp ppf "@[<v1>(@[<1>(file %S)@]" inf;
        let fs = match f with `TTF -> "TTF" | `CFF -> "CFF" in
        pp ppf "@,@[<1>(flavor %s)@]" fs;
        match Otfm.table_list d with 
        | `Error e -> log_err inf e
        | `Ok ts ->
            pp ppf "@,@[<1>(tables ";
            List.iter (fun t -> pp ppf "@ %a" Otfm.Tag.pp t) ts; 
            pp ppf ")@]";
            pp_tables ppf inf d;
            pp ppf ")@]@."

let pp_files files = match files with
| [] -> pp_file Format.std_formatter "-" 
| fs -> List.iter (pp_file Format.std_formatter) fs

let main () = 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... [OTFFILE]...\n\
    \ Pretty-print OpenType file font information on stdout.\n\
    Options:" exec 
  in
  let cmd = ref `Pp in 
(*  let set_cmd v () = cmd := v in  *)
  let files = ref [] in 
  let add_file f = files := f :: !files in
  let options = [
(*    "-ocaml", Arg.Unit (set_cmd `OCaml), " Outputs an OCaml module"; *)
  ]
  in
  Arg.parse (Arg.align options) add_file usage; 
  begin match !cmd with 
  | `Pp -> pp_files (List.rev !files)
(*  | `OCaml -> failwith "TODO" *)
  end; 
  if !err then exit 1 else exit 0 

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
