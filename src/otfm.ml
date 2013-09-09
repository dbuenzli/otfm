(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Invalid_arg strings *)

let err_invalid_tag s = Printf.sprintf "invalid OpenType tag (%S)" s

(* Unsafe string byte manipulations.

   If you don't believe the author's invariants, replacing with safe
   versions makes everything safe in the module. He won't be
   upset. *)

let unsafe_byte s j = Char.code (String.get s j)

(* Pretty printers *)

let pp = Format.fprintf 
let pp_string = Format.pp_print_string
let pp_space = Format.pp_print_space
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function 
| [] -> ()
| v :: vs -> 
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

(* OpenType tags *)

type tag = int32
module Tag = struct 
  type t = tag

  (* Table tags *)

  (* OpenType version tags. *)

  let v_wOFF = 0x774F4646l
  let v_OTTO = 0x4F54544Fl 
  let v_ttcf = 0x74746366l
  let v_true = 0x74727565l (* may happen in the wild. *)

  (* Required common tables tags *)

  let t_cmap = 0x636D6170l
  let t_head = 0x68656164l
  let t_hhea = 0x68686561l
  let t_hmtx = 0x686D7478l
  let t_maxp = 0x6D617870l
  let t_name = 0x6E616D65l
  let t_OS_2 = 0x4F532F32l
  let t_post = 0x706F7374l

  let t_common =
    [ t_cmap; t_head; t_hhea; t_hmtx; t_maxp; t_name; t_OS_2; t_post ]

  (* TTF font table tags *)

  let t_cvt  = 0x63767420l
  let t_fpgm = 0x6670676Dl
  let t_glyf = 0x676C7966l
  let t_loca = 0x6C6F6361l
  let t_prep = 0x70726570l

  (* CFF font table tags *) 

  let t_CFF  = 0x43464620l
  let t_VORG = 0x564F5247l

  (* Bitmap glyph tables *)

  let t_EBDT = 0x45424454l
  let t_EBLC = 0x45424C43l
  let t_EBSC = 0x45425343l

  (* Optional tables. *)

  let t_DSIG = 0x44534947l
  let t_gasp = 0x67617370l
  let t_hdmx = 0x68646D78l
  let t_kern = 0x6B65726El
  let t_LTSH = 0x4C545348l
  let t_PCLT = 0x50434C54l
  let t_VDMX = 0x56444D58l
  let t_vhea = 0x76686561l
  let t_vmtx = 0x766D7478l

  (* Advanced Open Type font layout tables *)

  let t_BASE = 0x42415345l
  let t_GDEF = 0x47444546l
  let t_GPOS = 0x47504F53l
  let t_GSUB = 0x47535542l
  let t_JSTF = 0x4A535446l

  (* Functions *)

  let of_bytes s = 
    if String.length s <> 4 then invalid_arg (err_invalid_tag s) else
    let s0 = Int32.of_int ((Char.code s.[0] lsl 8) lor (Char.code s.[1])) in 
    let s1 = Int32.of_int ((Char.code s.[2] lsl 8) lor (Char.code s.[3])) in 
    Int32.(logor (shift_left s0 16) s1)

  let to_bytes t = 
    let c0 = Char.chr (Int32.(to_int (shift_right t 24))) in 
    let c1 = Char.chr (Int32.(to_int (shift_right t 16)) land 0xFF) in 
    let c2 = Char.chr (Int32.(to_int (shift_right t 8)) land 0xFF) in 
    let c3 = Char.chr (Int32.(to_int t) land 0xFF) in
    Printf.sprintf "%c%c%c%c" c0 c1 c2 c3
      
  let to_int32 x = x
  let compare : int32 -> int32 -> int = Pervasives.compare
  let pp ppf t = pp ppf "'%s'" (to_bytes t)
end

(* Unicode code points *)

type cp = int 
type cp_range = cp * cp
let is_cp i = 0x0000 <= i && i <= 0x10FFFF
let is_scalar_value i = 
  (0x0000 <= i && i <= 0xD7FF) || (0xE000 <= i && i <= 0x10FFFF)
  
let pp_cp ppf cp =
  if cp < 0 || cp > 0x10FFFF then pp ppf "U+Invalid(%X)" cp else
  if cp <= 0xFFFF then pp ppf "U+%04X" cp else 
  pp ppf "U+%X" cp

(* Decode *)

type error_ctx = [ `Table of tag | `Offset_table | `Table_directory ]
type error = [ 
  | `Unknown_flavour of tag
  | `Unsupported_TTC
  | `Unsupported_cmaps of (int * int * int) list
  | `Missing_required_table of tag
  | `Unknown_version of error_ctx * int32
  | `Invalid_offset of error_ctx * int
  | `Invalid_cp of int
  | `Invalid_cp_range of int * int
  | `Unexpected_eoi of error_ctx ]

let pp_ctx ppf = function 
| `Table tag -> pp ppf "table %a" Tag.pp tag
| `Offset_table -> pp ppf "offset table"
| `Table_directory -> pp ppf "table directory"

let pp_error ppf = function 
| `Unknown_flavour tag -> 
    pp ppf "@[Unknown@ OpenType@ flavour (%a)@]" Tag.pp tag
| `Missing_required_table tag -> 
     pp ppf "@[Missing@ required@ table (%a)@]" Tag.pp tag
| `Unsupported_TTC ->
    pp ppf "@[True@ Type@ collections (TTC)@ are@ not@ supported@]"
| `Unsupported_cmaps maps -> 
    let pp_sep ppf () = pp ppf ",@ " in 
    let pp_map ppf (pid, eid, fmt) = pp ppf "(%d,%d,%d)" pid eid fmt in
    pp ppf "@[All@ cmaps:@ %a@ are@ unsupported@]" (pp_list ~pp_sep pp_map) maps
| `Unknown_version (ctx, v) -> 
    pp ppf "@[Unknown@ version (%lX)@ in@ %a@]" v pp_ctx ctx 
| `Invalid_offset (ctx, o) -> 
    pp ppf "@[Invalid@ offset (%d)@ in@ %a@]" o pp_ctx ctx
| `Invalid_cp u -> 
    pp ppf "@[Invalid@ Unicode@ code@ point@ (%a)@]" pp_cp u
| `Invalid_cp_range (u0, u1) -> 
    pp ppf "@[Invalid@ Unicode@ code@ point@ range (%a, %a)@]" pp_cp u0 pp_cp u1
| `Unexpected_eoi ctx -> 
    pp ppf "@[Unexpected@ end@ of@ input@ in %a@]" pp_ctx ctx;

(* N.B. Offsets and lengths are decoded as OCaml ints. On 64 bits
   platforms they fit, on 32 bits we are limited by string size
   anyway. *)

type flavour = [ `TTF | `CFF ]
type src = [ `String of string ] 
type decoder =
  { mutable i : string;                                       (* input data. *)
    mutable i_pos : int;                          (* input current position. *)
    mutable i_max : int;                          (* input maximal position. *)
    mutable t_pos : int;                  (* current decoded table position. *)
    mutable state : [ `Fatal of error | `Start | `Ready ]; (* decoder state. *)
    mutable ctx : error_ctx;                   (* the current error context. *)
    mutable flavour : [ `TTF | `CFF ];                   (* decoded flavour. *)
    mutable tables : (tag * int * int) list }      (* decoded table records. *)
  
let decoder_src d = (`String d.i)
let decoder src = 
  let i , i_pos, i_max = match src with
  | `String s -> s, 0, String.length s - 1 
  in 
  { i; i_pos; i_max; t_pos = 0; 
    state = `Start; ctx = `Offset_table; flavour = `TTF; tables = []; }
  
let ( >>= ) x f = match x with `Ok v -> f v | `Error _ as e -> e
let err e = `Error e
let err_eoi d = `Error (`Unexpected_eoi d.ctx)
let err_version d v = `Error (`Unknown_version (d.ctx, v))
let err_fatal d e = d.state <- `Fatal e; `Error e
let set_ctx d ctx = d.ctx <- ctx
let miss d count = d.i_max - d.i_pos + 1 < count
let raw_byte d =
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1; (unsafe_byte d.i j) 
  
let cur_pos d = d.i_pos
let seek_pos pos d =
  if pos > d.i_max then err (`Invalid_offset (d.ctx, pos)) else 
  (d.i_pos <- pos; `Ok ())
  
let seek_table_pos pos d = seek_pos (d.t_pos + pos) d 
let seek_table tag d () = 
  try
    let _, pos, len = List.find (fun (t, _, _) -> tag = t) d.tables in
    if pos > d.i_max then err (`Invalid_offset (`Table tag, pos)) else
    (set_ctx d (`Table tag); d.t_pos <- pos; d.i_pos <- pos; `Ok (Some len))
  with Not_found -> `Ok None
  
let seek_required_table tag d () = match seek_table tag d () with 
| `Ok (Some _) -> `Ok ()
| `Ok None -> err (`Missing_required_table tag)
| `Error _ as e -> e

let d_skip n d = 
  if miss d n then err_eoi d else 
  (d.i_pos <- d.i_pos + n; `Ok ())
  
let d_uint8 d = if miss d 1 then err_eoi d else `Ok (raw_byte d)
let d_int8 d = match d_uint8 d with
| `Ok i -> `Ok (if i > 0x7F then i - 0x100 else i)
| `Error _ as e -> e
  
let d_uint16 d =
  if miss d 2 then err_eoi d else
  let b0 = raw_byte d in
  let b1 = raw_byte d in 
  `Ok ((b0 lsl 8) lor b1)

let d_int16 d = match d_uint16 d with
| `Ok i -> `Ok (if i > 0x7FFF then i - 0x10000 else i) 
| `Error _ as e -> e
  
let d_uint24 d = 
  if miss d 3 then err_eoi d else
  let b0 = raw_byte d in 
  let b1 = raw_byte d in 
  let b2 = raw_byte d in 
  `Ok ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let d_uint32 d = 
  if miss d 4 then err_eoi d else
  let b0 = raw_byte d in let b1 = raw_byte d in 
  let b2 = raw_byte d in let b3 = raw_byte d in
  let s0 = Int32.of_int ((b0 lsl 8) lor b1) in 
  let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
  `Ok (Int32.logor (Int32.shift_left s0 16) s1)

let d_uint32_int d =
  if miss d 4 then err_eoi d else
  let b0 = raw_byte d in let b1 = raw_byte d in 
  let b2 = raw_byte d in let b3 = raw_byte d in 
  let s0 = (b0 lsl 8) lor b1 in 
  let s1 = (b2 lsl 8) lor b3 in 
  `Ok ((s0 lsl 16) lor s1)

let d_time d =                       (* LONGDATETIME as a unix time stamp. *)
  if miss d 8 then err_eoi d else 
  let b0 = raw_byte d in let b1 = raw_byte d in 
  let b2 = raw_byte d in let b3 = raw_byte d in 
  let b4 = raw_byte d in let b5 = raw_byte d in 
  let b6 = raw_byte d in let b7 = raw_byte d in 
  let s0 = Int64.of_int ((b0 lsl 8) lor b1) in 
  let s1 = Int64.of_int ((b2 lsl 8) lor b3) in
  let s2 = Int64.of_int ((b4 lsl 8) lor b5) in 
  let s3 = Int64.of_int ((b6 lsl 8) lor b7) in
  let v = (Int64.logor (Int64.shift_left s0 48) 
             (Int64.logor (Int64.shift_left s1 32)
                (Int64.logor (Int64.shift_left s2 16) s3)))
  in
  let unix_epoch = 2_082_844_800L (* in seconds since 1904-01-01 00:00:00 *) in
  `Ok (Int64.to_float (Int64.sub v unix_epoch))

let d_fixed d = 
  if miss d 4 then err_eoi d else
  let b0 = raw_byte d in let b1 = raw_byte d in
  let b2 = raw_byte d in let b3 = raw_byte d in 
  let s0 = Int32.of_int ((b0 lsl 8) lor b1) in 
  let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
  `Ok (s0, s1)

let rec d_table_records d count = 
  if count = 0 then (d.state <- `Ready; `Ok ()) else
  d_uint32     d >>= fun tag ->
  d_skip 4     d >>= fun () ->
  d_uint32_int d >>= fun off -> 
  d_uint32_int d >>= fun len -> 
  d.tables <- (tag, off, len) :: d.tables;
  d_table_records d (count - 1)

let d_version d = 
  d_uint32 d >>= function
  | t when t = Tag.v_OTTO -> d.flavour <- `CFF; `Ok ()
  | t when (t = Tag.v_true || t = 0x00010000l) -> d.flavour <- `TTF; `Ok ()
  | t when t = Tag.v_ttcf -> `Error `Unsupported_TTC
  | t -> `Error (`Unknown_flavour t)

let d_structure d =                   (* offset table and table directory. *)
  d_version      d >>= fun () ->                          (* offset table. *)
  d_uint16       d >>= fun count ->                          (* numTables. *)
  d_skip (3 * 2) d >>= fun () ->
  set_ctx d `Table_directory;                          (* table directory. *)
  d_table_records d count   

let init_decoder d = match d.state with
| `Ready -> `Ok ()
| `Fatal e -> `Error e 
| `Start -> 
    match d_structure d with
    | `Ok () as ok -> ok 
    | `Error e -> err_fatal d e

let flavour d = init_decoder d >>= fun () -> `Ok d.flavour

let table_list d = 
  let tags d = List.rev_map (fun (t, _, _) -> t) d.tables in
  init_decoder d >>= fun () -> `Ok (tags d)

let table_mem d tag = 
  let exists_tag tag d = List.exists (fun (t, _, _) -> tag = t) d.tables in
  init_decoder d >>= fun () -> `Ok (exists_tag tag d)

let table_raw d tag = 
  init_decoder   d >>= 
  seek_table tag d >>= fun len -> 
  match len with 
  | None -> `Ok None 
  | Some len -> 
      if d.i_pos + len > d.i_max then err_eoi d else
      `Ok (Some (String.sub d.i d.i_pos len))

(* cmap table *)

type glyph_id = int
type map_kind = [ `Glyph | `Glyph_range ]

let rec d_array el count i a d = 
  if i = count then `Ok a else
  el d >>= fun v -> a.(i) <- v; d_array el count (i + 1) a d

let d_cmap_4_ranges cmap d f acc u0s u1s delta offset count =       (* ugly. *)
  let garray_pos = cur_pos d in
  let rec loop acc i = 
    if i = count then `Ok (cmap, acc) else
    let i' = i + 1 in
    let offset = offset.(i) in
    let delta = delta.(i) in 
    let u0 = u0s.(i) in if not (is_cp u0) then err (`Invalid_cp u0) else
    let u1 = u1s.(i) in if not (is_cp u1) then err (`Invalid_cp u1) else 
    if u0 > u1 then err (`Invalid_cp_range (u0, u1)) else
    if offset = 0 then begin
      (* The arithmetic must be performed mod 65536, this is problematic
         for Otfm's interface semantics. We need to split the range 
         if the the glyph range spans the bounds. *)
      let g0 = u0 + delta in 
      let g1 = u1 + delta in 
      if g0 < 0 && g1 >= 0 then  
        let acc' = f acc `Glyph_range (u0, - delta - 1) (g0 land 65535) in
        loop (f acc' `Glyph_range (- delta, u1) 0) i'
      else
      if g0 <= 65535 && g1 > 65535 then 
        let acc' = f acc `Glyph_range (u0, 65535 - delta) g0 in 
        loop (f acc' `Glyph_range (65536 - delta, u1) 0) i'
      else (* glyph range is inside [0;65535] or completly outside *)
      loop (f acc `Glyph_range (u0, u1) (g0 land 65535)) i'
    end else begin
      let rec garray acc u u1 () = 
        if u > u1 then `Ok acc else
        d_uint16 d >>= fun gindex ->
        let g = (gindex + delta) land 65535 in
        garray (f acc `Glyph (u, u) g) (u + 1) u1 ()
      in
      let pos = garray_pos - (count - i) * 2 + offset in
      seek_pos pos d >>= 
      garray acc u0 u1 >>= fun acc -> 
      loop acc i'
    end
  in
  loop acc 0

let d_cmap_4 cmap d f acc () =
  d_skip (3 * 2) d >>= fun () ->
  d_uint16       d >>= fun count2 -> 
  let count = count2 / 2 in
  let a () = Array.make count 0 in
  d_skip (3 * 2)                  d >>= fun () ->
  d_array d_uint16 count 0 (a ()) d >>= fun u1s ->
  d_skip 2                        d >>= fun () -> (* pad *) 
  d_array d_uint16 count 0 (a ()) d >>= fun u0s ->
  d_array d_int16  count 0 (a ()) d >>= fun delta -> 
  d_array d_uint16 count 0 (a ()) d >>= fun offset ->
  d_cmap_4_ranges cmap d f acc u0s u1s delta offset count

let rec d_cmap_groups cmap d count f kind acc =
  if count = 0 then `Ok (cmap, acc) else
  d_uint32_int d >>= fun u0 -> if not (is_cp u0) then err (`Invalid_cp u0) else 
  d_uint32_int d >>= fun u1 -> if not (is_cp u1) then err (`Invalid_cp u1) else 
  if u0 > u1 then err (`Invalid_cp_range (u0, u1)) else
  d_uint32_int d >>= fun gid -> 
  d_cmap_groups cmap d (count - 1) f kind (f acc kind (u0, u1) gid)

let d_cmap_seg cmap kind d f acc () = 
  d_skip (2 * 2 + 2 * 4) d >>= fun () ->
  d_uint32_int           d >>= fun count ->
  d_cmap_groups cmap d count f kind acc

let d_cmap_12 cmap d f acc () = d_cmap_seg cmap `Glyph_range d f acc ()
let d_cmap_13 cmap d f acc () = d_cmap_seg cmap `Glyph d f acc ()

let rec d_cmap_records d count acc = 
  if count = 0 then `Ok acc else
  d_uint16           d >>= fun pid ->
  d_uint16           d >>= fun eid -> 
  d_uint32_int       d >>= fun pos -> 
  let cur = cur_pos d in
  seek_table_pos pos d >>= fun () ->
  d_uint16           d >>= fun fmt -> 
  seek_pos cur       d >>= fun () ->
  d_cmap_records d (count - 1) ((pos, pid, eid, fmt) :: acc)

let select_cmap cmaps =
  let rec loop f sel = function 
  | (_, _, _, (4 | 12 | 13 as f') as c) :: cs when f' > f -> loop f (Some c) cs
  | [] -> sel
  | _ :: cs -> loop f sel cs
  in
  loop min_int None cmaps

let cmap d f acc = 
  init_decoder d >>= 
  seek_required_table Tag.t_cmap d >>= fun () ->
  d_uint16 d >>= fun version ->                           (* cmap header. *)
  if version <> 0 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun count ->                               (* numTables. *)
  d_cmap_records d count [] >>= fun cmaps -> 
  match select_cmap cmaps with 
  | None -> 
      let drop_pos (_, pid, eid, fmt) = (pid, eid, fmt) in
      err (`Unsupported_cmaps (List.map drop_pos cmaps))
  | Some (pos, pid, eid, fmt) -> 
      let cmap = match fmt with 
      | 4 -> d_cmap_4 | 12 -> d_cmap_12 | 13 -> d_cmap_13 | _ -> assert false
      in
      seek_table_pos pos d >>= cmap (pid, eid, fmt) d f acc

(* head table *) 

type head = 
  { head_font_revision : int32;
    head_flags : int;
    head_units_per_em : int; 
    head_created : float; 
    head_modified : float; 
    head_xmin : int; 
    head_ymin : int; 
    head_xmax : int; 
    head_ymax : int; 
    head_mac_style : int; 
    head_lowest_rec_ppem : int;
    head_index_to_loc_format : int; }
    
let head d = 
  init_decoder d >>= 
  seek_required_table Tag.t_head d >>= fun () -> 
  d_uint32 d >>= fun version -> 
  if version <> 0x00010000l then err_version d version else 
  d_uint32 d >>= fun head_font_revision -> 
  d_skip 8 d >>= fun () -> (* checkSumAdjustement, magicNumber *)
  d_uint16 d >>= fun head_flags -> 
  d_uint16 d >>= fun head_units_per_em ->
  d_time   d >>= fun head_created -> 
  d_time   d >>= fun head_modified -> 
  d_int16  d >>= fun head_xmin -> 
  d_int16  d >>= fun head_ymin -> 
  d_int16  d >>= fun head_xmax -> 
  d_int16  d >>= fun head_ymax -> 
  d_uint16 d >>= fun head_mac_style ->
  d_uint16 d >>= fun head_lowest_rec_ppem -> 
  d_skip 2 d >>= fun () -> (* fontDirectionHint *) 
  d_uint16 d >>= fun head_index_to_loc_format ->
  `Ok { head_font_revision; head_flags; head_units_per_em; head_created; 
        head_modified; head_xmin; head_ymin; head_xmax; head_ymax; 
        head_mac_style; head_lowest_rec_ppem; head_index_to_loc_format }
  
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
