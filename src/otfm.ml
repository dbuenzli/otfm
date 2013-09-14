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
    
let unsafe_chr = Char.unsafe_chr
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

  let cmap = 0x636D6170l
  let head = 0x68656164l
  let hhea = 0x68686561l
  let hmtx = 0x686D7478l
  let maxp = 0x6D617870l
  let name = 0x6E616D65l
  let os2  = 0x4F532F32l
  let post = 0x706F7374l

  let t_common = [ cmap; head; hhea; hmtx; maxp; name; os2; post ]

  (* TTF font table tags *)

  let cvt  = 0x63767420l
  let fpgm = 0x6670676Dl
  let glyf = 0x676C7966l
  let loca = 0x6C6F6361l
  let prep = 0x70726570l

  (* CFF font table tags *) 

  let cff  = 0x43464620l
  let vorg = 0x564F5247l

  (* Bitmap glyph tables *)

  let ebdt = 0x45424454l
  let eblc = 0x45424C43l
  let ebsc = 0x45425343l

  (* Optional tables. *)

  let dsig = 0x44534947l
  let gasp = 0x67617370l
  let hdmx = 0x68646D78l
  let kern = 0x6B65726El
  let ltsh = 0x4C545348l
  let pclt = 0x50434C54l
  let vdmx = 0x56444D58l
  let vhea = 0x76686561l
  let vmtx = 0x766D7478l
    
  (* Advanced Open Type font layout tables *)
    
  let base = 0x42415345l
  let gdef = 0x47444546l
  let gpos = 0x47504F53l
  let gsub = 0x47535542l
  let jstf = 0x4A535446l

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
  let of_int32 x = x
  let compare : int32 -> int32 -> int = Pervasives.compare
  let pp ppf t = pp ppf "'%s'" (to_bytes t)
end

(* Unicode code points *)

type cp = int 
type cp_range = cp * cp
let is_cp i = 0x0000 <= i && i <= 0x10FFFF
                                      
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
  | `Invalid_postscript_name of string
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
    pp ppf "@[Invalid@ Unicode@ code@ point@ (%a)@]" Uutf.pp_cp u
| `Invalid_cp_range (u0, u1) -> 
    pp ppf "@[Invalid@ Unicode@ code@ point@ range (%a, %a)@]" 
      Uutf.pp_cp u0 Uutf.pp_cp u1
| `Invalid_postscript_name n -> 
    pp ppf "@[Invalid@ PostScript@ name (%s)@]" (String.escaped n)
| `Unexpected_eoi ctx -> 
    pp ppf "@[Unexpected@ end@ of@ input@ in %a@]" pp_ctx ctx
    
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
    mutable tables : (tag * int * int) list;       (* decoded table records. *)
    mutable buf : Buffer.t; }                            (* internal buffer. *) 
  
let decoder_src d = (`String d.i)
let decoder src = 
  let i , i_pos, i_max = match src with
  | `String s -> s, 0, String.length s - 1 
  in 
  { i; i_pos; i_max; t_pos = 0; 
    state = `Start; ctx = `Offset_table; flavour = `TTF; tables = []; 
    buf = Buffer.create 253; }
  
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
  
let d_bytes len d = 
  if miss d len then err_eoi d else 
  begin 
    Buffer.clear d.buf; 
    for i = 1 to len do Buffer.add_char d.buf (unsafe_chr (raw_byte d)) done; 
    `Ok (Buffer.contents d.buf)
  end
  
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
    
let u_rep = 0xFFFD                                 (* replacement character. *)
let d_utf_16be len (* in bytes *) d =            (* returns an UTF-8 string. *) 
  match d_bytes len d with
  | `Error _ as e ->  e
  | `Ok s -> 
      let rec add_utf_8 b i = function
      | `Malformed _ -> add_utf_8 b i (`Uchar Uutf.u_rep) 
      | `Uchar u -> Uutf.Buffer.add_utf_8 b u; b
      in
      Buffer.clear d.buf;
      `Ok (Buffer.contents (Uutf.String.fold_utf_16be add_utf_8 d.buf s))
    
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
        
(* convenience *)
        
let glyph_count d = 
  init_decoder d >>=
  seek_required_table Tag.maxp d >>= fun () -> 
  d_skip 4 d >>= fun () -> 
  d_uint16 d >>= fun count -> 
  `Ok count
    
let postscript_name d = (* rigorous postscript name lookup, see OT spec p. 39 *)
  init_decoder d >>=
  seek_required_table Tag.name d >>= fun () -> 
  d_uint16 d >>= fun version -> 
  if version < 0 || version > 1 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun ncount ->
  d_uint16 d >>= fun soff -> 
  let rec loop ncount () = 
    if ncount = 0 then `Ok None else
    let ncount' = ncount - 1 in
    let look_for the_eid the_lid decode = 
      d_uint16 d >>= fun eid -> 
      if eid <> the_eid then d_skip (4 * 2) d >>= loop ncount' else 
      d_uint16 d >>= fun lid -> 
      if lid <> the_lid then d_skip (3 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun nid -> 
      if nid <> 6 then d_skip (2 * 2) d >>= loop ncount' else 
      d_uint16 d >>= fun len ->
      d_uint16 d >>= fun off ->
      seek_table_pos (soff + off) d >>= fun () ->
      decode len d >>= fun name ->
      let invalid name = `Error (`Invalid_postscript_name name) in
      let name_len = String.length name in 
      if name_len > 63 then invalid name else           
      try 
        for i = 0 to name_len - 1 do match Char.code name.[i] with
        | d when d < 33 || d > 126 -> raise Exit
        | 91 | 93 | 40 | 41 | 123 | 125 | 60 | 62 | 47 | 37 -> raise Exit
        | _ -> ()
        done;
        `Ok (Some name)
      with Exit -> invalid name
    in
    d_uint16 d >>= function
    | 3 -> look_for 1 0x409 d_utf_16be
    | 1 -> look_for 0 0 d_bytes
    | _ -> d_skip (5 * 2) d >>= loop (ncount - 1)  
  in
  loop ncount ()
    
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
  seek_required_table Tag.cmap d >>= fun () ->
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
  seek_required_table Tag.head d >>= fun () -> 
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
  
(* hhea table *) 

type hhea = 
  { hhea_ascender : int; 
    hhea_descender : int; 
    hhea_line_gap : int; 
    hhea_advance_width_max : int; 
    hhea_min_left_side_bearing : int; 
    hhea_min_right_side_bearing : int; 
    hhea_xmax_extent : int;
    hhea_caret_slope_rise : int; 
    hhea_caret_slope_run : int; 
    hhea_caret_offset : int; }

let hhea d = 
  init_decoder d >>= 
  seek_required_table Tag.hhea d >>= fun () -> 
  d_uint32 d >>= fun version -> 
  if version <> 0x00010000l then err_version d version else 
  d_int16  d >>= fun hhea_ascender -> 
  d_int16  d >>= fun hhea_descender -> 
  d_int16  d >>= fun hhea_line_gap -> 
  d_uint16 d >>= fun hhea_advance_width_max -> 
  d_int16  d >>= fun hhea_min_left_side_bearing -> 
  d_int16  d >>= fun hhea_min_right_side_bearing -> 
  d_int16  d >>= fun hhea_xmax_extent ->
  d_int16  d >>= fun hhea_caret_slope_rise -> 
  d_int16  d >>= fun hhea_caret_slope_run -> 
  d_int16  d >>= fun hhea_caret_offset -> 
  `Ok { hhea_ascender; hhea_descender; hhea_line_gap; hhea_advance_width_max; 
        hhea_min_left_side_bearing; hhea_min_right_side_bearing; 
        hhea_xmax_extent; hhea_caret_slope_rise; hhea_caret_slope_run; 
        hhea_caret_offset; }
    
(* hmtx table *) 
    
let d_hm_count d = 
  seek_required_table Tag.hhea d () >>= fun () -> 
  d_skip (4 + 15 * 2) d >>= fun () -> 
  d_uint16            d >>= fun hm_count -> 
  `Ok hm_count
    
let rec d_hmetric goffset i f acc last_adv d = 
  if i = 0 then `Ok (acc, last_adv) else
  d_uint16 d >>= fun adv ->
  d_int16  d >>= fun lsb ->
  let acc' = f acc (goffset - i) adv lsb in
  d_hmetric goffset (i - 1) f acc' adv d
    
let rec d_hlsb goffset i f acc adv d = 
  if i = 0 then `Ok acc else 
  d_int16 d >>= fun lsb -> 
  let acc' = f acc (goffset - i) adv lsb in
  d_hlsb goffset (i - 1) f acc' adv d
    
let hmtx d f acc =
  glyph_count d >>= fun glyph_count ->
  d_hm_count  d >>= fun hm_count ->
  seek_required_table Tag.hmtx d () >>= fun () ->
  d_hmetric hm_count hm_count f acc (-1) d >>= fun (acc, last_adv) -> 
  d_hlsb glyph_count (glyph_count - hm_count) f acc last_adv d
    
(* name table *) 
    
(* Source: https://skia.googlecode.com/svn/trunk/src/sfnt/SkOTTable_name.cpp 
   BSD3 licensed (c) 2011 Google Inc. *)
let lcid_to_bcp47 = [
  0x0401, "ar-sa";  0x0402, "bg-bg";  0x0403, "ca-es";  0x0404, "zh-tw"; 
  0x0405, "cs-cz";  0x0406, "da-dk";  0x0407, "de-de";  0x0408, "el-gr"; 
  0x0409, "en-us";  0x040a, "es-es_tradnl";             0x040b, "fi-fi"; 
  0x040c, "fr-fr";  0x040d, "he-il";  0x040d, "he";     0x040e, "hu-hu"; 
  0x040e, "hu";     0x040f, "is-is";  0x0410, "it-it";  0x0411, "ja-jp"; 
  0x0412, "ko-kr";  0x0413, "nl-nl";  0x0414, "nb-no";  0x0415, "pl-pl"; 
  0x0416, "pt-br";  0x0417, "rm-ch";  0x0418, "ro-ro";  0x0419, "ru-ru"; 
  0x041a, "hr-hr";  0x041b, "sk-sk";  0x041c, "sq-al";  0x041d, "sv-se"; 
  0x041e, "th-th";  0x041f, "tr-tr";  0x0420, "ur-pk";  0x0421, "id-id"; 
  0x0422, "uk-ua";  0x0423, "be-by";  0x0424, "sl-si";  0x0425, "et-ee"; 
  0x0426, "lv-lv";  0x0427, "lt-lt";  0x0428, "tg-cyrl-tj"; 
  0x0429, "fa-ir";  0x042a, "vi-vn";  0x042b, "hy-am";  0x042c, "az-latn-az";
  0x042d, "eu-es";  0x042e, "hsb-de"; 0x042f, "mk-mk";  0x0432, "tn-za"; 
  0x0434, "xh-za";  0x0435, "zu-za";  0x0436, "af-za";  0x0437, "ka-ge"; 
  0x0438, "fo-fo";  0x0439, "hi-in";  0x043a, "mt-mt";  0x043b, "se-no"; 
  0x043e, "ms-my";  0x043f, "kk-kz";  0x0440, "ky-kg";  0x0441, "sw-ke"; 
  0x0442, "tk-tm";  0x0443, "uz-latn-uz";               0x0443, "uz";    
  0x0444, "tt-ru";  0x0445, "bn-in";  0x0446, "pa-in";  0x0447, "gu-in"; 
  0x0448, "or-in";  0x0449, "ta-in";  0x044a, "te-in";  0x044b, "kn-in"; 
  0x044c, "ml-in";  0x044d, "as-in";  0x044e, "mr-in";  0x044f, "sa-in"; 
  0x0450, "mn-cyrl";0x0451, "bo-cn";  0x0452, "cy-gb";  0x0453, "km-kh"; 
  0x0454, "lo-la";  0x0456, "gl-es";  0x0457, "kok-in"; 0x045a, "syr-sy"; 
  0x045b, "si-lk";  0x045d, "iu-cans-ca";               0x045e, "am-et"; 
  0x0461, "ne-np";  0x0462, "fy-nl";  0x0463, "ps-af";  0x0464, "fil-ph"; 
  0x0465, "dv-mv";  0x0468, "ha-latn-ng";               0x046a, "yo-ng";
  0x046b, "quz-bo"; 0x046c, "nso-za"; 0x046d, "ba-ru";  0x046e, "lb-lu"; 
  0x046f, "kl-gl";  0x0470, "ig-ng";  0x0478, "ii-cn";  0x047a, "arn-cl";
  0x047c, "moh-ca"; 0x047e, "br-fr";  0x0480, "ug-cn";  0x0481, "mi-nz"; 
  0x0482, "oc-fr";  0x0483, "co-fr";  0x0484, "gsw-fr"; 0x0485, "sah-ru";
  0x0486, "qut-gt"; 0x0487, "rw-rw";  0x0488, "wo-sn";  0x048c, "prs-af";
  0x0491, "gd-gb";  0x0801, "ar-iq";  0x0804, "zh-hans";0x0807, "de-ch"; 
  0x0809, "en-gb";  0x080a, "es-mx";  0x080c, "fr-be";  0x0810, "it-ch"; 
  0x0813, "nl-be";  0x0814, "nn-no";  0x0816, "pt-pt";  0x081a, "sr-latn-cs";
  0x081d, "sv-fi";  0x082c, "az-cyrl-az";               0x082e, "dsb-de";
  0x082e, "dsb";    0x083b, "se-se";  0x083c, "ga-ie";  0x083e, "ms-bn"; 
  0x0843, "uz-cyrl-uz";               0x0845, "bn-bd";  0x0850, "mn-mong-cn";
  0x085d, "iu-latn-ca";               0x085f, "tzm-latn-dz"; 
  0x086b, "quz-ec"; 0x0c01, "ar-eg";  0x0c04, "zh-hant";0x0c07, "de-at"; 
  0x0c09, "en-au";  0x0c0a, "es-es";  0x0c0c, "fr-ca";  0x0c1a, "sr-cyrl-cs";
  0x0c3b, "se-fi";  0x0c6b, "quz-pe"; 0x1001, "ar-ly";  0x1004, "zh-sg"; 
  0x1007, "de-lu";  0x1009, "en-ca";  0x100a, "es-gt";  0x100c, "fr-ch"; 
  0x101a, "hr-ba";  0x103b, "smj-no"; 0x1401, "ar-dz";  0x1404, "zh-mo"; 
  0x1407, "de-li";  0x1409, "en-nz";  0x140a, "es-cr";  0x140c, "fr-lu"; 
  0x141a, "bs-latn-ba";               0x141a, "bs";     0x143b, "smj-se";
  0x143b, "smj";    0x1801, "ar-ma";  0x1809, "en-ie";  0x180a, "es-pa"; 
  0x180c, "fr-mc";  0x181a, "sr-latn-ba";               0x183b, "sma-no";
  0x1c01, "ar-tn";  0x1c09, "en-za";  0x1c0a, "es-do";  0x1c1a, "sr-cyrl-ba";
  0x1c3b, "sma-se"; 0x1c3b, "sma";    0x2001, "ar-om";  0x2009, "en-jm"; 
  0x200a, "es-ve";  0x201a, "bs-cyrl-ba";               0x201a, "bs-cyrl";
  0x203b, "sms-fi"; 0x203b, "sms";    0x2401, "ar-ye";  0x2409, "en-029";
  0x240a, "es-co";  0x241a, "sr-latn-rs";               0x243b, "smn-fi";
  0x2801, "ar-sy";  0x2809, "en-bz";  0x280a, "es-pe";  0x281a, "sr-cyrl-rs";
  0x2c01, "ar-jo";  0x2c09, "en-tt";  0x2c0a, "es-ar";  0x2c1a, "sr-latn-me";
  0x3001, "ar-lb";  0x3009, "en-zw";  0x300a, "es-ec";  0x301a, "sr-cyrl-me";
  0x3401, "ar-kw";  0x3409, "en-ph";  0x340a, "es-cl";  0x3801, "ar-ae"; 
  0x380a, "es-uy";  0x3c01, "ar-bh";  0x3c0a, "es-py";  0x4001, "ar-qa"; 
  0x4009, "en-in";  0x400a, "es-bo";  0x4409, "en-my";  0x440a, "es-sv"; 
  0x4809, "en-sg";  0x480a, "es-hn";  0x4c0a, "es-ni";  0x500a, "es-pr"; 
  0x540a, "es-us"; ]

type lang = string
  
let rec d_name_langs soff ncount d = 
  d_skip (ncount * 6 * 2) d >>= fun () -> 
  d_uint16                d >>= fun lcount -> 
  let rec loop i acc = 
    if ncount = 0 then `Ok acc else 
    d_uint16 d >>= fun len -> 
    d_uint16 d >>= fun off ->
    let cpos = cur_pos d in 
    seek_table_pos (soff + off) d >>= fun () -> 
    d_utf_16be len d >>= fun lang -> 
    seek_pos cpos d >>= fun () ->
    loop (i - 1) ((0x8000 + (ncount - i), lang) :: acc)
  in
  loop ncount [] 
    
let rec d_name_records soff ncount f acc langs seen d =
  if ncount = 0 then `Ok acc else
  d_uint16 d >>= fun pid -> 
  d_uint16 d >>= fun eid -> 
  d_uint16 d >>= fun lid -> 
  d_uint16 d >>= fun nid -> 
  d_uint16 d >>= fun len -> 
  d_uint16 d >>= fun off ->
  match pid, eid with 
  | (0 | 2), _ | 3, 1 -> 
      let cpos = cur_pos d in
      let n = (nid, lid) in 
      if List.mem n seen 
      then d_name_records soff (ncount - 1) f acc langs seen d 
      else
      seek_table_pos (soff + off) d >>= fun () -> 
      d_utf_16be len d >>= fun v -> 
      seek_pos cpos  d >>= fun () ->
      let lang = try List.assoc lid langs with Not_found -> "und" in
      let acc' = f acc nid lang v in 
      d_name_records soff (ncount - 1) f acc' langs (n :: seen) d
  | _ -> 
      d_name_records soff (ncount - 1) f acc langs seen d
        
let name d f acc =
  init_decoder d >>= 
  seek_required_table Tag.name d >>= fun () -> 
  d_uint16 d >>= fun version -> 
  if version < 0 || version > 1 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun ncount ->
  d_uint16 d >>= fun soff -> 
  let cpos = cur_pos d in
  (if version = 0 then `Ok [] else d_name_langs soff ncount d) >>= fun langs -> 
  let langs = List.rev_append langs lcid_to_bcp47 in 
  seek_pos cpos d >>= fun () ->
  d_name_records soff ncount f acc langs [] d
    
(* OS/2 table *)
    
type os2 = 
  { os2_x_avg_char_width : int; 
    os2_us_weight_class : int;
    os2_us_width_class : int;
    os2_fs_type : int; 
    os2_y_subscript_x_size : int; 
    os2_y_subscript_y_size : int; 
    os2_y_subscript_x_offset : int; 
    os2_y_subscript_y_offset : int;
    os2_y_superscript_x_size : int; 
    os2_y_superscript_y_size : int; 
    os2_y_superscript_x_offset : int; 
    os2_y_superscript_y_offset : int;
    os2_y_strikeout_size : int;
    os2_y_strikeout_position : int;
    os2_family_class : int;
    os2_panose : string; (* 10 bytes *)
    os2_ul_unicode_range1 : int32;
    os2_ul_unicode_range2 : int32; 
    os2_ul_unicode_range3 : int32; 
    os2_ul_unicode_range4 : int32; 
    os2_ach_vend_id : int32;
    os2_fs_selection : int; 
    os2_us_first_char_index : int; 
    os2_us_last_char_index : int; 
    os2_s_typo_ascender : int;
    os2_s_type_descender : int; 
    os2_s_typo_linegap : int; 
    os2_us_win_ascent : int; 
    os2_us_win_descent : int;
    os2_ul_code_page_range_1 : int32 option;     
    os2_ul_code_page_range_2 : int32 option;
    os2_s_x_height : int option; 
    os2_s_cap_height : int option;
    os2_us_default_char : int option; 
    os2_us_break_char : int option; 
    os2_us_max_context : int option; }
  
let os2 d = 
  init_decoder d >>= 
  seek_required_table Tag.os2 d >>= fun () -> 
  d_uint16 d >>= fun version -> 
  if version > 0x0004 then err_version d (Int32.of_int version) else
  let opt v dec d = if version < v then `Ok None else match dec d with 
  | `Error _ as e -> e | `Ok v -> `Ok (Some v)
  in
  d_int16  d >>= fun os2_x_avg_char_width ->
  d_uint16 d >>= fun os2_us_weight_class ->
  d_uint16 d >>= fun os2_us_width_class ->
  d_uint16 d >>= fun os2_fs_type ->
  d_int16  d >>= fun os2_y_subscript_x_size ->
  d_int16  d >>= fun os2_y_subscript_y_size ->
  d_int16  d >>= fun os2_y_subscript_x_offset ->
  d_int16  d >>= fun os2_y_subscript_y_offset ->
  d_int16  d >>= fun os2_y_superscript_x_size ->
  d_int16  d >>= fun os2_y_superscript_y_size ->
  d_int16  d >>= fun os2_y_superscript_x_offset ->
  d_int16  d >>= fun os2_y_superscript_y_offset ->
  d_int16  d >>= fun os2_y_strikeout_size ->
  d_int16  d >>= fun os2_y_strikeout_position ->
  d_int16  d >>= fun os2_family_class -> 
  d_bytes 10 d >>= fun os2_panose ->
  d_uint32 d >>= fun os2_ul_unicode_range1 -> 
  d_uint32 d >>= fun os2_ul_unicode_range2 -> 
  d_uint32 d >>= fun os2_ul_unicode_range3 -> 
  d_uint32 d >>= fun os2_ul_unicode_range4 ->
  d_uint32 d >>= fun os2_ach_vend_id ->
  d_uint16 d >>= fun os2_fs_selection -> 
  d_uint16 d >>= fun os2_us_first_char_index -> 
  d_uint16 d >>= fun os2_us_last_char_index -> 
  d_int16  d >>= fun os2_s_typo_ascender -> 
  d_int16  d >>= fun os2_s_type_descender -> 
  d_int16  d >>= fun os2_s_typo_linegap -> 
  d_uint16 d >>= fun os2_us_win_ascent ->
  d_uint16 d >>= fun os2_us_win_descent ->
  opt 0x0001 d_uint32 d >>= fun os2_ul_code_page_range_1 ->  
  opt 0x0001 d_uint32 d >>= fun os2_ul_code_page_range_2 ->
  opt 0x0002 d_int16  d >>= fun os2_s_x_height ->
  opt 0x0002 d_int16  d >>= fun os2_s_cap_height ->
  opt 0x0002 d_uint16 d >>= fun os2_us_default_char -> 
  opt 0x0002 d_uint16 d >>= fun os2_us_break_char -> 
  opt 0x0002 d_uint16 d >>= fun os2_us_max_context ->
  `Ok { os2_x_avg_char_width; os2_us_weight_class; os2_us_width_class;
        os2_fs_type; os2_y_subscript_x_size; os2_y_subscript_y_size;
        os2_y_subscript_x_offset; os2_y_subscript_y_offset;
        os2_y_superscript_x_size; os2_y_superscript_y_size;
        os2_y_superscript_x_offset; os2_y_superscript_y_offset;
        os2_y_strikeout_size; os2_y_strikeout_position;
        os2_family_class; os2_panose; os2_ul_unicode_range1;
        os2_ul_unicode_range2; os2_ul_unicode_range3;
        os2_ul_unicode_range4; os2_ach_vend_id; os2_fs_selection;
        os2_us_first_char_index; os2_us_last_char_index;
        os2_s_typo_ascender; os2_s_type_descender; os2_s_typo_linegap;
        os2_us_win_ascent; os2_us_win_descent;
        os2_ul_code_page_range_1; os2_ul_code_page_range_2;
        os2_s_x_height; os2_s_cap_height; os2_us_default_char;
        os2_us_break_char; os2_us_max_context; }

(* kern table *) 

type kern_info =
  { kern_dir : [ `H | `V ]; 
    kern_kind : [ `Min | `Kern ]; 
    kern_cross_stream : bool; }
                   
let kern_info c = 
  { kern_dir = (if c land 0x1 > 0 then `H else `V); 
    kern_kind = (if c land 0x2 > 0 then `Min else `Kern); 
    kern_cross_stream = c land 0x4 > 0 }

let rec kern_tables ntables t p acc d = 
  if ntables = 0 then `Ok acc else 
  d_uint16 d >>= fun version ->
  if version > 0 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun len -> 
  d_uint16 d >>= fun coverage -> 
  let format = coverage lsr 8 in 
  let skip acc = 
    d_skip (len - 3 * 2) d >>= fun () -> 
    kern_tables (ntables - 1) t p acc d
  in
  if format <> 0 then skip acc else 
  match t acc (kern_info coverage) with 
  | `Skip, acc -> skip acc
  | `Fold, acc ->
      let rec d_pairs len acc d = 
        if len < 3 * 2 then d_skip len d >>= fun () -> `Ok acc else
        d_uint16 d >>= fun left -> 
        d_uint16 d >>= fun right -> 
        d_int16 d >>= fun values -> 
        d_pairs (len - 3 * 2) (p acc left right values) d
      in
      d_skip (4 * 2)  d >>= fun () ->
      d_pairs len acc d >>= fun acc -> 
      kern_tables (ntables - 1) t p acc d
        
let kern d t p acc = 
  init_decoder d >>=
  seek_table Tag.kern d >>= function 
  | None -> `Ok acc
  | Some _ -> 
      d_uint16 d >>= fun version -> 
      if version > 0 then err_version d (Int32.of_int version) else
      d_uint16 d >>= fun ntables ->
      kern_tables ntables t p acc d
        
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
