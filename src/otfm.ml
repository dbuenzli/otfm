(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Invalid_arg strings *)

let err_invalid_tag s = Printf.sprintf "invalid OpenType tag (%S)" s

(* Unsafe string byte manipulations. If you don't believe the author's
   invariants, replacing with safe versions makes everything safe in
   the module. He won't be upset. *)

let unsafe_chr = Char.unsafe_chr
let unsafe_byte s j = Char.code (String.get s j)

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


(* OpenType table record 
   We use ints, on 64 bits platforms they fit, on 32 bits we 
   are limited by the string size anyway. *)

type t_rec = { tag : tag; off : int; len : int } 

(* Decode *)

type error = [ 
  | `Unknown_flavour of tag 
  | `Missing_required_table of tag
  | `TTC_unsupported
  | `Invalid_table_bounds of (tag * int * int)
  | `Unexpected_eoi of [ `Offset_table | `Table_directory | `Table of tag ]]

let pp_error ppf = function 
| `Unknown_flavour tag -> 
    pp ppf "@[Unknown OpenType flavour (%a)@]" Tag.pp tag
| `Missing_required_table tag -> 
    pp ppf "@[Missing required table (%a)@]" Tag.pp tag
| `TTC_unsupported ->
    pp ppf "@[True Type collections (TTC) are not supported@]"
| `Invalid_table_bounds (t, o, l) -> 
    pp ppf "@[Invalid table bounds (%a offset %d length %d)" Tag.pp t o l
| `Unexpected_eoi loc -> 
    pp ppf "@[Unexpected end of input in ";
    begin match loc with 
    | `Offset_table -> pp ppf "offset table@]"
    | `Table_directory -> pp ppf "table directory@]"
    | `Table tag -> pp ppf "table %a@]" Tag.pp tag
    end

type flavour = [ `TTF | `CFF ]
type src = [ `String of string ] 
type decoder =
  { mutable i : string;                                       (* input data. *)
    mutable i_pos : int;                          (* input current position. *) 
    mutable i_max : int;                          (* input maximal position. *)
    mutable state : [ `Fatal of error | `Start | `Ready ]; (* decoder state. *)
    mutable c_table :                                (* table being decoded. *)
      [ `Offset_table | `Table_directory | `Table of tag ];
    mutable flavour : [ `TTF | `CFF ];                   (* decoded flavour. *)
    mutable t_recs : t_rec list }                  (* decoded table records. *)

let decoder_src d = (`String d.i)
let fatal d e = d.state <- `Fatal e; `Error e
let i_rem d = d.i_max - d.i_pos + 1
let raw_byte d = 
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1; (unsafe_byte d.i j) 

(* TODO redo we can be much more efficient if we source only from 
   a single in-memory string *)

let d_uint8 d =
  let rem = i_rem d in 
  if rem < 1 then `Error (`Unexpected_eoi d.c_table) else
  `Ok (raw_byte d)

let d_int8 d = match d_uint8 d with
| `Ok i -> `Ok (if i > 0x7F then i - 0x100 else i)
| `Error _ as e -> e

let d_uint16 d =
  let rem = i_rem d in
  if rem < 2 then `Error (`Unexpected_eoi d.c_table) else
  let b0 = raw_byte d in
  let b1 = raw_byte d in 
  `Ok ((b0 lsl 8) lor b1)

let d_int16 d = match d_uint16 d with
| `Ok i -> `Ok (if i > 0x7FFF then i - 0x10000 else i) 
| `Error _ as e -> e

let d_uint24 d = 
  let rem = i_rem d in 
  if rem < 3 then `Error (`Unexpected_eoi d.c_table) else 
  let b0 = raw_byte d in 
  let b1 = raw_byte d in 
  let b2 = raw_byte d in 
  `Ok ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let d_uint32 d = 
  let rem = i_rem d in 
  if rem < 4 then `Error (`Unexpected_eoi d.c_table) else
  let b0 = raw_byte d in
  let b1 = raw_byte d in 
  let b2 = raw_byte d in 
  let b3 = raw_byte d in 
  let s0 = Int32.of_int ((b0 lsl 8) lor b1) in 
  let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
  `Ok (Int32.logor (Int32.shift_left s0 16) s1)

let d_uint32_int d =
  let rem = i_rem d in 
  if rem < 4 then `Error (`Unexpected_eoi d.c_table) else
  let b0 = raw_byte d in
  let b1 = raw_byte d in 
  let b2 = raw_byte d in 
  let b3 = raw_byte d in 
  let s0 = b0 lsl 8 lor b1 in 
  let s1 = b2 lsl 8 lor b3 in 
  `Ok ((s0 lsl 16) lor s1)
  
let d_fixed d = 
  let rem = i_rem d in 
  if rem < 4 then `Error (`Unexpected_eoi d.c_table) else
  let b0 = raw_byte d in
  let b1 = raw_byte d in 
  let b2 = raw_byte d in 
  let b3 = raw_byte d in 
  let s0 = Int32.of_int ((b0 lsl 8) lor b1) in 
  let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
  `Ok (s0, s1)

let decoder src = 
  let i , i_pos, i_max = match src with
  | `String s -> s, 0, String.length s - 1 
  in 
  { i; i_pos; i_max; state = `Start; c_table = `Offset_table; 
    flavour = `TTF; t_recs = []; }
    
let ( >>= ) x f = match x with `Ok v -> f v | `Error _ as e -> e

let rec d_table_records d count = 
  if count = 0 then (d.state <- `Ready; `Ok ()) else
  d_uint32 d >>= fun tag ->
  d_uint32 d >>= fun _ -> 
  d_uint32_int d >>= fun off -> 
  d_uint32_int d >>= fun len -> 
  d.t_recs <- { tag; off; len } :: d.t_recs;
  d_table_records d (count - 1)

let d_version d = 
  d_uint32 d >>= function
  | t when t = Tag.v_OTTO -> d.flavour <- `CFF; `Ok ()
  | t when (t = Tag.v_true || t = 0x00010000l) -> d.flavour <- `TTF; `Ok ()
  | t when t = Tag.v_ttcf -> `Error `TTC_unsupported
  | t -> `Error (`Unknown_flavour t)

let d_structure d =                   (* Offset table and table directory. *)
  d_version d >>= fun () ->                               (* Offset table. *)
  d_uint16 d >>= fun count ->                               (* numTables. *)
  d_uint16 d >>= fun _ ->
  d_uint16 d >>= fun _ -> 
  d_uint16 d >>= fun _ -> 
  d.c_table <- `Table_directory;
  d_table_records d count                             (* Table directory. *)

let init_decoder d = match d.state with
| `Ready -> `Ok ()
| `Fatal e -> `Error e 
| `Start -> 
    match d_structure d with
    | `Error e as err -> d.state <- `Fatal e; err 
    | `Ok () as ok -> ok 

let flavour d = init_decoder d >>= fun () -> `Ok d.flavour
let table_list d = 
  let tags d = List.rev_map (fun r -> r.tag) d.t_recs in
  init_decoder d >>= fun () -> `Ok (tags d)

let table_mem d t = 
  let exists_tag t r = List.exists (fun r -> t = r.tag) d.t_recs in
  init_decoder d >>= fun () -> `Ok (exists_tag t d)

let table_raw d t = 
  init_decoder d >>= fun () -> 
  try 
    let t = List.find (fun r -> t = r.tag) d.t_recs in
    if t.off > d.i_max || t.off + t.len - 1 > d.i_max 
    then `Error (`Invalid_table_bounds (t.tag, t.off, t.len))
    else `Ok (Some (String.sub d.i t.off t.len))
  with Not_found -> `Ok None
  
  

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
