(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf 
let exec = Filename.basename Sys.executable_name 
let log msg = Format.eprintf ("%s: " ^^ msg ^^ "@.") exec 
let err = ref false
let log_err inf e =
  err := true; Format.eprintf "%s: %s: %a" exec inf Otfm.pp_error e
  

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

let dump_cmap ppf inf d =
  let i = ref 0 in
  let pp_binding ppf k (u0, u1) gid =
    incr i;
    let k = match k with `Glyph -> "glyph" | `Glyph_range -> "glyph-range" in
    pp ppf "@ (%a %a %s %d)" Otfm.pp_cp u0 Otfm.pp_cp u1 k gid; ppf 
  in 
  pp ppf "@,@[<1>(cmap";
  begin match Otfm.table_cmap d pp_binding ppf with 
  | `Ok _ -> pp ppf ")@]"; | `Error e -> log_err inf e 
  end
   
let dump_tables ppf inf d =
  dump_cmap ppf inf d
 
let dump_file ppf inf = match string_of_file inf with
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
            dump_tables ppf inf d
(*
            pp ppf ")@]@."
*)

let dump files = match files with
| [] -> dump_file Format.std_formatter "-" 
| fs -> List.iter (dump_file Format.std_formatter) fs

let main () = 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... [OTFFILE]...\n\
    \ Dump OpenType file font information on stdout.\n\
    Options:" exec 
  in
  let cmd = ref `Dump in 
  let set_cmd v () = cmd := v in 
  let files = ref [] in 
  let add_file f = files := f :: !files in
  let options = [
    "-ocaml", Arg.Unit (set_cmd `OCaml), " Outputs an OCaml module";
  ]
  in
  Arg.parse (Arg.align options) add_file usage; 
  begin match !cmd with 
  | `Dump -> dump (List.rev !files)
  | `OCaml -> failwith "TODO"
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
