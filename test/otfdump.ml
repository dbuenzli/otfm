(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf 
let exec = Filename.basename Sys.executable_name 
let log msg = Format.eprintf ("%s: " ^^ msg ^^ "@?") exec 

let string_of_channel ic = 
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
  | Exit -> `Ok (Buffer.contents b)
  | Failure _ -> `Error
        
let dump_flavor inf ppf d = match Otfm.flavour d with 
| `Error e -> log "%s: %a@." inf Otfm.pp_error e; false
| `Ok f ->
    let fs = match f with `TTF -> "TTF" | `CFF -> "CFF" in
    pp ppf "(flavor %s)@." fs; true

let dump_table_list inf ppf d = match Otfm.table_list d with 
| `Error e -> log "%s: %a@." inf Otfm.pp_error e; false
| `Ok ts -> 
    pp ppf "@[<1>(tables ";
    List.iter (fun t -> pp ppf "@ %a" Otfm.Tag.pp t) ts; 
    pp ppf ")@]"; true

let dump inf ppf = 
  try 
    let ic = if inf = "-" then stdin else open_in_bin inf in
    begin match string_of_channel ic with 
    | `Error -> log "%s: max buffer size exceeded (32 bit plaform ?)" inf
    | `Ok s -> 
        let d = Otfm.decoder (`String s) in
        ignore begin 
          dump_flavor inf ppf d && dump_table_list inf ppf d 
        end
    end;
    close_in ic
  with
  | Sys_error e -> log "%s@." e

let main () = 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... [INFILE]\n\
    \ Dump OpenType file font information on stdout.\n\
    Options:" exec 
  in
  let cmd = ref `Dump in 
  let set_cmd v () = cmd := v in 
  let inf = ref "-" in 
  let set_inf f = 
    if !inf <> "-" then raise (Arg.Bad "only one file can be specified") else
    inf := f
  in
  let options = [
    "-ocaml", Arg.Unit (set_cmd `OCaml), " Outputs an OCaml module";
  ]
  in
  Arg.parse (Arg.align options) set_inf usage; 
  match !cmd with 
  | `Dump -> dump !inf Format.std_formatter 
  | `OCaml -> failwith "TODO"

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
