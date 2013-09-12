(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Format.sprintf
let log f = Format.printf (f ^^ "@?") 
let fail fmt = 
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt
  
let test_tag_constants () = 
  log "Testing tag constants.\n";
  let test t str = 
    if Otfm.Tag.compare t (Otfm.Tag.of_bytes str) <> 0 
    then fail "Wrong constant for '%s' tag" str
  in
  test Otfm.Tag.cmap "cmap";
  test Otfm.Tag.head "head";
  test Otfm.Tag.hhea "hhea";
  test Otfm.Tag.hmtx "hmtx";
  test Otfm.Tag.maxp "maxp";
  test Otfm.Tag.name "name";
  test Otfm.Tag.os2  "OS/2";
  test Otfm.Tag.post "post";
  test Otfm.Tag.cvt  "cvt ";
  test Otfm.Tag.fpgm "fpgm";
  test Otfm.Tag.glyf "glyf";
  test Otfm.Tag.loca "loca";
  test Otfm.Tag.prep "prep";
  test Otfm.Tag.cff  "CFF ";
  test Otfm.Tag.vorg "VORG";
  test Otfm.Tag.ebdt "EBDT";
  test Otfm.Tag.eblc "EBLC";
  test Otfm.Tag.ebsc "EBSC";
  test Otfm.Tag.dsig "DSIG";
  test Otfm.Tag.gasp "gasp";
  test Otfm.Tag.hdmx "hdmx";
  test Otfm.Tag.kern "kern";
  test Otfm.Tag.ltsh "LTSH";
  test Otfm.Tag.pclt "PCLT";
  test Otfm.Tag.vdmx "VDMX";
  test Otfm.Tag.vhea "vhea";
  test Otfm.Tag.vmtx "vmtx";
  test Otfm.Tag.base "BASE";
  test Otfm.Tag.gdef "GDEF";
  test Otfm.Tag.gpos "GPOS";
  test Otfm.Tag.gsub "GSUB";
  test Otfm.Tag.jstf "JSTF"

let test () =
  Printexc.record_backtrace true; 
  test_tag_constants ()

let () = if not (!Sys.interactive) then test () 
 

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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


let () = ()

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
