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
  test Otfm.Tag.t_cmap "cmap";
  test Otfm.Tag.t_head "head";
  test Otfm.Tag.t_hhea "hhea";
  test Otfm.Tag.t_hmtx "hmtx";
  test Otfm.Tag.t_maxp "maxp";
  test Otfm.Tag.t_name "name";
  test Otfm.Tag.t_OS_2 "OS/2";
  test Otfm.Tag.t_post "post";
  test Otfm.Tag.t_cvt  "cvt ";
  test Otfm.Tag.t_fpgm "fpgm";
  test Otfm.Tag.t_glyf "glyf";
  test Otfm.Tag.t_loca "loca";
  test Otfm.Tag.t_prep "prep";
  test Otfm.Tag.t_CFF  "CFF ";
  test Otfm.Tag.t_VORG "VORG";
  test Otfm.Tag.t_EBDT "EBDT";
  test Otfm.Tag.t_EBLC "EBLC";
  test Otfm.Tag.t_EBSC "EBSC";
  test Otfm.Tag.t_DSIG "DSIG";
  test Otfm.Tag.t_gasp "gasp";
  test Otfm.Tag.t_hdmx "hdmx";
  test Otfm.Tag.t_kern "kern";
  test Otfm.Tag.t_LTSH "LTSH";
  test Otfm.Tag.t_PCLT "PCLT";
  test Otfm.Tag.t_VDMX "VDMX";
  test Otfm.Tag.t_vhea "vhea";
  test Otfm.Tag.t_vmtx "vmtx";
  test Otfm.Tag.t_BASE "BASE";
  test Otfm.Tag.t_GDEF "GDEF";
  test Otfm.Tag.t_GPOS "GPOS";
  test Otfm.Tag.t_GSUB "GSUB";
  test Otfm.Tag.t_JSTF "JSTF"


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
