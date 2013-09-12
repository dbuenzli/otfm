(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** OpenType font decoder. 

    [Otfm] is an in-memory decoder for the OpenType font data format.
    It provides low-level access to OpenType fonts tables and functions
    to decode some of them.

    Consult the {{!limitations}limitations} and {{!examples}example} of 
    use. 

    {b Note.} Unless otherwise noted the strings returned are UTF-8 
    encoded.

    {e Release %%VERSION%% — %%MAINTAINER%% }
    {3 References}
    {ul 
    {- Microsoft.
    {e {{:http://www.microsoft.com/typography/otspec/default.htm}
        The OpenType Specification}}, 2009.}} *)

(** {1 Tags} *)

type tag
(** The type for OpenType tags. *)

(** Tags.

    OpenType tags are four bytes identifiers. *)
module Tag : sig

  (** {1 Tags} *)
  
  type t = tag
  (** The type for OpenType tags. *)
  
  (** {1 Table tags} *)

  (** {2:req Required tables} *)

  val t_cmap : tag 
  (** The {{:http://www.microsoft.com/typography/otspec/cmap.htm}cmap} table. *)
  val t_head : tag
  (** The {{:http://www.microsoft.com/typography/otspec/head.htm}head} table. *)
  val t_hhea : tag
  (** The {{:http://www.microsoft.com/typography/otspec/hhea.htm}hhea} table. *)
  val t_hmtx : tag
  (** The {{:http://www.microsoft.com/typography/otspec/hmtx.htm}hmtx} table. *)
  val t_maxp : tag
  (** The {{:http://www.microsoft.com/typography/otspec/maxp.htm}maxp} table. *)
  val t_name : tag
  (** The {{:http://www.microsoft.com/typography/otspec/name.htm}name} table. *)
  val t_OS_2 : tag
  (** The {{:http://www.microsoft.com/typography/otspec/os2.htm}os2} table. *)
  val t_post : tag
  (** The {{:http://www.microsoft.com/typography/otspec/post.htm}post} table. *)
    
  (** {2 TTF font tables} *)

  val t_cvt  : tag
  (** The {{:http://www.microsoft.com/typography/otspec/cvt.htm}cvt} table. *)
  val t_fpgm : tag
  (** The {{:http://www.microsoft.com/typography/otspec/fpgm.htm}fpgm} table. *)
  val t_glyf : tag
  (** The {{:http://www.microsoft.com/typography/otspec/glyf.htm}glyf} table. *)
  val t_loca : tag
  (** The {{:http://www.microsoft.com/typography/otspec/loca.htm}loca} table. *)
  val t_prep : tag
  (** The {{:http://www.microsoft.com/typography/otspec/prep.htm}prep} table. *)

  (** {2 CFF font tables} *) 
    
  val t_CFF  : tag
  (** The {{:http://www.microsoft.com/typography/otspec/cff.htm}CFF} table. *)
  val t_VORG : tag
  (** The {{:http://www.microsoft.com/typography/otspec/vorg.htm}VORG} table. *)
    
  (** {2 Bitmap glyph tables} *)
    
  val t_EBDT : tag
  (** The {{:http://www.microsoft.com/typography/otspec/ebdt.htm}EBDT} table. *)

  val t_EBLC : tag
  (** The {{:http://www.microsoft.com/typography/otspec/eblc.htm}EBLC} table. *)

  val t_EBSC : tag
  (** The {{:http://www.microsoft.com/typography/otspec/ebsc.htm}EBSC} table. *)
    
  (** {2 Optional tables} *)
    
  val t_DSIG : tag
  (** The {{:http://www.microsoft.com/typography/otspec/dsig.htm}DSIG} table. *)
  val t_gasp : tag
  (** The {{:http://www.microsoft.com/typography/otspec/gasp.htm}gasp} table. *)
  val t_hdmx : tag
  (** The {{:http://www.microsoft.com/typography/otspec/hdmx.htm}hdmx} table. *)
  val t_kern : tag
  (** The {{:http://www.microsoft.com/typography/otspec/kern.htm}kern} table. *)
  val t_LTSH : tag
  (** The {{:http://www.microsoft.com/typography/otspec/ltsh.htm}LTSH} table. *)
  val t_PCLT : tag
  (** The {{:http://www.microsoft.com/typography/otspec/pclt.htm}PCLT} table. *)
  val t_VDMX : tag
  (** The {{:http://www.microsoft.com/typography/otspec/vdmx.htm}VDMX} table. *)
  val t_vhea : tag
  (** The {{:http://www.microsoft.com/typography/otspec/vhea.htm}vhea} table. *)
  val t_vmtx : tag
  (** The {{:http://www.microsoft.com/typography/otspec/vmtx.htm}vmtx} table. *)

  (** {2 Advanced typographic tables} *)

  val t_BASE : tag
  (** The {{:http://www.microsoft.com/typography/otspec/base.htm}BASE} table. *)
  val t_GDEF : tag
  (** The {{:http://www.microsoft.com/typography/otspec/gdef.htm}GDEF} table. *)
  val t_GPOS : tag
  (** The {{:http://www.microsoft.com/typography/otspec/gpos.htm}GPOS} table. *)
  val t_GSUB : tag
  (** The {{:http://www.microsoft.com/typography/otspec/gsub.htm}GSUB} table. *)
  val t_JSTF : tag
  (** The {{:http://www.microsoft.com/typography/otspec/jstf.htm}JSTF} table. *)

  (** {1 Functions} *)

  val of_bytes : string -> tag
  (** [of_bytes s] is a tag corresponding to [s]. 
      @Raise Invalid_argument if [s] is not four byte long. *)

  val to_bytes : tag -> string 
  (** [to_string t] is the tag as a four byte long string. *)

  val to_int32 : tag -> int32 
  (** [to_int32 t] is the tag as an unsigned 32 bits integer. *)

  val of_int32 : int32 -> tag
  (** [of_int32 t] is the tag from and unsigned 32 bits integer. *)

  val compare : tag -> tag -> int
  (** [compare t t'] is [Pervasives.compare t t'] *)

  val pp : Format.formatter -> tag -> unit 
  (** [pp t] prints a textual representation of [t] on [ppf]. *)
end

(** {1 Unicode code points} 

    For some reason OpenType allows the (textually meaningless)
    {{:http://unicode.org/glossary/#surrogate_code_point}surrogate
    code points} to be mapped to glyphs. Hence we deal with Unicode
    {{:http://unicode.org/glossary/#code_point}code points} not
    {{:http://unicode.org/glossary/#unicode_scalar_value} scalar
    value}. *)

type cp = int 
(** The type for Unicode 
    {{:http://unicode.org/glossary/#code_point}code points}, ranges
    from [0x0000] to [0x10_FFFF]. Any code point returned by 
    [Otfm] is guaranteed to be in the range. *)

type cp_range = cp * cp
(** The type for Unicode code point ranges. Any range [(u0, u1)] 
    returned by [Otfm] has [u0 <= u1]. *)

val is_cp : int -> bool
(** [is_cp i] is [true] if [i] is an 
    Unicode {{:http://unicode.org/glossary/#code_point}code point}. *)

val is_scalar_value : int -> bool
(** [is_scalar_value i] is [true] if [i] is an Unicode
    {{:http://unicode.org/glossary/#unicode_scalar_value} scalar
    value}. *)

val pp_cp : Format.formatter -> int -> unit
(** [pp_cp ppf i] prints a textual representation of the
    {{:http://unicode.org/glossary/#code_point}code point} [i] on
    [ppf]. If [i] is not a valid code point ["U+Invalid(X)"] is
    printed where [X] is the hexadecimal integer value. *)

(** {1 Decode} *)

type error_ctx =
  [ `Table of tag | `Offset_table | `Table_directory ]
(** The type for error contexts. *)

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
(** The type for decoding errors.
    
    {b Note.} In case of [`Invalid_poscript_name] a string of {e bytes} is 
    returned. *)

val pp_error : Format.formatter -> [< error] -> unit 
(** [pp_error ppf e] prints an uspecified representation of [e] on [ppf].*)

type src = [ `String of string ] 
(** The type for input sources. *)

type decoder
(** The type for OpenType font decoders. *)

val decoder : [< src ] -> decoder 
(** [decoder src] is a decoder decoding from [src]. *)

val decoder_src : decoder -> src 
(** [decoder_src d] is [d]'s input source. *)

(** {1 Table decoding} 

    These functions can be used in any order and are robust: when
    they return an error the decoder is back to a consistant state and
    can be used further. However if {!flavour} or {!table_list} returns
    an error you can safely assume that all other functions will. The fields
    are in general not documented please refer to the OpenType specification
    for details. *)

type flavour = [ `TTF | `CFF ]
(** The type for OpenType flavours. *)

val flavour : decoder -> [> `Ok of flavour | `Error of error ]
(** [decode_flavour d] is the flavour of the font decoded by [d]. *)

val table_list : decoder -> [> `Ok of tag list | `Error of error ]
(** [table_list t] is the list of tables of the font decoded by [d]. *)

val table_mem : decoder -> tag -> [> `Ok of bool | `Error of error ] 
(** [table_mem d t] is [true] if table [t] is in the font decoded by [d]. *)

val table_raw : decoder -> tag -> [> `Ok of string option | `Error of error ]
(** [table_raw d t] is the (unpadded) data of the table [t] as a
    string if the table [t] exists. *)

(** {2:convenience Convenience decodes} 

    These functions lookup data in the right table. *)

val glyph_count : decoder -> [> `Ok of int | `Error of error ]
(** [glyph_count d] is the number of glyphs in the font (bounded by [65535]). *)

val postscript_name : decoder -> [> `Ok of string option | `Error of error ] 
(** [poscript_name d] is the PostScript name of [d]. Looks up and validates
    as mandated by the OTF standard, don't rely on {!name} if you really
    need this information. *)

(** {2:cmap cmap table} *)

type glyph_id = int
(** The type for glyph ids. From [0] to [65534]*)

type map_kind = [ `Glyph | `Glyph_range ]
(** The type for map kinds. 

    Determines how an unicode range [(u0, u1)] and a glyph id [gid]
    must be interpreted in the folding function of {!cmap}.
    {ul 
    {- [`Glyph] all characters in the range map to to [gid].}
    {- [`Glyph_range], [u0] maps to [gid], [u0 + 1] to [gid + 1], ... 
       and [u1] to [gid + (u1 - u0)]}} *)

val cmap : decoder -> ('a -> map_kind -> cp_range -> glyph_id -> 'a) -> 
  'a -> [> `Ok of (int * int * int) * 'a | `Error of error ]
(** [cmap d f acc] folds over a mapping from unicode
    scalar values to glyph ids by reading the 
    {{:http://www.microsoft.com/typography/otspec/cmap.htm}cmap} table.
    The triple of integer indicates the platform id, encoding
    id and format of the cmap used.
        
    {b Limitations.} Only the format 13 (last resort font), format 12
    (UCS-4) and format 4 (UCS-2) cmap table formats are supported.

    If multiple tables are present, it favours 13 over 12 over 4.  If
    multiple tables of the same format are present it takes the first
    one it finds. 

    If no supported cmap table is found the error [`Unsupported_cmaps]
    is returned with the list of platform id, encoding id, format
    available in the font. *)

(** {2:head head table} *)

type head = 
  { head_font_revision : int32;
    head_flags : int;
    head_units_per_em : int; 
    head_created : float;  (** Unix timestamp. *) 
    head_modified : float; (** Unix timestamp. *) 
    head_xmin : int; 
    head_ymin : int; 
    head_xmax : int; 
    head_ymax : int; 
    head_mac_style : int; 
    head_lowest_rec_ppem : int;
    head_index_to_loc_format : int; }
(** The type for representing 
    {{:http://www.microsoft.com/typography/otspec/head.htm}head} tables. *)

val head : decoder -> [> `Ok of head | `Error of error ]
(** [head d] is the head table. *)

(** {2:hhea hhea table} *)

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
(** The type for 
    {{:http://www.microsoft.com/typography/otspec/hhea.htm}hhea} tables. *)

val hhea : decoder -> [> `Ok of hhea | `Error of error ] 
(** [hhea d] is the hhea table. *)

(** {2:hmtx hmtx table} *)

val hmtx : decoder -> ('a -> glyph_id -> int -> int -> 'a) -> 
  'a -> [> `Ok of 'a | `Error of error ]
(** [hmtx d f acc] folds over the horizontal metrics of the font by
    reading the
    {{:http://www.microsoft.com/typography/otspec/hmtx.htm}hmtx}
    table.  [f] is applied on each entry with [f acc' gid adv lsb]
    with [gid] the glyph id (guaranteed to range, in order, from
    [0] to glyph count minus one), [adv] the (unsigned) advance width,
    and [lsb] the (signed) left side bearing. *)

(** {2:name name table} *)

type lang = string
(** The type for {{:http://tools.ietf.org/html/bcp47}BCP 47} language tags. 

    {b Note.} The module normalizes Windows language ids to BCP 47 and 
    returns lowercased tags. *) 

val name : decoder -> ('a -> int -> lang -> string -> 'a) -> 'a -> 
  [> `Ok of 'a | `Error of error ]
(** [name d f acc] folds over the name records of the font by 
    reading the {{:http://www.microsoft.com/typography/otspec/name.htm}name}
    table. [f] is applied on each name id entry with [f acc' nid lang name] 
    with [nid] the name id, lang the language tag, and [name] the UTF-8 
    encoded name value.

    {b Note.} If you are looking for the postcript name use 
    {!postscript_name}. 

    {b Limitations.} Lookups data only in platform ids 0, 2 and 3 (Unicode, 
    ISO and Windows) with UTF-16BE encoding and reports only the data of 
    the first one it finds for a given name id. *)

(** {2:os2 OS/2 table} *) 

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
    os2_panose : string; (** 10 bytes *)
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
(** The type for 
    {{:http://www.microsoft.com/typography/otspec/os2.htm}OS/2} tables. *)

val os2 : decoder -> [> `Ok of os2 | `Error of error ]
(** [os2 d] is the OS/2 table. *)


(** {2:kern kern table} *) 

type kern_info =
  { kern_dir : [ `H | `V ]; 
    kern_kind : [ `Min | `Kern ]; 
    kern_cross_stream : bool; }
(** The type for kerning (sub)table information. *) 

val kern : decoder -> 
  ('a -> kern_info -> [`Skip | `Fold ] * 'a) -> 
  ('a -> glyph_id -> glyph_id -> int -> 'a) -> 'a -> 
  [> `Ok of 'a | `Error of error ]
(** [kern d t p acc] folds over the kerning tables of [d] by 
    reading the {{:http://www.microsoft.com/typography/otspec/kern.htm}kern}
    table. [t] is called on each new (sub)table, the table pairs are skipped if
    it returns [`Skip] otherwise [p acc' left right value] is called on 
    each kerning pair of the table. The function returns [acc] if there
    is no kern table.

    {b Limitations.} Only format 0 kerning tables are supported. *)
                                            

(** {1:limitations Limitations} 

    As it stands [Otfm] has the following limitations.  Some of these
    may be lifted in the future and a few of these can be overcome 
    by pre-processing your font (e.g. extract [.ttc] files to [.ttf], or 
    removing hinting information to reduce the font size). See also 
    the individual table decoding functions for other limitations.

    {ul
    {- True Type collections ([.ttc] files) are not supported}
    {- The whole font needs to be loaded in memory as a string. This may
       be a limiting factor on 32 bits platforms (but non [.ttc] font 
       files tend to be smaller than 16 Mo).}
    {- Table checksums are not verified.}}
*)

(** {1:examples Examples} *)


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
