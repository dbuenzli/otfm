

- Require OCaml >= 4.08.0
- Add the `Otfm.cmap_fold_uchars` convenience function.
- Add `Otfm.error_to_string`.
- Add `Otfm.{decoder_collection,in_collection}` to support OpenType font 
  collections.
- `otftrip` tool overhaul. Among other things supports OpenType font
  collection decodes and depends on `cmdliner`. The command line
  interface is incompatible. See `otftrip --help` for details.

v0.4.0 2020-03-23 Concise
-------------------------

- Fix decoding of `kern` table. Thanks to Philippe Veber for the patch (#4).
- Fix decoding of language tag records in `name` tables with
  format 1. Thanks to Philippe Veber for the report. (#3).
- Require OCaml >= 4.05.0

v0.3.0 2016-11-25 Zagreb
------------------------

- Use standard library `result` type.
- Support uutf 1.0.0.
- Safe string support.
- Build depend on topkg.
- Relicense from BSD3 to ISC.


v0.2.0 2014-08-23 Cambridge (UK)
--------------------------------

- Add `loca` and `glyf` table decoding.


v0.1.0 2013-09-24 Lausanne
--------------------------

First release.  
Sponsored by Citrix Systems R&D and OCaml Labs.
