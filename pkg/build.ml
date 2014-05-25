#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let () = 
  Pkg.describe "otfm" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/otfm";
    Pkg.bin ~auto:true "test/otftrip";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/examples.ml"; ]
