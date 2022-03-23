#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "otfm" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/otfm.mllib";
       Pkg.bin ~cond:cmdliner "test/otftrip";
       Pkg.doc "test/examples.ml";
       Pkg.test "test/examples"; ]
