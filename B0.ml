open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let uutf = B0_ocaml.libname "uutf"
let otfm = B0_ocaml.libname "otfm"

(* Libraries *)

let otfm_lib =
  let srcs = Fpath.[ `Dir (v "src") ] in
  let requires = [uutf] in
  B0_ocaml.lib otfm ~doc:"Otfm library" ~srcs ~requires

(* Tests *)

let test_src f = `File (Fpath.v (Fmt.str "test/%s" f))

let otftrip =
  let srcs = [test_src "otftrip.ml"] in
  let requires = [ otfm ] in
  let meta =
    B0_meta.(empty |> tag test |>
             add B0_unit.Action.exec_cwd B0_unit.Action.scope_cwd)
  in
  let doc = "Prints OTF tables in human readable format." in
  B0_ocaml.exe "otftrip" ~doc ~meta ~srcs ~requires

let example =
  let srcs = [test_src "examples.ml"] in
  let requires = [ otfm ] in
  let meta = B0_meta.empty |> B0_meta.tag B0_meta.test in
  let doc = "Sample code" in
  B0_ocaml.exe "examples" ~doc ~meta ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The otfm programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/otfm"
    |> add online_doc "https://erratique.ch/software/otfm/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/otfm.git"
    |> add issues "https://github.com/dbuenzli/otfm/issues"
    |> add description_tags ["OpenType"; "ttf"; "font"; "decoder"; "graphics";
                             "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|>= build & >= "1.0.3"|};
        "uutf", {|>= "1.0.0"|};
      ]
  in
  B0_pack.v "default" ~doc:"otfm package" ~meta ~locked:true @@
  B0_unit.list ()
