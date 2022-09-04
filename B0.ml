open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"
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
  let doc = "Prints OTF tables in human readable format." in
  let srcs = [test_src "otftrip.ml"] in
  let requires = [cmdliner; uutf; otfm] in
  let meta =
    B0_meta.empty
    |> B0_meta.(tag test)
    |> B0_meta.add B0_unit.Action.cwd `Scope_dir
  in
  B0_ocaml.exe "otftrip" ~public:true ~doc ~meta ~srcs ~requires

let example =
  let srcs = [test_src "examples.ml"] in
  let requires = [otfm] in
  let meta = B0_meta.empty |> B0_meta.tag B0_meta.test in
  let doc = "Sample code" in
  B0_ocaml.exe "examples" ~doc ~meta ~srcs ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The otfm programmers"]
    |> B0_meta.(add maintainers)
      ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/otfm"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/otfm/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/otfm.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/otfm/issues"
    |> B0_meta.(add description_tags)
      ["OpenType"; "ttf"; "font"; "decoder"; "graphics"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uutf", {|>= "1.0.0"|};
      ]
    |> B0_meta.add B0_opam.depopts [ "cmdliner", ""]
    |> B0_meta.add B0_opam.conflicts [ "cmdliner", {|< "1.1.0"|} ]
  in
  B0_pack.make "default" ~doc:"otfm package" ~meta ~locked:true @@
  B0_unit.list ()
