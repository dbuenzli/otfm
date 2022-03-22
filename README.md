Otfm — OpenType font decoder for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Otfm is an in-memory decoder for the OpenType font data format. It
provides low-level access to font tables and functions to decode some
of them.

Otfm is made of a single module and depends on [Uutf][uutf]. It is distributed 
under the ISC license.

[uutf]: https://erratique.ch/software/uutf
     
Homepage: <https://erratique.ch/software/otfm>

## Installation

Otfm can be installed with `opam`:

    opam install otfm

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation 

The documentation can be consulted [online][doc] or via `odig doc otfm`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/otfm/doc/
[ocaml-forum]: https://discuss.ocaml.org/

## Sample programs 

Sample programs are located in the `test` directory of the
distribution. They can be built with:

    topkg build --tests true 

- `otftrip.native`, among other things, reads an OpenType file and
  prints a human readable representation on `stdout`. Invoke with
  `--help` for more information.
