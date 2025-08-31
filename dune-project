(lang dune 3.17)

(name rt)

(source
 (github bp3r/ocaml-rt))

(authors "bp3r")

(package
 (name rt)
 (synopsis "A simple parallel raytracer written in a (conservative subset of) multicore OCaml.")
 (depends
   (ocaml (>= 5.0))
   domainslib
   yojson-five))
