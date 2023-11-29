(lang dune 2.8)

(name ocamlformat_preview)

(generate_opam_files true)

(authors "Guillaume Petiot <guillaume@tarides.com>")

(maintainers "Guillaume Petiot <guillaume@tarides.com>")

(source
 (github gpetiot/ocamlformat-preview))

(package
 (name ocamlformat_preview)
 (synopsis "OCamlFormat in the browser")
 (description "Preview of the auto-formatter OCamlFormat in the browser.")
 (depends
  (ocaml
   (>= 4.08))
  dune
  js_of_ocaml
  (ocaml-version
   (>= 3.3.0))
  (ocamlformat-lib
   (>= 0.25.1))))
