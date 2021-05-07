(lang dune 2.8)

(name h1)

(generate_opam_files true)

(package
 (name h1)
 (depends
  bigstringaf
  (ocaml
   (>= 4.11.0))
  sexplib0
  ppx_sexp_conv
  (ppx_expect :with-test)
  (base :with-test)
  (stdio :with-test)
  (ppx_custom_printf :with-test)))
