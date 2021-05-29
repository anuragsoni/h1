(lang dune 2.8)

(using mdx 0.1)

(name h1)

(generate_opam_files true)

(package
 (name h1-types)
 (depends
  fmt
  (ocaml
   (>= 4.11.0))))

(package
 (name h1-parser)
 (depends
  (h1-types
   (= :version))
  bigstringaf
  (ppx_expect :with-test)
  (base :with-test)
  (stdio :with-test)
  (ppx_sexp_conv :with-test)
  (base_quickcheck :with-test)
  (ppx_assert :with-test)))

(package
 (name h1)
 (depends
  (h1-types
   (= :version))
  (h1-parser
   (= :version))
  bigstringaf))
