(lang dune 2.8)

(name h1)

(authors "Anurag Soni")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(generate_opam_files true)

(documentation "https://anuragsoni.github.io/h1")

(source
 (github anuragsoni/h1))

(license MIT)

(package
 (name bytebuffer)
 (synopsis "Extensible buffers built on top of bigarrays")
 (depends
  base_bigstring
  (alcotest :with-test)
  (ocaml
   (>= 4.11.0))))

(package
 (name h1-types)
 (synopsis "HTTP 1.1 types")
 (depends
  fmt
  (ocaml
   (>= 4.11.0))))

(package
 (name h1-parser)
 (synopsis "Parser for HTTP 1.1")
 (depends
  (h1-types
   (= :version))
  base_bigstring
  (base :with-test)
  (ppx_sexp_conv :with-test)
  (ppx_compare :with-test)
  (ppx_here :with-test)
  (base_quickcheck :with-test)
  (ppx_assert :with-test)))

(package
 (name h1)
 (synopsis "HTTP 1.1 server state machine")
 (depends
  (h1-types
   (= :version))
  (h1-parser
   (= :version))
  base_bigstring))

(package
 (name h1-async)
 (synopsis "Async based http server")
 (depends
  (h1
   (= :version))
  core
  async
  (bytebuffer
   (= :version))
  ppx_jane))
