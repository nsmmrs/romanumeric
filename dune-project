(lang dune 2.8)
(name romanum)
(source (github noahsummers/romanum))
(license MIT)
(authors "Noah Summers")
(maintainers "Noah Summers")

(using mdx 0.1)

(generate_opam_files true)

(package
 (name romanum)
 (synopsis "A library and CLI for decoding and encoding Roman numerals")
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 2.0))
  (alcotest :with-test)
  (mdx :with-test)
  (ppx_inline_test :with-test)
  (odoc :with-doc)
  cmdliner
  logs
  fmt))
