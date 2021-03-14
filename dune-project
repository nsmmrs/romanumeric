(lang dune 2.0)
(name romanum)
(source (github noahsummers/romanum))
(license MIT)
(authors "Noah Summers")
(maintainers "Noah Summers")

(generate_opam_files true)

(package
 (name romanum)
 (synopsis "A library and CLI for the decoding and encoding of Roman numerals")
 (description "\
A library and CLI for the decoding and encoding of Roman numerals
")
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 2.0))
  (alcotest :with-test)
  (odoc :with-doc)
  cmdliner
  logs
  fmt))
