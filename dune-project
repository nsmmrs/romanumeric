(lang dune 3.0)
(name romanumeric)
(source (github nsmmrs/romanumeric))
(license MIT)
(authors "Noah Summers")
(maintainers "Noah Summers")

(using mdx 0.1)

(generate_opam_files true)

(package
 (name romanumeric)
 (synopsis "A library for decoding and encoding Roman numerals")
 (depends
  (ocaml (>= 4.06.0))
  (containers (>= 3.0))
  (alcotest :with-test)
  (mdx :with-test)
  (odoc :with-doc)
))
