(lang dune 2.0)
(name angstrom)

(license BSD-3-Clause)
(authors "Spiros Eliopoulos <spiros@inhabitedtype.com>")
(maintainers "Spiros Eliopoulos <spiros@inhabitedtype.com>")
(source
 (github inhabitedtype/angstrom))
(bug_reports "https://github.com/inhabitedtype/angstrom/issues")
(homepage "https://github.com/inhabitedtype/angstrom")

(generate_opam_files true)

(package
 (name angstrom)
 (synopsis "Parser combinators built for speed and memory-efficiency")
 (description "\
Angstrom is a parser-combinator library that makes it easy to write efficient,
expressive, and reusable parsers suitable for high-performance applications. It
exposes monadic and applicative interfaces for composition, and supports
incremental input through buffered and unbuffered interfaces. Both interfaces
give the user total control over the blocking behavior of their application,
with the unbuffered interface enabling zero-copy IO. Parsers are backtracking by
default and support unbounded lookahead.
")
 (depends
  (alcotest
   (and
    :with-test
    (>= 0.8.1)))
  (ocaml
   (>= 4.04))
  (ocaml-syntax-shims :build)
  (odoc :with-doc)
  (ppx_let
   (and
    :with-test
    (>= 0.14)))
  bigstringaf
  result))

(package
 (name angstrom-async)
 (synopsis "Async support for Angstrom")
 (depends
  (ocaml
   (>= 4.04))
  (odoc :with-doc)
  angstrom
  async))

(package
 (name angstrom-lwt-unix)
 (synopsis "Lwt_unix support for Angstrom")
 (depends
  (ocaml
   (>= 4.04))
  (odoc :with-doc)
  angstrom
  base-unix
  lwt))

(package
 (name angstrom-unix)
 (synopsis "Unix support for Angstrom")
 (depends
  (ocaml
   (>= 4.04))
  (odoc :with-doc)
  angstrom
  base-unix))
