(lang dune 3.16)
(name angstrom)

(formatting (enabled_for dune))
(generate_opam_files true)

(source (github inhabitedtype/angstrom))
(license "BSD-3-clause")
(authors "Spiros Eliopoulos <spiros@inhabitedtype.com>")
(maintainers "Spiros Eliopoulos <spiros@inhabitedtype.com>")

(package
 (name angstrom)
 (synopsis "Parser combinators built for speed and memory-efficiency")
 (description
"\| Angstrom is a parser-combinator library that makes it easy to write
"\| efficient, expressive, and reusable parsers suitable for
"\| high-performance applications. It exposes monadic and applicative
"\| interfaces for composition, and supports incremental input through
"\| buffered and unbuffered interfaces. Both interfaces give the user
"\| total control over the blocking behavior of their application, with
"\| the unbuffered interface enabling zero-copy IO. Parsers are
"\| backtracking by default and support unbounded lookahead.
 )
 (depends
  (ocaml (>= "4.04.0"))
  (alcotest (and (>= "0.8.1") :with-test))
  (bigstringaf (>= "0.10.0"))
  (ppx_let (and (>= "v0.14.0") :with-test))
  (core_bench :with-test)
  (core_unix (and (>= "v0.15.0") :with-test))
  (ocaml-syntax-shims :build))) 

(package
 (name angstrom-unix)
 (synopsis "Unix support for Angstrom")
 (depends
  (ocaml (>= "4.03.0"))
  (angstrom (= :version))
  base-unix))

(package
 (name angstrom-lwt-unix)
 (synopsis "Lwt_unix support for Angstrom")
 (depends
  (ocaml (>= "4.03.0"))
  (angstrom (= :version))
  lwt
  base-unix
  (core_bench :with-test)))

(package
 (name angstrom-async)
 (synopsis "Async support for Angstrom")
 (depends
  (ocaml (>= "4.04.1"))
  (angstrom (= :version))
  (async (>= "v0.10.0"))
  (core_bench :with-test)
  (core_unix (and (>= "v0.15.0") :with-test))))
