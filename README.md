# ocaml-angstrom

Angstrom is a parser combinator library built for speed and memory efficiency.

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install angstrom
```

## Usage

```ocaml
open Angstrom

let comma  = char ','
let value  = take_while (fun c -> c <> ',' && c <> '\n' && c <> '\r')
let record = sep_by value comma in
let csv =
  many (record <* end_of_line)

(* : string -> string list list *)
let parse_csv input =
  parse_only csv input
```

## Development

To install development versions of the library, pin the package from the root
of the repository:

```bash
opam pin add .
```

You can install the latest changes by commiting them to the local git
repository and running:

```bash
opam upgrade angstrom
```

For building and running the tests during development, you will need to install
the `alcotest` package and reconfigure the build process to enable tests:

```bash
opam install alcotest
./configure --enable-tests
make && make test
```

## License

BSD3, see LICENSE file for its text.
