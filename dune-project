(lang dune 3.6)

(name jenga)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Joydeep Mukherjee")

(maintainers "Joydeep Mukherjee")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jenga)
 (synopsis "A stack-oriented, concatenative programming language, for fun")
 (depends ocaml dune))

(using menhir 2.1)
