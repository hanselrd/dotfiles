(lang dune 3.23)

(name dotfiles)

(version 0.0.1-alpha)

(generate_opam_files true)

(source
 (github hanselrd/dotfiles))

(authors "Hansel De La Cruz <18725263+hanselrd@users.noreply.github.com>")

(maintainers "Hansel De La Cruz <18725263+hanselrd@users.noreply.github.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dotfiles)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml (>= 5.5.0))
  cmdliner
  dune-build-info
  eio
  eio_main
  fmt
  genspio
  logs
  ppx_deriving
  re)
 (tags
  ("add topics" "to describe" your project)))
