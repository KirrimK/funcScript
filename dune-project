(lang dune 2.9)
(using menhir 2.1)

(name funcScript)

(generate_opam_files true)

(source
 (github KirrimK/funcScript))

(authors "Rémy B")

(maintainers "Rémy B")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name funcScript)
 (synopsis "A simplistic embeddable purely functionnal scripting language")
 (description "A longer description")
 (depends
    ocaml
    dune
    (menhir (>= 2.1)))
 (tags
  (functional scripting embedded language)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
