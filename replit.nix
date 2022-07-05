{ pkgs }: {
    deps = [
        pkgs.ocaml
        pkgs.dune_2
        pkgs.ocamlPackages.menhir
        pkgs.ocamlPackages.bisect_ppx
        pkgs.ocamlPackages.findlib
        pkgs.ocamlPackages.dune-build-info
    ];
}