{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {

    name = "lt4la";

    buildInputs = [
        (texlive.combine {
            inherit (texlive)
            scheme-basic
            booktabs
            charter
            etoolbox # minted
            fancyvrb # minted
            float # minted
            framed
            hyperref
            ifplatform # minted
            latexmk
            lineno # minted
            minted
            parskip
            setspace
            supertabular
            texcount
            tocbibind
            xcolor
            xstring # minted
            ;
        })
        pythonPackages.pygments
        ocamlPackages.ott
    ];

    src = ./.;

    installPhase = ''
      mkdir -p $out
      cp *.pdf $out
    '';
}

# vim: set sw=4 sts=4 ft=conf:
