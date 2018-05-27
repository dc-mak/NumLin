{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {

    name = "lt4la";

    buildInputs = [
        (texlive.combine {
            inherit (texlive)
            ucs
            ec
            caption
            sectsty
            tocloft
            titlesec
            minitoc
            cm-super
            scheme-basic
            booktabs
            bussproofs
            charter
            epstopdf
            etoolbox # minted
            fancyvrb # minted
            float # minted
            framed
            graphics
            hyperref
            ifplatform # minted
            latexmk
            lineno # minted
            ms # pgfplots
            minted
            mdwtools # syntax
            parskip
            pgf
            pgfplots
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
        ghostscript
        which
    ];

    src = ./.;

    installPhase = ''
      mkdir -p $out
      cp *.pdf $out
    '';
}

# vim: set sw=4 sts=4 ft=conf:
