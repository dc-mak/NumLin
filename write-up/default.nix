{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {

    name = "numlin";

    buildInputs = [
        (texlive.combine {
            inherit (texlive)
            amsmath
            academicons
            booktabs
            bussproofs
            caption
            charter
            cleveref
            cm-super
            comment
            ec
            epstopdf
            etoolbox # minted
            fancyvrb # minted
            float # minted
            framed
            graphics
            hyperref
            ifplatform # minted
            latexmk
            latexpand
            lastpage
            lineno # minted
            listings
            mdwtools # syntax
            microtype
            minitoc
            minted
            ms # pgfplots
            multirow
            parskip
            pgf
            pgfplots
            scheme-basic
            sectsty
            setspace
            supertabular
            soul
            texcount
            titlesec
            threeparttable
            tocbibind
            tocloft
            ucs
            xcolor
            xstring # minted
            ;
        })
        ghostscript
        ocamlPackages.ott
        pythonPackages.pygments
        which
    ];

    src = ./.;

    installPhase = ''
      mkdir -p $out
      cp *.pdf $out
    '';
}

# vim: set sw=4 sts=4 ft=conf:
