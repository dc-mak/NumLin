# Dockerfile to build LT4LA for DEVELOPMENT
# (I need to read up more on Docker for minimal deployments)
# Based on ryanrhymes/owl Dockerfile
# By Dhruv Makwana <dcm41@cam.ac.uk>,
#    Liang Wang <liang.wang@cl.cam.ac.uk>
############################################################

FROM ocaml/opam2:ubuntu-16.04-opam
MAINTAINER Dhruv Makwana

# OS Prerequisites
# lipcre3-dev used by patdiff used by expect_tests
RUN sudo apt-get update        \
    && sudo apt-get -y install \
        m4                \
        libshp-dev        \
        libplplot-dev     \
        libgsl-dev        \
        libopenblas-dev   \
        liblapacke-dev    \
        # LT4LA           \
        libpcre3-dev

# Permissions - recursive takes waaaay too long
RUN chown opam:opam $HOME $HOME/*
WORKDIR $HOME

# OCaml Packages Used
RUN opam init -y
RUN opam install -y           \
        oasis                 \
        "ocamlmod=0.0.9"      \
        jbuilder              \
        ocaml-compiler-libs   \
        ctypes                \
        utop                  \
        plplot                \
        "gsl=1.20.0"          \
        # LT4LA               \
        base                  \
        ppx_driver            \
        ppx_expect            \
        ppx_inline_test       \
        ppx_jane              \
        ppx_traverse          \
        ppx_traverse_builtins \
        menhir                \
        lambda-term           \
        lwt lwt_ppx           \
    && opam env

# Environment variables
ENV OPAM_SWITCH_PREFIX /home/opam/.opam/default
ENV CAML_LD_LIBRARY_PATH /home/opam/.opam/default/lib/stublibs:/home/opam/.opam/default/lib/ocaml/stublibs:/home/opam/.opam/default/lib/ocaml
ENV OCAML_TOPLEVEL_PATH /home/opam/.opam/default/lib/toplevel
ENV MANPATH /home/opam/.opam/default/man
ENV PATH /home/opam/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH

# Eigen from source
ENV EIGENPATH $HOME/eigen
RUN git clone https://github.com/ryanrhymes/eigen.git                                           \
    # Owl: FIXME                                                                                \
    && sed -i -- 's/-flto/ /g' $EIGENPATH/lib/Makefile                                          \
    # Owl: FIXME                                                                                \
    && sed -i -- 's/-flto/ /g' $EIGENPATH/_oasis                                                \
    && make -C $EIGENPATH oasis                                                                 \
    # LT4LA: Hack - needs to copy to /usr/local/bin                                             \
    && sudo sh -c 'PATH='"$PATH"' && make -C '"$EIGENPATH"' && make -C'"$EIGENPATH"' install'

# Owl from source
ENV OWLPATH $HOME/owl
RUN git clone https://github.com/ryanrhymes/owl.git                            \
    # Owl: FIXME (hacking ... needs to be fixed)                               \
    && sed -i -- 's/-lopenblas/-lopenblas -llapacke/g' $OWLPATH/src/owl/jbuild \
    && make -C $OWLPATH                                                        \
    && make -C $OWLPATH install

# Build LT4LA in the current (host) directory
ENV LT4LAPATH $HOME/lt4la
ADD --chown=opam:opam . $LT4LAPATH
WORKDIR $LT4LAPATH
RUN sed -i -- 's~(name runtest)~& (locks (../dot_owl_dir))~g' test/jbuild oldtest/jbuild
RUN jbuilder runtest --dev --display=short \
    && jbuilder build bin/repl.exe
ENTRYPOINT /bin/bash
