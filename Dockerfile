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
        m4              \
        libshp-dev      \
        libplplot-dev   \
        libopenblas-dev \
        liblapacke-dev  \
        libpcre3-dev    # LT4LA

# Permissions - recursive takes waaaay too long
RUN chown opam:opam $HOME $HOME/*
WORKDIR $HOME

# 0) 'opam init -y' runs fine on Travis CI, but not on my Docker CE install.
# 1) Opam 2.0 RC now includes sandboxing using bwrap. Leads to this error:
#    bwrap: No permissions to creating new namespace, likely because the kernel does not allow
#    non-privileged user namespaces. On e.g. debian this can be enabled with
#    'sysctl kernel.unprivileged_userns_clone=1'.
# 2) Setting 'sysctl kernel.unprivileged_userns_clone=1' as suggested does not work.
# 3) This is because the default Docker profile blocks many system calls,
#    including the one required, 'clone' (docs.docker.com/engine/security/seccomp
#    /#significant-syscalls-blocked-by-the-default-profile).
# 4) Running 'docker run --security-opt seccomp=unconfined' (I'm not going to
#    set up my own security profile for this project) runs into
#    'bwrap: Failed to make / slave: Permission denied'
# 5) Hence --disable-sandboxing
RUN opam init -y --bare --disable-sandboxing && opam switch create 4.06.1
# OCaml Packages Used
RUN opam install -y           \
        alcotest              \
        ctypes                \
        dune                  \
        configurator          \
        "eigen=0.0.5"         \
        ocaml-compiler-libs   \
        "ocamlmod=0.0.9"      \
        plplot                \
        utop                  \
        # LT4LA               \
        base                  \
        lambda-term           \
        lwt lwt_ppx           \
        menhir                \
        ppx_driver            \
        ppx_expect            \
        ppx_inline_test       \
        ppx_jane              \
        ppx_traverse          \
        ppx_traverse_builtins \
    && opam env

# Environment variables
ENV OPAM_SWITCH_PREFIX /home/opam/.opam/4.06.1
ENV CAML_LD_LIBRARY_PATH /home/opam/.opam/4.06.1/lib/stublibs:/home/opam/.opam/4.06.1/lib/ocaml/stublibs:/home/opam/.opam/4.06.1/lib/ocaml
ENV OCAML_TOPLEVEL_PATH /home/opam/.opam/4.06.1/lib/toplevel
ENV MANPATH /home/opam/.opam/default/man:/home/opam/.opam/4.06.1/man
ENV PATH /home/opam/.opam/4.06.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# Owl from source
ENV OWLPATH $HOME/owl
RUN git clone https://github.com/ryanrhymes/owl.git                                                               
RUN sed -i -- 's:/usr/local/opt/openblas/lib:/usr/lib/x86_64-linux-gnu:g' $OWLPATH/src/owl/config/configure.ml \
    && make -C $OWLPATH && make -C $OWLPATH install && make -C $OWLPATH clean

# Build LT4LA in the current (host) directory
ENV LT4LAPATH $HOME/lt4la
ADD --chown=opam:opam . $LT4LAPATH
WORKDIR $LT4LAPATH
RUN sed -i -- 's~(name runtest)~& (locks (/home/opam/owl))~g' test/jbuild old/test/jbuild
RUN dune runtest --display=short && dune build bin/repl.exe
ENTRYPOINT /bin/bash
