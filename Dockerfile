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
# 5) The Docker image I'm pulling from does not include '--show-default-opamrc'
#    flag yet so I can't modify that. Even if I could...
# 6) Creating one without bwrap/sandboxing/wrap-* configurations and using
#    'opam init -y --config=~/.opamrc' didn't work for me.
# 7) Neither did doing the same, but for .opam/config.
# 8) github.com/ocaml/opam/blob/bc0e83df7dcc71e1a14396d868883d18cef6df32/doc/pages/FAQ.md#--why-does-opam-require-bwrap
# 9) An alternative, much more sound way of doing the following would be this
#    github.com/OCamlPro/opam/blob/b5834a944842890d642a801976b7cdd42a56cc64/.travis-ci.sh#L43
#    cat <<EOF >>~/.opamrc
#    wrap-build-commands: []
#    wrap-install-commands: []
#    wrap-remove-commands: []
#    EOF
RUN opam init -y || \
    sed -i -- 's/wrap-\(.*\)-commands: \[.*\]$/wrap-\1-commands: []/g' ~/.opam/config \
    && opam switch create 4.06.1
# OCaml Packages Used
RUN opam install -y           \
        oasis                 \
        "ocamlmod=0.0.9"      \
        jbuilder              \
        ocaml-compiler-libs   \
        ctypes                \
        utop                  \
        plplot                \
        "gsl=1.20.0"          \
        alcotest              \
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
ENV OPAM_SWITCH_PREFIX /home/opam/.opam/4.06.1
ENV CAML_LD_LIBRARY_PATH /home/opam/.opam/4.06.1/lib/stublibs:/home/opam/.opam/4.06.1/lib/ocaml/stublibs:/home/opam/.opam/4.06.1/lib/ocaml
ENV OCAML_TOPLEVEL_PATH /home/opam/.opam/4.06.1/lib/toplevel
ENV MANPATH /home/opam/.opam/default/man:/home/opam/.opam/4.06.1/man
ENV PATH /home/opam/.opam/4.06.1/bin:/home/opam/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH

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
RUN git clone https://github.com/ryanrhymes/owl.git                                                   \
    # Owl: FIXME (hacking ... needs to be fixed)                                                      \
    && sed -i -- 's/-lopenblas/-lopenblas -llapacke/g' $OWLPATH/src/owl/jbuild                        \
    && sed -i -- 's:/usr/local/opt/openblas/lib:/usr/lib/x86_64-linux-gnu/:g' $OWLPATH/src/owl/jbuild \
    && make -C $OWLPATH                                                                               \
    && make -C $OWLPATH install

# Build LT4LA in the current (host) directory
ENV LT4LAPATH $HOME/lt4la
ADD --chown=opam:opam . $LT4LAPATH
WORKDIR $LT4LAPATH
RUN sed -i -- 's~(name runtest)~& (locks (../dot_owl_dir))~g' test/jbuild oldtest/jbuild
RUN jbuilder runtest --dev --display=short \
    && jbuilder build bin/repl.exe
ENTRYPOINT /bin/bash
