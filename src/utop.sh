#/usr/bin/env bash
# Because `jbuilder utop` crashes and burns
# Run from within src directory

set -euo pipefail
jbuilder build lt4la.a
jbuilder exec -- utop -init utop_init -I ./_build/default/src
