#!/bin/sh

set -ex

# update cabal file
hpack
# lint
hlint --git
# format
git ls-files '*.hs' '*.lhs' | xargs stylish-haskell -i
# pedantic build
stack --verbosity=warn build --pedantic --fast
