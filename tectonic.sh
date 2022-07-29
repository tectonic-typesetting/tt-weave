#! /bin/bash

set -xeuo pipefail

(cd ~/sw/tex/tectonic && cargo build --release)
cd template
~/sw/tex/tectonic/target/release/tectonic -X build
cp build/default/* ../app/src/
