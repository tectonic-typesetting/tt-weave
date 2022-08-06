#! /bin/bash

set -xeuo pipefail

(cd ~/sw/tex/tectonic && cargo build --release)
cd template
rm -rf build/default ../app/src/ttw
~/sw/tex/tectonic/target/release/tectonic -X build
mkdir -p ../app/src/ttw/
cp build/default/* ../app/src/ttw/
