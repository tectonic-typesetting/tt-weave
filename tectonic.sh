#! /bin/bash

set -xeuo pipefail

cd template
rm -rf build/default ../app/src/ttw
tectonic -X build "$@"
mkdir -p ../app/src/ttw/
cp build/default/* ../app/src/ttw/
