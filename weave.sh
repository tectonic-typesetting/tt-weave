#! /bin/bash

if [ -z "$1" ] ; then
    input=weave.web
else
    input="$1"
fi

echo "Processing $input ..."
exec cargo run --release -- "$input" >template/src/index.tex
