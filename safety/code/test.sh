#!/bin/sh
rm -f .allout
for (( i=0; i<20; ++i )); do
  { ./gen.byte $i | OCAMLRUNPARAM=b ./interp.byte examples/*.j 2> .out; } || { cat .out; echo "seed $i"; exit 1; }
  cat .out >> .allout
done
sort -u .allout
