#!/bin/bash
LC_ALL=POSIX # so that * is expanded in the same order on all systems
rm -f .allout
for (( i=0; i<20; ++i )); do
  { ./gen.byte $i | OCAMLRUNPARAM=b ./interp.byte examples/*.j 2> .out; } || { cat .out; echo "seed $i"; exit 1; }
  cat .out >> .allout
done
sort -t : -k 1,2 .allout | uniq
