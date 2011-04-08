#!/bin/bash
for f in examples/*.j; do
  echo "==$f"
  rm -f out
  touch out
  for i in `seq 0 19`; do
    ./gen.byte $i | ./interp.byte $f 2>> out
  done
  sort -u out
done
