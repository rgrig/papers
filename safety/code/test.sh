#!/bin/bash
for f in examples/*.j; do
  echo "==$f"
  rm -f out
  touch out
  for (( i = 0; i < 20; ++i )); do
    ./gen.byte $i | ./interp.byte $f 2>> out
  done
  sort -u out
done
