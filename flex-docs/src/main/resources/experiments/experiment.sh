#!/bin/bash

pys=(*.py)
size=${#pys[@]}

for py in "${!pys[@]}"; do
  f=${pys[$py]}
  python3 "$f"
  echo "($((py+1))/$size) $f Completed"
done
