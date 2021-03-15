#!/bin/bash

cd ..

for i in */*.ad?; do
  echo $i
  grep -i "Bipartite" $i
done
