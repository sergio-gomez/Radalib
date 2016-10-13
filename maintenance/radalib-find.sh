#!/bin/bash

cd ..

for i in */*.ad?; do
  echo $i
  grep -i "Floor" $i
done
