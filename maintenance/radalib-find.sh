#!/bin/bash

cd ..

for i in */*.ad?; do
  echo $i
  grep -i "2018" $i
done
