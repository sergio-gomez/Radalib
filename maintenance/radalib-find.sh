#!/bin/bash

cd ..

for i in */*.ad?; do
  echo $i
  grep -i "Strong_Components" $i
done
