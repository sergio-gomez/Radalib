#!/bin/sh

for i in *.bat ; do
  echo ""
  echo ""
  echo "======================================="
  echo "=== "$i
  echo "======================================="
  echo " " | ./$i
done