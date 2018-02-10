#!/bin/sh

for i in [a-zA-Z]*.bat [a-zA-Z]*.sh ; do
  if [ -f "$i" ] ; then
    echo ""
    echo ""
    echo "======================================="
    echo "=== "$i
    echo "======================================="
    ./$i
  fi
done
