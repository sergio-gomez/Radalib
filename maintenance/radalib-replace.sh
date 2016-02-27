#!/bin/bash

YEAR=`date "+%Y"`
TMPFILE=_tmp.tmp

cd ..

for i in */*.ads; do
  echo $i
  # sed -e "s/Copyright (c) ..../Copyright (c) ${YEAR}/g" $i > $TMPFILE
  # sed -e "s/[.]All/.all/g" $i > $TMPFILE
  # mv -f $TMPFILE $i
done

for i in */*.adb; do
  echo $i
  # sed -e "s/Copyright (c) ..../Copyright (c) ${YEAR}/g" $i > $TMPFILE
  # sed -e "s/[.]All/.all/g" $i > $TMPFILE
  # mv -f $TMPFILE $i
done
