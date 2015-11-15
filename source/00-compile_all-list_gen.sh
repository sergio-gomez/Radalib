#!/bin/sh

FN=00-compile_all-list.txt

rm -f $FN

for i in *.adb ; do
  echo $i | sed "s./.\\\.g" >> $FN
  echo -n .
done

for i in *.ads ; do
  ADB=`echo $i | sed "s/ad.$/adb/g"`
  if [ ! -f $ADB ] ; then
    echo $i | sed "s./.\\\.g"  >> $FN
    echo -n .
  fi
done
