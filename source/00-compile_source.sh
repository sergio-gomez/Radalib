#!/bin/sh

echo "Compiling source..."

for i in *.adb ; do
  gnatmake -O2 -D $RADALIB_ROOT/compiled $i
  #gnatmake -g -D $RADALIB_ROOT/compiled $i
done

for i in *.ads ; do
  ADB=`echo $i | sed "s/ad.$/adb/g"`
  if [ ! -f $ADB ] ; then
    gnatmake -O2 -D $RADALIB_ROOT/compiled $i
    #gnatmake -g -D $RADALIB_ROOT/compiled $i
  fi
done

echo "------"
