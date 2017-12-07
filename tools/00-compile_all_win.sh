#!/bin/sh

export ADA_INCLUDE_PATH=$RADALIB_ROOT/source
export ADA_OBJECTS_PATH=$RADALIB_ROOT/compiled

for i in *.adb ; do
  iexe=`echo $i | sed "s/[.]adb$/.exe/g"`
  #gnatmake -g -o $iexe $i
  gnatmake -O2 -o $iexe $i
  echo "---"
done

export LARGE_STACK="Communities_Detection.adb Mesoscales_Detection.adb Network_Properties.adb"

for i in $LARGE_STACK ; do
  iexe=`echo $i | sed "s/[.]adb$/.exe/g"`
  #gnatmake -g -o $iexe $i
  rm -f $iexe
  gnatmake -O2 -o $iexe $i -largs -Wl,--stack=500000000
  echo "---"
done

rm -f *.ali *.o b~*.ad?
