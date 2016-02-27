#!/bin/sh

export ADA_INCLUDE_PATH=$RADALIB_ROOT/source
export ADA_OBJECTS_PATH=$RADALIB_ROOT/compiled

for i in *.adb ; do
  iexe=`echo $i | sed "s/[.]adb$/.exe/g"`
  #gnatmake -g -o $iexe $i
  gnatmake -O2 -o $iexe $i
  echo "---"
done

rm -f *.ali *.o b~*.ad?
