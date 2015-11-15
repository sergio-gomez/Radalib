#!/bin/sh

export ADA_INCLUDE_PATH=$RADALIB_ROOT/source
export ADA_OBJECTS_PATH=$RADALIB_ROOT/compiled

for i in *.adb ; do
  gnatmake -O2 $i
  #gnatmake -g $i
  echo "---"
done

rm -f *.ali *.o b~*.ad?
