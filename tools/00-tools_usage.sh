#!/bin/bash

USAGE_FILE=00-tools_usage.txt

rm -f $USAGE_FILE

NUM_ADB=`ls -1 *.adb 2> /dev/null | wc -l`
NUM_EXE=`ls -1 *.exe 2> /dev/null | wc -l`
if [ "$NUM_ADB" != "$NUM_EXE" ] ; then
  echo "Compiling:"
  echo "---"
  ./00-compile_all.sh
fi

echo -n "Generating usage file: "
for i in *.exe; do
  echo "" >> $USAGE_FILE
  echo "" >> $USAGE_FILE
  echo "===================================================================" >> $USAGE_FILE
  printf "== %-61s ==\n" $i >> $USAGE_FILE
  ./$i | tail -n +8 | sed "s/^Usage:.*radalib.tools./Usage:  /g" >> $USAGE_FILE
  echo -n "."
done
echo ""
echo "---"
