#!/bin/bash

USAGE_FILE=00-tools_usage.txt

rm -f $USAGE_FILE

./00-compile_all.sh

for i in *.exe; do
  echo "" >> $USAGE_FILE
  echo "" >> $USAGE_FILE
  echo "===================================================================" >> $USAGE_FILE
  printf "== %-61s ==\n" $i >> $USAGE_FILE
  ./$i | tail -n +8 | sed "s/^Usage:.*radalib.tools./Usage:  /g" >> $USAGE_FILE
done

./00-clear.sh
