#!/bin/sh

FN_IN=test-modularities_join.txt
FN_AUX=test-modularities_join-aux.txt
FN_OUT=test-modularities_join-check.txt

rm -f $FN_OUT

grep "(Dq)" $FN_IN > $FN_AUX
cat $FN_AUX | sed -e "s/^.*(Dq) = //g" -e "s/ =.*$//g" > $FN_OUT
mv $FN_OUT $FN_AUX

ko=false
while read -r line ; do
  read -r line2
  if [ "$line" == "$line2" ] ; then
    echo "OK: "$line
  else
    echo ">>> KO: "$line"  <>  "$line2
    ko=true
  fi
done < $FN_AUX > $FN_OUT

if [ "$ko" == "true" ] ; then
  echo "ERROR: some modularity values do not match"
else
  echo "INFO: all modularity values match"
fi

rm -f $FN_AUX
