#!/bin/bash

VERS=`date "+%Y%m%d-%H%M%S"`
VERSNAME=radalib-$VERS
VERSFILE=$VERSNAME.txt
README=radalib-README.txt
READMETMP=radalib-README.tmp

cd ..

rm -f radalib-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9].txt
echo $VERSNAME > $VERSFILE

sed -e "s/^== Radalib .*==$/== Radalib $VERS                                       ==/g" $README > $READMETMP
mv $READMETMP $README
