#!/bin/bash

cd ..

LIST=`ls */*.ad? | wc`
ADA_FILES=`echo $LIST | cut -f 1 -d ' '`

LIST=`ls */* | wc`
ALL_FILES=`echo $LIST | cut -f 1 -d ' '`

SIZE=`wc */*ad? | tail -1`
LINES=`echo $SIZE | cut -f 1 -d ' '`

echo ""
echo "Code   :   "$ADA_FILES" Ada files"
echo "Files  :   "$ALL_FILES" files"
echo "Source : "$LINES" lines of Ada code"
echo ""
