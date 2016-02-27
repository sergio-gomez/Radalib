#!/bin/bash

# Check OS
THE_OS=`echo $OS|tr A-Z a-z`
if [ "$THE_OS" == "windows_nt" ] ; then
  echo "No installation under Windows!"
  exit
fi

# Unzip
echo
echo "==============="
echo "== UNZIPPING =="
echo "==============="
echo
unzip radalib.zip 2> /dev/null

# Convert
echo
echo "================"
echo "== CONVERTING =="
echo "================"
echo
cp radalib/maintenance/radalib-win2linux.sh .
chmod 755 radalib-win2linux.sh
./radalib-win2linux.sh 2> /dev/null

# Compile
echo
echo "==============="
echo "== COMPILING =="
echo "==============="
echo
rm radalib/compiled/[a-z]*
cd radalib/source
chmod 755 00-compile_source.sh
./00-compile_source.sh
