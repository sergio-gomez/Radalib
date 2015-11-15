#!/bin/bash

# Check OS
THE_OS=`echo $OS|tr A-Z a-z`
if [ "$THE_OS" == "windows_nt" ] ; then
  echo "No installation under Windows!"
  exit
fi

# Unzip
unzip radalib.zip 2> /dev/null

# Convert
cp radalib/maintenance/radalib-win2mac.sh .
chmod 755 radalib-win2mac.sh
./radalib-win2mac.sh 2> /dev/null

# Compile
rm radalib/compiled/[a-z]*
cd radalib/source
chmod 755 00-compile_all-mac.sh
./00-compile_all-mac.sh
