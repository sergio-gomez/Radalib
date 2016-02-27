#!/bin/bash


function convert_wd()
{
	# Ada files and Windows scripts to lowercase names
  for i in *.ad[bs] *.bat ; do
    FN=`echo $i | tr A-Z a-z | grep -v "[*]"`
    if [ "$FN" != "" ] ; then
      if [ "$i" != "$FN" ] ; then
        echo $i" -> "$FN
        mv $i $FN
      fi
    fi
  done

  # Convert *.bat -> *.sh
  for i in *.bat; do
    FN=`echo $i | sed "s/[.]bat$/.sh/g" | grep -v "[*]"`
    if [ "$FN" != "" ] ; then
      echo "Creating "$FN"..."
      PROG_ORI=`grep "del .*exe" $i | sed -e"s/\r$//g" -e"s/^del[ ]*//g" -e"s/.exe//g"`
      PROG_LC=`echo $PROG_ORI | tr A-Z a-z`

      grep -v "echo off" $i | \
        grep -v "^set ADA_" | \
        grep -v "pause" | \
        sed "s,^del /q,rm -f,g" | \
        sed "s,^del,rm -f,g" | \
        sed "s. -largs -Wl,--stack=500000000..g" | \
        sed "s,rem ulimit,ulimit,g" | \
        sed "s,^rem,#,g" | \
        sed "s,$PROG_ORI,$PROG_LC,g" | \
        sed "s,^$PROG_LC,./$PROG_LC,g" | \
        sed "s,\\\\,/,g" | \
        sed "s,adb,adb -o $PROG_LC.exe,g" | \
        tr -d '\r' > $FN
      chmod 755 $FN
      rm -f $i
    fi
  done

  # execution permission for all *.sh files
  chmod 755 *.sh
}

function convert_recursive()
{
  echo
  echo "-------"
  pwd
  echo "-------"

  convert_wd

  for folder in *; do
    if [ -d "$folder" ] ; then
      if [ -x "$folder" ] ; then
        cd "$folder"
        convert_recursive
        cd ..
      fi
    fi
  done
}

function convert_dir_recursive()
{
  HERE=`pwd`
  cd $1
  convert_recursive
  cd $HERE
}

function convert_dir()
{
  HERE=`pwd`
  cd $1

  echo
  echo "-------"
  pwd
  echo "-------"

  convert_wd

  cd $HERE
}

# Check OS
THE_OS=`echo $OS|tr A-Z a-z`
if [ "$THE_OS" == "windows_nt" ] ; then
  echo "No conversion under Windows!"
  exit
fi

# convert radalib
convert_dir_recursive  $RADALIB_ROOT/source
convert_dir_recursive  $RADALIB_ROOT/test
convert_dir_recursive  $RADALIB_ROOT/tools
chmod 755 $RADALIB_ROOT/compiled/*.sh
chmod 755 $RADALIB_ROOT/maintenance/*.sh

# convert other folders and/or subfolders
#convert_dir            afolder
#convert_dir_recursive  afolderandsubfolders

