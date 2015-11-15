@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Communities_Network.adb
del *.ali
del *.o
echo -----------
Communities_Network.exe  test-zachary_unwh.net  test-zachary-lol1.clu  test-zachary-lol1.net  Integer
Communities_Network.exe  test-zachary_unwh.net  test-zachary-lol2.txt  4  test-zachary-lol2.net  Float  3
echo -----------
del Communities_Network.exe
pause
