@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Compare_Partitions.adb
del /q *.ali
del /q *.o
echo -----------
Compare_Partitions.exe  test-zachary-lol1.txt  test-zachary-lol2.txt  4
Compare_Partitions.exe  test-zachary-lol1.clu  test-zachary-lol2.txt  test-zachary-ct.txt  4
Compare_Partitions.exe  test-zachary-lol1.clu  test-zachary-lols.txt  test-zachary-ctsv.txt  V  4
Compare_Partitions.exe  test-zachary-lol1.clu  test-zachary-lols.txt  test-zachary-ctst.txt  T  4
echo -----------
del Compare_Partitions.exe
pause
