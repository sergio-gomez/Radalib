@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
rem ulimit -s unlimited
gnatmake -O2 Communities_Detection.adb -largs -Wl,--stack=500000000
del /q *.ali
del /q *.o
echo -----------
Communities_Detection.exe  v  WN erfrtrfr   5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
Communities_Detection.exe  v  WN ebfbtrfr  10  test-dolphins.net  test-dolphins-lol.txt
echo -----------
del Communities_Detection.exe
pause
