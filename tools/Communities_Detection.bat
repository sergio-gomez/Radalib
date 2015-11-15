@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Communities_Detection.adb
del /q *.ali
del /q *.o
echo -----------
communities_detection.exe  v  WN erfrtrfr   5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
communities_detection.exe  v  WN ebfbtrfr  10  test-dolphins.net  test-dolphins-lol.txt
echo -----------
del Communities_Detection.exe
pause
