@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Communities_Detection.adb
del /q *.ali
del /q *.o
echo -----------
Communities_Detection.exe  v  WN el-lr-ll-srfr-trfr       5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
Communities_Detection.exe  v  WN rt-llt-ert+srt.ft!trfr  10  test-dolphins.net      test-dolphins-lol.txt
echo -----------
del Communities_Detection.exe
pause
