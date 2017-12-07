@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
rem ulimit -s unlimited
gnatmake -O2 Mesoscales_Detection.adb -largs -Wl,--stack=500000000
del /q *.ali
del /q *.o
echo -----------
rem Mesoscales_Detection.exe  test-zachary_unwh.net  WS  trfr  10  100  1.0
rem Mesoscales_Detection.exe  test-dolphins.net      WS  trfr  10  100  1.0
Mesoscales_Detection.exe  test-zachary_unwh.net  WS  rfr  10  100  1.0
Mesoscales_Detection.exe  test-dolphins.net      WS  rfr  10  100  2.0
echo -----------
del Mesoscales_Detection.exe
pause
