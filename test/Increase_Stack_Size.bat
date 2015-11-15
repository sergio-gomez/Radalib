@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
rem ulimit -s unlimited
gnatmake -O2 Increase_Stack_Size.adb -largs -Wl,--stack=500000000
del /q *.ali
del /q *.o
echo -----------
Increase_Stack_Size.exe
echo -----------
del Increase_Stack_Size.exe
pause
