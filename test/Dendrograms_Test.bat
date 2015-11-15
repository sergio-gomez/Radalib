@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Dendrograms_Test.adb
del /q *.ali
del /q *.o
echo -----------
Dendrograms_Test.exe
echo -----------
del Dendrograms_Test.exe
pause
