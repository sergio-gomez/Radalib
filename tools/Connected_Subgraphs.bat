@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Connected_Subgraphs.adb
del /q *.ali
del /q *.o
echo -----------
Connected_Subgraphs.exe  test-dolphins
Connected_Subgraphs.exe  test-dolphins  S
Connected_Subgraphs.exe  test-disconnected  1
Connected_Subgraphs.exe  test-disconnected  strong  1
Connected_Subgraphs.exe  test-disconnected  strong  5
Connected_Subgraphs.exe  test-disconnected  S
echo -----------
del Connected_Subgraphs.exe
pause
