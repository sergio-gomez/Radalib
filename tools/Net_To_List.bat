@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Net_To_List.adb
del /q *.ali
del /q *.o
echo -----------
Net_To_List.exe  test-net_toy_01.net  test-net_toy_01-list.txt
Net_To_List.exe  test-net_toy_02.net  test-net_toy_02-list.txt
echo -----------
del Net_To_List.exe
pause
