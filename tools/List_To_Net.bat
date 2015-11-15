@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 List_To_Net.adb
del /q *.ali
del /q *.o
echo -----------
List_To_Net.exe  test-list_toy_01.txt  test-list_toy_01.net  undirected
List_To_Net.exe  test-list_toy_02.txt  test-list_toy_02.net
echo -----------
del List_To_Net.exe
pause
