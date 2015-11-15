@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Extract_Subgraphs.adb
del /q *.ali
del /q *.o
echo -----------
Extract_Subgraphs.exe  test-disconnected.net  test-disconnected-sub.clu  test-disconnected-sub
Extract_Subgraphs.exe  test-dolphins.net      test-dolphins-list1.txt    test-dolphins-list1  4
Extract_Subgraphs.exe  test-dolphins.net      test-dolphins-list2.txt    test-dolphins-list2
echo -----------
del Extract_Subgraphs.exe
pause
