@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Hierarchical_Clustering.adb
del /q *.ali
del /q *.o
echo -----------
Hierarchical_Clustering.exe  test-hierarchical_toy_wout.txt  test-hierarchical_toy_wout-md-cl    MD  DIST  CL               c
Hierarchical_Clustering.exe  test-hierarchical_toy_wout.txt  test-hierarchical_toy_wout-bd-cl    BD  DIST  CL
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-ua    MD  D     UA  1
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-ua-c  BD  D     UA  1  Count
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-ua-s  BD  D     UA  1  Sorted
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-ua-u  BD  D     UA  1  Unsorted  clus
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-ua-b  BD  D     UA     Best      none
echo -----------
del Hierarchical_Clustering.exe
pause
