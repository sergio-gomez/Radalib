@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Hierarchical_Clustering.adb
del /q *.ali
del /q *.o
echo -----------
Hierarchical_Clustering.exe  test-hierarchical_toy_wout.txt  test-hierarchical_toy_wout-md-cl     MD  DIST  1  CL                      clus
Hierarchical_Clustering.exe  test-hierarchical_toy_wout.txt  test-hierarchical_toy_wout-bd-cl     BD  DIST     CL
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-al     MD  D     1  AL  UW
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-gl     MD  D     1  GL  UW
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-hl     MD  D        HL  W
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-sl     MD  D        SL  W
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-c   BD  D     1  AL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-s   BD  D     1  AL  UW        Sorted
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-u   BD  D     1  AL  UW        Unsorted  c
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-b   BD  D        AL  UW        Best      none
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-bf+1.0 MD  D        BF  UW  +1.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-bf+0.0 MD  D     1  BF  UW   0.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-bf-1.0 MD  D        BF  W   -1.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl+1.0 MD  D     1  VL  UW  +1.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl+0.1 MD  D     1  VL  UW  +0.1
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl+0.0 MD  D     1  VL  UW   0.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl-0.1 MD  D        VL  W   -0.1
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl-1.0 MD  D        VL  W   -1.0
echo -----------
del Hierarchical_Clustering.exe
pause
