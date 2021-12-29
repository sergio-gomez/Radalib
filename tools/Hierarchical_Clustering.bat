@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Hierarchical_Clustering.adb
del /q *.ali
del /q *.o
echo -----------
Hierarchical_Clustering.exe  test-hierarchical_toy_wout.txt  test-hierarchical_toy_wout-md-cl      MD  DIST  1  CL            clus
Hierarchical_Clustering.exe  test-hierarchical_toy_wout.txt  test-hierarchical_toy_wout-bd-cl      BD  D        CL            Unsorted  100

Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-al      MD  DIST  1  AL  UW        internal_node_c
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-gl      MD  D     1  GL  UW        c
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-hl      MD  D        HL  W
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-sl      MD  D        SL  W

Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-bf+1.0  MD  D        BF  UW  +1.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-bf+0.0  MD  D     1  BF  UW   0.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-bf-1.0  MD  D        BF  W   -1.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl+1.0  MD  D     1  VL  UW  +1.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl+0.1  MD  D     1  VL  UW  +0.1
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl+0.0  MD  D     1  VL  UW   0.0
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl-0.1  MD  D        VL  W   -0.1
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-vl-1.0  MD  D        VL  W   -1.0

Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-c    BD  D     1  AL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-s    BD  D     1  AL  UW        Sorted    1000      c
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-u    BD  D     1  AL  UW        Unsorted
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-m    BD  D     1  AL  UW        Sample    100  0.1
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-al-b    BD  D        AL  UW        Best

Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-md-s-al    MD  SIM   1  AL  UW
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-s-al    BD  S     1  AL  UW        Sorted  C
Hierarchical_Clustering.exe  test-hierarchical_toy_with.txt  test-hierarchical_toy_with-bd-s-vl    BD  S     1  VL  UW  -0.1  Best

Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-md-al2     MD  DIST  2  AL  UW
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al2-s   BD  D     2  AL  UW        Sorted
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al2-u   BD  D     2  AL  UW        Unsorted  100
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al2-m   BD  D     2  AL  UW        Sample    100  0.5
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al2-b   BD  D     2  AL  UW        Best

Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al2-c   BD  D     2  AL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al-c    BD  D        AL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-al6-c   BD  D     6  AL  UW        Count

Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-sl6-c   BD  D     6  SL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-cl6-c   BD  D     6  CL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-hl6-c   BD  D     6  HL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-gl6-c   BD  D     6  GL  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-wd6-c   BD  D     6  WD  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-cd6-c   BD  D     6  CD  UW        Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-vl6-c   BD  D     6  VL  UW  0.5   Count
Hierarchical_Clustering.exe  test-hierarchical_toy_wmny.txt  test-hierarchical_toy_wmny-bd-bf6-c   BD  D     6  BF  UW  0.5   Count
echo -----------
del Hierarchical_Clustering.exe
pause
