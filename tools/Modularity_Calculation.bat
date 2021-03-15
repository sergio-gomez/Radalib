@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Modularity_Calculation.adb
del /q *.ali
del /q *.o
echo ---------------------------------------------------------------------
echo --- Unweighted undirected unsigned network: test-zachary_unwh.net ---
echo ---------------------------------------------------------------------
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  UN
echo --- modularity details ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  WN  T    4
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol2.txt  WN  TC   4
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol2.txt  WN  TN   4
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol2.txt  WN  TCN  4
echo --- 3 arguments ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  WN
echo --- 4 arguments ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  2.0  WN
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  WN  4
echo --- 5 arguments ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  2.0  0.5  WN
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  2.0  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  2.0  WN  4
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  WN  T  4
echo --- 6 arguments ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  2.0  WN  T  4
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  2.0  0.5  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  2.0  0.5  WN  4
echo --- 7 arguments ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.txt  2.0  0.5  WN  T  4
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  UN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  UN  T
echo ---
modularity_calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        UUN  T
modularity_calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  UUN  T
modularity_calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  UUN  T
modularity_calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  UUN  T
modularity_calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  UUN  T
modularity_calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  UUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  UUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  UUN  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WN  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WS  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WS  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WUN  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WLA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WLA  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WULA  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WULA  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5 WLUN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5 WLUN  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WNN  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WNN  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WLR  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WLR  T
echo ---
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu        WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   1.0  WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   5.0  WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -1.0  WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu  -5.0  WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   0.0  1.5  WBPM  T
Modularity_Calculation.exe  test-zachary_unwh.net  test-zachary-lol1.clu   2.0  0.5  WBPM  T

echo -----------------------------------------------------------------
echo --- Weighted directed signed network: test-modularity_toy.net ---
echo -----------------------------------------------------------------
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  UN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  UN  T
echo ---
modularity_calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        UUN  T
modularity_calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  UUN  T
modularity_calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  UUN  T
modularity_calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  UUN  T
modularity_calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  UUN  T
modularity_calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  UUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  UUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  UUN  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WN  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WS  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WS  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WUN  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WLA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WLA  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WULA  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WULA  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WLUN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WLUN  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WNN  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WNN  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WLR  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WLR  T
echo ---
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu        WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   1.0  WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   5.0  WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -1.0  WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu  -5.0  WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   0.0  1.5  WBPM  T
Modularity_Calculation.exe  test-modularity_toy.net  test-modularity_toy.clu   2.0  0.5  WBPM  T
echo -----------
del Modularity_Calculation.exe
pause
