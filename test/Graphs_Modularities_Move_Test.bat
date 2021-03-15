@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Modularities_Move_Test.adb
del /q *.ali
del /q *.o
echo ---------------------------------------------------------------------
echo --- Test UN and WN
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  UN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WN  4

echo ---------------------------------------------------------------------
echo --- Test UUN and WUN
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WUN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WUN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  UUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WUN  4

echo ---------------------------------------------------------------------
echo --- Test WS and WN
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WS  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WN  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WS  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WS  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net  test-zachary-lol2.txt  3.0  WN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net  test-zachary-lol2.txt  3.0  WS  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WS  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WS  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  -3.0  WS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  -3.0  WS  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net  test-zachary-lol2.txt   3.0  WS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net  test-zachary-lol2.txt  -3.0  WS  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net  test-zachary-lol2.txt   3.0  0.5  WS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net  test-zachary-lol2.txt  -3.0  0.5  WS  4

echo ---------------------------------------------------------------------
echo --- Test WNN
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt   0.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt   0.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt   0.0  0.0  WUN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  -3.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  -3.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  -3.0  0.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt   0.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt   0.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt   0.0  0.0  WUN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  -3.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  -3.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  -3.0  0.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt   0.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt   0.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt   0.0  0.0  WUN  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  -3.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  -3.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  -3.0  0.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net    test-zachary-lol2.txt  -3.0  1.0  WNN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net    test-zachary-lol2.txt  -3.0  0.0  WN   4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net    test-zachary-lol2.txt  -3.0  0.0  WUN  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net    test-zachary-lol2.txt  -3.0  0.5  WNN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net    test-zachary-lol2.txt  -3.0  0.5  WN   4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_sgn_wh.net    test-zachary-lol2.txt  -3.0  0.5  WUN  4

echo ---------------------------------------------------------------------
echo --- Test WLUN
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WLUN  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WN    4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  UN    4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WLUN  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WN    4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  UN    4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WLUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WN    4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  UN    4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WLUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WN    4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  UN    4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WLUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WN    4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  UN    4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WLUN  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WN    4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  UN    4

echo ---------------------------------------------------------------------
echo --- Test WLA
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WLA  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WLA  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WLA  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WLA  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WLA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WLA  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WLA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WLA  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WLA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WLA  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WLA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WLA  4

echo ---------------------------------------------------------------------
echo --- Test WULA
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WULA  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WULA  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WULA  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WULA  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WULA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WULA  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WULA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WULA  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WULA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WULA  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WULA  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WULA  4

echo ---------------------------------------------------------------------
echo --- Test WLR
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WLR  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WLR  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WLR  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WLR  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WLR  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WLR  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WLR  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WLR  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WLR  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WLR  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WLR  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WLR  4

echo ---------------------------------------------------------------------
echo --- Test WBPM
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WBPM  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WBPM  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WBPM  4
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WBPM  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WBPM  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WBPM  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WBPM  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WBPM  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  0.0  WBPM  4
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  0.0  0.5  WBPM  4
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  3.0  0.5  WBPM  4
echo -----------

echo ---------------------------------------------------------------------
echo --- Test WBPS
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  0.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  0.0  WBPM  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zachary_unwh.net  test-zachary-lol2.txt  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zachary_wh.net    test-zachary-lol2.txt  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  0.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  0.0  WBPM  4
echo ------
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_unwh.net  test-zachary-lol2.txt  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_sl_wh.net    test-zachary-lol2.txt  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_unwh.net  test-zachary-lol2.txt  3.0  0.5  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WBPS  4
Graphs_Modularities_Move_Test.exe  test-zacharymod_dir_sl_wh.net    test-zachary-lol2.txt  3.0  0.5  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  0.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  0.0  WBPM  4
echo ------
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  3.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  3.0  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  0.0  0.5  WBPS  4
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  3.0  0.5  WBPS  4
Graphs_Modularities_Move_Test.exe  test-bipartite.net  test-bipartite.clu  3.0  0.5  WBPM  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-bipartite_sgn.net  test-bipartite.clu   0.0  WBPS  4
Graphs_Modularities_Move_Test.exe  test-bipartite_sgn.net  test-bipartite.clu  -3.0  WBPS  4
echo ---------------------------------------------------------------------
Graphs_Modularities_Move_Test.exe  test-bipartite_sgn.net  test-bipartite.clu   0.0  0.5  WBPS  4
Graphs_Modularities_Move_Test.exe  test-bipartite_sgn.net  test-bipartite.clu  -3.0  0.5  WBPS  4
echo -----------
del Graphs_Modularities_Move_Test.exe
pause
