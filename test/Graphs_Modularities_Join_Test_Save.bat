@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Modularities_Join_Test.adb
del /q *.ali
del /q *.o
echo ---------------------------------------------------------------------
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt            WN    4   > test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  3.0       WLA   4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  UN    4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  UUN   4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WN    4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WS    4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WUN   4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WLA   4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WULA  4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WLUN  4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WNN   4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WLR   4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WBPM  4  >> test-modularities_join.txt
Graphs_Modularities_Join_Test.exe  test-zachary_wh.net  test-zachary-lol2.txt  2.0  0.5  WBPS  4  >> test-modularities_join.txt
echo -----------
del Graphs_Modularities_Join_Test.exe
pause
