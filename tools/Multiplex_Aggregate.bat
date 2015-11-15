@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Multiplex_Aggregate.adb
del /q *.ali
del /q *.o
echo -----------
Multiplex_Aggregate.exe  test-multiplex_toy_01.txt  test-multiplex_toy_01-aggr-uw.net  undirected  weighted  integer
Multiplex_Aggregate.exe  test-multiplex_toy_01.txt  test-multiplex_toy_01-aggr-uu.net  undirected  unweighted  integer
Multiplex_Aggregate.exe  test-multiplex_toy_02.txt  test-multiplex_toy_02-aggr-dw.net  directed  weighted  float  1
Multiplex_Aggregate.exe  test-multiplex_toy_02.txt  test-multiplex_toy_02-aggr-du.net  D  U  F
echo -----------
del Multiplex_Aggregate.exe
pause
