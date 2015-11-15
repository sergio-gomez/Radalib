@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Data_To_Proximities.adb
del /q *.ali
del /q *.o
echo -----------
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-cols-prox.net  cols    ns   EUCL       nt    6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-cols-prox.txt  columns s01  EUCL  2.0  nt
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox01.txt  rows  s01  EUCL       nt    6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox02.txt  rows  szs  MANH       nt    2
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox03.txt  rows  s01  CHEB       nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox04.txt  rows  s01  MINK       nt    6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox05.txt  rows  szs  MINK  1    nt    2
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox06.txt  rows  s01  MINK  0.5  nt    8
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox07.txt  rows  s01  CANB       nt    10
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox08.txt  rows  s01  BRAY       nt    20
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox09.txt  rows  s01  CORD       nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox10.txt  rows  s01  CORD  P    nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox11.txt  rows  s01  CORD  S    nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox12.txt  rows  s01  CODI       nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox13.txt  rows  s01  CABS       nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox14.txt  rows  s01  CSQR       nt    4
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox15.txt  rows  s01  COSI       nt    6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox16.txt  rows  s01  COSI       omd   6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox17.txt  rows  s01  COSI       om2d  6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox18.txt  rows  s01  EUCL       iod   6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox19.txt  rows  s01  EUCL       eomd  6
Data_To_Proximities.exe  test-data_toy.txt  test-data_toy-rows-prox20.txt  rows  ns   MANH       oiz   2
echo -----------
del Data_To_Proximities.exe
pause
