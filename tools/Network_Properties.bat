@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
rem ulimit -s unlimited
gnatmake -O2 Network_Properties.adb -largs -Wl,--stack=500000000
del /q *.ali
del /q *.o
echo -----------
Network_Properties.exe  test-network_props_01.net  all  6
Network_Properties.exe  test-network_props_02.net  NEUF  4
Network_Properties.exe  test-network_props_02.net  GELU  4
Network_Properties.exe  test-network_props_02.net  A
Network_Properties.exe  test-network_props_03.net  6
Network_Properties.exe  test-network_props_04.net  8
Network_Properties.exe  test-zachary_unwh.net  all  6
echo -----------
del Network_Properties.exe
pause
