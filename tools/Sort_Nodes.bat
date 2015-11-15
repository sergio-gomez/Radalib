@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Sort_Nodes.adb
del /q *.ali
del /q *.o
echo -----------
Sort_Nodes.exe  test-network_props_01.net  test-network_props_01-sorted_asc.net
Sort_Nodes.exe  test-network_props_02.net  test-network_props_02-sorted_asc.net
Sort_Nodes.exe  test-network_props_03.net  test-network_props_03-sorted_asc.net
Sort_Nodes.exe  test-zachary_unwh.net  test-zachary_unwh-sorted_asc.net   Asc
Sort_Nodes.exe  test-zachary_unwh.net  test-zachary_unwh-sorted_desc.net  Desc
Sort_Nodes.exe  test-zachary_unwh.net  test-zachary_unwh-sorted_rand.net  Rand
echo -----------
del Sort_Nodes.exe
pause
