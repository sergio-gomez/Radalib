@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Links_Info.adb
del /q *.ali
del /q *.o
echo -----------
Links_Info.exe  test-zachary_unwh.net      test-zachary_unwh-links_info-all.txt  0
Links_Info.exe  test-zachary_unwh.net  78  test-zachary_unwh-links_info-all.txt  0
Links_Info.exe  test-zachary_unwh.net  99  test-zachary_unwh-links_info-all.txt  0
Links_Info.exe  test-zachary_unwh.net  10  test-zachary_unwh-links_info-10.txt   0
Links_Info.exe  test-zachary_unwh.net  30  test-zachary_unwh-links_info-30.txt   0
Links_Info.exe  test-zachary_unwh.net  70  test-zachary_unwh-links_info-70.txt   0
echo -----------
del Links_Info.exe
pause
