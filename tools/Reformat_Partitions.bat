@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Reformat_Partitions.adb
del /q *.ali
del /q *.o
echo -----------
Reformat_Partitions.exe  test-zachary_unwh.net  test-zachary-lol1.txt  test-zachary-lol1-reformat1.txt  4  CH
Reformat_Partitions.exe  test-zachary_unwh.net  test-zachary-lol1.clu  test-zachary-lol1-reformat2.txt  5  7  0
Reformat_Partitions.exe  test-zachary_unwh.net  test-zachary-lols.txt  test-zachary-lols-reformat.txt  4  SH  5  13  0
Reformat_Partitions.exe  test-dolphins.net  test-dolphins-lol-best.txt  test-dolphins-lol-best-reformat.txt  4  CH  5  13  0
echo -----------
del Reformat_Partitions.exe
pause
