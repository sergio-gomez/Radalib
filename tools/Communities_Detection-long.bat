@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Communities_Detection.adb
del /q *.ali
del /q *.o

echo ------------------
echo --- Log levels ---
echo ------------------
Communities_Detection.exe  none      WN  exhaustive  5  test-circle8.net  test-circle8-lol.txt
del test-circle8-lol.txt*
Communities_Detection.exe  summary   WN  erfrt       5  test-circle8.net  test-circle8-lol.txt
del test-circle8-lol.txt*
Communities_Detection.exe  progress  WN  erfrt       5  test-circle8.net  test-circle8-lol.txt
del test-circle8-lol.txt*
Communities_Detection.exe  verbose   WN  htsefrb     5  test-circle8.net  test-circle8-lol.txt
del test-circle8-lol.txt*

echo ------------------
echo --- Heuristics ---
echo ------------------
Communities_Detection.exe  s  WN  r     1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  b     1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  t     3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  te    3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  sl    3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  sbfb  3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  ebfb  3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  e     3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
Communities_Detection.exe  s  WN  r     1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
Communities_Detection.exe  s  WN  f     1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
Communities_Detection.exe  s  WN  r     1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  p  WN  erfr  3 test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  p  WN  trfrerfr  5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  p  WN  lrfr  10  test-dolphins.net  test-dolphins-lol.txt
del test-dolphins-lol.txt*

echo --------------------------------------
echo --- Heuristics and Initializations ---
echo --------------------------------------
Communities_Detection.exe  s  WN  ini_together 1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  +.!          1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  el-l-srfr-t  1  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  ll+sr+e+t.t  3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WN  r-e-s!rfr    3  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*

echo ------------------
echo --- Resistance ---
echo ------------------
Communities_Detection.exe  s  WS  h  5  -2.0  test-circle8.net  test-circle8-lol.txt
del test-circle8-lol.txt*
Communities_Detection.exe  s  WS  t  5  -1.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  e  5   0.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  l  5   1.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  s  5   2.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  f  5   3.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  r  5   4.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  b  5   4.5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*

echo -------------------
echo --- Other tests ---
echo -------------------
Communities_Detection.exe  s  WS  trfr  5   1.0  1.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  erfr  5   0.0  1.5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  lrfr  5   1.0  1.5  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  srfr  5  -1.0  1.0  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  rlfr  5   0.0  0.8  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
Communities_Detection.exe  s  WS  erlt  5  -1.0  0.8  test-zachary_unwh.net  test-zachary_unwh-lol.txt
del test-zachary_unwh-lol.txt*
echo -----------
del Communities_Detection.exe
pause
