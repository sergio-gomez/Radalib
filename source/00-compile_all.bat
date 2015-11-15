@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled

goto compile_without_initialize

echo ---------------
echo Initializing...
echo ---------------

00-compile_all-list_gen.sh

:compile_without_initialize
echo ------------
echo Compiling...
echo ------------

for /F %%f in (00-compile_all-list.txt) do gnatmake -O2 -D %RADALIB_ROOT%\compiled %%f
rem for /F %%f in (00-compile_all-list.txt) do gnatmake -g -D %RADALIB_ROOT%\compiled %%f
rem del 00-compile_all-list.txt

echo ---
pause
