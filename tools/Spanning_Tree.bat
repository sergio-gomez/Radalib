@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Spanning_Tree.adb
del *.ali
del *.o
echo -----------
Spanning_Tree.exe  test-spanning_tree_toy.net  test-spanning_tree_toy-min_st.net   MIN  Integer
Spanning_Tree.exe  test-spanning_tree_toy.net  test-spanning_tree_toy-max1_st.net  MAX  Float  2
Spanning_Tree.exe  test-spanning_tree_toy.net  test-spanning_tree_toy-max2_st.net  MAX  Double
echo -----------
del Spanning_Tree.exe
pause
