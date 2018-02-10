#!/bin/sh

# general
rm -f *.exe *.ali *.o b~*.ad?

# Chrone_Test
rm -f test-chrono.txt

# Eps_Plots_Test
rm -f test-eps_plots_0?.eps

# Finite_Disjoint_Lists_Test
rm -f test-finite_disjoint_lists.txt

# Graph_Eigenvalues
rm -f test-zachary_unwh-eig.txt

# Multilayer_Test
rm -f test-multiplex_out??.txt

# Pajek_IO_Test
rm -f test-pajek_out.clu test-pajek_out?.net

# Trees_Test
rm -f test-trees-text.txt test-trees-newick.txt test-trees-json.txt test-trees-json.html

# Utils_Properties_Test
rm -f test-utils_properties-out.txt
