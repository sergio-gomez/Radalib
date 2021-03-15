#!/bin/sh

# general
rm -f *.exe *.ali *.o b~*.ad?

# Communities_Detection
rm -f test-circle8-lol.txt* test-zachary_unwh-lol.txt* test-dolphins-lol.txt*
rm -f test-bipartite-lol.txt* test-bipartite_sgn-lol.txt*

# Communities_Network
rm -f test-zachary-lol1.net test-zachary-lol2.net

# Compare_Partitions
rm -f test-zachary-ct.txt test-zachary-ctsv.txt test-zachary-ctst.txt

# Connected_Subgraphs
rm -f test-disconnected-cc.txt test-disconnected-cc-0?.net

# Convert_Clu_To_Lol
rm -f test-convert_clu_toy-lol.txt test-convert_clu_toy-sorted-lol.txt

# Convert_Lol_To_Clu
rm -f test-convert_lol_toy.clu

# Data_Statistics
rm -f test-data_toy-stats.txt

# Data_To_Correlations
rm -f test-data_toy-rows.net test-data_toy-cols.net

# Data_To_Proximities
rm -f test-data_toy-cols-prox.net test-data_toy-cols-prox.txt
rm -f test-data_toy-rows-prox??.txt

# Extract_Subgraphs
rm -f test-disconnected-sub-sg-0?.net
rm -f test-dolphins-list1-sg-0?.net
rm -f test-dolphins-list2-sg.net

# Hierarchical_Clustering
rm -f test-hierarchical_toy_w???-bd-??*.txt test-hierarchical_toy_w???-md-??*.txt test-hierarchical_toy_w???-*.json

# Links_Info
rm -f test-zachary_unwh-links_info-*.txt

# List_To_Net
rm -f test-list_toy_01.net test-list_toy_02.net

# Matrix_To_List
rm -f test-matrix_toy_01-list.txt test-matrix_toy_02-list.txt test-matrix_toy_03-list.txt

# Matrix_To_Net
rm -f test-matrix_toy_01.net test-matrix_toy_02.net test-matrix_toy_03.net

# Mesoscales_Detection
rm -f test-zachary_unwh-lols.txt test-zachary_unwh-table.txt
rm -f test-dolphins-lols.txt test-dolphins-table.txt

# Mesoscales_Fine_Tuning
rm -f test-zachary_unwh-lols-improved.txt test-zachary_unwh-table-improved.txt
rm -f test-dolphins-lols-improved.txt test-dolphins-table-improved.txt

# Multiplex_Aggregate
rm -f test-multiplex_toy_01-aggr*.net test-multiplex_toy_02-aggr*.net

# Multiplex_Extract_Layers
rm -f test-multiplex_toy_01-lay*.net test-multiplex_toy_02-lay*.net

# Net_To_List
rm -f test-net_toy_01-list.txt test-net_toy_02-list.txt

# Net_To_Matrix
rm -f test-net_toy_01-matrix.txt test-net_toy_02-matrix.txt

# Network_Properties
rm -f test-network_props_01-info_*.txt test-network_props_01-info_*.net
rm -f test-network_props_02-info_*.txt test-network_props_02-info_*.net
rm -f test-network_props_03-info_*.txt test-network_props_03-info_*.net
rm -f test-network_props_04-info_*.txt test-network_props_04-info_*.net
rm -f test-zachary_unwh-info_*.txt test-zachary_unwh-info_*.net

# Reformat_Partitions
rm -f test-zachary-lol1-reformat1.txt test-zachary-lol1-reformat2.txt test-dolphins-lol-best-reformat.txt
rm -f test-zachary-lols-reformat.txt

# Size_Reduction
rm -f test-size_toy-reduced.net test-size_toy-reducer.txt
rm -f test-zachary_unwh-reduced.net test-zachary_unwh-reducer.txt
rm -f test-dolphins-reduced.net test-dolphins-reducer.txt

# Size_Reduction_Lol_Expand
rm -f test-size_toy-expanded-lol.txt

# Sort_Nodes
rm -f test-network_props_01-sorted_asc.net test-network_props_02-sorted_asc.net test-network_props_03-sorted_asc.net
rm -f test-zachary_unwh-sorted_asc.net test-zachary_unwh-sorted_desc.net test-zachary_unwh-sorted_rand.net

# Spanning_Tree
rm -f test-spanning_tree_toy-min_st.net test-spanning_tree_toy-max1_st.net test-spanning_tree_toy-max2_st.net

# Symmetrize_Network
rm -f test-size_toy-sym.net
