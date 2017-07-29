===================================================================
== Radalib 20170729-210105                                       ==
==                                                               ==
== Copyright (c) 2016 by                                         ==
==   Sergio Gomez             (sergio.gomez@urv.cat)             ==
==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==
==                                                               ==
== See LICENSE.txt                                               ==
===================================================================


--------
Contents
--------

- Description
- Structure
- Compilation
- Radalib library
- Radalib tools
- License
- Acknowledgements


-----------
Description
-----------

Radalib, Ada library and tools for the analysis of Complex Networks and more.

Radalib is a library we have continuously been updating for our research within the
Alephsys research group, led by Alex Arenas, at Universitat Rovira i Virgili (URV),
Tarragona, since 2004. Previous experience showed that the continuous reuse of code
was a painful task, thus we decided to be more structured and separate general purpose
code (e.g. manipulation of networks and partitions) from the specific details of
particular applications (e.g. Monte Carlo simulation of epidemic spreading). The
result was the development of a general purpose library, mostly devoted to complex
networks, and developed around abstract data types. This means the types are defined
as "private", with public subprograms operating on them and encapsulating their
implementations, thus allowing for future enhancements without having to modify the
programs already using them. We could have used object oriented programming, but we
believe polymorphism and inheritance are basically useless for this kind of
applications.

The selected language was Ada, for several reasons: performance (it is a compiled
language, not interpreted), readable code, support for abstract data types, strict
data type system (allows catching many errors at compile time), advanced support of
generics, high level support for concurrent programming (just in case it is needed),
availability of high quality compilers for the main platforms (Windows, Linux, MacOS),
and the confidence in your code when using it. The main drawback was the absence of
code from other people we could reuse, but that was not a problem since we wanted full
control and detailed understanding of every line of code used for our research.

Radalib is structured in three parts: source (the library itself), test and tools.
Tools are programs which solve a certain problem, e.g. community detection, partitions
comparison, network properties, connected components, file format conversion, etc.,
and which are basically mere interfaces to functionalities given by the library. The
requests to make public implementations of some of the algorithms presented in our
scientific papers led to the publication of Radatools, which are just executables
for Windows, Linux and MacOS of some of the Radalib tools.


---------
Structure
---------

Radalib is distributed in a single compressed file containing the following
folders or directories:

   radalib
   radalib\source
   radalib\test
   radalib\tools
   radalib\compiled
   radalib\maintenance

Their contents are:

- radalib
    Root of Radalib, containing README.txt, LICENSE.txt, version and howto files.

- radalib\source
    All the packages which form the library, and scripts for their compilation.

- radalib\test
    Test programs for most of the packages in the source folder, each one with a
    script for its compilation and execution, and some test data files.

- radalib\tools
    Programs which take advantage of the source packages, mainly for the analysis
    of complex networks. There are also scripts for their compilation and execution,
    and test data files.

- radalib\compiled
    Object files obtained from the compilation of the source packages. By default,
    they are for Windows; they need to be rebuilt when working in other platforms.

- radalib\maintenance
    Scripts to simplify the upgrade and installation of Radalib in the different
    platforms.


The size of radalib at version radalib-20160227-144606 is:

Code   :   249 Ada files
Files  :   654 files
Source : 60407 lines of Ada code


-----------
Compilation
-----------

Radalib has been programmed using the Ada language, and with the aid of the
GNAT Ada GPL Edition compilers from Adacore. Follow the indications in your
corresponding "howto" file to install GNAT and compile radalib:

- Windows : radalib-howto-windows.txt
- Linux   : radalib-howto-linux.txt
- MacOS   : radalib-howto-mac.txt


---------------
Radalib library
---------------

The main packages and sets of packages in Radalib are:

- Graphs
    Graph type for weighted directed or undirected networks. Based on vectors
    of adjacency lists. Supposes the number of vertices is fixed. There are child
    packages for Algorithms, Modularities, Operations, Properties and Multilayer
    algorithms. The Graph Structure defines a public data type for fast graph
    access (e.g. in Monte Carlo simulations).

- Finite_Disjoint_Lists
    List_Of_Lists type used to handle general purpose partitions. Very efficient
    for most of the operations.

- Disjoint_Sets
    Disjoint_Set type used to handle partitions optimized for additive percolation
    processes.

- Contingency_Tables
    Contingency_Table type to compare partitions in List_Of_Lists form.

- Linked_Lists
    Linked_List type implementing doubly linked lists.

- Stacks
    Stack type for LIFO storage.

- Queues
    Queue type for FIFO storage.

- Minheaps
    Minheap type implementing binary heaps.

- Trees
    Tree type used to handle hierarchical structures.

- Dendrograms
    Dendrogram type is a particular case of Tree in which nodes have several
    additional properties, the most relevant being a height.

- Modularities and Modularity_Optimization
    Modularity type and algorithms for its optimization.

- Utils and Arrays
    Several packages with general purpose types and subprograms, with emphasys
    on one- and two-dimensional dynamic objects, strings, input-output and files
    manipulation.

- Random_Numbers
    Several random number generators.

- Histograms
    Histogram type for the calculation of linear and logarithmic histograms.

- Statistics
    Calculation of the main statistics measures.

- Eps_Plots and Eps_Utils
    Utils for the direct generation of EPS plots.

- Chrono_Utils
    Chronometer type to measure elapsed times.

Many of the packages are just instantiations of the generic packages above for
elements of simple types such as Integer, Float, Double or String.


-------------
Radalib tools
-------------

The tools in Radalib are:

- Communities_Detection
    Community detection in complex networks by optimization of modularity, using
    the following heuristics: (h) exhaustive, (t) tabu, (e) extremal, (s) spectral,
    (f) fast, (r) fine-tuning by reposition, (b) fine-tuning based on tabu.

- Communities_Network
    Given a network and a community, returns the weighted network of communities.

- Compare_Partitions
    Calculate similarity and dissimilarity indices between two partitions.

- Connected_Subgraphs
    Split a network into its (weak or strong) connected components.

- Convert_Clu_To_Lol
    Convert a partition in Pajek format (*.clu) into a partition in our Lol format.

- Convert_Lol_To_Clu
    Convert a partition in our Lol format into a partition in Pajek format (*.clu).

- Data_Statistics
    Calculate statistics of rows or columns in a data file.

- Data_To_Correlations
    Calculate the correlations network of a data file.

- Data_To_Proximities
    Calculate many types of proximities (distances or similarities) between rows or
    columns in a data file.

- Extract_Subgraphs
    Create subgraphs of a graph.

- Hierarchical_Clustering
    Agglomerative hierarchical clustering with multidendrograms and binary dendrograms.

- Links_Info
    Calculate the degrees and strengths of the nodes attached to each link in a network.

- List_To_Net
    Convert a network in list format to Pajek format (*.net).

- Matrix_To_List
    Convert a matrix to list format.

- Matrix_To_Net
    Convert a network in matrix format to Pajek format (*.net).

- Mesoscales_Detection
    Mesoscales detection in complex networks by optimization of modularity for variable
    common self-loops.

- Mesoscales_Fine_Tuning
    Fine Tuning of the mesoscales obtained with Mesoscales_Detection.

- Modularity_Calculation
    Calculate the modularity of a partition of a network, detailing the contributions
    of individual nodes and communities.

- Multiplex_Aggregate
    Calculate the aggregate network of a multiplex network.

- Multiplex_Extract_Layers
    Extract the layers of a multiplex network.

- Net_To_List
    Convert a network in Pajek format (*.net) to list format.

- Net_To_Matrix
    Convert a network in Pajek format (*.net) to matrix format.

- Network_Properties
    Calculate many properties of a network, including connectedness, degrees, strengths,
    clustering coefficients, assortativities, path lengths, efficiencies, diameters,
    entropies and betweenness. Handles all kinds of networks, even weighted, directed
    and signed.

- Reformat_Partitions
    Reformat partitions in Pajek and Lol formats changing nodes' indices by nodes'
    names.

- Size_Reduction
    Elimination of simple and triangular 'hairs' of a network to speed-up modularity
    optimization.

- Size_Reduction_Lol_Expand
    Convert a partition of a sized reduced network into a partition of the original
    network.

- Sort_Nodes
    Sort nodes of a network randomly or according to degree.

- Spanning_Tree
    Calculate the minimum and maximum spanning tree of a graph.

- Symmetrize_Network
    Symmetrization of a directed graph.


-------
License
-------

Radalib, Copyright (c) 2016 by
Sergio Gomez (sergio.gomez@urv.cat), Alberto Fernandez (alberto.fernandez@urv.cat)

This library is free software; you can redistribute it and/or modify it under the terms
of the GNU Lesser General Public License version 2.1 as published by the Free Software
Foundation.

This library is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with
this library (see LICENSE.txt); if not, see http://www.gnu.org/licenses/


----------------
Acknowledgements
----------------

We thank Javier Borge-Holthoefer for his important contributions to Radalib, Clara
Granell and Pau Erola for their influence in its development, Albert Sole-Ribalta for
discovering and helping to solve some bugs, and Alex Arenas for leading the Alephsys
research group at Universitat Rovira i Virgili (URV), Tarragona, in which all this
software has become useful for our research.
