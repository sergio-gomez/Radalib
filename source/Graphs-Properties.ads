-- Radalib, Copyright (c) 2022 by
-- Sergio Gomez (sergio.gomez@urv.cat), Alberto Fernandez (alberto.fernandez@urv.cat)
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the
-- GNU Lesser General Public License version 2.1 as published by the Free Software Foundation.
--
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License along with this
-- library (see LICENSE.txt); if not, see http://www.gnu.org/licenses/


-- @filename Graphs-Properties.ads
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 24/10/2007
-- @revision 03/11/2014
-- @brief Calculation of Properties of Graphs

with Utils; use Utils;
with Graphs_Double;
with Statistics;

generic
  type Num is digits <>;
  type Nums is array(Integer range <>) of Num;
  type PNums is access Nums;
  type Numss is array(Integer range <>, Integer range <>) of Num;
  type PNumss is access Numss;
  with function To_Num(Value: in Edge_Value) return Num is <>;
  with function To_Value(Value: in Num) return Edge_Value is <>;
  with function Alloc(First: in Integer; Last: in Integer) return PNums is <>;
  with procedure Free(P: in out PNums) is <>;
  with function Alloc(First1, Last1, First2, Last2: in Integer) return PNumss is <>;
  with procedure Free(P: in out PNumss) is <>;
package Graphs.Properties is

  package Stats is new Statistics(Num => Num, Nums => Nums, PNums => PNums);
  use Stats;

  -- Type to choose a Subset of Links
  type Links_Subset is (All_Links, Positive_Links, Negative_Links);
  -- Type to choose the Direction of the Links
  type Links_Direction is (From_Links, To_Links);
  -- Type to choose the Values Statistic Type
  type Values_Statistic_Type is (Size_Value, Min_Value, Max_Value, Sum_Value, Avg_Value, Std_Dev_Value);

  -- Type to represent a Path in a Graph
  package Linked_Edges is new Linked_Lists(Edge);
  subtype Graph_Path is Linked_Edges.Linked_List;

  Plus_Infinity: constant Num := Num'Last;

  Disconnected_Graph_Error: exception;
  Negative_Weights_Error: exception;


  -- Purpose : To know if a Weight is of the given kind
  --
  -- Wh      : The Weight
  -- Ls      : The Links Subset
  -- return  : True if Weight of the given kind
  function Belongs_To(Wh: in Num; Ls: in Links_Subset) return Boolean;

  -- Purpose : To know if an Edge is of the given kind
  --
  -- E       : The Edge
  -- Ls      : The Links Subset
  -- return  : True if Edge of the given kind
  function Belongs_To(E: in Edge; Ls: in Links_Subset) return Boolean;

  -- Purpose : Obtain the Value of an Edge of the given kind
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- E       : The Edge
  -- Ls      : The Links Subset
  -- return  : The Value
  function Value(E: in Edge; Ls: in Links_Subset) return Num;

  -- Purpose : To know if there are more Edges left in an Edges List of the given kind
  -- Note    : Skips all Edges until one of the given kind is found
  --
  -- El      : The Edges List
  -- Ls      : The Links Subset
  -- return  : True if more Edges left
  function Has_Next(El: in Edges_List; Ls: in Links_Subset) return Boolean;

  -- Purpose : To know if a Vertex has links of the given kind
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : True if has links of the given kind, False otherwise
  function Has_Links(V: in Vertex; Ls: in Links_Subset := All_Links) return Boolean;

  -- Purpose : To know if a Graph has links of the given kind
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : True if has links of the given kind, False otherwise
  -- raises  : Uninitialized_Graph_Error
  function Has_Links(Gr: in Graph; Ls: in Links_Subset := All_Links) return Boolean;

  -- Purpose : To know if a Vertex has a Self-Loop of the given kind
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : True if has a Self-Loop of the given kind, False otherwise
  function Has_Self_Loop(V: in Vertex; Ls: in Links_Subset) return Boolean;

  -- Purpose : Obtain the Number of Edges of a Graph
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Number of Edges
  -- raises  : Uninitialized_Graph_Error
  function Number_Of_Edges(Gr: in Graph; Ls: in Links_Subset) return Num;

  -- Purpose : Obtain the Number of Self-Loops of a Graph
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Number of Self-Loops
  -- raises  : Uninitialized_Graph_Error
  function Number_Of_Self_Loops(Gr: in Graph; Ls: in Links_Subset) return Num;

  -- Purpose : Obtain the Degree from or to a given Vertex
  --
  -- V       : The Vertex
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Degree
  function Degree(V: in Vertex; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Degree from each Vertex
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : The Degree
  function Degree_From(V: in Vertex; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Degree to each Vertex
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : The Degree
  function Degree_To(V: in Vertex; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Degree from or to each Vertex
  --
  -- Gr      : The Graph
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Degrees
  -- raises  : Uninitialized_Graph_Error
  function Degree(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Degree from each Vertex
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Degrees
  -- raises  : Uninitialized_Graph_Error
  function Degree_From(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Degree to each Vertex
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Degrees
  -- raises  : Uninitialized_Graph_Error
  function Degree_To(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Total Degree of a Graph
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Total Degree
  -- raises  : Uninitialized_Graph_Error
  function Total_Degree(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Average Degree of a Graph
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Average Degree
  -- raises  : Uninitialized_Graph_Error
  function Average_Degree(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Minimum Degree of a Graph
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Minimum Degree
  -- raises  : Uninitialized_Graph_Error
  function Minimum_Degree(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Maximum Degree of a Graph
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Maximum Degree
  -- raises  : Uninitialized_Graph_Error
  function Maximum_Degree(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain a Statistic of the Values of a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  -- Note    : For Vertices without Links of the desired kind, a zero is returned
  --
  -- V       : The Vertex
  -- Vst     : The Values Statistic Type
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Statistic of the Values
  function Values_Statistic(V: in Vertex; Vst: in Values_Statistic_Type; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain a Statistic of the Values From a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  -- Note    : For Vertices without Links of the desired kind, a zero is returned
  --
  -- V       : The Vertex
  -- Vst     : The Values Statistic Type
  -- Ls      : The Links Subset
  -- return  : The Statistic of the Values
  function Values_Statistic_From(V: in Vertex; Vst: in Values_Statistic_Type; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain a Statistic of the Values to a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  -- Note    : For Vertices without Links of the desired kind, a zero is returned
  --
  -- V       : The Vertex
  -- Vst     : The Values Statistic Type
  -- Ls      : The Links Subset
  -- return  : The Statistic of the Values
  function Values_Statistic_To(V: in Vertex; Vst: in Values_Statistic_Type; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain a Statistic of the Values of each Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  -- Note    : For Vertices without Links of the desired kind, a zero is returned
  --
  -- Gr      : The Graph
  -- Vst     : The Values Statistic Type
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Statistic of the Values
  -- raises  : Uninitialized_Graph_Error
  function Values_Statistic(Gr: in Graph; Vst: in Values_Statistic_Type; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Minimum of the Values of the Links
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Minimum Value
  -- raises  : Uninitialized_Graph_Error
  function Minimum_Value(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Maximum of the Values of the Links
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Maximum Value
  -- raises  : Uninitialized_Graph_Error
  function Maximum_Value(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Strength of a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- V       : The Vertex
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Strength
  function Strength(V: in Vertex; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Strength from a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : The Strength
  function Strength_From(V: in Vertex; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Strength to a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : The Strength
  function Strength_To(V: in Vertex; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Strength from each Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Strengths
  -- raises  : Uninitialized_Graph_Error
  function Strength(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Strength from each Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Strengths
  -- raises  : Uninitialized_Graph_Error
  function Strength_From(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Strength to each Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Strengths
  -- raises  : Uninitialized_Graph_Error
  function Strength_To(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Total Strength of a Graph
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Total Strength
  -- raises  : Uninitialized_Graph_Error
  function Total_Strength(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Average Strength of a Graph
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Average Strength
  -- raises  : Uninitialized_Graph_Error
  function Average_Strength(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Minimum Strength of a Graph
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Minimum Strength
  -- raises  : Uninitialized_Graph_Error
  function Minimum_Strength(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Maximum Strength of a Graph
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Maximum Strength
  -- raises  : Uninitialized_Graph_Error
  function Maximum_Strength(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Self-Loop of a given Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  -- Note    : Returns zero if Self-Loop does not exist
  --
  -- V       : The Vertex
  -- Ls      : The Links Subset
  -- return  : The Self-Loop
  function Self_Loop(V: in Vertex; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Self-Loop of each Vertex
  -- Note    : For the Negative Links Subset, the absolute value is returned
  -- Note    : Returns zero if Self-Loop does not exist
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Self-Loops
  -- raises  : Uninitialized_Graph_Error
  function Self_Loop(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Total Self-Loops Strength of a Graph
  -- Note    : For the Negative Links Subset, the absolute value is returned
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- return  : The Total Self-Loops Strength
  -- raises  : Uninitialized_Graph_Error
  function Total_Self_Loops_Strength(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Asymmetry of a Graph
  --
  -- Gr      : The Graph
  -- Ignore_Self_Loops: True to ignore Self-loops in the calculations
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Asymmetry
  -- raises  : Uninitialized_Graph_Error
  function Asymmetry(Gr: in Graph; Ignore_Self_Loops: in Boolean := False; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Reciprocity of a Graph
  --
  -- Gr      : The Graph
  -- Ignore_Self_Loops: True to ignore Self-loops in the calculations
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Reciprocity
  -- raises  : Uninitialized_Graph_Error
  function Reciprocity(Gr: in Graph; Ignore_Self_Loops: in Boolean := False; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Entropy of a Vertex
  -- Note    : If weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Entropy is 1 if Unweighted and Normalized
  --
  -- V       : The Vertex
  -- Ld      : The Links Direction
  -- Weighted: True if the Weights of the links are considered
  -- Normalized: True if Normalized Entropy is required
  -- return  : The Vertex Entropy
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Entropy(V: in Vertex; Ld: in Links_Direction := From_Links; Weighted: in Boolean := True; Normalized: in Boolean := False) return Num;

  -- Purpose : Obtain the Entropy of each Vertex
  -- Note    : If weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Entropy is 1 if Unweighted and Normalized
  --
  -- Gr      : The Graph
  -- Ld      : The Links Direction
  -- Weighted: True if the Weights of the links are considered
  -- Normalized: True if Normalized Entropy is required
  -- return  : The Vertices Entropy
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Entropy(Gr: in Graph; Ld: in Links_Direction := From_Links; Weighted: in Boolean := True; Normalized: in Boolean := False) return PNums;

  -- Purpose : Obtain the Average Entropy of the Vertices in a Graph
  -- Note    : If weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Average Entropy is 1 if Unweighted and Normalized
  --
  -- Gr      : The Graph
  -- Ld      : The Links Direction
  -- Weighted: True if the Weights of the links are considered
  -- Normalized: True if Normalized Entropy is required
  -- return  : The Average Entropy
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Average_Entropy(Gr: in Graph; Ld: in Links_Direction := From_Links; Weighted: in Boolean := True; Normalized: in Boolean := False) return Num;

  -- Purpose : Obtain the Shortest Paths and Lengths for all pairs of vertices
  -- Note    : Weights must be non-negative, otherwise exception is raised
  -- Note    : Floyd-Warshall Algorithm
  --
  -- Gr      : The Graph
  -- Dists   : The Shortest Path Lengths
  -- Preds   : The Predecessors in the Shortest Paths
  -- Allow_Infinite_Distances: True to allow Infinite Distances, otherwise raise Disconnected_Graph_Error
  -- Weighted: True if the Weights of the links are considered
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  procedure Shortest_Paths(Gr: in Graph; Dists: out PNumss; Preds: out PIntegerss; Allow_Infinite_Distances: in Boolean := False; Weighted: in Boolean := True);

  -- Purpose : Obtain a Shortest Path between two vertices
  -- Note    : Returns empty path if Shortest Path does not exist
  -- Note    : Returns empty path if From and To are not different vertices
  --
  -- V_From  : The origin Vertex of the Shortest Path
  -- V_To    : The destination Vertex of the Shortest Path
  -- Dists   : The Shortest Path Lengths
  -- Preds   : The Predecessors in the Shortest Paths
  -- return  : The Shortest Path
  -- raises  : Incompatible_Graphs_Error
  function Shortest_Path(V_From, V_To: in Vertex; Dists: in PNumss; Preds: in PIntegerss) return Graph_Path;

  -- Purpose : Obtain the Shortest Path Lengths from or to a given Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Dijkstra's Algorithm if Weighted Graph, Breadth First Search if Unweighted
  --
  -- V       : The Vertex
  -- Allow_Infinite_Distances: True to allow Infinite Distances, otherwise raise Disconnected_Graph_Error
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Shortest Path Lengths
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Shortest_Path_Length(V: in Vertex; Allow_Infinite_Distances: in Boolean := False; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums;

  -- Purpose : Obtain the Average Path Length from or to a given Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- V       : The Vertex
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Average Path Length
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Average_Path_Length(V: in Vertex; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return Num;

  -- Purpose : Obtain the Average Path Length from or to each Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Average Path Lengths
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Average_Path_Length(Gr: in Graph; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums;

  -- Purpose : Obtain the all-pairs Average Path Length of a Graph
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Average Path Length
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Average_Path_Length(Gr: in Graph; Weighted: in Boolean := True) return Num;

  -- Purpose : Obtain the Maximum Path Length from or to a given Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- V       : The Vertex
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Maximum Path Length
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Maximum_Path_Length(V: in Vertex; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return Num;

  -- Purpose : Obtain the Maximum Path Length from or to each Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Maximum Path Length
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Maximum_Path_Length(Gr: in Graph; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums;

  -- Purpose : Obtain the all-pairs Diameter of a Graph
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Diameter
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Disconnected_Graph_Error
  -- raises  : Negative_Weights_Error
  function Diameter(Gr: in Graph; Weighted: in Boolean := True) return Num;

  -- Purpose : Obtain the Efficiency from a given Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- V       : The Vertex
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Efficiency
  -- raises  : Negative_Weights_Error
  function Efficiency(V: in Vertex; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return Num;

  -- Purpose : Obtain the Efficiency from each Vertex
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- return  : The Efficiencies
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Efficiency(Gr: in Graph; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums;

  -- Purpose : Obtain the Efficiency of a Graph
  -- Note    : If Weighted, weights must be non-negative, otherwise exception is raised
  -- Defined : V. Latora, M. Marchiori: Phys. Rev. Lett., 87:198701, 2001
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Efficiency
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Efficiency(Gr: in Graph; Weighted: in Boolean := True) return Num;

  -- Purpose : Obtain the Clustering Coefficient of a given Vertex
  -- Note    : Self-Loops are ignored
  -- Defined : Fagiolo: Phys. Rev. E, 76:026107, 2007
  -- Defined : Ahnert, Garlaschelli, Fink, Caldarelli: Phys. Rev. E, 76:016101, 2007
  --
  -- V       : The Vertex
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Clustering Coefficient
  function Clustering_Coefficient(V: in Vertex; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Clustering Coefficient of each Vertex
  -- Note    : Self-Loops are ignored
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- return  : The Clustering Coefficients
  -- raises  : Uninitialized_Graph_Error
  function Clustering_Coefficient(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Average Clustering Coefficient of a Graph
  -- Note    : Self-Loops are ignored
  --
  -- Gr      : The Graph
  -- return  : The Average Clustering Coefficient
  -- Weighted: True if the Weights of the links are considered
  -- raises  : Uninitialized_Graph_Error
  function Average_Clustering_Coefficient(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Betweenness of each Vertex
  -- Note    : If weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Self-Loops are ignored
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- Normalized: True if Normalized Betweenness is required
  -- return  : The Vertices Betweenness
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Vertex_Betweenness(Gr: in Graph; Weighted: in Boolean := True; Normalized: in Boolean := True) return PNums;

  -- Purpose : Obtain the Betweenness of each Edge
  -- Note    : If weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Self-Loops are ignored
  --
  -- Gr      : The Graph
  -- Weighted: True if the Weights of the links are considered
  -- Normalized: True if Normalized Betweenness is required
  -- return  : The Edge Betweenness
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  function Edge_Betweenness(Gr: in Graph; Weighted: in Boolean := True; Normalized: in Boolean := True) return Graphs_Double.Graph;

  -- Purpose : Obtain the Betweenness of Vertices and Edges
  -- Note    : If weighted, weights must be non-negative, otherwise exception is raised
  -- Note    : Self-Loops are ignored
  --
  -- Gr      : The Graph
  -- Vertex_Bet: The Vertex Betweenness
  -- Edge_Bet: The Edge Betweenness
  -- Weighted: True if the Weights of the links are considered
  -- Normalized: True if Normalized Betweenness is required
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Negative_Weights_Error
  procedure Betweenness(Gr: in Graph; Vertex_Bet: out PNums; Edge_Bet: out Graphs_Double.Graph; Weighted: in Boolean := True; Normalized: in Boolean := True);

  -- Purpose : Obtain the Correlation of given Node values for linked Nodes of a Graph
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- Gr      : The Graph
  -- Pfrom   : The Values at the Nodes origin of Links
  -- Pto     : The Values at the Nodes destination of the Links
  -- Weighted: True if the Weights of the links are considered
  -- Ls      : The Links Subset
  -- Ct      : The Correlation type
  -- return  : The Linked Nodes Correlation
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  function Linked_Nodes_Correlation(Gr: in Graph; Pfrom, Pto: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson) return Num;

  -- Purpose : Obtain the Correlation Error of given Node values for linked Nodes of a Graph
  -- Note    : If Weighted, the absolute value of the weights are used
  -- Note    : Jackknife method
  --
  -- Gr      : The Graph
  -- Pfrom   : The Values at the Nodes origin of Links
  -- Pto     : The Values at the Nodes destination of the Links
  -- Weighted: True if the Weights of the links are considered
  -- Ls      : The Links Subset
  -- Ct      : The Correlation type
  -- Cet     : The Correlation Error type
  -- return  : The Linked Nodes Correlation Error
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  function Linked_Nodes_Correlation_Error(Gr: in Graph; Pfrom, Pto: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson; Cet: in Stats.Correlation_Error_Type := Auto) return Num;

  -- Purpose : Obtain the Assortativity of a Graph
  -- Note    : Assortativity equals the Weigthed/Unweighted Linked Nodes Correlation of the Nodes' Strengths/Degrees
  -- Note    : For the Weighting, the absolute value of the weights are used
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- Ct      : The Correlation type
  -- return  : The Assortativity
  -- raises  : Uninitialized_Graph_Error
  function Assortativity(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson) return Num;

  -- Purpose : Obtain the Assortativity Error of a Graph
  -- Note    : For the Weighting, the absolute value of the weights are used
  -- Note    : Jackknife method
  --
  -- Gr      : The Graph
  -- Ls      : The Links Subset
  -- Ct      : The Correlation type
  -- Cet     : The Correlation Error type
  -- return  : The Assortativity Error
  -- raises  : Uninitialized_Graph_Error
  function Assortativity_Error(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson; Cet: in Stats.Correlation_Error_Type := Auto) return Num;

  -- Purpose : Obtain the Average of given Node values of Nearest Neighbors from a given Vertex
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- V       : The Vertex
  -- P       : The Values at the Nodes
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Nearest Neighbors Average
  -- raises  : Incompatible_Values_Error
  function Nearest_Neighbors_Average(V: in Vertex; P: in PNums; Weighted: in Boolean := True; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Average of given Node values of Nearest Neighbors from a given Vertex
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- V       : The Vertex
  -- P       : The Values at the Nodes
  -- Weighted: True if the Weights of the links are considered
  -- Ls      : The Links Subset
  -- return  : The Nearest Neighbors Average
  -- raises  : Incompatible_Values_Error
  function Nearest_Neighbors_Average_From(V: in Vertex; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Average of given Node values of Nearest Neighbors to a given Vertex
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- V       : The Vertex
  -- P       : The Values at the Nodes
  -- Weighted: True if the Weights of the links are considered
  -- Ls      : The Links Subset
  -- return  : The Nearest Neighbors Average
  -- raises  : Incompatible_Values_Error
  function Nearest_Neighbors_Average_To(V: in Vertex; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num;

  -- Purpose : Obtain the Average of given Node values of Nearest Neighbors from each Vertex
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- Gr      : The Graph
  -- P       : The Values at the Nodes
  -- Weighted: True if the Weights of the links are considered
  -- Ld      : The Links Direction
  -- Ls      : The Links Subset
  -- return  : The Nearest Neighbors Averages
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  function Nearest_Neighbors_Average(Gr: in Graph; P: in PNums; Weighted: in Boolean := True; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Average of given Node values of Nearest Neighbors from each Vertex
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- Gr      : The Graph
  -- P       : The Values at the Nodes
  -- Weighted: True if the Weights of the links are considered
  -- Ls      : The Links Subset
  -- return  : The Nearest Neighbors Averages
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  function Nearest_Neighbors_Average_From(Gr: in Graph; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return PNums;

  -- Purpose : Obtain the Average of given Node values of Nearest Neighbors to each Vertex
  -- Note    : If Weighted, the absolute value of the weights are used
  --
  -- Gr      : The Graph
  -- P       : The Values at the Nodes
  -- Weighted: True if the Weights of the links are considered
  -- Ls      : The Links Subset
  -- return  : The Nearest Neighbors Averages
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  function Nearest_Neighbors_Average_To(Gr: in Graph; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return PNums;

end Graphs.Properties;
