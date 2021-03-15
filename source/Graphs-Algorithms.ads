-- Radalib, Copyright (c) 2021 by
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


-- @filename Graphs-Algorithms.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/08/2005
-- @revision 10/09/2020
-- @brief Implementation of Graphs algorithms

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;

generic
  Zero_Value: Edge_Value;
  No_Value: Edge_Value;
  with function "+"(Left, Right: in Edge_Value) return Edge_Value is <>;
  with function "<"(Left, Right: in Edge_Value) return Boolean is <>;
  with function ">"(Left, Right: in Edge_Value) return Boolean is <>;
package Graphs.Algorithms is

  package Linked_Lists_Of_Lists is new Linked_Lists(List);
  use Linked_Lists_Of_Lists;

  type Components_Type is (Weak_Components, Strong_Components);
  type Optimum_Type is (Minimum, Maximum);

  Unknown_Components_Type_Error: exception;
  Unknown_Optimum_Type_Error: exception;
  Incompatible_List_Of_Lists_Error: exception;
  Incompatible_List_Error: exception;
  Void_List_Error: exception;


  -- Purpose : Obtain a Components Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    W | Weak
  -- Note    :    S | Strong
  --
  -- Comp_Name: The Components Type Name
  -- return  : The Components Type
  -- raises  : Unknown_Components_Type_Error
  function To_Components_Type(Comp_Name: in String) return Components_Type;

  -- Purpose : Obtain a Optimum Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    MAX | Maximum
  -- Note    :    MIN | Minimum
  --
  -- Opt_Name: The Optimum Type Name
  -- return  : The Optimum Type
  -- raises  : Unknown_Optimum_Type_Error
  function To_Optimum_Type(Opt_Name: in String) return Optimum_Type;

  -- Purpose : Symmetrize a Directed Graph to an Undirected one
  -- Note    : Undirected Graphs are unchanged
  -- Note    : Undirected Edge Values are the sum of their corresponding Directed Edge Values
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  procedure Symmetrize(Gr: in Graph);

  -- Purpose : To know if a Graph is Connected
  --
  -- Gr      : The Graph
  -- Ct      : Components Type
  -- return  : True if Connected
  function Is_Connected(Gr: in Graph; Ct: in Components_Type := Weak_Components) return Boolean;

  -- Purpose : Calculate the Connected Components of a Graph
  -- Note    : Weak and Strong connected components are equal for undirected networks
  --
  -- Gr      : The Graph
  -- Lol     : The List of Lists of Connected Components
  -- Ct      : Components Type
  -- raises  : Uninitialized_Graph_Error
  procedure Connected_Components(Gr: in Graph; Lol: out List_Of_Lists; Ct: in Components_Type := Weak_Components);

  -- Purpose : Calculate the Weak Connected Components of a Graph within a List of Lists
  -- Note    : Strong connected components not available
  --
  -- Gr      : The Graph
  -- Lol_In  : The input List of Lists
  -- Lol_Out : The output List of Lists of Connected Components
  -- raises  : Uninitialized_Graph_Error
  procedure Connected_Components(Gr: in Graph; Lol_In: in List_Of_Lists; Lol_Out: out List_Of_Lists);

  -- Purpose : Update the Weak Connected Components of a Graph within a List
  -- Note    : Strong connected components not available
  -- Note    : If the List is connected, it becomes the only member of the output Queue
  --
  -- Gr      : The Graph
  -- L       : The input List
  -- Ls_Out  : The Linked List containing the new Connected Components of the List
  -- raises  : Uninitialized_Graph_Error
  procedure Update_List_Connected_Components(Gr: in Graph; L: in List; Ls_Out: out Linked_Lists_Of_Lists.Linked_List);

  -- Purpose : Isolate a List of Vertices from a Graph
  -- Note    : Edges with one end in the List and the other out of the List are removed
  --
  -- Gr      : The Graph
  -- L       : The List to Isolate
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_List_Error
  procedure Isolate_List(Gr: in Graph; L: in List);

  -- Purpose : Remove all internal edges of a List of Vertices from a Graph
  --
  -- Gr      : The Graph
  -- L       : The List to Isolate
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_List_Error
  procedure Clear_List(Gr: in Graph; L: in List);

  -- Purpose : Create the Subgraph formed by the Vertices in a List
  -- Note    : The order of the Vertices in the List is mantained
  --
  -- Gr      : The Graph
  -- L       : The List of Vertices of the Subgraph
  -- Sub_Gr  : The Subgraph
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_List_Error
  -- raises  : Void_List_Error
  procedure Create_Subgraph(Gr: in Graph; L: in List; Sub_Gr: out Graph);

  -- Purpose : Renormalize a Graph according to a Renormalizing List of Lists
  -- Note    : Empty Lists generate isolated Vertices in the Renormalized Graph
  -- Note    : Names of Renormalized Vertices are those of the first Element of the Lists with a '*' appended
  -- Note    : Tags of Renormalized Vertices are those of the first Element of the Lists
  -- Note    : Resistance ignored if equals Zero_Value or No_Value
  -- Note    : For Bipartite Graphs, the Renormalizing List of Lists is Sorted by first Element
  --
  -- Gr      : The Graph
  -- Ren     : The Renormalizing List of Lists
  -- Gr_Ren  : The Renormalized Graph
  -- R       : The Resistance
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_List_Of_Lists_Error
  procedure Renormalize_Graph(Gr: in Graph; Ren: in List_Of_Lists; Gr_Ren: out Graph; R: in Edge_Value := No_Value);

  -- Purpose : Unrenormalize a List of Lists according to a Renormalizing List of Lists
  --
  -- Lol_Ren : The Renormalized List of Lists
  -- Ren     : The Renormalizing List of Lists
  -- Lol     : The Unrenormalized List of Lists
  -- raises  : Incompatible_Lists_Of_Lists_Error
  procedure Unrenormalize_List_Of_Lists(Lol_Ren: in List_Of_Lists; Ren: in List_Of_Lists; Lol: out List_Of_Lists);

  -- Purpose : Calculate the Minimum or Maximum Spanning Tree of a Graph
  -- Note    : If the Graph is not strongly connected, the Tree becomes a Forest
  -- Note    : Implements Kruskal's algorithm
  --
  -- Gr      : The Graph
  -- Gr_MST  : The Spanning Tree
  -- Optim   : The Optimum Type
  -- raises  : Uninitialized_Graph_Error
  procedure Spanning_Tree(Gr: in Graph; Gr_Mst: out Graph; Optim: in Optimum_Type := Minimum);

  -- Purpose : Remove Edges within the same Class of nodes in a Bipartite Graph
  -- Note    : Graph unchanged if not Bipartite
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  procedure Force_Bipartiteness(Gr: in Graph);

end Graphs.Algorithms;
