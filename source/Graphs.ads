-- Radalib, Copyright (c) 2016 by
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


-- @filename Graphs.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 18/11/2004
-- @revision 26/08/2016
-- @brief Treatment of Graphs

with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Linked_Lists;

generic
  type Edge_Value is private;
  Default_Edge_Value: Edge_Value;
package Graphs is

  -- Represents a Graph
  -- It is possible to assign Values to each Edge
  -- The number of Vertices cannot be changed
  type Graph is private;

  -- Represents a Vertex of the Graph
  type Vertex is private;

  -- Represents an Edge of the Graph
  type Edge is private;

  -- Represents a set of Edges to or from a given Vertex
  type Edges_List is private;

  -- Sentinel Edge to represent a non-existent Edge
  No_Edge: constant Edge;

  Uninitialized_Graph_Error: exception;
  Index_Error: exception;
  Incompatible_Graphs_Error: exception;
  Incompatible_Values_Error: exception;
  No_Edge_Error: exception;
  No_More_Edges_Error: exception;
  Non_Unique_Names_Error: exception;
  No_Name_Error: exception;


  -- Purpose : Initialize a Graph to a given number of Vertices
  --
  -- Gr           : The Graph
  -- Num_Vertices : The number of Vertices
  -- Directed     : True for Digraphs, False for Undirected Graphs
  procedure Initialize(Gr: out Graph; Num_Vertices: in Positive; Directed: in Boolean);

  -- Purpose : To know whether a Graph is Initialized
  --
  -- Gr      : The Graph
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Gr: in Graph) return Boolean;

  -- Purpose : Deallocate all the space used by a Graph
  --
  -- Gr      : The Graph
  procedure Free(Gr: in out Graph);

  -- Purpose : Create a copy of a Graph
  -- Note    : Current positions and Saved positions are not cloned
  --
  -- Gr      : The Graph
  -- return  : The Clone
  -- raises  : Uninitialized_Graph_Error
  function Clone(Gr: in Graph) return Graph;

  -- Purpose : Obtain the number of Vertices in a Graph
  --
  -- Gr      : The Graph
  -- return  : The number of Vertices
  -- raises  : Uninitialized_Graph_Error
  function Number_Of_Vertices(Gr: in Graph) return Natural;

  -- Purpose : To know if a Graph is Directed
  --
  -- Gr      : The Graph
  -- return  : True if Directed, False if Undirected
  -- raises  : Uninitialized_Graph_Error
  function Is_Directed(Gr: in Graph) return Boolean;

  -- Purpose : Transform an Undirected Graph to a Directed one
  -- Note    : Directed Graphs are unchanged
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  procedure To_Directed(Gr: in Graph);

  -- Purpose : Transform a Directed Graph to an Undirected one
  -- Note    : Undirected Graphs are unchanged
  -- Note    : Existent Symmetric Edges must share the same Value
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  procedure To_Undirected(Gr: in Graph);

  -- Purpose : To know if a Graph is Symmetric
  -- Note    : Undirected Graphs are Symmetric
  --
  -- Gr      : The Graph
  -- return  : True if Symmetric
  -- raises  : Uninitialized_Graph_Error
  function Is_Symmetric(Gr: in Graph) return Boolean;

  -- Purpose : To know if a Graph is Weighted
  -- Note    : Is considered Weighted if the Values of all Edges are equal to the Default_Edge_Value
  --
  -- Gr      : The Graph
  -- return  : True if Weighted
  -- raises  : Uninitialized_Graph_Error
  function Is_Weighted(Gr: in Graph) return Boolean;

  -- Purpose : Transform a Weighted Graph to an Unweighted one
  -- Note    : Unweighted Graphs are unchanged
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  procedure To_Unweighted(Gr: in Graph);

  -- Purpose : Obtain a Vertex from an index
  --
  -- Gr      : The Graph
  -- P       : The index
  -- return  : The Vertex
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Index_Error
  function Get_Vertex(Gr: in Graph; P: in Positive) return Vertex;

  -- Purpose : Obtain the index of a Vertex
  --
  -- V       : The Vertex
  -- return  : The index
  function Index_Of(V: in Vertex) return Positive;

  -- Purpose : Add an Edge between the given From and To Vertices
  -- Note    : If the Edge already exists, it is replaced by the new one
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- Value   : The Value assigned to the Edge
  -- raises  : Incompatible_Graphs_Error
  procedure Add_Edge(From: in Vertex; To: in Vertex; Value: in Edge_Value := Default_Edge_Value);

  -- Purpose : Add an Edge between the given From and To Vertices
  -- Note    : If the Edge already exists, it is replaced by the new one
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- Value   : The Value assigned to the Edge
  -- return  : The Edge
  -- raises  : Incompatible_Graphs_Error
  function Add_Edge(From: in Vertex; To: in Vertex; Value: in Edge_Value := Default_Edge_Value) return Edge;

  -- Purpose : Add an Edge between the given From and To Vertices
  -- Note    : No check is made on previous existence and Edges Lists may be unsorted
  -- Note    : Unsorted Edges Lists may result in malfunction of several operations on Graphs
  -- Note    : Use Restore_Consistency to bring the Graph to a consistent state once all Edges have been Added
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- Value   : The Value assigned to the Edge
  -- raises  : Incompatible_Graphs_Error
  procedure Add_Edge_Unchecked(From: in Vertex; To: in Vertex; Value: in Edge_Value := Default_Edge_Value);

  -- Purpose : Ensure all Edges Lists are sorted and without repeated Edges
  -- Note    : Needed after the use of Add_Edge_Unchecked
  -- Note    : If an Edge appears several times and the Values are different, an exception is raised
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Values_Error
  procedure Restore_Consistency(Gr: in Graph);

  -- Purpose : To know if an Edge between the given From and To Vertices exists
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- return  : True if Edge exists
  -- raises  : Incompatible_Graphs_Error
  function Edge_Exists(From: in Vertex; To: in Vertex) return Boolean;

  -- Purpose : Get the Edge between the given From and To Vertices
  -- Note    : Exception raised if Edge does not exist
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- return  : The Edge
  -- raises  : Incompatible_Graphs_Error
  -- raises  : No_Edge_Error
  function Get_Edge(From: in Vertex; To: in Vertex) return Edge;

  -- Purpose : Get the Edge between the given From and To Vertices
  -- Note    : No_Edge returned if Edge does not exist
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- return  : The Edge, or No_Edge
  -- raises  : Incompatible_Graphs_Error
  function Get_Edge_Or_No_Edge(From: in Vertex; To: in Vertex) return Edge;

  -- Purpose : Obtain the Number of Edges of a Graph
  --
  -- Gr      : The Graph
  -- return  : The Number of Edges
  -- raises  : Uninitialized_Graph_Error
  function Number_Of_Edges(Gr: in Graph) return Natural;

  -- Purpose : Remove an Edge between the given From and To Vertices
  -- Note    : No exception is raised if the Edge does not exists
  --
  -- From    : The origin Vertex of the Edge
  -- To      : The destination Vertex of the Edge
  -- raises  : Incompatible_Graphs_Error
  procedure Remove_Edge(From: in Vertex; To: in Vertex);

  -- Purpose : Remove all Edges of a Graph
  --
  -- Gr      : The Graph
  -- raises  : Uninitialized_Graph_Error
  procedure Clear(Gr: in Graph);

  -- Purpose : To know if a Vertex has a Self-Loop
  --
  -- V       : The Vertex
  -- return  : True if Vertex has a Self-Loop
  function Has_Self_Loop(V: in Vertex) return Boolean;

  -- Purpose : Get the Self-Loop of a given Vertex
  -- Note    : Exception raised if Self-Loop does not exist
  --
  -- V       : The Vertex
  -- return  : The Edge
  -- raises  : No_Edge_Error
  function Get_Self_Loop(V: in Vertex) return Edge;

  -- Purpose : Get the Self-Loop of a given Vertex
  -- Note    : No_Edge returned if Self-Loop does not exist
  --
  -- V       : The Vertex
  -- return  : The Self-Loop, or No_Edge
  function Get_Self_Loop_Or_No_Edge(V: in Vertex) return Edge;

  -- Purpose : Obtain the Number of Self-Loops of a Graph
  --
  -- Gr      : The Graph
  -- return  : The Number of Self-Loops
  -- raises  : Uninitialized_Graph_Error
  function Number_Of_Self_Loops(Gr: in Graph) return Natural;

  -- Purpose : Obtain the Number of Edges from a given Vertex
  --
  -- V       : The Vertex
  -- return  : The Number of Edges
  function Degree_From(V: in Vertex) return Natural;

  -- Purpose : Obtain the Number of Edges to a given Vertex
  --
  -- V       : The Vertex
  -- return  : The Number of Edges
  function Degree_To(V: in Vertex) return Natural;

  -- Purpose : Obtain the Total Degree of a Graph
  --
  -- Gr      : The Graph
  -- return  : The Total Degree
  -- raises  : Uninitialized_Graph_Error
  function Total_Degree(Gr: in Graph) return Natural;

  -- Purpose : Obtain the List of Edges from a given Vertex
  -- Note    : The List may be void
  --
  -- V       : The Vertex
  -- return  : The Edges List
  function Edges_From(V: in Vertex) return Edges_List;

  -- Purpose : Obtain the List of Edges to a given Vertex
  -- Note    : The List may be void
  --
  -- V       : The Vertex
  -- return  : The Edges List
  function Edges_To(V: in Vertex) return Edges_List;

  -- Purpose : Obtain the number of Edges in an Edges List
  --
  -- El      : The Edges List
  -- return  : The number of Edges
  function Number_Of_Edges(El: in Edges_List) return Natural;

  -- Purpose : To know if there are more Edges left in an Edges List
  --
  -- El      : The Edges List
  -- return  : True if more Edges left
  function Has_Next(El: in Edges_List) return Boolean;

  -- Purpose : Obtain the current Edge in an Edges List
  --
  -- El      : The Edges List
  -- return  : The current Edge
  -- raises  : No_More_Edges_Error
  function Get(El: in Edges_List) return Edge;

  -- Purpose : Obtain the current Vertex in an Edges List
  --
  -- El      : The Edges List
  -- return  : The current Vertex
  -- raises  : No_More_Edges_Error
  function Get(El: in Edges_List) return Vertex;

  -- Purpose : Obtain and skip the next Edge in an Edges List
  --
  -- El      : The Edges List
  -- return  : The next Edge
  -- raises  : No_More_Edges_Error
  function Next(El: in Edges_List) return Edge;

  -- Purpose : Obtain and skip the next Vertex in an Edges List
  --
  -- El      : The Edges List
  -- return  : The next Vertex
  -- raises  : No_More_Edges_Error
  function Next(El: in Edges_List) return Vertex;

  -- Purpose : Skip the next Edge in an Edges List
  --
  -- El      : The Edges List
  -- raises  : No_More_Edges_Error
  procedure Next(El: in Edges_List);

  -- Purpose : Set an Edges List to the first Edge
  --
  -- El      : The Edges List
  procedure Reset(El: in Edges_List);

  -- Purpose : Save the current position in an Edges List
  -- Note    : There is a stack of saved positions
  --
  -- El      : The Edges List
  procedure Save(El: in Edges_List);

  -- Purpose : Restore the current position in an Edges List to the previously saved one
  -- Note    : Saved position must exist, no check is made
  --
  -- El      : The Edges List
  procedure Restore(El: in Edges_List);

  -- Purpose : Remove the current Edge in an Edges List
  -- Note    : Warning! low level procedure:
  -- Note    :   Two Remove calls are necessary,
  -- Note    :   one for Vf.To(Vt) and another for Vt.From(Vf)
  --
  -- El      : The Edges List
  -- raises  : No_More_Edges_Error
  procedure Remove(El: in Edges_List);

  -- Purpose : Obtain the origin Vertex of an Edge
  --
  -- E       : The Edge
  -- return  : The origin Vertex
  function From(E: in Edge) return Vertex;

  -- Purpose : Obtain the destination Vertex of an Edge
  --
  -- E       : The Edge
  -- return  : The destination Vertex
  function To(E: in Edge) return Vertex;

  -- Purpose : Obtain the Value of an Edge
  --
  -- E       : The Edge
  -- return  : The Value
  function Value(E: in Edge) return Edge_Value;

  -- Purpose : Set the Value of an Edge
  --
  -- E       : The Edge
  -- Value   : The Value
  -- raises  : No_Edge_Error
  procedure Set_Value(E: in out Edge; Value: in Edge_Value := Default_Edge_Value);

  -- Purpose : Remove an Edge
  -- Note    : No exception is raised if the Edge does not exists
  --
  -- E       : The Edge
  procedure Remove(E: in out Edge);

  -- Purpose : Set the Name of a Vertex
  --
  -- V       : The Vertex
  -- Name    : The Name
  procedure Set_Name(V: in Vertex; Name: in String);

  -- Purpose : Obtain the Name of a Vertex
  --
  -- V       : The Vertex
  -- return  : The Name
  function Get_Name(V: in Vertex) return String;

  -- Purpose : Set the Tag of a Vertex
  --
  -- V       : The Vertex
  -- Name    : The Tag
  procedure Set_Tag(V: in Vertex; Tag: in String);

  -- Purpose : Obtain the Tag of a Vertex
  --
  -- V       : The Vertex
  -- return  : The Tag
  function Get_Tag(V: in Vertex) return String;

  -- Purpose : Set the status of the Mark of a Vertex
  --
  -- V       : The Vertex
  -- Marked  : The status of the Mark
  procedure Set_Mark(V: in Vertex; Marked: in Boolean);

  -- Purpose : Get the status of a Vertex
  --
  -- V       : The Vertex
  -- return  : True if Marked
  function Is_Marked(V: in Vertex) return Boolean;

  -- Purpose : Set the status of a Vertex to Marked
  --
  -- V       : The Vertex
  procedure Mark(V: in Vertex);

  -- Purpose : Set the status of a Vertex to Not Marked
  --
  -- V       : The Vertex
  procedure Unmark(V: in Vertex);

  -- Purpose : Set the status of all Vertices to Marked
  --
  -- V       : The Vertex
  procedure Mark(Gr: in Graph);

  -- Purpose : Set the status of all Vertices to Not Marked
  --
  -- V       : The Vertex
  procedure Unmark(Gr: in Graph);

  -- Purpose : Update the Names Info of a Graph
  -- Note    : This Info allows to obtain a Vertex from its Name
  -- Note    : This Info is not automatically updated to improve performance and to save space
  --
  -- Gr      : The Graph
  -- raises  : Non_Unique_Names_Error
  -- raises  : Uninitialized_Graph_Error
  procedure Update_Names_Info(Gr: in Graph);

  -- Purpose : Obtain a Vertex from its Name
  -- Note    : A call to Update_Names_Info is done on first call to this function
  -- Note    : If names have changed since first call to Get_Vertex, call Update_Names_Info
  --
  -- Gr      : The Graph
  -- Name    : The Name
  -- return  : The Vertex
  -- raises  : No_Name_Error
  function Get_Vertex(Gr: in Graph; Name: in String) return Vertex;


private

  package Unbounded_Strings_Maps is new Ada.Containers.Hashed_Maps(
    Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
    Element_Type    => Integer,
    Hash            => Ada.Strings.Unbounded.Hash,
    Equivalent_Keys => Ada.Strings.Unbounded."=");


  --------------
  -- Edge_Rec --
  --------------

  type Edge_Rec is record
    Vertex: Positive;
    Value: Edge_Value := Default_Edge_Value;
  end record;

  package Linked_Edge_Recs is new Linked_Lists(Edge_Rec);
  use Linked_Edge_Recs;

  ----------------
  -- Vertex_Rec --
  ----------------

  type Vertex_Rec is record
    From: Linked_List;
    To: Linked_List;
    Name: Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
    Tag: Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
    Marked: Boolean := False;
  end record;

  type Vertex_Recs is array(Positive range <>) of Vertex_Rec;

  -----------
  -- Graph --
  -----------

  type Graph_Rec(Size: Positive) is record
    Vertices: Vertex_Recs(1..Size);
    Directed: Boolean;
    Names_Map: Unbounded_Strings_Maps.Map;
  end record;

  type Graph is access Graph_Rec;

  ------------
  -- Vertex --
  ------------

  type Vertex is record
    Gr: Graph;
    Index: Positive;
  end record;

  ----------
  -- Edge --
  ----------

  type Edge is record
    Gr: Graph;
    From: Positive;
    To: Positive;
    Value: Edge_Value := Default_Edge_Value;
  end record;

  No_Edge: constant Edge := (Gr => null, From => Positive'Last, To => Positive'Last, Value => Default_Edge_Value);

  ----------------
  -- Edges_List --
  ----------------

  type Edges_List is record
    Gr: Graph;
    Index: Positive;
    Index_Is_From: Boolean;
    Ll: Linked_List;
  end record;

end Graphs;
