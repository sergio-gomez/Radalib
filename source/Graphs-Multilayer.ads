-- Radalib, Copyright (c) 2015 by
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


-- @filename Graphs-Multilayer.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/10/2014
-- @revision 14/01/2015
-- @brief Multilayer Graphs

with Utils; use Utils;
with Utils_Generics; use Utils_Generics;

generic
  with function "+"(Left, Right: in Edge_Value) return Edge_Value is <>;
package Graphs.Multilayer is

  -- Graph arrays
  type Graphs is array(Integer range <>) of Graph;
  type PGraphs is access Graphs;

  -- Multiplex type
  type Multiplex is private;

  Uninitialized_Multiplex_Error: exception;
  Incompatible_Multiplex_Error: exception;
  Layer_Index_Error: exception;


  -- Purpose : Allocate a Graph array
  -- Note    : The individual Graphs are not Initialized
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the Graph array
  function Alloc is new Alloc_1D_Array(Graph, Graphs, PGraphs);

  -- Purpose : Deallocate the space used by a Graph array
  -- Note    : The individual Graphs are not Removed, only the array
  --
  -- P       : The access to the array
  procedure Free is new Free_1D_Array(Graph, Graphs, PGraphs);

  -- Purpose : Initialize a Multiplex to a given number of Layers and Vertices
  -- Note    : Layers are Initialized to empty Graphs
  --
  -- Mpx          : The Multiplex
  -- Num_Layers   : The number of Layers
  -- Num_Vertices : The number of Vertices
  -- Directed     : True for Directed Multiplex, False for Undirected Multiplex
  procedure Initialize(Mpx: out Multiplex; Num_Layers, Num_Vertices: in Positive; Directed: in Boolean);

  -- Purpose : To know whether a Multiplex is Initialized
  --
  -- Mpx     : The Multiplex
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Mpx: in Multiplex) return Boolean;

  -- Purpose : Deallocate all the space used by a Multiplex
  --
  -- Mpx     : The Multiplex
  procedure Free(Mpx: in out Multiplex);

  -- Purpose : Create a copy of a Multiplex
  -- Note    : Current positions and Saved positions are not cloned
  --
  -- Mpx     : The Multiplex
  -- return  : The Clone
  -- raises  : Uninitialized_Multiplex_Error
  function Clone(Mpx: in Multiplex) return Multiplex;

  -- Purpose : Obtain the number of Layers in a Multiplex
  --
  -- Mpx     : The Multiplex
  -- return  : The number of Layers
  -- raises  : Uninitialized_Multiplex_Error
  function Number_Of_Layers(Mpx: in Multiplex) return Natural;

  -- Purpose : Obtain the number of Vertices in a Multiplex
  --
  -- Mpx     : The Multiplex
  -- return  : The number of Vertices
  -- raises  : Uninitialized_Multiplex_Error
  function Number_Of_Vertices(Mpx: in Multiplex) return Natural;

  -- Purpose : To know if a Multiplex is Directed
  -- Note    : Undirected if so in all Layers, otherwise Directed
  --
  -- Mpx     : The Multiplex
  -- return  : True if Directed, False if Undirected
  -- raises  : Uninitialized_Multiplex_Error
  function Is_Directed(Mpx: in Multiplex) return Boolean;

  -- Purpose : Set all Layers of a Multiplex
  -- Note    : Previous Layers are removed
  -- Note    : If Layers and/or Names are empty, the previous ones are conserved
  --
  -- Mpx     : The Multiplex
  -- Grs     : The Layers
  -- Names   : The Layers Names
  -- raises  : Uninitialized_Multiplex_Error
  -- raises  : Incompatible_Multiplex_Error
  procedure Set_Layers(Mpx: in Multiplex; Grs: in PGraphs; Names: in PUstrings := null);

  -- Purpose : Set a Layer of a Multiplex
  -- Note    : Previous Layer is removed
  -- Note    : If Name is empty, the previous one is conserved
  --
  -- Mpx     : The Multiplex
  -- Index   : The Layer Index
  -- Gr      : The Layer Graph
  -- Name    : The Layer Name
  -- raises  : Uninitialized_Multiplex_Error
  -- raises  : Incompatible_Multiplex_Error
  -- raises  : Layer_Index_Error
  procedure Set_Layer(Mpx: in Multiplex; Index: in Positive; Gr: in Graph; Name: in Ustring := Null_Ustring);

  -- Purpose : Set the Name of a Layer of a Multiplex
  --
  -- Mpx     : The Multiplex
  -- Index   : The Layer Index
  -- Name    : The Layer Name
  -- raises  : Uninitialized_Multiplex_Error
  -- raises  : Layer_Index_Error
  procedure Set_Layer_Name(Mpx: in Multiplex; Index: in Positive; Name: in Ustring);

  -- Purpose : Get all Layers of a Multiplex
  -- Note    : Changes to the returned Layers will affect the Multiplex, and no checks available
  --
  -- Mpx     : The Multiplex
  -- return  : The Layers
  -- raises  : Uninitialized_Multiplex_Error
  function Get_Layers(Mpx: in Multiplex) return PGraphs;

  -- Purpose : Get a Layer of a Multiplex
  -- Note    : Changes to the returned Layer will affect the Multiplex, and no checks available
  --
  -- Mpx     : The Multiplex
  -- Index   : The Layer Index
  -- return  : The Layer Graph
  -- raises  : Uninitialized_Multiplex_Error
  -- raises  : Layer_Index_Error
  function Get_Layer(Mpx: in Multiplex; Index: in Positive) return Graph;

  -- Purpose : Get the Name of a Layer of a Multiplex
  --
  -- Mpx     : The Multiplex
  -- Index   : The Layer Index
  -- return  : The Layer Name
  -- raises  : Uninitialized_Multiplex_Error
  -- raises  : Layer_Index_Error
  function Get_Layer_Name(Mpx: in Multiplex; Index: in Positive) return Ustring;

  -- Purpose : Get the Aggregated Graph of a Multiplex
  -- Note    : All Layers aggegated if null List of Layer Indices
  --
  -- Mpx     : The Multiplex
  -- Indices : The Layer Indices
  -- return  : The Aggregated Graph
  -- raises  : Uninitialized_Multiplex_Error
  -- raises  : Layer_Index_Error
  function Aggregate_Layers(Mpx: in Multiplex; Indices: in PIntegers := null; Weighted: in Boolean := True) return Graph;


private

  ---------------
  -- Multiplex --
  ---------------

  type Multiplex_Rec is record
    Layer: PGraphs;
    Name: PUstrings;
  end record;

  type Multiplex is access Multiplex_Rec;


  -- Purpose : Sum of two Graphs
  -- Note    : Vertices Names and Tags are inherited from the Left Graph
  --
  -- Left    : The Left Graph
  -- Right   : The Right Graph
  -- return  : The output Graph
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Graphs_Error
  function Sum(Gr_Left, Gr_Right: in Graph; Weighted: in Boolean := True) return Graph;

end Graphs.Multilayer;
