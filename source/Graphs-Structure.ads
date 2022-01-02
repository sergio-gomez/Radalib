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


-- @filename Graphs-Structure.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2009
-- @revision 02/03/2012
-- @brief Graphs Structure

with Utils; use Utils;

generic
  Zero_Value: Edge_Value;
  type Edge_Valuess is array(Integer range <>, Integer range <>) of Edge_Value;
  type PEdge_Valuess is access Edge_Valuess;
  with function Alloc(First1, Last1, First2, Last2: in Integer) return PEdge_Valuess is <>;
package Graphs.Structure is

  -- Graph Structure
  type Public_Edge is record
    Index: Positive;
    Value: Edge_Value;
  end record;

  type Public_Edges is array(Integer range <>) of Public_Edge;
  type PPublic_Edges is access Public_Edges;

  type Public_Vertex is record
    From: PPublic_Edges;
    To: PPublic_Edges;
  end record;

  type Public_Vertices is array(Integer range <>) of Public_Vertex;
  type Public_Graph is access Public_Vertices;


  -- Purpose : Adjacency Matrix of a Graph
  --
  -- Gr      : The Graph
  -- return  : The Adjacency Matrix
  -- raises  : Uninitialized_Graph_Error
  function Adjacency_Matrix(Gr: in Graph) return PIntegerss;

  -- Purpose : Weights Matrix of a Graph
  --
  -- Gr      : The Graph
  -- return  : The Weights Matrix
  -- raises  : Uninitialized_Graph_Error
  function Weights_Matrix(Gr: in Graph) return PEdge_Valuess;

  -- Purpose : Structure of a Graph
  --
  -- Gr      : The Graph
  -- return  : The Public Graph Structure
  -- raises  : Uninitialized_Graph_Error
  function Graph_Structure(Gr: in Graph) return Public_Graph;

  -- Purpose : Convert a Graph Structure into a Graph
  --
  -- Pgr     : The Public Graph Structure
  -- return  : The Graph
  -- raises  : Uninitialized_Graph_Error
  function Graph_Structure_To_Graph(Pgr: in Public_Graph) return Graph;

  -- Purpose : Deallocate all the space used by a Public Graph
  --
  -- Pgr     : The Public Graph
  procedure Free(Pgr: in out Public_Graph);

end Graphs.Structure;
