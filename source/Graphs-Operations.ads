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


-- @filename Graphs-Operations.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/08/2009
-- @revision 08/09/2020
-- @brief Implementation of Graphs operations

generic
  Zero_Value: Edge_Value;
  with function "+"(Left, Right: in Edge_Value) return Edge_Value is <>;
  with function "*"(Left, Right: in Edge_Value) return Edge_Value is <>;
package Graphs.Operations is

  -- Purpose : Transpose a Graph
  -- Note    : A new Graph is returned even if its undirected
  --
  -- Gr      : The Graph
  -- return  : The Transposed Graph
  -- raises  : Uninitialized_Graph_Error
  function Transpose(Gr: in Graph) return Graph;

  -- Purpose : Sum of two Graphs
  -- Note    : Vertices Names and Tags are inherited from the Left Graph
  --
  -- Gr_Left : The Left Graph
  -- Gr_Right: The Right Graph
  -- return  : The output Graph
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Graphs_Error
  function "+"(Gr_Left, Gr_Right: in Graph) return Graph;

  -- Purpose : Multiplication of two Graphs
  -- Note    : Vertices Names and Tags are inherited from the Left Graph
  --
  -- Gr_Left : The Left Graph
  -- Gr_Right: The Right Graph
  -- return  : The output Graph
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Graphs_Error
  function "*"(Gr_Left, Gr_Right: in Graph) return Graph;

  -- Purpose : Power of a Graph
  -- Note    : A new Graph is returned even if the exponent is 1
  --
  -- Gr      : The Graph
  -- return  : The Power Graph
  -- raises  : Uninitialized_Graph_Error
  function "**"(Gr: in Graph; Power: in Natural) return Graph;

end Graphs.Operations;
