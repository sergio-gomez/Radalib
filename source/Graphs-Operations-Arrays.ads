-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


-- @filename Graphs-Operations.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/08/2009
-- @revision 24/08/2020
-- @brief Implementation of Graphs operations

generic
  type Edge_Values is array(Integer range <>) of Edge_Value;
  type PEdge_Values is access Edge_Values;
  type Edge_Valuess is array(Integer range <>, Integer range <>) of Edge_Value;
  type PEdge_Valuess is access Edge_Valuess;
  with function Alloc(First: in Integer; Last: in Integer) return PEdge_Values is <>;
  with function Alloc(First1, Last1, First2, Last2: in Integer) return PEdge_Valuess is <>;
package Graphs.Operations.Arrays is

    Incompatible_Dimensions_Error: exception;
    Uninitialized_Vector_Error: exception;

  -- Purpose : Multiplication of a Graph and a Vector
  --
  -- Gr      : The Graph
  -- V       : The Vector
  -- return  : The output Vector
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Incompatible_Dimensions_Error
  function "*"(Gr: in Graph; V: in Edge_Values) return Edge_Values;

  -- Purpose : Multiplication of a Graph and a Vector
  --
  -- Gr      : The Graph
  -- V       : The Vector
  -- return  : The output Vector
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Uninitialized_Vector_Error
  -- raises  : Incompatible_Dimensions_Error
  function "*"(Gr: in Graph; V: in PEdge_Values) return PEdge_Values;

  -- Purpose : Convert a Graph to Array
  -- Note    : Missing edges are converted to Zero
  --
  -- Gr      : The Graph
  -- return  : The Array
  -- raises  : Uninitialized_Graph_Error
  function To_Array(Gr: in Graph) return Edge_Valuess;

  -- Purpose : Convert a Graph to Array
  -- Note    : Missing edges are converted to Zero
  --
  -- Gr      : The Graph
  -- return  : The Array
  -- raises  : Uninitialized_Graph_Error
  function To_Array(Gr: in Graph) return PEdge_Valuess;

end Graphs.Operations.Arrays;
