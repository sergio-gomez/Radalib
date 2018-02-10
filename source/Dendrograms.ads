-- Radalib, Copyright (c) 2018 by
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


-- @filename Dendrograms.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/05/2013
-- @revision 09/03/2015
-- @brief Instantiation of Trees to Dendrograms Node Info Values

with Dendrograms_Nodes; use Dendrograms_Nodes;
with Trees;

package Dendrograms is

  -- Dendrogram definitions
  package Trees_Dendrograms is new Trees(Node_Value => Node_Info, Default_Node_Value => Void_Node_Info);
  use Trees_Dendrograms;
  use Trees_Dendrograms.Nodes_Lists;

  subtype Dendrogram is Tree;
  subtype List_Of_Dendrograms is List_Of_Trees;


  -- Purpose : Set Positions and Widths of the Clusters of a Dendrogram
  --
  -- T       : The Dendrogram
  procedure Set_Positions_And_Widths(T: in Dendrogram);

end Dendrograms;
