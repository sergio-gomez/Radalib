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


-- @filename Friends_Of_Friends_Utils.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/10/2004
-- @revision 5/04/2005
-- @brief Friends of Friends Clustering Utils

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Simple; use Graphs_Simple;
with Data_Clusters.Algorithms;

package Friends_Of_Friends_Utils is

  Data_Dim: constant := 3;

  type Data_Item is array(1..Data_Dim) of Float;
  type Data_Array is array(Integer range <>) of Data_Item;
  type Pdata_Array is access Data_Array;

  package These_Data_Clusters is new Data_Clusters(Data_Item, Data_Array, Pdata_Array);
  use These_Data_Clusters;
  package These_Algorithms is new These_Data_Clusters.Algorithms;
  use These_Algorithms;

  procedure Read_Data(Fn: in String; Num_Data: in Positive; Data: out Pdata_Array);
  procedure Make_Fof(Data: in Pdata_Array; Cut: in Float);
  procedure Make_Fof_Graph(Data: in Pdata_Array; Cut: in Float);
  procedure Free(Data: in out Pdata_Array);

private

  function Euclidean_Friends(D1, D2: in Data_Item) return Boolean;

  procedure Put(L: in List);
  procedure Put(Lol: in List_Of_Lists);
  procedure Put(Gr: in Graph);

end Friends_Of_Friends_Utils;
