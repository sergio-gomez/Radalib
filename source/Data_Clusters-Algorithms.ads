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


-- @filename Data_Clusters-Algorithms.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 20/03/2006
-- @brief Implementation of Clustering algorithms

with Graphs_Simple; use Graphs_Simple;

generic
package Data_Clusters.Algorithms is

  -- Type of functions to compare Data Items
  type Comparator is access function(D1, D2: in Data_Item) return Boolean;

  generic
    with function Friends(D1, D2: in Data_Item) return Boolean is <>;
  -- Purpose : Generate the Friends of Friends Data Clustering of a given Data set
  --
  -- Data    : The Data set
  -- Friends : The function to know whether two Data Items are Friends
  -- Dc      : The Data Clustering
  -- raises  : Void_Data_Set_Error
  procedure Generic_Friends_Of_Friends(Data: in Pdata_Array; Dc: out Data_Clustering);

  -- Purpose : Generate the Friends of Friends Data Clustering of a given Data set
  --
  -- Data    : The Data set
  -- Friends : The function to know whether two Data Items are Friends
  -- Dc      : The Data Clustering
  -- raises  : Void_Data_Set_Error
  procedure Friends_Of_Friends(Data: in Pdata_Array; Friends: in Comparator; Dc: out Data_Clustering);

  generic
    with function Friends(D1, D2: in Data_Item) return Boolean is <>;
  -- Purpose : Generate the Friends of Friends Data Clustering and Graph of a given Data set
  --
  -- Data    : The Data set
  -- Friends : The function to know whether two Data Items are Friends
  -- Dc      : The Data Clustering
  -- Gr      : The Undirected Graph
  -- raises  : Void_Data_Set_Error
  procedure Generic_Friends_Of_Friends_Graph(Data: in Pdata_Array; Dc: out Data_Clustering; Gr: out Graph);

  -- Purpose : Generate the Friends of Friends Data Clustering and Graph of a given Data set
  --
  -- Data    : The Data set
  -- Friends : The function to know whether two Data Items are Friends
  -- Dc      : The Data Clustering
  -- Gr      : The Undirected Graph
  -- raises  : Void_Data_Set_Error
  procedure Friends_Of_Friends_Graph(Data: in Pdata_Array; Friends: in Comparator; Dc: out Data_Clustering; Gr: out Graph);

end Data_Clusters.Algorithms;
