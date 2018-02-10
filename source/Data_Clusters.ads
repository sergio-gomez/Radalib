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


-- @filename Data_Clusters.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 08/04/2012
-- @brief Treatment of Data Clusters

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;

generic
  type Data_Item is private;
  type Data_Array is array(Integer range <>) of Data_Item;
  type Pdata_Array is access Data_Array;
package Data_Clusters is

  -- Represents the Data Set organized in Clusters
  type Data_Clustering is limited private;


  Void_Data_Set_Error: exception;


  -- Purpose : Initialize a Data Clustering to a given Data set
  --
  -- Dc      : The Data Clustering
  -- Data    : The Data set
  -- raises  : Void_Data_Set_Error
  procedure Initialize(Dc: out Data_Clustering; Data: in Pdata_Array);

  -- Purpose : Deallocate all the space used by a Data Clustering
  -- Note    : The Data Array is not deallocated, only the Clustering information
  --
  -- Dc      : The Data Clustering
  procedure Free(Dc: in out Data_Clustering);

  -- Purpose : Obtain the List of Lists which represents the clustering of the Data Clustering
  --
  -- Dc      : The Data Clustering
  -- return  : The List of Lists
  function Get_Clusters(Dc: in Data_Clustering) return List_Of_Lists;

  -- Purpose : Obtain the Data Item corresponding to a given Element of the Clustering
  --
  -- Dc      : The Data Clustering
  -- E       : The Element
  -- return  : The Data Item
  -- raises  : Incompatible_Lists_Of_Lists_Error
  function Get_Data_Item(Dc: in Data_Clustering; E: in Element) return Data_Item;


private

  type Data_Clustering_Rec is record
    Lol: List_Of_Lists;
    Dat: Pdata_Array;
  end record;

  type Data_Clustering is access Data_Clustering_Rec;

end Data_Clusters;
