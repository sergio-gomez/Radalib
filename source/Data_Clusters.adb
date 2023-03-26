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


-- @filename Data_Clusters.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 08/04/2012
-- @brief Treatment of Data Clusters

with Ada.Unchecked_Deallocation;

package body Data_Clusters is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Data_Clustering_Rec, Data_Clustering);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize (Dc: out Data_Clustering; Data: in Pdata_Array) is
  begin
    if Data = null then
      raise Void_Data_Set_Error;
    end if;
    Dc := new Data_Clustering_Rec;
    Dc.Dat := Data;
    Initialize(Dc.Lol, Data'Length);
  end Initialize;

  ----------
  -- Free --
  ----------

  procedure Free(Dc: in out Data_Clustering) is
  begin
    if Dc /= null then
      Free(Dc.Lol);
      Dispose(Dc);
      Dc := null;
    end if;
  end Free;

  ------------------
  -- Get_Clusters --
  ------------------

  function Get_Clusters (Dc: in Data_Clustering) return List_Of_Lists is
  begin
    return Dc.Lol;
  end Get_Clusters;

  -------------------
  -- Get_Data_Item --
  -------------------

  function Get_Data_Item(Dc: in Data_Clustering; E: in Element) return Data_Item is
  begin
    if not Belongs_To(E, Dc.Lol) then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;
    return Dc.Dat(Dc.Dat'First + Index_Of(E) - 1);
  end Get_Data_Item;

end Data_Clusters;

