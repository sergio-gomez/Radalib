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


-- @filename Dendrograms-Structure.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/04/2015
-- @revision 13/04/2015
-- @brief Structure of Dendrograms for Export and Plot

with Utils; use Utils;
with Utils_Generics; use Utils_Generics;

package Dendrograms.Structure is

  -- Dendrogram Structure
  type Node_Type is (Leaf, Cluster, Root);

  type Dendrogram_Structure_Rec is record
    Kind      : Node_Type;
    Node_Id   : Integer;
    Parent_Id : Integer;
    Position  : Double;
    Height    : Double;
    Width     : Double;
    Length    : Double;
    Margin    : Double;
    Name      : Ustring;
  end record;

  type Dendrogram_Structure_Recs is array(Integer range <>) of Dendrogram_Structure_Rec;
  type Dendrogram_Structure is access Dendrogram_Structure_Recs;

  function Alloc is new Alloc_1D_Array(Dendrogram_Structure_Rec, Dendrogram_Structure_Recs, Dendrogram_Structure);
  procedure Free is new Free_1D_Array(Dendrogram_Structure_Rec, Dendrogram_Structure_Recs, Dendrogram_Structure);


  -- Dendrogram Plot Information
  type Plot_Info_Type is (Member, Cluster, Band);

  type Dendrogram_Plot_Info_Rec is record
    Kind      : Plot_Info_Type;
    Id        : Integer;
    Position1 : Double;
    Height1   : Double;
    Position2 : Double;
    Height2   : Double;
    Name      : Ustring;
  end record;

  type Dendrogram_Plot_Info_Recs is array(Integer range <>) of Dendrogram_Plot_Info_Rec;
  type Dendrogram_Plot_Info is access Dendrogram_Plot_Info_Recs;

  function Alloc is new Alloc_1D_Array(Dendrogram_Plot_Info_Rec, Dendrogram_Plot_Info_Recs, Dendrogram_Plot_Info);
  procedure Free is new Free_1D_Array(Dendrogram_Plot_Info_Rec, Dendrogram_Plot_Info_Recs, Dendrogram_Plot_Info);


  -- Purpose : Get the Dendrogram Structure
  --
  -- T       : The Dendrogram
  -- return  : The Dendrogram Structure
  function Get_Dendrogram_Structure(T: in Dendrogram) return Dendrogram_Structure;

  -- Purpose : Get the Dendrogram Plot Information
  --
  -- T       : The Dendrogram
  -- return  : The Dendrogram Plot Information
  function Get_Dendrogram_Plot_Info(T: in Dendrogram; Include_Bands: Boolean := True) return Dendrogram_Plot_Info;

end Dendrograms.Structure;
