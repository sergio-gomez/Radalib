-- Radalib, Copyright (c) 2021 by
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


-- @filename Dendrograms-IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 01/02/2016
-- @brief Input and Output of Dendrograms

with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Trees.IO;

package Dendrograms.IO is

  function To_S(Nod_Inf: in Node_Info; Precision: in Natural; Format: in Tree_Format) return String;
  procedure Get_Node_Info(Nod_Inf: out Node_Info; Format: in Tree_Format);

  package Trees_IO_Dendro is new Trees_Dendrograms.IO(To_S, Get_Node_Info);

  -- Purpose : Put Dendrogram to Current Output or File in Text, Newick or JSON formats
  -- Note    : Names of Nodes must not contain these characters (no check is made):
  -- Note    :   ' '   ','   ';'   ':'   '|'   '['    ']'   '('   ')'   '{'   '}'
  --
  -- T       : The Dendrogram
  -- Precision: The Precision
  -- Format  : Tree Format
  -- raises  : Empty_Tree_Error
  -- raises  : Tree_IO_Error
  procedure Put_Dendrogram(T: in Dendrogram; Precision: in Natural; Format: in Tree_Format := Default_Tree_Format) renames Trees_IO_Dendro.Put_Tree;
  procedure Put_Dendrogram(Ft: in File_Type; T: in Dendrogram; Precision: in Natural; Format: in Tree_Format := Default_Tree_Format) renames Trees_IO_Dendro.Put_Tree;
  procedure Put_Dendrogram(Fn: in String; T: in Dendrogram; Precision: in Natural; Mode: in File_Mode := Out_File; Format: in Tree_Format := Default_Tree_Format) renames Trees_IO_Dendro.Put_Tree;

  -- Purpose : Get Dendrogram from Current Input or File in Text, Newick or JSON formats
  --
  -- T       : The Dendrogram
  -- raises  : Tree_IO_Error
  procedure Get_Dendrogram(T: out Dendrogram);
  procedure Get_Dendrogram(Ft: in File_Type; T: out Dendrogram);
  procedure Get_Dendrogram(Fn: in String; T: out Dendrogram);

  -- Purpose : Put Dendrogram Structure to Current Output or File
  -- Note    : The output fields are:
  -- Note    :   Name Position Height Width Length Margin Kind Id Parent_Id
  --
  -- T       : The Dendrogram
  -- Aft     : The number of digits after the decimal point
  procedure Put_Dendrogram_Structure(T: in Dendrogram; Aft: in Field := Default_Double_Aft);
  procedure Put_Dendrogram_Structure(Ft: in File_Type; T: in Dendrogram; Aft: in Field := Default_Double_Aft);
  procedure Put_Dendrogram_Structure(Fn: in String; T: in Dendrogram; Mode: in File_Mode := Out_File; Aft: in Field := Default_Double_Aft);

  -- Purpose : Put Dendrogram Plot Information to Current Output or File
  -- Note    : The output fields are:
  -- Note    :   Kind Id Pos1 Height1 Pos2 Height2 Name
  --
  -- T       : The Dendrogram
  -- Include_Bands: True to Include the Bands in the output
  -- Aft     : The number of digits after the decimal point
  procedure Put_Dendrogram_Plot_Info(T: in Dendrogram; Include_Bands: Boolean := True; Aft: in Field := Default_Double_Aft);
  procedure Put_Dendrogram_Plot_Info(Ft: in File_Type; T: in Dendrogram; Include_Bands: Boolean := True; Aft: in Field := Default_Double_Aft);
  procedure Put_Dendrogram_Plot_Info(Fn: in String; T: in Dendrogram; Include_Bands: Boolean := True; Mode: in File_Mode := Out_File; Aft: in Field := Default_Double_Aft);

private

  procedure Get_Tree(T: out Dendrogram) renames Trees_IO_Dendro.Get_Tree;
  procedure Get_Tree(Ft: in File_Type; T: out Dendrogram) renames Trees_IO_Dendro.Get_Tree;
  procedure Get_Tree(Fn: in String; T: out Dendrogram) renames Trees_IO_Dendro.Get_Tree;

  procedure Complete_Information_Text(T: in Dendrogram);
  procedure Complete_Information_Newick(T: in Dendrogram);
  procedure Complete_Information_Json(T: in Dendrogram);

  procedure Set_Id(T: in Dendrogram);
  procedure Set_Num_Leaves(T: in Dendrogram);
  procedure Set_Heights(T: in Dendrogram);
end Dendrograms.IO;
