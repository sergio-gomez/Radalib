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


-- @filename Dendrograms_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/05/2013
-- @revision 28/12/2017
-- @brief Test of Dendrograms packages

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Utils; use Utils;
with Data_IO_Double; use Data_IO_Double;
with Dendrograms_Nodes; use Dendrograms_Nodes;
with Dendrograms; use Dendrograms;
with Dendrograms.IO; use Dendrograms.IO;
with Dendrograms.Algorithms; use Dendrograms.Algorithms;
with Dendrograms.Structure; use Dendrograms.Structure;

procedure Dendrograms_Test is

  use Trees_Dendrograms;
  use Trees_Dendrograms.Nodes_Lists;

  Fn_Text1 : constant String := "test-dendrograms-text_1.txt";
  Fn_Text2 : constant String := "test-dendrograms-text_2.txt";
  Fn_Newick: constant String := "test-dendrograms-newick.txt";
  Fn_Json  : constant String := "test-dendrograms-json.txt";
  Fn_Dist  : constant String := "test-dendrograms-dist.txt";

  -- Show Data
  procedure Put_Data(Data: in PDoubless; Col_Name, Row_Name: in PUstrings; No_Value: in Double; Aft: in Field := Default_Double_Aft) is
  begin
    if Col_Name /= null then
      if Row_Name /= null then
        Put(" ");
      end if;
      for J in Col_Name'Range loop
        Put(HTab & U2S(Col_Name(J)));
      end loop;
      New_Line;
    end if;
    for I in Data'Range(1) loop
      if Row_Name /= null then
        Put(U2S(Row_Name(I)));
      end if;
      for J in Data'Range(2) loop
        if Data(I, J) /= No_Value then
          Put(Htab & D2Se0(Data(I, J), Aft => Aft));
        else
          Put(Htab & "N/A");
        end if;
      end loop;
      New_Line;
    end loop;
  end Put_Data;

  -- Check and prepare data
  Proximity_Matrix_Error: exception;

  procedure Prepare_Data(Data: in PDoubless; No_Value: in Double) is
    N: Natural;
  begin
    if Data'Length(1) /= Data'Length(2) then
      Put_Line("Input data is not a squared matrix");
      raise Proximity_Matrix_Error;
    end if;
    N := Data'Length(1);
    for I in 1..N loop
      Data(I, I) := 0.0;
      for J in (I+1)..N loop
        if Data(I, J) = No_Value and Data(J, I) /= No_Value then
          Data(I, J) := Data(J, I);
        elsif Data(I, J) /= No_Value and Data(J, I) = No_Value then
          Data(J, I) := Data(I, J);
        elsif Data(I, J) = No_Value and Data(J, I) = No_Value then
          Put_Line("No values in pair (" & I2S(I) & "," & I2S(J) & ") and its symmetric");
          raise Proximity_Matrix_Error;
        elsif Data(I, J) /= Data(J, I) then
          Put_Line("Non-symmetric values for pair (" & I2S(I) & "," & I2S(J) & ")");
          raise Proximity_Matrix_Error;
        end if;
        if Data(I, J) < 0.0 then
          Put_Line("Negative value in pair (" & I2S(I) & "," & I2S(J) & ")");
          raise Proximity_Matrix_Error;
        end if;
        Data(J, I) := Data(I, J);
      end loop;
    end loop;
  end Prepare_Data;

  -- Show Dendrogram Structure
  procedure Print_Dendrogram_Structure(Dendro: in Dendrogram) is
    Ds: Dendrogram_Structure;
  begin
    Put_Line("#Name" & Htab & "Pos"    & Htab & "Height" & Htab & "Width" & Htab & "Length"
                     & Htab & "Margin" & Htab & "Kind"   & Htab & "Id"    & Htab & "Parent_Id");
    Ds := Get_Dendrogram_Structure(Dendro);
    for I in Ds'Range loop
      Put(U2S(Ds(I).Name));
      Put(Htab & D2Se0(Ds(I).Position, Aft => 2));
      Put(Htab & D2Se0(Ds(I).Height, Aft => 2));
      Put(Htab & D2Se0(Ds(I).Width, Aft => 2));
      Put(Htab & D2Se0(Ds(I).Length, Aft => 2));
      Put(Htab & D2Se0(Ds(I).Margin, Aft => 2));
      Put(Htab & Capitalize(Node_Type'Image(Ds(I).Kind)));
      Put(Htab & I2S(Ds(I).Node_Id));
      Put(Htab & I2S(Ds(I).Parent_Id));
      New_Line;
    end loop;
    Free(Ds);
  end Print_Dendrogram_Structure;

  -- Show Dendrogram Info
  procedure Print_Node(Nod: in Node) is
    Nod_Inf: Node_Info;
  begin
    Nod_Inf := Value(Nod);
    Put(Get_Name(Nod_Inf));
    Put(Htab & D2Se0(Get_Position(Nod_Inf), Aft => 2));
    Put(Htab & D2Se0(Get_Height(Nod_Inf), Aft => 2));
    Put(Htab & D2Se0(Get_Width(Nod_Inf), Aft => 2));
    Put(Htab & D2Se0(Get_Length(Nod_Inf), Aft => 2));
    Put(Htab & D2Se0(Get_Margin(Nod_Inf), Aft => 2));
    if Is_Root(Nod) then
      Put(HTab & "Root");
    elsif Is_Leaf(Nod_Inf) then
      Put(Htab & "Leaf");
    else
      Put(Htab & "Cluster");
    end if;
    Put(Htab & I2S(Get_Id(Nod_Inf)));
    if Is_Root(Nod) then
      Put(Htab & I2S(0));
    else
      Put(Htab & I2S(Get_Id(Value(Get_Parent(Nod)))));
    end if;
    New_Line;
  end Print_Node;

  procedure Print_Dendrogram_Nodes is new Generic_Depth_First_Postorder_Traversal(Print_Node);

  -- Show Dendrogram Plot Header
  procedure Print_Dendrogram_Plot_Header is
  begin
    Put_Line("#Type" & Htab & "Id" & Htab & "x1" & Htab & "y1" & Htab & "x2" & Htab & "y2" & Htab & "Name");
  end Print_Dendrogram_Plot_Header;

  -- Show Deviation Measures
  procedure Print_Deviation_Measures(Data: in PDoubless; Dendro: in Dendrogram) is
    Coph, Coph_Err, Nmse, Nmae: Double;
    Um: PDoubless;
  begin
    Um := Get_Ultrametric_Matrix(Dendro);
    Get_Cophenetic_Correlation(Data, Um, Coph, Coph_Err);
    Nmse := Get_Normalized_Mean_Squared_Error(Data, Um);
    Nmae := Get_Normalized_Mean_Absolute_Error(Data, Um);
    Free(Um);
    Put_Line("Cophenetic correlation         : " & D2Se0(Coph, Aft => 6) & " +- " & D2Se0(Coph_Err, Aft => 6));
    Put_Line("Normalized Mean Squared Error  : " & D2Se0(Nmse, Aft => 6));
    Put_Line("Normalized Mean Absolute Error : " & D2Se0(Nmae, Aft => 6));
  end Print_Deviation_Measures;


  No_Value: constant Double := Double'First;

  Dendro: Dendrogram;
  Dendros: List_Of_Dendrograms;
  Data, Rdata, Um: PDoubless;
  Col_Name, Row_Name: PUstrings;
  Decimals, Precision: Natural;
  Pt: Proximity_Type;
  Ct: Clustering_Type;

begin

  Get_Dendrogram(Fn_Text1, Dendro);
  Put_Dendrogram(Fn_Newick, Dendro, Precision => 1, Format => Newick_Tree);
  Put_Dendrogram(Fn_Json, Dendro, Precision => 1, Format => Json_Tree);
  Free(Dendro);

  -- Dendrogram in Text Form
  Put_Line("---------------------------");
  Put_Line("Get Dendrogram in Text form");
  Put_Line("---------------------------");
  New_Line;

  Get_Dendrogram(Fn_Text1, Dendro);

  Put_Dendrogram(Dendro, Precision => 1, Format => Text_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Newick_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Json_Tree);
  New_Line;
  Print_Dendrogram_Structure(Dendro);          -- implemented here using Dendrograms.Structure
  New_Line;
  Print_Dendrogram_Nodes(Dendro);              -- implemented here using Dendrograms_Nodes
  New_Line;
  Put_Dendrogram_Structure(Dendro, Aft => 2);  -- using Dendrograms.IO
  New_Line;
  Print_Dendrogram_Plot_Header;
  Put_Dendrogram_Plot_Info(Dendro, Include_Bands => True, Aft => 2);
  New_Line;
  Print_Dendrogram_Plot_Header;
  Put_Dendrogram_Plot_Info(Dendro, Include_Bands => False, Aft => 2);
  New_Line;
  Um := Get_Ultrametric_Matrix(Dendro);
  Put_Data(Um, null, null, No_Value, Aft => 1);
  New_Line;

  Free(Um);
  Free(Dendro);

  -- Dendrogram in Text Form
  Put_Line("---------------------------");
  Put_Line("Get Dendrogram in Text form");
  Put_Line("---------------------------");
  New_Line;

  Get_Dendrogram(Fn_Text2, Dendro);

  Put_Dendrogram(Dendro, Precision => 1, Format => Text_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Newick_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Json_Tree);
  New_Line;
  Put_Dendrogram_Structure(Dendro, Aft => 2);
  New_Line;
  Um := Get_Ultrametric_Matrix(Dendro);
  Put_Data(Um, null, null, No_Value, Aft => 1);
  New_Line;

  Free(Um);
  Free(Dendro);

  -- Dendrogram in Newick Form
  Put_Line("-----------------------------");
  Put_Line("Get Dendrogram in Newick form");
  Put_Line("-----------------------------");
  New_Line;

  Get_Dendrogram(Fn_Newick, Dendro);

  Put_Dendrogram(Dendro, Precision => 1, Format => Text_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Newick_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Json_Tree);
  New_Line;
  Put_Dendrogram_Structure(Dendro, Aft => 2);
  New_Line;
  Um := Get_Ultrametric_Matrix(Dendro);
  Put_Data(Um, null, null, No_Value, Aft => 1);
  New_Line;

  Free(Um);
  Free(Dendro);

  -- Dendrogram in JSON Form
  Put_Line("---------------------------");
  Put_Line("Get Dendrogram in JSON form");
  Put_Line("---------------------------");
  New_Line;

  Get_Dendrogram(Fn_Json, Dendro);

  Put_Dendrogram(Dendro, Precision => 1, Format => Text_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Newick_Tree);
  New_Line;
  Put_Dendrogram(Dendro, Precision => 1, Format => Json_Tree);
  New_Line;
  Put_Dendrogram_Structure(Dendro, Aft => 2);
  New_Line;
  Um := Get_Ultrametric_Matrix(Dendro);
  Put_Data(Um, null, null, No_Value, Aft => 1);
  New_Line;

  Free(Um);
  Free(Dendro);

  -- Hierarchical Clustering: Data
  Put_Line("-----------------------------");
  Put_Line("Hierarchical Clustering: Data");
  Put_Line("-----------------------------");
  New_Line;

  Get_Data(Fn_Dist, Data, Col_Name, Row_Name, Decimals, No_Value, Data_IO_D.Auto);

  Put_Data(Data, Col_Name, Row_Name, No_Value, Decimals);
  New_Line;

  Prepare_Data(Data, No_value);

  Put_Data(Data, Col_Name, Row_Name, No_Value, Decimals);
  New_Line;

  -- Hierarchical Clustering: MultiDendrograms
  Put_Line("-----------------------------------------");
  Put_Line("Hierarchical Clustering: Multidendrograms");
  Put_Line("-----------------------------------------");
  New_Line;

  for Precision in reverse 1..2 loop
    Rdata := Round_Data(Data, Precision);
    for Pt in Proximity_Type loop
      for Ct in Clustering_Type loop
        Put_Line("------");
        Put_Line(To_Name(Pt) & ", " & To_Name(Ct) & ", " & I2S(Precision));
        Hierarchical_Clustering(Data, Col_Name, Pt => Pt, Ct => Ct, Precision => Precision, Md => Dendro);

        Put_Dendrogram(Dendro, Precision => Precision, Format => Text_Tree);
        New_Line;
        Put_Dendrogram(Dendro, Precision => Precision, Format => Newick_Tree);
        New_Line;
        Put_Dendrogram_Plot_Info(Dendro, Include_Bands => True, Aft => Precision);
        New_Line;
        Put_Dendrogram_Structure(Dendro, Aft => 2);
        New_Line;
        Print_Deviation_Measures(Rdata, Dendro);
        New_Line;

        Free(Dendro);
      end loop;
    end loop;
    Free(Rdata);
  end loop;

  -- Hierarchical Clustering: Binary Dendrograms
  Put_Line("-------------------------------------------");
  Put_Line("Hierarchical Clustering: Binary Dendrograms");
  Put_Line("-------------------------------------------");
  New_Line;

  Pt := Similarity;
  Ct := Complete_Linkage;
  Precision := 1;

  Rdata := Round_Data(Data, Precision);
  Hierarchical_Clustering(Data, Col_Name, Pt => Pt, Ct => Ct, Precision => Precision, Bds => Dendros);

  Put_Line(To_Name(Pt) & ", " & To_Name(Ct) & ", " & I2S(Precision));
  Put_Line(I2S(Size(Dendros)) & " Binary Dendrograms");
  New_Line;

  Save(Dendros);
  Reset(Dendros);
  while Has_Next(Dendros) loop
    Put_Line("------");
    Dendro := Next(Dendros);
    Put_Dendrogram(Dendro, Precision => Precision, Format => Text_Tree);
    New_Line;
--    Put_Dendrogram(Dendro, Precision => Precision, Format => Newick_Tree);
--    New_Line;
--    Put_Dendrogram_Plot_Info(Dendro, Include_Bands => True, Aft => Precision);
--    New_Line;
--    Put_Dendrogram_Structure(Dendro, Aft => 2);
--    New_Line;
--    Print_Deviation_Measures(RData, Dendro);
--    New_Line;
    Free(Dendro);
  end loop;
  Restore(Dendros);

  Free(Rdata);
  Free(Dendros);

  -- Free Data
  Free(Data);
  Free(Col_Name);
  Free(Row_Name);
exception
  when E: others =>
    Put(NLine & Exception_Information(E));
end Dendrograms_Test;
