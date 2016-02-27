-- Radalib, Copyright (c) 2016 by
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


-- @filename Hierarchical_Clustering.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/05/2013
-- @revision 26/02/2016
-- @brief Agglomerative Hierarchical Clustering with MultiDendrograms and Binary Dendrograms

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Generic_Array_Sort;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Statistics_Double; use Statistics_Double;
with Data_IO_Double; use Data_IO_Double;
with Dendrograms; use Dendrograms;
with Dendrograms.IO; use Dendrograms.IO;
with Dendrograms.Algorithms; use Dendrograms.Algorithms;


procedure Hierarchical_Clustering is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2016 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Agglomerative Hierarchical Clustering with MultiDendrograms   ==");
    Put_Line("== and Binary Dendrograms, for distances and similarities        ==");
    Put_Line("== Algorithms implemented:                                       ==");
    Put_Line("==   - Single linkage                                            ==");
    Put_Line("==   - Complete linkage                                          ==");
    Put_Line("==   - Unweighted average                                        ==");
    Put_Line("==   - Weighted average                                          ==");
    Put_Line("==   - Unweighted centroid                                       ==");
    Put_Line("==   - Weighted centroid                                         ==");
    Put_Line("==   - Ward                                                      ==");
    Put_Line("== MultiDendrograms generates always a unique dendrogram         ==");
    Put_Line("== For Binary Dendrograms, in case of ties, many dendrograms     ==");
    Put_Line("== may exist, and this tool can enumerate or count all of them,  ==");
    Put_Line("== or choose the one with maximum cophenetic correlation         ==");
    Put_Line("== See http://deim.urv.cat/~sergio.gomez/multidendrograms.php    ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  use Trees_Dendrograms;
  use Trees_Dendrograms.Nodes_Lists;

  -- Binary Dendrogram Modes
  type Dendrogram_Mode is (Sorted, Unsorted, Best, Count);

  Unknown_Dendrogram_Mode: exception;

  function Get_Dendrogram_Mode(Name: in String) return Dendrogram_Mode is
  begin
    if    To_Lowercase(Name) = "sorted"   then
      return Sorted;
    elsif To_Lowercase(Name) = "unsorted" then
      return Unsorted;
    elsif To_Lowercase(Name) = "best"     then
      return Best;
    elsif To_Lowercase(Name) = "count"    then
      return Count;
    else
      raise Unknown_Dendrogram_Mode;
    end if;
  end Get_Dendrogram_Mode;

  function To_Name(Dm: in Dendrogram_Mode) return String is
  begin
    return Capitalize(Dendrogram_Mode'Image(Dm));
  end To_Name;

  -- Check and prepare data
  Proximity_Matrix_Error: exception;

  procedure Prepare_Data(Data: in PDoubless; No_Value: in Double; Pt: in Proximity_Type) is
    N: Natural;
    Filling_Value: Double;
  begin
    if Data'Length(1) /= Data'Length(2) then
      Put_Line("Input data is not a squared matrix");
      raise Proximity_Matrix_Error;
    end if;
    case Pt is
      when Distance =>
        Filling_Value := Double'Last;
      when Similarity =>
        Filling_Value := 0.0;
    end case;
    N := Data'Length(1);
    for I in 1..N loop
      Data(I, I) := 0.0;
      for J in (I+1)..N loop
        if Data(I, J) = No_Value and Data(J, I) /= No_Value then
          Data(I, J) := Data(J, I);
        elsif Data(I, J) /= No_Value and Data(J, I) = No_Value then
          Data(J, I) := Data(I, J);
        elsif Data(I, J) = No_Value and Data(J, I) = No_Value then
          -- Put_Line("No values in pair (" & I2S(I) & "," & I2S(J) & ") and its symmetric");
          -- raise Proximity_Matrix_Error;
          Data(I, J) := Filling_Value;
          Data(J, I) := Filling_Value;
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

  -- Dendrogram Info
  type Dendro_Info_Rec is record
    Dendro: Dendrogram;
    Coph, Coph_Err, Nmse, Nmae: Double;
  end record;

  type Dendro_Info_Array is array(Integer range <>) of Dendro_Info_Rec;
  type PDendro_Info_Array is access Dendro_Info_Array;

  procedure Free(P: in out PDendro_Info_Array) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Dendro_Info_Array, PDendro_Info_Array);
  begin
    if P /= null then
      Dispose(P);
      P := null;
    end if;
  end Free;

  function "="(Left, Right: in Dendro_Info_Rec) return Boolean is
    Eps: constant Double := 1.0E-9;
  begin
    return abs (Left.Coph - Right.Coph) < Eps
       and abs (Left.Nmse - Right.Nmse) < Eps
       and abs (Left.Nmae - Right.Nmae) < Eps;
  end "=";

  function "<"(Left, Right: in Dendro_Info_Rec) return Boolean is
    Eps: constant Double := 1.0E-9;
  begin
    if Left.Coph > Right.Coph + Eps then
      return True;
    elsif abs (Left.Coph - Right.Coph) < Eps and Left.Nmse < Right.Nmse - Eps then
      return True;
    elsif abs (Left.Coph - Right.Coph) < Eps and abs (Left.Nmse - Right.Nmse) < Eps and Left.Nmae < Right.Nmae then
      return True;
    end if;
    return False;
  end "<";

  procedure Sort is new Ada.Containers.Generic_Array_Sort(Integer, Dendro_Info_Rec, Dendro_Info_Array);

  -- Calculation of Dendrogram Info
  function Get_Dendrogram_Info(T: in Dendrogram; Data: in PDoubless; Cet: in Correlation_Error_Type) return Dendro_Info_Rec is
    Di: Dendro_Info_Rec;
    Um: PDoubless;
    Coph, Coph_Err, Nmse, Nmae: Double;
  begin
    Um := Get_Ultrametric_Matrix(T);
    Get_Cophenetic_Correlation(Data, Um, Coph, Coph_Err, Cet);
    Nmse := Get_Normalized_Mean_Squared_Error(Data, Um);
    Nmae := Get_Normalized_Mean_Absolute_Error(Data, Um);
    Free(Um);
    Di := (Dendro => T, Coph => Coph, Coph_Err => Coph_Err, Nmse => Nmse, Nmae => Nmae);
    return Di;
  end Get_Dendrogram_Info;

  -- Put Ultrametric Matrix
  procedure Put_Ultrametric(Ft: in File_Type; Um: in PDoubless; Names: in PUstrings; Precision: in Natural) is
  begin
    if Names /= null then
      for J in Names'Range loop
        if J /= Names'First then
          Put(Ft, HTab);
        end if;
        Put(Ft, U2S(Names(J)));
      end loop;
      New_Line(Ft);
    end if;
    for I in Um'Range(1) loop
      for J in Um'Range(2) loop
        if J /= Um'First(2) then
          Put(Ft, HTab);
        end if;
        Put(Ft, D2Se0(Um(I, J), Aft => Precision));
      end loop;
      New_Line(Ft);
    end loop;
  end Put_Ultrametric;

  -- Constants and variables
  Default_Dendrogram_Mode: constant Dendrogram_Mode := Sorted;

  Text_Sufix     : constant String  := "-tree.txt";
  Json_Sufix     : constant String  := ".json";
  Newick_Sufix   : constant String  := "-newick.txt";
  Measures_Sufix : constant String  := "-measures.txt";
  Ultra_Sufix    : constant String  := "-ultrametric.txt";
  No_Value       : constant Double  := Double'First;
  Max_Num_Dendro : constant Longint := 1000;

  Fn_In, Fn_Out: Ustring;
  Dt: Dendrogram_Type;
  Pt: Proximity_Type;
  Ct: Clustering_Type;
  Precision: Natural := 0;
  Dm: Dendrogram_Mode;
  Px: Ustring := Null_Ustring;

  Auto_Precision: Boolean;
  Fn_Out_Text: Ustring;
  Fn_Out_Json: Ustring;
  Fn_Out_Newick: Ustring;
  Fn_Out_Measures: Ustring;
  Fn_Out_Ultra: Ustring;
  Ft_Text: File_Type;
  Ft_Json: File_Type;
  Ft_Newick: File_Type;
  Ft_Measures: File_Type;
  Ft_Ultra: File_Type;
  N: Natural;
  Data, Um: PDoubless;
  Dendros: List_Of_Dendrograms;
  Col_Name, Row_Name, Names: PUstrings;
  Md, T: Dendrogram;
  Di: PDendro_Info_Array;
  Index: Natural;
  Cet: Correlation_Error_Type := Auto;

  Num_Dendro: Longint := 0;
  Num_Saved: Natural := 0;
  Dir_Best: Dendro_Info_Rec := (Dendro => null, Coph => Double'First, Coph_Err => 0.0, Nmse => Double'Last, Nmae => Double'Last);


  -- Action with the Dendrogram
  procedure Dendrogram_Action(T: in Dendrogram) is
    Dir: Dendro_Info_Rec;
    T_Aux: Dendrogram;
  begin
    Num_Dendro := Num_Dendro + 1;
    case Dm is
      when Sorted =>
        Add_Last(Clone(T), Dendros);
      when Unsorted =>
        Dir := Get_Dendrogram_Info(T, Data, Cet);
        if Num_Dendro <= Max_Num_Dendro then
          Put_Dendrogram(Ft_Text, T, Precision, Text_Tree);
          Put_Line(Ft_Text, "----------");
          Put_Dendrogram(Ft_Json, T, Precision, Json_Tree);
          New_line(Ft_Json);
        end if;
        Put_Dendrogram(Ft_Newick, T, Precision, Newick_Tree);
        Put_Line(Ft_Measures, D2Se0(Dir.Coph, Aft => 6) & HTab &
                              D2Se0(Dir.Coph_Err, Aft => 6) & HTab &
                              D2Se0(Dir.Nmse, Aft => 6) & HTab &
                              D2Se0(Dir.Nmae, Aft => 6));
      when Best =>
        Dir := Get_Dendrogram_Info(T, Data, Cet);
        if Dir_Best = Dir then
          Add_Last(Clone(T), Dendros);
        elsif Dir < Dir_Best then
          Dir_Best := Dir;
          Save(Dendros);
          Reset(Dendros);
          while Has_Next(Dendros) loop
            T_Aux := Next(Dendros);
            Free(T_Aux);
          end loop;
          Restore(Dendros);
          Clear(Dendros);
          Add_Last(Clone(T), Dendros);
        end if;
      when Count =>
        null;
    end case;
    if Num_Dendro mod Max_Num_Dendro = 0 then
      Put(".");
    end if;
    if Num_Dendro mod (50 * Max_Num_Dendro) = 0 then
      Put_Line(" " & L2S(Num_Dendro) & " dendrograms");
    end if;
  end Dendrogram_Action;

  -- Hierarchical Clustering for all Modes
  procedure Hierarchical_Clust is new Generic_Hierarchical_Clustering(Dendrogram_Action);


begin
  Put_Info;

  if Argument_Count = 5 then
    Fn_In  := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Dt := Get_Dendrogram_Type(Argument(3));
    Pt := Get_Proximity_Type(Argument(4));
    Ct := Get_Clustering_Type(Argument(5));
    Auto_Precision := True;
    Dm := Default_Dendrogram_Mode;
    Px := Null_Ustring;
  elsif Argument_Count = 6 then
    Fn_In  := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Dt := Get_Dendrogram_Type(Argument(3));
    Pt := Get_Proximity_Type(Argument(4));
    Ct := Get_Clustering_Type(Argument(5));
    if Is_Integer(Argument(6)) then
      Precision := S2I(Argument(6));
      Auto_Precision := False;
      Dm := Default_Dendrogram_Mode;
      Px := Null_Ustring;
    else
      Auto_Precision := True;
      begin
        Dm := Get_Dendrogram_Mode(Argument(6));
        Px := Null_Ustring;
      exception
        when others =>
          Dm := Default_Dendrogram_Mode;
          Px := S2U(Argument(6));
      end;
    end if;
  elsif Argument_Count = 7 then
    Fn_In  := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Dt := Get_Dendrogram_Type(Argument(3));
    Pt := Get_Proximity_Type(Argument(4));
    Ct := Get_Clustering_Type(Argument(5));
    if Is_Integer(Argument(6)) then
      Precision := S2I(Argument(6));
      Auto_Precision := False;
      begin
        Dm := Get_Dendrogram_Mode(Argument(7));
        Px := Null_Ustring;
      exception
        when others =>
          Dm := Default_Dendrogram_Mode;
          Px := S2U(Argument(7));
      end;
    else
      Auto_Precision := True;
      Dm := Get_Dendrogram_Mode(Argument(6));
      Px := S2U(Argument(7));
    end if;
  elsif Argument_Count = 8 then
    Fn_In  := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Dt := Get_Dendrogram_Type(Argument(3));
    Pt := Get_Proximity_Type(Argument(4));
    Ct := Get_Clustering_Type(Argument(5));
    Precision := S2I(Argument(6));
    Auto_Precision := False;
    Dm := Get_Dendrogram_Mode(Argument(7));
    Px := S2U(Argument(8));
  else
    Put_Line("Usage:  " & Command_Name & "  proximities_name  output_prefix  dendrogram_type  proximity_type  clustering_type  [ precision ]  [ dendrogram_mode ]  [ internal_nodes_prefix ]");
    New_Line;
    Put_Line("   proximities_name      :  name of the proximities file, either in matrix or list form");
    Put_Line("                              in matrix form, the names may be in first column, first row, or none");
    Put_Line("                              in list form, missing values are filled with:");
    Put_Line("                                Double'Last for Distances");
    Put_Line("                                0.0         for Similarities");
    New_Line;
    Put_Line("   output_prefix         :  prefix of the output files");
    New_Line;
    Put_Line("   dendrogram_type       :  MD | BD");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive short and full names (Distance, ...)");
    Put_Line("                              MD | Multidendrogram");
    Put_Line("                              BD | Binary_Dendrogram");
    New_Line;
    Put_Line("   proximity_type        :  D | S");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive short and full names (Distance, ...)");
    Put_Line("                              D | DIST | Distance");
    Put_Line("                              S | SIM  | Similarity");
    New_Line;
    Put_Line("   clustering_type       :  SL | CL | UA | WA | UC | WC | WD | UPGMA | WPGMA");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive short and full names (Single_Linkage, ...)");
    Put_Line("                              SL = Single_Linkage");
    Put_Line("                              CL = Complete_Linkage");
    Put_Line("                              UA = UPGMA = Unweighted_Average");
    Put_Line("                              WA = WPGMA = Weighted_Average");
    Put_Line("                              UC = Unweighted_Centroid");
    Put_Line("                              WC = Weighted_Centroid");
    Put_Line("                              WD = Ward");
    New_Line;
    Put_Line("   precision             :  Number of decimal significant digits of the data and for the calculations");
    Put_Line("                              if not specified, is that of the value with largest number of decimal digits");
    New_Line;
    Put_Line("   dendrogram_mode       :  Sorted | Unsorted | Best | Count");
    Put_Line("                              also case-insensitive full names)");
    Put_Line("                              default => " & To_Name(Default_Dendrogram_Mode));
    Put_Line("                              mode discarded for MultiDendrograms");
    New_Line;
    Put_Line("   internal_nodes_prefix :  Prefix for the names of the internal nodes");
    Put_Line("                              if 'None' (case insensitive) no names are assigned to internal nodes");
    Put_Line("                              default => " & U2S(Internal_Node_Name_Prefix));
    return;
  end if;

  -- Initializations
  Put_Line(U2S(Fn_In) & " -> " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  if Px /= Null_Ustring then
    Internal_Node_Name_Prefix := Px;
  end if;

  Fn_Out_Text     := S2U(U2S(Fn_Out) & Text_Sufix);
  Fn_Out_Json     := S2U(U2S(Fn_Out) & Json_Sufix);
  Fn_Out_Newick   := S2U(U2S(Fn_Out) & Newick_Sufix);
  Fn_Out_Measures := S2U(U2S(Fn_Out) & Measures_Sufix);
  Fn_Out_Ultra    := S2U(U2S(Fn_Out) & Ultra_Sufix);

  Delete_File(U2S(Fn_Out_Text));
  Delete_File(U2S(Fn_Out_Json));
  Delete_File(U2S(Fn_Out_Newick));
  Delete_File(U2S(Fn_Out_Measures));
  Delete_File(U2S(Fn_Out_Ultra));

  if Dt = Multidendrogram then
    Dm := Sorted;
  end if;

  -- Get and prepare Data
  if Auto_Precision then
    Get_Data(U2S(Fn_In), Data, Col_Name, Row_Name, Precision, No_Value);
  else
    Get_Data(U2S(Fn_In), Data, Col_Name, Row_Name, No_Value);
  end if;
  Prepare_Data(Data, No_Value, Pt);

  if Col_Name /= null then
    Names := Col_Name;
  elsif Row_Name /= null then
    Names := Row_Name;
  else
    Names := null;
  end if;

  N := Data'Length(1);
  Put_Line("  Proximity  type : " & To_Name(Pt));
  Put_Line("  Clustering type : " & To_Name(Ct));
  Put_Line("  Size            : " & I2S(N));
  Put_Line("  Precision       : " & I2S(Precision));
  if Dt = Binary_Dendrogram then
    Put_Line("  Dendrogram mode : " & To_Name(Dm));
  end if;

  -- Prepare output files
  if Dm /= Count then
    Open_Or_Create(Ft_Text    , U2S(Fn_Out_Text));
    Open_Or_Create(Ft_Json    , U2S(Fn_Out_Json));
    Open_Or_Create(Ft_Newick  , U2S(Fn_Out_Newick));
    Open_Or_Create(Ft_Measures, U2S(Fn_Out_Measures));

    if Dm = Unsorted then
      Put_Line(Ft_Text, "# First Binary Dendrograms (maximum " & L2S(Max_Num_Dendro) & ")");
      Put_Line(Ft_Json, "# First Binary Dendrograms (maximum " & L2S(Max_Num_Dendro) & ")");
      Put_Line(Ft_Newick, "# All Binary Dendrograms");
      Cet := Fisher_Transform;
    end if;
    Put_Line(Ft_Measures, "Cophenetic_Correlation"        & HTab & "Cophenetic_Correlation_Error" & HTab &
                          "Normalized_Mean_Squared_Error" & HTab & "Normalized_Mean_Absolute_Error");
  end if;

  Initialize(Dendros);

  -- Perform the Hierarchical Clustering
  case Dt is
    when Multidendrogram =>
      Hierarchical_Clustering(Data, Names, Pt, Ct, Precision, Md);
      Add_Last(Md, Dendros);
    when Binary_Dendrogram =>
      Hierarchical_Clust(Data, Names, Pt, Ct, Precision);
      if Num_Dendro >= Max_Num_Dendro then
        New_Line;
      end if;
  end case;

  -- Show Number of Dendrograms
  Num_Saved := Size(Dendros);

  case Dt is
    when Multidendrogram =>
      Put_Line("  Result          : MultiDendrogram");
    when Binary_Dendrogram =>
      if Num_Dendro = 1 then
        Put_Line("  Result          : Binary Dendrogram");
      elsif Dm = Best then
        Put_Line("  Result          : " & L2S(Num_Dendro) & " Binary Dendrograms, " & I2S(Num_Saved) & " with Highest Cophenetic Correlation");
      else
        Put_Line("  Result          : " & L2S(Num_Dendro) & " Binary Dendrograms");
      end if;
  end case;
  New_Line;

  -- Treatment of Saved Dendrograms
  if Num_Saved > 0 then
    -- Calculate Deviation Measures and Sort in Descending Cophenetic Correlation
    Di := new Dendro_Info_Array(1..Num_Saved);

    Cet := Auto;
    if Longint(Num_Saved) > Max_Num_Dendro then
      Cet := Fisher_Transform;
    end if;

    Index := 0;
    Save(Dendros);
    Reset(Dendros);
    while Has_Next(Dendros) loop
      T := Next(Dendros);
      Index := Index + 1;
      Di(Index) := Get_Dendrogram_Info(T, Data, Cet);
    end loop;
    Restore(Dendros);

    Sort(Di.all);

    -- Write Saved Dendrograms and Deviation Measures
    case Dt is
      when Multidendrogram =>
        Put_Line(Ft_Text  , "# MultiDendrogram");
        Put_Line(Ft_Json  , "# MultiDendrogram");
        Put_Line(Ft_Newick, "# MultiDendrogram");
      when Binary_Dendrogram =>
        if Num_Dendro = 1 then
          Put_Line(Ft_Text  , "# Binary Dendrogram");
          Put_Line(Ft_Json  , "# Binary Dendrogram");
          Put_Line(Ft_Newick, "# Binary Dendrogram");
        elsif Num_Saved = 1 then
          Put_Line(Ft_Text  , "# Binary Dendrogram with Highest Cophenetic Correlation");
          Put_Line(Ft_Json  , "# Binary Dendrogram with Highest Cophenetic Correlation");
          Put_Line(Ft_Newick, "# Binary Dendrogram with Highest Cophenetic Correlation");
        elsif Dm = Best then
          Put_Line(Ft_Text  , "# The " & I2S(Num_Saved) & " Binary Dendrograms with Highest Cophenetic Correlation");
          Put_Line(Ft_Json  , "# The " & I2S(Num_Saved) & " Binary Dendrograms with Highest Cophenetic Correlation");
          Put_Line(Ft_Newick, "# The " & I2S(Num_Saved) & " Binary Dendrograms with Highest Cophenetic Correlation");
        elsif Num_Dendro <= Max_Num_Dendro then
          Put_Line(Ft_Text  , "# Binary Dendrograms");
          Put_Line(Ft_Json  , "# Binary Dendrograms");
          Put_Line(Ft_Newick, "# Binary Dendrograms");
        else
          Put_Line(Ft_Text  , "# First " & L2S(Max_Num_Dendro) & " Binary Dendrograms");
          Put_Line(Ft_Json  , "# First " & L2S(Max_Num_Dendro) & " Binary Dendrograms");
          Put_Line(Ft_Newick, "# Binary Dendrograms");
        end if;
    end case;

    for I in Di'Range loop
      if Longint(I) <= Max_Num_Dendro then
        Put_Dendrogram(Ft_Text, Di(I).Dendro, Precision, Text_Tree);
        Put_Line(Ft_Text, "----------");
        Put_Dendrogram(Ft_Json, Di(I).Dendro, Precision, Json_Tree);
        New_Line(Ft_Json);
      end if;
      Put_Dendrogram(Ft_Newick, Di(I).Dendro, Precision, Newick_Tree);
      Put_Line(Ft_Measures, D2Se0(Di(I).Coph, Aft => 6) & HTab & D2Se0(Di(I).Coph_Err, Aft => 6) & HTab &
                            D2Se0(Di(I).Nmse, Aft => 6) & HTab & D2Se0(Di(I).Nmae, Aft => 6));
    end loop;
  end if;

  if Dm /= Count then
    Close(Ft_Text);
    Close(Ft_Json);
    Close(Ft_Newick);
    Close(Ft_Measures);
  end if;

  -- Write Ultrametric Matrix
  if Num_Saved = 1 or Dm = Sorted or Dm = Best then
    Open_Or_Create(Ft_Ultra, U2S(Fn_Out_Ultra));
    if Dt = Multidendrogram then
      Put_Line(Ft_Ultra, "# Ultrametric of MultiDendrogram");
    elsif Num_Dendro = 1 then
      Put_Line(Ft_Ultra, "# Ultrametric of Binary Dendrogram");
    elsif Dm = Best and Num_Saved > 1 then
      Put_Line(Ft_Ultra, "# Ultrametric of first Binary Dendrogram with Highest Cophenetic Correlation");
    else
      Put_Line(Ft_Ultra, "# Ultrametric of Binary Dendrogram with Highest Cophenetic Correlation");
    end if;
    Um := Get_Ultrametric_Matrix(Di(1).Dendro);
    Put_Ultrametric(Ft_Ultra, Um, Names, Precision);
    Free(Um);
    Close(Ft_Ultra);
  end if;

  -- Free space
  Free(Data);
  Free(Col_Name);
  Free(Row_Name);
  Free(Dendros);

  if Num_Saved > 0 then
    for I in Di'Range loop
      Free(Di(I).Dendro);
    end loop;
    Free(Di);
  end if;

exception
  when Proximity_Matrix_Error =>
    Put_Line("Invalid proximity matrix");
  when Tied_Pairs_Combinations_Overflow =>
    Put_Line("Too many combinations of tied pairs in a single step, aborting!");
  when Storage_Error =>
    New_Line;
    Put_Line("Not enough memory!");
    Put_Line("Try Count, Best or Unsorted modes, which do not require all binary dendrograms in memory.");
  when E: others =>
    New_Line;
    Put_Line(Exception_Information(E));
end Hierarchical_Clustering;
