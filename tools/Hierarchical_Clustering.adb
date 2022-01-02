-- Radalib, Copyright (c) 2022 by
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
-- @author Alberto Fernandez
-- @version 1.0
-- @date 08/05/2013
-- @revision 02/01/2022
-- @brief Agglomerative Hierarchical Clustering with MultiDendrograms and Binary Dendrograms

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Generic_Array_Sort;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Statistics_Double; use Statistics_Double;
with Data_IO_Double; use Data_IO_Double;
with Dendrograms; use Dendrograms;
with Dendrograms.IO; use Dendrograms.IO;
with Dendrograms.Algorithms; use Dendrograms.Algorithms;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;


procedure Hierarchical_Clustering is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2022 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Agglomerative Hierarchical Clustering with MultiDendrograms   ==");
    Put_Line("== and Binary Dendrograms, for distances and similarities        ==");
    Put_Line("== Algorithms implemented:                                       ==");
    Put_Line("==   - (VL) Versatile linkage       - (HL) Harmonic linkage      ==");
    Put_Line("==   - (SL) Single linkage          - (WD) Ward                  ==");
    Put_Line("==   - (CL) Complete linkage        - (CD) Centroid              ==");
    Put_Line("==   - (AL) Arithmetic linkage      - (BF) Beta flexible         ==");
    Put_Line("==   - (GL) Geometric linkage                                    ==");
    Put_Line("==                                                               ==");
    Put_Line("== Equivalences between clustering algorithms for distances:     ==");
    Put_Line("==   Arithmetic Linkage Unweighted  = UPGMA                      ==");
    Put_Line("==   Versatile Linkage (param +1.0) = Complete Linkage           ==");
    Put_Line("==   Versatile Linkage (param +0.1) = Arithmetic Linkage         ==");
    Put_Line("==   Versatile Linkage (param  0.0) = Geometric Linkage          ==");
    Put_Line("==   Versatile Linkage (param -0.1) = Harmonic Linkage           ==");
    Put_Line("==   Versatile Linkage (param -1.0) = Single Linkage             ==");
    Put_Line("==   Beta Flexible     (param  0.0) = Arithmetic Linkage         ==");
    Put_Line("== For similarities, the signs of param must be exchanged        ==");
    Put_Line("==                                                               ==");
    Put_Line("== MultiDendrograms generates always a unique dendrogram         ==");
    Put_Line("== For Binary Dendrograms, in case of ties, many dendrograms     ==");
    Put_Line("== may exist, and this tool can enumerate or count all of them,  ==");
    Put_Line("== or choose the one with maximum cophenetic correlation         ==");
    Put_Line("== See also                                                      ==");
    Put_Line("==   http://deim.urv.cat/~sergio.gomez/mdendro.php               ==");
    Put_Line("==   http://deim.urv.cat/~sergio.gomez/multidendrograms.php      ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  use Trees_Dendrograms;
  use Trees_Dendrograms.Nodes_Lists;

  -- Binary Dendrogram Modes
  type Dendrogram_Mode is (Sorted, Unsorted, Sample, Best, Count);

  Unknown_Dendrogram_Mode: exception;

  function Get_Dendrogram_Mode(Name: in String) return Dendrogram_Mode is
  begin
    if    To_Lowercase(Name) = "sorted"   then
      return Sorted;
    elsif To_Lowercase(Name) = "unsorted" then
      return Unsorted;
    elsif To_Lowercase(Name) = "sample"   then
      return Sample;
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
    Um: PDoubless;
    Coph, Coph_Err, Nmse, Nmae: Double;
  end record;

  type Dendro_Info_Array is array(Integer range <>) of Dendro_Info_Rec;
  type PDendro_Info_Array is access Dendro_Info_Array;

  procedure Free(Dir: in out Dendro_Info_Rec) is
  begin
    if Dir.Um /= null then
      Free(Dir.Dendro);
      Free(Dir.Um);
    end if;
  end Free;

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
    Di := (Dendro => T, Um => Um, Coph => Coph, Coph_Err => Coph_Err, Nmse => Nmse, Nmae => Nmae);
    return Di;
  end Get_Dendrogram_Info;

  -- Convert Ultrametric to Vector
  function Ultrametric_To_Vector(M: in PDoubless) return PDoubles is
    P: PDoubles;
    N, Np, Ind: Natural;
    Os1, Os2: Integer;
  begin
    N := M'Length(1);
    Os1 := M'First(1) - 1;
    Os2 := M'First(2) - 1;
    if N mod 2 = 0 then
      Np := (N / 2) * (N - 1);
    else
      Np := ((N - 1) / 2) * N;
    end if;

    P := Alloc(1, Np);

    Ind := 0;
    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        Ind := Ind + 1;
        P(Ind) := M(Os1 + I, Os2 + J);
      end loop;
    end loop;

    return P;
  end Ultrametric_To_Vector;

  -- Get Dendrograms Similarity
  function Dendrograms_Similarity(Um1, Um2: in PDoubless) return Double is
    Sim: Double;
    PUm1, PUm2: PDoubles;
  begin
    PUm1 := Ultrametric_To_Vector(Um1);
    PUm2 := Ultrametric_To_Vector(Um2);

    Sim := Pearson_Correlation(PUm1, PUm2);

    Free(PUm1);
    Free(PUm2);

    return Sim;
  end Dendrograms_Similarity;

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

  function Identical_Patterns(Lol_Deg: in List_of_Lists) return UString is
    Uip: UString := Null_Ustring;
    L: List;
    I: Positive;
  begin
    Save(Lol_Deg);
    Reset(Lol_Deg);
    while Has_Next_List(Lol_Deg) loop
      L := Next_List(Lol_Deg);
      if Number_Of_Elements(L) > 1 then
        Uip := Uip & S2U("(");
        Save(L);
        Reset(L);
        I := Index_Of(Next_Element(L));
        Uip := Uip & S2U(I2S(I));
        while Has_Next_Element(L) loop
          I := Index_Of(Next_Element(L));
          Uip := Uip & S2U(", " & I2S(I));
        end loop;
        Restore(L);
        Uip := Uip & S2U(") ");
      end if;
    end loop;
    Restore(Lol_Deg);
    return Trim_Spaces(Uip);
  end Identical_Patterns;


  -- Constants and variables
  Default_Weighting_Type       : constant Weighting_Type := Unweighted;
  Default_Clustering_Parameter : constant Double := 0.0;
  Default_Dendrogram_Mode      : constant Dendrogram_Mode := Sorted;
  Default_Sample_Probability   : constant Double := 0.02;
  Default_Max_Num_Dendro       : constant Longint := 1000;

  Count_Sufix    : constant String  := "-count.txt";
  Text_Sufix     : constant String  := "-tree.txt";
  Json_Sufix     : constant String  := ".json";
  Newick_Sufix   : constant String  := "-newick.txt";
  Measures_Sufix : constant String  := "-measures.txt";
  Ultra_Sufix    : constant String  := "-ultrametric.txt";
  Sel_Text_Sufix     : constant String  := "-selected_tree.txt";
  Sel_Measures_Sufix : constant String  := "-selected_measures.txt";
  Sel_Ultra_Sufix    : constant String  := "-selected_ultrametric.txt";
  No_Value       : constant Double  := Double'First;

  Fn_In, Fn_Out: Ustring;
  Dt: Dendrogram_Type;
  Pt: Proximity_Type;
  Precision: Natural := 0;
  Ct: Clustering_Type;
  Wt: Weighting_Type := Default_Weighting_Type;
  Cp: Double := Default_Clustering_Parameter;
  Dm: Dendrogram_Mode := Default_Dendrogram_Mode;
  Sp: Double := Default_Sample_Probability;
  Mxd: Longint := Default_Max_Num_Dendro;
  Px: Ustring := Null_Ustring;

  Auto_Precision: Boolean := True;
  Fn_Out_Count: Ustring;
  Fn_Out_Text: Ustring;
  Fn_Out_Json: Ustring;
  Fn_Out_Newick: Ustring;
  Fn_Out_Measures: Ustring;
  Fn_Out_Ultra: Ustring;
  Fn_Out_Sel_Text: Ustring;
  Fn_Out_Sel_Measures: Ustring;
  Fn_Out_Sel_Ultra: Ustring;
  Ft_Text: File_Type;
  Ft_Json: File_Type;
  Ft_Newick: File_Type;
  Ft_Measures: File_Type;
  Ft_Ultra: File_Type;
  Ft_Sel_Text: File_Type;
  Ft_Sel_Measures: File_Type;
  Ft_Sel_Ultra: File_Type;
  I, N: Natural;
  Ve: Double;
  Data: PDoubless;
  Dendros: List_Of_Dendrograms;
  Col_Name, Row_Name, Names: PUstrings;
  Md, T: Dendrogram;
  Di: PDendro_Info_Array;
  Index: Natural;
  Us: UString;
  Cet: Correlation_Error_Type := Fisher_Transform;

  Sel_Index: Integers(1..2);
  Sel_Di: Dendro_Info_Array(1..2);
  Sim, Min_Sim: Double;

  G: Generator;
  Num_Dendro: Longint := 0;
  Num_Saved: Natural := 0;
  Lol_Degenerated: List_of_Lists;
  Num_Degenerated: Longint := 0;
  Uip: UString;
  Search_Aborted: Boolean := False;
  Tick_Size, Big_Tick_Size: Longint;
  Dir_Best: Dendro_Info_Rec := (Dendro => null, Um => null, Coph => Double'First, Coph_Err => 0.0, Nmse => Double'Last, Nmae => Double'Last);


  -- Action with the Dendrogram
  procedure Dendrogram_Action(T: in Dendrogram) is
    Stop_Recursion: Boolean;
    Dir: Dendro_Info_Rec;
    T_Aux: Dendrogram;
  begin
    Num_Dendro := Num_Dendro + 1;
    Stop_Recursion := False;

    case Dm is
      when Sorted =>
        Add_Last(Clone(T), Dendros);
        if Num_Dendro >= Mxd then
          Stop_Recursion := True;
        end if;

      when Unsorted =>
        Dir := Get_Dendrogram_Info(T, Data, Cet);
        Free(Dir.Um);
        Put_Dendrogram(Ft_Text, T, Precision, Text_Tree);
        Put_Line(Ft_Text, "----------");
        Put_Dendrogram(Ft_Json, T, Precision, Json_Tree);
        New_line(Ft_Json);
        Put_Dendrogram(Ft_Newick, T, Precision, Newick_Tree);
        Put_Line(Ft_Measures, D2Se0(Dir.Coph, Aft => 6) & HTab &
                              D2Se0(Dir.Coph_Err, Aft => 6) & HTab &
                              D2Se0(Dir.Nmse, Aft => 6) & HTab &
                              D2Se0(Dir.Nmae, Aft => 6));
        if Num_Dendro >= Mxd then
          Stop_Recursion := True;
        end if;

      when Sample =>
        if Num_Dendro = 1 or else (Longint(Num_Saved) < Mxd and then Double(Random(G)) <= Sp) then
          Add_Last(Clone(T), Dendros);
          Num_Saved := Num_Saved + 1;
          if Longint(Num_Saved) >= Mxd then
            Stop_Recursion := True;
          end if;
        end if;

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
        if Num_Dendro >= Mxd then
          Stop_Recursion := True;
        end if;

      when Count =>
        null;
    end case;

    if Num_Dendro mod Tick_Size = 0 then
      Put(".");
    end if;
    if Num_Dendro mod Big_Tick_Size = 0 then
      Put_Line(" " & L2S(Num_Dendro) & " dendrograms");
      Delete_File(U2S(Fn_Out_Count));
      Put_String_Line(U2S(Fn_Out_Count), L2S(Num_Dendro) & " Binary Dendrograms so far");
    end if;

    if Stop_Recursion then
      Search_Aborted := True;
      raise Stop_Dendrograms_Recursion;
    end if;
  end Dendrogram_Action;

  -- Hierarchical Clustering for all Modes
  procedure Hierarchical_Clust is new Generic_Hierarchical_Clustering(Dendrogram_Action);


begin
  Put_Info;

  if 5 <= Argument_Count and Argument_Count <= 11 then
    Fn_In  := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Dt := Get_Dendrogram_Type(Argument(3));
    Pt := Get_Proximity_Type(Argument(4));
    I := 5;
    if Is_Integer(Argument(I)) then
      Precision := S2I(Argument(I));
      I := I + 1;
      Auto_Precision := False;
    end if;
    Ct := Get_Clustering_Type(Argument(I));
    I := I + 1;
    if I <= Argument_Count then
      begin
        Wt := Get_Weighting_Type(Argument(I));
        I := I + 1;
      exception
        when others =>
          Wt := Default_Weighting_Type;
      end;
    end if;
    if I <= Argument_Count then
      begin
        Cp := S2D(Argument(I));
        I := I + 1;
      exception
        when others =>
          Cp := Default_Clustering_Parameter;
      end;
    end if;
    if I <= Argument_Count then
      begin
        Dm := Get_Dendrogram_Mode(Argument(I));
        I := I + 1;
      exception
        when others =>
          Dm := Default_Dendrogram_Mode;
      end;
      if I <= Argument_Count then
        begin
          Mxd := S2L(Argument(I));
          I := I + 1;
        exception
          when others =>
            Mxd := Default_Max_Num_Dendro;
        end;
      end if;
      if I <= Argument_Count then
        begin
          Sp := S2D(Argument(I));
          I := I + 1;
        exception
          when others =>
            Sp := Default_Sample_Probability;
        end;
      end if;
    end if;
    if I <= Argument_Count then
      Px := S2U(Argument(I));
      I := I + 1;
    end if;
  else
    Put_Line("Usage:  " & Command_Name & "  proximities_name  output_prefix  dendrogram_type  proximity_type  [ precision ]  clustering_type  [ weighting_type ]  [ clustering_parameter ]  [ dendrogram_mode  [ max_num_dendrograms ]  [ sample_probability ] ]  [ internal_nodes_prefix ]");
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
    Put_Line("                              also case-insensitive short and full names (Multidendrogram, ...)");
    Put_Line("                              MD | Multidendrogram");
    Put_Line("                              BD | Binary_Dendrogram");
    New_Line;
    Put_Line("   proximity_type        :  D | S");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive short and full names (Distance, ...)");
    Put_Line("                              D | DIST | Distance");
    Put_Line("                              S | SIM  | Similarity");
    New_Line;
    Put_Line("   precision             :  Number of decimal significant digits of the data and for the calculations");
    Put_Line("                              if not specified, is that of the value with largest number of decimal digits");
    New_Line;
    Put_Line("   clustering_type       :  VL | SL | CL | AL | GL | HL | WD | CD | BF");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive short and full names (Versatile_Linkage, ...)");
    Put_Line("                              VL = Versatile_Linkage");
    Put_Line("                              SL = Single_Linkage");
    Put_Line("                              CL = Complete_Linkage");
    Put_Line("                              AL = Arithmetic_Linkage");
    Put_Line("                              GL = Geometric_Linkage");
    Put_Line("                              HL = Harmonic_Linkage");
    Put_Line("                              WD = Ward");
    Put_Line("                              CD = Centroid");
    Put_Line("                              BF = Beta_Flexible");
    New_Line;
    Put_Line("   weighting_type        :  W | UW");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive short and full names (Weighted, ...)");
    Put_Line("                              W  = Weighted");
    Put_Line("                              UW = Unweighted");
    Put_Line("                              default => " & To_Name(Default_Weighting_Type));
    New_Line;
    Put_Line("   clustering_parameter  :  Clustering parameter, between -1.0 and +1.0, necessary for");
    Put_Line("                              VL = Versatile_Linkage");
    Put_Line("                              BF = Beta_Flexible");
    Put_Line("                              default => 0");
    Put_Line("                              ignored for the other clustering types");
    Put_Line("                              for VL");
    Put_Line("                                -1.0 corresponds to SL for DIST, and to CL for SIM");
    Put_Line("                                -0.1 corresponds to HL for DIST, and to AL for SIM");
    Put_Line("                                 0.0 corresponds to GL");
    Put_Line("                                +0.1 corresponds to AL for DIST, and to HL for SIM");
    Put_Line("                                +1.0 corresponds to CL for DIST, and to SL for SIM");
    Put_Line("                              for BF");
    Put_Line("                                 0.0 corresponds to AL");
    New_Line;
    Put_Line("   dendrogram_mode       :  Sorted | Unsorted | Sample | Best | Count");
    Put_Line("                              also case-insensitive full names");
    Put_Line("                              default => " & To_Name(Default_Dendrogram_Mode));
    Put_Line("                              mode discarded for MultiDendrograms");
    Put_Line("                              Sorted   : outputs all dendrograms sorted by decreasing cophenetic correlation");
    Put_Line("                              Unsorted : outputs the first binary dendrograms found");
    Put_Line("                              Sample   : outputs a sorted sample of binary dendrograms");
    Put_Line("                              Best     : outputs the dendrogram(s) with largest cophenetic correlation");
    Put_Line("                              Count    : outputs the number of binary dendrograms");
    New_Line;
    Put_Line("   max_num_dendrograms   :  Maximum number of binary dendrograms");
    Put_Line("                              default => " & L2S(Default_Max_Num_Dendro));
    Put_Line("                              stops recursion except for Count mode");
    New_Line;
    Put_Line("   sample_probability    :  Sample probability, between 0.0 and 1.0");
    Put_Line("                              default => " & D2Ss(Default_Sample_Probability));
    Put_Line("                              necessary for Sample mode, discarded for the rest");
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

  Reset(G);

  if Px /= Null_Ustring then
    Internal_Node_Name_Prefix := Px;
  end if;

  Fn_Out_Count    := S2U(U2S(Fn_Out) & Count_Sufix);
  Fn_Out_Text     := S2U(U2S(Fn_Out) & Text_Sufix);
  Fn_Out_Json     := S2U(U2S(Fn_Out) & Json_Sufix);
  Fn_Out_Newick   := S2U(U2S(Fn_Out) & Newick_Sufix);
  Fn_Out_Measures := S2U(U2S(Fn_Out) & Measures_Sufix);
  Fn_Out_Ultra    := S2U(U2S(Fn_Out) & Ultra_Sufix);

  Fn_Out_Sel_Text     := S2U(U2S(Fn_Out) & Sel_Text_Sufix);
  Fn_Out_Sel_Measures := S2U(U2S(Fn_Out) & Sel_Measures_Sufix);
  Fn_Out_Sel_Ultra    := S2U(U2S(Fn_Out) & Sel_Ultra_Sufix);

  Delete_File(U2S(Fn_Out_Count));
  Delete_File(U2S(Fn_Out_Text));
  Delete_File(U2S(Fn_Out_Json));
  Delete_File(U2S(Fn_Out_Newick));
  Delete_File(U2S(Fn_Out_Measures));
  Delete_File(U2S(Fn_Out_Ultra));

  Delete_File(U2S(Fn_Out_Sel_Text));
  Delete_File(U2S(Fn_Out_Sel_Measures));
  Delete_File(U2S(Fn_Out_Sel_Ultra));

  if Dt = Multidendrogram then
    Dm := Sorted;
  end if;

  if Dm = Count or Dm = Sample then
    Tick_Size := Mxd;
  else
    Tick_Size := Max(Mxd / 50, 100);
  end if;
  Big_Tick_Size := 50 * Tick_Size;

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

  -- Show clustering information
  Put_Line("  Proximity  type  : " & To_Name(Pt));
  Put_Line("  Clustering type  : " & To_Name(Ct));
  Put_Line("  Weighting  type  : " & To_Name(Wt));
  if Ct = Beta_Flexible then
    Put_Line("  Clustering param : " & D2Sea(Cp, Aft => 3));
  elsif Ct = Versatile_Linkage then
    Put("  Clustering param : " & D2Sea(Cp, Aft => 4));
    Ve := Versatile_Power(Pt, Ct, Cp);
    if Ve = Double'Last then
      Put_Line(" -> Versatile exponent : +Infinity");
    elsif Ve = Double'First then
      Put_Line(" -> Versatile exponent : -Infinity");
    else
      Put_Line(" -> Versatile exponent : " & D2Sea(Ve, Aft => 4));
    end if;
  end if;
  Put_Line("  Size             : " & I2S(N));
  Put_Line("  Precision        : " & I2S(Precision));
  if Dt = Binary_Dendrogram then
    Put_Line("  Dendrogram mode  : " & To_Name(Dm));
    if Dm /= Count then
      Put_Line("  Max dendrograms  : " & L2S(Mxd));
    end if;
  end if;

  -- Prepare output for Unsorted
  if Dm = Unsorted then
    Open_Or_Create(Ft_Text    , U2S(Fn_Out_Text));
    Open_Or_Create(Ft_Json    , U2S(Fn_Out_Json));
    Open_Or_Create(Ft_Newick  , U2S(Fn_Out_Newick));
    Open_Or_Create(Ft_Measures, U2S(Fn_Out_Measures));

    Put_Line(Ft_Text  , "# First Binary Dendrograms (maximum " & L2S(Mxd) & ")");
    Put_Line(Ft_Json  , "# First Binary Dendrograms (maximum " & L2S(Mxd) & ")");
    Put_Line(Ft_Newick, "# First Binary Dendrograms (maximum " & L2S(Mxd) & ")");
    Put_Line(Ft_Measures, "Cophenetic_Correlation"        & HTab & "Cophenetic_Correlation_Error" & HTab &
                          "Normalized_Mean_Squared_Error" & HTab & "Normalized_Mean_Absolute_Error");
  end if;

  Initialize(Dendros);

  -- Find Identical Patterns Degeneration
  Find_Indentical_Patterns(Data, Pt, Precision, Lol_Degenerated);
  Num_Degenerated := Indentical_Patterns_Degeneration(Lol_Degenerated);
  Uip := Identical_Patterns(Lol_Degenerated);
  Free(Lol_Degenerated);

  -- Perform the Hierarchical Clustering
  case Dt is
    when Multidendrogram =>
      Hierarchical_Clustering(Data, Names, Pt, Precision, Ct, Wt, Cp, Md);
      Add_Last(Md, Dendros);
    when Binary_Dendrogram =>
      Hierarchical_Clust(Data, Names, Pt, Precision, Ct, Wt, Cp);
      if Num_Dendro >= Tick_Size and Num_Dendro mod Big_Tick_Size /= 0 then
        New_Line;
      end if;
  end case;

  -- Show Number of Dendrograms
  Num_Saved := Size(Dendros);

  case Dt is
    when Multidendrogram =>
      Put_Line("  Result           : MultiDendrogram");
    when Binary_Dendrogram =>
      if Num_Dendro = 1 then
        Put_Line("  Result           : Binary Dendrogram");
      elsif Dm = Best then
        Put_Line("  Result           : " & L2S(Num_Dendro) & " Binary Dendrograms, " & I2S(Num_Saved) & " with Highest Cophenetic Correlation");
      elsif Dm = Sample then
        Put_Line("  Result           : " & L2S(Num_Dendro) & " Binary Dendrograms, " & I2S(Num_Saved) & " saved");
      else
        Put_Line("  Result           : " & L2S(Num_Dendro) & " Binary Dendrograms");
      end if;
      if Num_Degenerated > 1 then
        Put_Line("                   : Degeneration " & L2S(Num_Degenerated) & " due to Identical Patterns");
        Put_Line("                   : " & L2S(Num_Dendro * Num_Degenerated) & " Binary Dendrograms if degeneration included");
      end if;

      Delete_File(U2S(Fn_Out_Count));
      if Search_Aborted then
        Put_String_Line(U2S(Fn_Out_Count), L2S(Num_Dendro) & " Binary Dendrograms found (search aborted, " & To_Name(Dm) & " mode)");
      else
        Put_String_Line(U2S(Fn_Out_Count), L2S(Num_Dendro) & " Binary Dendrograms found");
      end if;
      if Num_Degenerated > 1 then
        Put_String_Line(U2S(Fn_Out_Count), "Degeneration " & L2S(Num_Degenerated) & " due to Identical Patterns " & U2S(Uip));
        Put_String_Line(U2S(Fn_Out_Count), L2S(Num_Dendro * Num_Degenerated) & " Binary Dendrograms if degeneration included");
      end if;
  end case;

  -- Treatment of Saved Dendrograms
  if Num_Saved >= 1 then
    -- Calculate Deviation Measures and Sort in Descending Cophenetic Correlation
    Di := new Dendro_Info_Array(1..Num_Saved);

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

    -- Write results
    Open_Or_Create(Ft_Text    , U2S(Fn_Out_Text));
    Open_Or_Create(Ft_Json    , U2S(Fn_Out_Json));
    Open_Or_Create(Ft_Newick  , U2S(Fn_Out_Newick));
    Open_Or_Create(Ft_Measures, U2S(Fn_Out_Measures));
    Open_Or_Create(Ft_Ultra   , U2S(Fn_Out_Ultra));

    if Search_Aborted then
      Us := S2U(" (search aborted after " & L2S(Num_Dendro) & " dendrograms)");
    else
      Us := Null_Ustring;
    end if;

    case Dt is
      when Multidendrogram =>
        Put_Line(Ft_Text  , "# MultiDendrogram");
        Put_Line(Ft_Json  , "# MultiDendrogram");
        Put_Line(Ft_Newick, "# MultiDendrogram");
        Put_Line(Ft_Ultra , "# Ultrametric of MultiDendrogram");
      when Binary_Dendrogram =>
        if Num_Saved = 1 then
          Put_Line(Ft_Text  , "# Binary Dendrogram");
          Put_Line(Ft_Json  , "# Binary Dendrogram");
          Put_Line(Ft_Newick, "# Binary Dendrogram");
          Put_Line(Ft_Ultra , "# Ultrametric of Binary Dendrogram");
        else
          case Dm is
            when Sorted =>
              Put_Line(Ft_Text  , "# " & I2S(Num_Saved) & " Binary Dendrograms sorted by Cophenetic Correlation" & U2S(Us));
              Put_Line(Ft_Json  , "# " & I2S(Num_Saved) & " Binary Dendrograms sorted by Cophenetic Correlation" & U2S(Us));
              Put_Line(Ft_Newick, "# " & I2S(Num_Saved) & " Binary Dendrograms sorted by Cophenetic Correlation" & U2S(Us));
            when Unsorted =>
              null;
            when Sample =>
              Put_Line(Ft_Text  , "# " & I2S(Num_Saved) & " Sample Binary Dendrograms" & U2S(Us));
              Put_Line(Ft_Json  , "# " & I2S(Num_Saved) & " Sample Binary Dendrograms" & U2S(Us));
              Put_Line(Ft_Newick, "# " & I2S(Num_Saved) & " Sample Binary Dendrograms" & U2S(Us));
            when Best =>
              if Num_Saved = 1 then
                Put_Line(Ft_Text  , "# Binary Dendrogram with Highest Cophenetic Correlation" & U2S(Us));
                Put_Line(Ft_Json  , "# Binary Dendrogram with Highest Cophenetic Correlation" & U2S(Us));
                Put_Line(Ft_Newick, "# Binary Dendrogram with Highest Cophenetic Correlation" & U2S(Us));
              else
                Put_Line(Ft_Text  , "# The " & I2S(Num_Saved) & " Binary Dendrograms with Highest Cophenetic Correlation" & U2S(Us));
                Put_Line(Ft_Json  , "# The " & I2S(Num_Saved) & " Binary Dendrograms with Highest Cophenetic Correlation" & U2S(Us));
                Put_Line(Ft_Newick, "# The " & I2S(Num_Saved) & " Binary Dendrograms with Highest Cophenetic Correlation" & U2S(Us));
              end if;
            when Count =>
              null;
          end case;
          Put_Line(Ft_Ultra , "# Ultrametric of Binary Dendrograms");
        end if;
    end case;

    Put(Ft_Measures, "Dendrogram_Id" & HTab);
    Put_Line(Ft_Measures, "Cophenetic_Correlation"        & HTab & "Cophenetic_Correlation_Error" & HTab &
                          "Normalized_Mean_Squared_Error" & HTab & "Normalized_Mean_Absolute_Error");

    for I in Di'Range loop
      Put_Line(Ft_Text, "# Dendrogram " & I2S(I));
      Put_Dendrogram(Ft_Text, Di(I).Dendro, Precision, Text_Tree);
      New_Line(Ft_Text);

      Put_Line(Ft_Json, "# Dendrogram " & I2S(I));
      Put_Dendrogram(Ft_Json, Di(I).Dendro, Precision, Json_Tree);
      New_Line(Ft_Json);

      Put(Ft_Newick, I2S(I) & HTab);
      Put_Dendrogram(Ft_Newick, Di(I).Dendro, Precision, Newick_Tree);

      Put_Line(Ft_Ultra, "# Dendrogram " & I2S(I));
      Put_Ultrametric(Ft_Ultra, Di(I).Um, Names, Precision);
      New_Line(Ft_Ultra);

      Put(Ft_Measures, I2S(I) & HTab);
      Put_Line(Ft_Measures, D2Se0(Di(I).Coph, Aft => 6) & HTab & D2Se0(Di(I).Coph_Err, Aft => 6) & HTab &
                            D2Se0(Di(I).Nmse, Aft => 6) & HTab & D2Se0(Di(I).Nmae, Aft => 6));
    end loop;
  end if;

  -- Finalize output
  if Dm /= Count then
    Close(Ft_Text);
    Close(Ft_Json);
    Close(Ft_Newick);
    Close(Ft_Measures);
    if Dm /= Unsorted then
      Close(Ft_Ultra);
    end if;
  end if;

  -- Find and save most dissimilar dendrograms
  if Num_Saved >= 2 then
    -- Find most dissimilar dendrograms
    Min_Sim := Double'Last;
    for I in 1..(Num_Saved - 1) loop
      for J in (I + 1)..Num_Saved loop
        Sim := Dendrograms_Similarity(Di(I).Um, Di(J).Um);
        if Sim < Min_Sim then
          Min_Sim := Sim;
          Sel_Index(1) := I;
          Sel_Index(2) := J;
        end if;
      end loop;
    end loop;
    Sel_Di(1) := Di(Sel_Index(1));
    Sel_Di(2) := Di(Sel_Index(2));

    Put_Line("  Most dissimilar  : Dendrograms " & I2S(Sel_Index(1)) & " and " & I2S(Sel_Index(2)) & ", sim = " & D2Se0(Min_Sim, Aft => 6));

    -- Save most dissimilar dendrograms
    Open_Or_Create(Ft_Sel_Text    , U2S(Fn_Out_Sel_Text));
    Open_Or_Create(Ft_Sel_Measures, U2S(Fn_Out_Sel_Measures));
    Open_Or_Create(Ft_Sel_Ultra   , U2S(Fn_Out_Sel_Ultra));

    Put(Ft_Sel_Measures, "Dendrogram_Id" & HTab);
    Put_Line(Ft_Sel_Measures, "Cophenetic_Correlation"        & HTab & "Cophenetic_Correlation_Error" & HTab &
                              "Normalized_Mean_Squared_Error" & HTab & "Normalized_Mean_Absolute_Error");

    for I in Sel_Di'Range loop
      Put_Line(Ft_Sel_Text, "# Dendrogram " & I2S(Sel_Index(I)));
      Put_Dendrogram(Ft_Sel_Text, Sel_Di(I).Dendro, Precision, Text_Tree);
      New_Line(Ft_Sel_Text);

      Put_Line(Ft_Sel_Ultra, "# Dendrogram " & I2S(Sel_Index(I)));
      Put_Ultrametric(Ft_Sel_Ultra, Sel_Di(I).Um, Names, Precision);
      New_Line(Ft_Sel_Ultra);

      Put(Ft_Sel_Measures, I2S(Sel_Index(I)) & HTab);
      Put_Line(Ft_Sel_Measures, D2Se0(Sel_Di(I).Coph, Aft => 6) & HTab & D2Se0(Sel_Di(I).Coph_Err, Aft => 6) & HTab &
                                D2Se0(Sel_Di(I).Nmse, Aft => 6) & HTab & D2Se0(Sel_Di(I).Nmae, Aft => 6));
    end loop;

    New_Line(Ft_Sel_Measures);
    Put_Line(Ft_Sel_Measures, "Similarity between them");
    Put_Line(Ft_Sel_Measures, D2Se0(Min_Sim, Aft => 6));

    Close(Ft_Sel_Text);
    Close(Ft_Sel_Measures);
    Close(Ft_Sel_Ultra);
  end if;
  New_Line;

  -- Free space
  Free(Data);
  Free(Col_Name);
  Free(Row_Name);
  Free(Dendros);

  if Num_Saved > 0 then
    for I in Di'Range loop
      Free(Di(I));
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
