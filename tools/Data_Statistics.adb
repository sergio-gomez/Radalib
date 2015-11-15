-- Radalib, Copyright (c) 2015 by
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


-- @filename Data_Statistics.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/05/2013
-- @revision 19/09/2015
-- @brief Statistics of a Data set

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Statistics_Double; use Statistics_Double;
with Data_IO_Double; use Data_IO_Double;

procedure Data_Statistics is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2015 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Find the Statistics of a Data set                             ==");
    Put_Line("== See README.txt                                                ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  -- Orientation
  type Orientation_Type is (By_Rows, By_Columns);

  Unknown_Orientation_Error: exception;

  function Get_Orientation_Type(S: in String) return Orientation_Type is
  begin
    if    To_Uppercase(S) = "R" or To_Lowercase(S) = "rows" then
      return By_Rows;
    elsif To_Uppercase(S) = "C" or To_Lowercase(S) = "cols" or To_Lowercase(S) = "columns" then
      return By_Columns;
    else
      raise Unknown_Orientation_Error;
    end if;
  end Get_Orientation_Type;

  -- Check Data
  function Missing_Data(Data: in PDoubless; No_Value: in Double) return Boolean is
    Max_Warnings: constant Natural := 10;
    Num_Missing: Natural := 0;
  begin
    for R in Data'Range(1) loop
      for C in Data'Range(2) loop
        if Data(R, C) = No_Value then
          Num_Missing := Num_Missing + 1;
          if Num_Missing <= Max_Warnings then
            Put_Line("Error: missing value for Data(" & I2S(R) & ", " & I2S(C) & ")");
          end if;
        end if;
      end loop;
    end loop;
    if Num_Missing > Max_Warnings then
      Put_Line("...");
    end if;
    if Num_Missing > 0 then
      Put_Line("Error: total number of missing data: " & I2S(Num_Missing));
      New_Line;
    end if;
    return Num_Missing > 0;
  end Missing_Data;

  -- Calculate individual statistics
  procedure Calculate_Statistics(Fn: in Ustring; P: in PDoubles; Name: in Ustring; Aft: Natural) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    Pe: PDoubles;
    Pe_Indices: constant Integers(1..7) := (5, 10, 25, 50, 75, 90, 95);
  begin
    if Fn /= Null_Ustring then
      Open_Or_Create(Ft, U2S(Fn));
      Ft_Prev := Current_Output;
      Set_Output(Ft);
    end if;

    Put_Line(U2S(Name) & ":");

    Put_Line("  Minimum  : " & D2Sea(Minimum(P), Aft => Aft));
    Put_Line("  Maximum  : " & D2Sea(Maximum(P), Aft => Aft));
    Put_Line("  Arithmetic Mean : " & D2Sea(Arithmetic_Mean(P), Aft => Aft));
    if Minimum(P) >= 0.0 then
      Put_Line("  Geometric Mean  : " & D2Sea(Geometric_Mean(P), Aft => Aft));
      Put_Line("  Harmonic Mean   : " & D2Sea(Harmonic_Mean(P), Aft => Aft));
    else
      Put_Line("  Geometric Mean  : undefined");
      Put_Line("  Harmonic Mean   : undefined");
    end if;
    Put_Line("  Variance : " & D2Sea(Variance(P), Aft => Aft));
    Put_Line("  Std_Dev  : " & D2Sea(Standard_Deviation(P), Aft => Aft));
    Put_Line("  Skewness : " & D2Sea(Skewness(P), Aft => Aft));
    Put_Line("  Kurtosis : " & D2Sea(Kurtosis(P), Aft => Aft));
    for K in 1..5 loop
      Put_Line("  Central Moment Degree " & I2S(K) & " : " & D2Sea(Central_Moment(P, K), Aft => Aft));
    end loop;
    Pe := Percentiles(P);
    for K in Pe_Indices'Range loop
      Put_Line("  Percentile " & Right_Justify(I2S(Pe_Indices(K)), 2) & " : " & D2Sea(Pe(Pe_Indices(K)), Aft => Aft));
    end loop;
    New_Line;
    Free(Pe);

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Calculate_Statistics;

  -- Calculate pair statistics
  procedure Calculate_Statistics(Fn: in Ustring; P1, P2: in PDoubles; Name1, Name2: in Ustring; Aft: Natural) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    A, B: Double;
  begin
    if Fn /= Null_Ustring then
      Open_Or_Create(Ft, U2S(Fn));
      Ft_Prev := Current_Output;
      Set_Output(Ft);
    end if;

    Put_Line(U2S(Name1) & " vs " & U2S(Name2) & ":");

    Put_Line("  Covariance : " & D2Sea(Covariance(P1, P2), Aft => Aft));
    Put_Line("  Correlation (Pearson)  : " & D2Sea(Pearson_Correlation(P1, P2), Aft => Aft));
    Put_Line("  Correlation (Spearman) : " & D2Sea(Spearman_Correlation(P1, P2), Aft => Aft));
    Put_Line("  Correlation Error (Pearson , Jackknife)        : " & D2Sea(Pearson_Correlation_Error(P1, P2, Cet => Jackknife), Aft => Aft));
    Put_Line("  Correlation Error (Pearson , Bootstrap)        : " & D2Sea(Pearson_Correlation_Error(P1, P2, Cet => Bootstrap), Aft => Aft));
    Put_Line("  Correlation Error (Pearson , Fisher_Transform) : " & D2Sea(Pearson_Correlation_Error(P1, P2, Cet => Fisher_Transform), Aft => Aft));
    Put_Line("  Correlation Error (Spearman, Jackknife)        : " & D2Sea(Spearman_Correlation_Error(P1, P2, Cet => Jackknife), Aft => Aft));
    Put_Line("  Correlation Error (Spearman, Bootstrap)        : " & D2Sea(Spearman_Correlation_Error(P1, P2, Cet => Bootstrap), Aft => Aft));
    Put_Line("  Correlation Error (Spearman, Fisher_Transform) : " & D2Sea(Spearman_Correlation_Error(P1, P2, Cet => Fisher_Transform), Aft => Aft));
    Simple_Linear_Regression(P1, P2, A, B);
    Put_Line("  Simple Linear Regression Y = a X + b :  a = " & D2Sea(A, Aft => Aft) & "   b = " & D2Sea(B, Aft => Aft));
    Simple_Linear_Regression(P2, P1, A, B);
    Put_Line("  Simple Linear Regression X = a Y + b :  a = " & D2Sea(A, Aft => Aft) & "   b = " & D2Sea(B, Aft => Aft));
    New_Line;

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Calculate_Statistics;


  No_Value: constant Double := Double'First;

  Fn_In: Ustring;
  Fn_Out: Ustring := Null_Ustring;
  Index1, Index2: Natural := 0;
  Orientation: Orientation_Type;
  Aft: Field := Default_Double_Aft;

  Data: PDoubless;
  Col_Name, Row_Name: PUstrings;
  Nr, Nc: Natural;
  P, P1, P2: PDoubles;

begin
  Put_Info;

  Fn_Out := Null_Ustring;
  Index1 := 0;
  Index2 := 0;
  Aft    := Default_Double_Aft;
  if Argument_Count = 2 then
    Fn_In  := S2U(Argument(1));
    Orientation := Get_Orientation_Type(Argument(2));
  elsif Argument_Count = 3 then
    Fn_In  := S2U(Argument(1));
    if Is_Integer(Argument(2)) then
      Index1 := S2I(Argument(2));
      Orientation := Get_Orientation_Type(Argument(3));
    elsif Is_Integer(Argument(3)) then
      Orientation := Get_Orientation_Type(Argument(2));
      Aft := S2I(Argument(3));
    else
      Fn_Out := S2U(Argument(2));
      Orientation := Get_Orientation_Type(Argument(3));
    end if;
  elsif Argument_Count = 4 then
    Fn_In  := S2U(Argument(1));
    if Is_Integer(Argument(2)) and Is_Integer(Argument(3)) then
      Index1 := S2I(Argument(2));
      Index2 := S2I(Argument(3));
      Orientation := Get_Orientation_Type(Argument(4));
    elsif Is_Integer(Argument(2)) then
      Index1 := S2I(Argument(2));
      Orientation := Get_Orientation_Type(Argument(3));
      Aft := S2I(Argument(4));
    elsif Is_Integer(Argument(3)) then
      Fn_Out := S2U(Argument(2));
      Index1 := S2I(Argument(3));
      Orientation := Get_Orientation_Type(Argument(4));
    else
      Fn_Out := S2U(Argument(2));
      Orientation := Get_Orientation_Type(Argument(3));
      Aft := S2I(Argument(4));
    end if;
  elsif Argument_Count = 5 then
    Fn_In  := S2U(Argument(1));
    if Is_Integer(Argument(2)) then
      Index1 := S2I(Argument(2));
      Index2 := S2I(Argument(3));
      Orientation := Get_Orientation_Type(Argument(4));
      Aft := S2I(Argument(5));
    elsif Is_Integer(Argument(5)) then
      Fn_Out := S2U(Argument(2));
      Index1 := S2I(Argument(3));
      Orientation := Get_Orientation_Type(Argument(4));
      Aft := S2I(Argument(5));
    else
      Fn_Out := S2U(Argument(2));
      Index1 := S2I(Argument(3));
      Index2 := S2I(Argument(4));
      Orientation := Get_Orientation_Type(Argument(5));
    end if;
  elsif Argument_Count = 6 then
    Fn_In  := S2U(Argument(1));
      Fn_Out := S2U(Argument(2));
      Index1 := S2I(Argument(3));
      Index2 := S2I(Argument(4));
      Orientation := Get_Orientation_Type(Argument(5));
      Aft := S2I(Argument(6));
  else
    Put_Line("Usage:  " & Command_Name & "  data_name  [ statistics_name ]  [ index1 [ index2 ] ]  rows_or_columns  [ decimal_digits ]");
    New_Line;
    Put_Line("   data_name       :  name of the data file");
    New_Line;
    Put_Line("   statistics_name :  name of the file with the output proximities matrix");
    New_Line;
    Put_Line("   index1 index2   :  indices of the row(s) or column(s) to obtain the statistics");
    Put_Line("                        if no indices indicated, all individual and pair statistics calculated");
    Put_Line("                        if first index indicated, the statistics of that row or column calculated");
    Put_Line("                        if both indices indicated, the pair statistics are calculated");
    Put_Line("                        0 < index1 < index2");
    New_Line;
    Put_Line("   rows_or_columns :  R | C");
    Put_Line("                        also lowercase symbols");
    Put_Line("                        also case-insensitive full names (Rows, ...)");
    Put_Line("                        R = Rows");
    Put_Line("                        C = Cols = Columns");
    New_Line;
    Put_Line("   decimal_digits  :  number of decimal digits for float values");
    Put_Line("                        default => " & I2S(Default_Double_Aft));
    return;
  end if;

  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  if Fn_Out /= Null_Ustring then
    Delete_File(U2S(Fn_Out));
  end if;

  -- Get data and names
  Get_Data(U2S(Fn_In), Data, Col_Name, Row_Name, No_Value, Data_IO_D.Matrix_Form);
  if Missing_Data(Data, No_Value) then
    return;
  end if;
  Nr := Data'Length(1);
  Nc := Data'Length(2);

  if Col_Name = null then
    Col_Name := Alloc(1, Nc);
    for C in 1..Nc loop
      Col_Name(C) := S2U("Column_" & I2S(C));
    end loop;
  end if;
  if Row_Name = null then
    Row_Name := Alloc(1, Nr);
    for R in 1..Nr loop
      Row_Name(R) := S2U("Row_" & I2S(R));
    end loop;
  end if;

  if Fn_Out = Null_Ustring then
    Put_Line(To_String(Fn_In));
  else
    Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  end if;
  Put_Line("Size: " & I2S(Nr) & " x " & I2S(Nc));
  Put_Line(Capitalize(Orientation_Type'Image(Orientation)));
  New_Line;

  -- Check indices
  case Orientation is
    when By_Rows =>
      if (Index1 not in 0..Nr) or (Index2 not in 0..Nr) or (Index1 = 0 and Index2 /= 0) or (Index2 /= 0 and Index1 >= Index2) then
        Put_Line("Invalid row indices " & I2S(Index1) & " and " & I2S(Index2));
        return;
      end if;
    when By_Columns =>
      if (Index1 not in 0..Nc) or (Index2 not in 0..Nc) or (Index1 = 0 and Index2 /= 0) or (Index2 /= 0 and Index1 >= Index2) then
        Put_Line("Invalid column indices " & I2S(Index1) & " and " & I2S(Index2));
        return;
      end if;
  end case;

  -- Calculate individual statistics
  case Orientation is
    when By_Rows =>
      P := Alloc(1, Nc);
      for R in 1..Nr loop
        if Index1 = 0 or (Index1 = R and Index2 = 0) then
          for C in 1..Nc loop
            P(C) := Data(R, C);
          end loop;
          Calculate_Statistics(Fn_Out, P, Row_Name(R), Aft);
        end if;
      end loop;
      Free(P);
    when By_Columns =>
      P := Alloc(1, Nr);
      for C in 1..Nc loop
        if Index1 = 0 or (Index1 = C and Index2 = 0) then
          for R in 1..Nr loop
            P(R) := Data(R, C);
          end loop;
          Calculate_Statistics(Fn_Out, P, Col_Name(C), Aft);
        end if;
      end loop;
      Free(P);
  end case;

  -- Calculate pair statistics
  case Orientation is
    when By_Rows =>
      P1 := Alloc(1, Nc);
      P2 := Alloc(1, Nc);
      for R1 in 1..(Nr-1) loop
        if Index1 = 0 or Index1 = R1 then
          for R2 in (R1+1)..Nr loop
            if Index1 = 0 or Index2 = R2 then
              for C in 1..Nc loop
                P1(C) := Data(R1, C);
                P2(C) := Data(R2, C);
              end loop;
              Calculate_Statistics(Fn_Out, P1, P2, Row_Name(R1), Row_Name(R2), Aft);
            end if;
          end loop;
        end if;
      end loop;
      Free(P1);
      Free(P2);
    when By_Columns =>
      P1 := Alloc(1, Nr);
      P2 := Alloc(1, Nr);
      for C1 in 1..(Nc-1) loop
        if Index1 = 0 or Index1 = C1 then
          for C2 in (C1+1)..Nc loop
            if Index1 = 0 or Index2 = C2 then
              for R in 1..Nr loop
                P1(R) := Data(R, C1);
                P2(R) := Data(R, C2);
              end loop;
              Calculate_Statistics(Fn_Out, P1, P2, Col_Name(C1), Col_Name(C2), Aft);
            end if;
          end loop;
        end if;
      end loop;
      Free(P1);
      Free(P2);
  end case;

  -- Write Statistics

  Free(Col_Name);
  Free(Row_Name);
  Free(Data);

end Data_Statistics;
