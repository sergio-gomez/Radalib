-- Radalib, Copyright (c) 2019 by
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


-- @filename Data_To_Correlations.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 22/10/2008
-- @revision 14/01/2018
-- @brief Find the Correlation Network of a Data set

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Statistics_Double; use Statistics_Double;
with Graphs_Double; use Graphs_Double;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Data_To_Correlations is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2019 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Find the correlations network of a data set                   ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  type Correlation_Type is (Row_Correlations, Column_Correlations);
  type Scaling_Type is (No_Scaling, Scaling_01, Scaling_Z_Score);

  Unknown_Correlation_Error: exception;
  Unknown_Scaling_Error: exception;

  function Get_Correlation_Type(S: in String) return Correlation_Type is
  begin
    if    To_Uppercase(S) = "R" or To_Lowercase(S) = "rows" then
      return Row_Correlations;
    elsif To_Uppercase(S) = "C" or To_Lowercase(S) = "cols" or To_Lowercase(S) = "columns" then
      return Column_Correlations;
    else
      raise Unknown_Correlation_Error;
    end if;
  end Get_Correlation_Type;

  function Get_Scaling_Type(S: in String) return Scaling_Type is
  begin
    if    To_Lowercase(S) = "ns"  or To_Lowercase(S) = "no_scale" then
      return No_Scaling;
    elsif To_Lowercase(S) = "s01" or To_Lowercase(S) = "scale_01" then
      return Scaling_01;
    elsif To_Lowercase(S) = "zs"  or To_Lowercase(S) = "z_score"  then
      return Scaling_Z_Score;
    else
      raise Unknown_Scaling_Error;
    end if;
  end Get_Scaling_Type;

  Correlations: Correlation_Type;
  Scaling: Scaling_Type;
  Fn_In: Ustring;
  Fn_Out: Ustring;
  Aft: Field;
  F_In: File_Type;
  U: Ustring;
  Nr, Nc, I, J: Natural;
  Name_R1C1, Names_Col1, Names_Row1: Boolean := False;
  Data: PDoubless;
  Gr: Graph;
  P, P1, P2: PDoubles;
  Min, Max, Mean, Sigma, Corr: Double;

begin
  Put_Info;

  if Argument_Count = 4 then
    Fn_In := S2U(Argument(1));
    Correlations := Get_Correlation_Type(Argument(2));
    Scaling := Get_Scaling_Type(Argument(3));
    Fn_Out := S2U(Argument(4));
    Aft := Default_Double_Aft;
  elsif Argument_Count = 5 then
    Fn_In := S2U(Argument(1));
    Correlations := Get_Correlation_Type(Argument(2));
    Scaling := Get_Scaling_Type(Argument(3));
    Fn_Out := S2U(Argument(4));
    Aft := S2I(Argument(5));
  else
    Put_Line("Usage:  " & Command_Name & "  data_file  rows_or_columns  scaling_type  correlations_file  [ decimal_digits ]");
    New_Line;
    Put_Line("   rows_or_columns :  R | C");
    Put_Line("                        also lowercase symbols");
    Put_Line("                        also case-insensitive full names (Rows, ...)");
    Put_Line("                        R = Rows");
    Put_Line("                        C = Cols = Columns");
    New_Line;
    Put_Line("   scaling_type    :  NS | S01 | ZS");
    Put_Line("                        also lowercase symbols");
    Put_Line("                        also case-insensitive full names (No_Scale, ...)");
    Put_Line("                        NS  = No_Scale");
    Put_Line("                        S01 = Scale_01");
    Put_Line("                        ZS  = Z_Score");
    New_Line;
    Put_Line("   decimal_digits  :  number of decimal digits for float values");
    Put_Line("                        default => " & I2S(Default_Double_Aft));
    return;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  Open(F_In, In_File, U2S(Fn_In));

  -- Determine network size and names location (none, columns or rows)
  Name_R1C1 := False;
  Names_Row1 := False;
  Names_Col1 := False;
  Nc := 0;
  Comments_Skip(F_In);
  while not End_Of_Line(F_In) loop
    Line_Spaces_Skip(F_In);
    Line_Comment_Skip(F_In);
    if not End_Of_Line(F_In) then
      Get_Word(F_In, U);
      Nc := Nc + 1;
      Separator_Skip(F_In);
      if not Is_Real(U) then
        if Nc = 1 then
          Name_R1C1 := True;
        else
          Names_Row1 := True;
        end if;
      end if;
    end if;
  end loop;
  if not End_Of_File(F_In) then
    Skip_Line(F_In);
  end if;
  Nr := 1;
  while not End_Of_File(F_In) loop
    Comments_Skip(F_In);
    Line_Spaces_Skip(F_In);
    Line_Comment_Skip(F_In);
    if not End_Of_Line(F_In) then
      Get_Word(F_In, U);
      Nr := Nr + 1;
      if not Is_Real(U) then
        Names_Col1 := True;
      end if;
    end if;
    if not End_Of_File(F_In) then
      Skip_Line(F_In);
    end if;
  end loop;

  if Name_R1C1 and not (Names_Row1 or Names_Col1) then
    Names_Row1 := True;
    Names_Col1 := True;
  end if;
  if Names_Row1 then
    Nr := Nr - 1;
  end if;
  if Names_Col1 then
    Nc := Nc - 1;
  end if;

  Put_Line("  Size: " & I2S(Nr) & " x " & I2S(Nc));
  if Names_Col1 then
    Put_Line("  Names in first column");
  end if;
  if Names_Row1 then
    Put_Line("  Names in first row");
  end if;
  Put_Line("  " & Capitalize(Correlation_Type'Image(Correlations)));
  Put_Line("  " & Capitalize(Scaling_Type'Image(Scaling)));


  -- Initialize network and data
  case Correlations is
    when Row_Correlations =>
      Initialize(Gr, Nr, Directed => False);
    when Column_Correlations =>
      Initialize(Gr, Nc, Directed => False);
  end case;
  Data := Alloc(1, Nr, 1, Nc);
  Data.all := (others => (others => 0.0));

  -- Read data
  Reset(F_In);
  begin
    -- Read names in first row
    if Names_Row1 then
      Comments_Skip(F_In);
      if Names_Col1 then
        Get_Word(F_In, U);
        Separator_Skip(F_In);
      end if;
      J := 1;
      while not End_Of_Line(F_In) loop
        Line_Spaces_Skip(F_In);
        Line_Comment_Skip(F_In);
        if not End_Of_Line(F_In) then
          Get_Word(F_In, U);
          if Correlations = Column_Correlations then
            Set_Name(Get_Vertex(Gr, J), U2S(U));
          end if;
          Separator_Skip(F_In);
          J := J + 1;
        end if;
      end loop;
      Skip_Line(F_In);
    end if;
    -- Read links
    I := 1;
    while not End_Of_File(F_In) loop
      Comments_Skip(F_In);
      -- Read names in first column
      if Names_Col1 then
        Line_Spaces_Skip(F_In);
        Line_Comment_Skip(F_In);
        if not End_Of_Line(F_In) then
          Get_Word(F_In, U);
          if Correlations = Row_Correlations then
            Set_Name(Get_Vertex(Gr, I), U2S(U));
          end if;
          Separator_Skip(F_In);
        end if;
      end if;
      -- Read data
      J := 1;
      while not End_Of_Line(F_In) loop
        Line_Spaces_Skip(F_In);
        Line_Comment_Skip(F_In);
        if not End_Of_Line(F_In) then
          Get_Word(F_In, U);
          Data(I, J) := S2D(U2S(U));
          Separator_Skip(F_In);
          J := J + 1;
        end if;
      end loop;
      if not End_Of_File(F_In) then
        Skip_Line(F_In);
      end if;
      I := I + 1;
    end loop;
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Current position at " & U2S(Fn_In) & ": " & I2S(Integer(Line(F_In))) & ", " & I2S(Integer(Col(F_In))));
      Put_Line("Current data position: " & I2S(I) & ", " & I2S(J));
      return;
  end;
  Close(F_In);

  -- Calculate scaling
  case Scaling is
    when No_Scaling =>
      null;
    when Scaling_01 =>
      P := Alloc(1, Nr);
      for C in 1..Nc loop
        for R in 1..Nr loop
          P(R) := Data(R, C);
        end loop;
        Min := Minimum(P);
        Max := Maximum(P);
        if Min < Max then
          for R in 1..Nr loop
            Data(R, C) := (Data(R, C) - Min) / (Max - Min);
          end loop;
        end if;
      end loop;
      Free(P);
    when Scaling_Z_Score =>
      P := Alloc(1, Nr);
      for C in 1..Nc loop
        for R in 1..Nr loop
          P(R) := Data(R, C);
        end loop;
        Mean := Arithmetic_Mean(P);
        Sigma := Standard_Deviation(P);
        if Sigma > 0.0 then
          for R in 1..Nr loop
            Data(R, C) := (Data(R, C) - Mean) / Sigma;
          end loop;
        end if;
      end loop;
      Free(P);
  end case;

  -- Calculate correlations
  case Correlations is
    when Row_Correlations =>
      P1 := Alloc(1, Nc);
      P2 := Alloc(1, Nc);
      for R1 in 1..Nr loop
        for R2 in R1..Nr loop
          for C in 1..Nc loop
            P1(C) := Data(R1, C);
            P2(C) := Data(R2, C);
          end loop;
          Corr := Correlation(P1, P2);
          Add_Edge(Get_Vertex(Gr, R1), Get_Vertex(Gr, R2), Corr);
        end loop;
      end loop;
      Free(P1);
      Free(P2);
    when Column_Correlations =>
      P1 := Alloc(1, Nr);
      P2 := Alloc(1, Nr);
      for C1 in 1..Nc loop
        for C2 in C1..Nc loop
          for R in 1..Nr loop
            P1(R) := Data(R, C1);
            P2(R) := Data(R, C2);
          end loop;
          Corr := Correlation(P1, P2);
          Add_Edge(Get_Vertex(Gr, C1), Get_Vertex(Gr, C2), Corr);
        end loop;
      end loop;
      Free(P1);
      Free(P2);
  end case;

  -- Write network
  Put_Graph(U2S(Fn_Out), Gr, Aft => Aft);

  Free(Gr);
  Free(Data);

end Data_To_Correlations;
