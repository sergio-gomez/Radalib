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


-- @filename Data_To_Proximities.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 02/05/2012
-- @revision 26/02/2016
-- @brief Calculation of Proximities from a Data set

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Long_Elementary_Functions; use Ada.Numerics.Long_Elementary_Functions;

with Statistics_Double; use Statistics_Double;
with Graphs_Double; use Graphs_Double;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Data_To_Proximities is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2019 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Calculate many types of proximities (distances or             ==");
    Put_Line("== similarities) between rows or columns in a data set:          ==");
    Put_Line("==   - Eucldean, Manhattan, Chebyshev, Minkowski, Canberra       ==");
    Put_Line("==   - Bray Curtis, correlation, cosine                          ==");
    Put_Line("==   - several scalings and transformations available            ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  type Orientation_Type is (By_Rows, By_Columns);

  type Scaling_Type is (No_Scaling, Scaling_01, Scaling_Z_Score);

  type Dissimilarity_Type is (Euclidean_Distance, Manhattan_Distance, Chebyshev_Distance,
                              Minkowski_Distance, Canberra_Distance, Bray_Curtis_Dissimilarity,
                              Correlation_Distance, Correlation_Dissimilarity,
                              Correlation_Abs_Dissimilarity, Correlation_Sqr_Dissimilarity,
                              Cosine_Dissimilarity);
  subtype Correlation_Dissimilarity_Type is Dissimilarity_Type range Correlation_Distance..Correlation_Sqr_Dissimilarity;

  type Transform_Type is (No_Transform, One_Minus_Dissim, One_Minus_Two_Dissim,
                          Inverse_Of_Dissim, Exp_Of_Minus_Dissim, One_If_Zero);

  Unknown_Orientation_Error: exception;
  Unknown_Scaling_Error: exception;
  Unknown_Dissimilarity_Error: exception;
  Unknown_Transform_Error: exception;
  Unknown_Correlation_Error: exception;

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

  function Get_Scaling_Type(S: in String) return Scaling_Type is
  begin
    if    To_Uppercase(S) = "NS"  or To_Lowercase(S) = "no_scaling" then
      return No_Scaling;
    elsif To_Uppercase(S) = "S01" or To_Lowercase(S) = "scaling_01" then
      return Scaling_01;
    elsif To_Uppercase(S) = "SZS" or To_Lowercase(S) = "scaling_z_score"  then
      return Scaling_Z_Score;
    else
      raise Unknown_Scaling_Error;
    end if;
  end Get_Scaling_Type;

  function Get_Dissimilarity_Type(S: in String) return Dissimilarity_Type is
  begin
    if    To_Uppercase(S) = "EUCL" or To_Lowercase(S) = "euclidean_distance" then
      return Euclidean_Distance;
    elsif To_Uppercase(S) = "MANH" or To_Lowercase(S) = "manhattan_distance" then
      return Manhattan_Distance;
    elsif To_Uppercase(S) = "CHEB" or To_Lowercase(S) = "chebyshev_distance" then
      return Chebyshev_Distance;
    elsif To_Uppercase(S) = "MINK" or To_Lowercase(S) = "minkowski_distance" then
      return Minkowski_Distance;
    elsif To_Uppercase(S) = "CANB" or To_Lowercase(S) = "canberra_distance" then
      return Canberra_Distance;
    elsif To_Uppercase(S) = "BRAY" or To_Lowercase(S) = "bray_curtis_dissimilarity" then
      return Bray_Curtis_Dissimilarity;
    elsif To_Uppercase(S) = "CORD" or To_Lowercase(S) = "correlation_distance" then
      return Correlation_Distance;
    elsif To_Uppercase(S) = "CODI" or To_Lowercase(S) = "correlation_dissimilarity" then
      return Correlation_Dissimilarity;
    elsif To_Uppercase(S) = "CABS" or To_Lowercase(S) = "correlation_abs_dissimilarity" then
      return Correlation_Abs_Dissimilarity;
    elsif To_Uppercase(S) = "CSQR" or To_Lowercase(S) = "correlation_sqr_dissimilarity" then
      return Correlation_Sqr_Dissimilarity;
    elsif To_Uppercase(S) = "COSI" or To_Lowercase(S) = "cosine_dissimilarity" then
      return Cosine_Dissimilarity;
    else
      raise Unknown_Dissimilarity_Error;
    end if;
  end Get_Dissimilarity_Type;

  function Get_Transform_Type(S: in String) return Transform_Type is
  begin
    if    To_Uppercase(S) = "NT"   or To_Lowercase(S) = "no_transform" then
      return No_Transform;
    elsif To_Uppercase(S) = "OMD"  or To_Lowercase(S) = "one_minus_dissim" then
      return One_Minus_Dissim;
    elsif To_Uppercase(S) = "OM2D" or To_Lowercase(S) = "one_minus_two_dissim" then
      return One_Minus_Two_Dissim;
    elsif To_Uppercase(S) = "IOD"  or To_Lowercase(S) = "inverse_of_dissim" then
      return Inverse_Of_Dissim;
    elsif To_Uppercase(S) = "EOMD" or To_Lowercase(S) = "exp_of_minus_dissim" then
      return Exp_Of_Minus_Dissim;
    elsif To_Uppercase(S) = "OIZ"  or To_Lowercase(S) = "one_if_zero" then
      return One_If_Zero;
    else
      raise Unknown_Transform_Error;
    end if;
  end Get_Transform_Type;

  function Get_Correlation_Type(S: in String) return Correlation_Type is
  begin
    if    To_Uppercase(S) = "P" or To_Lowercase(S) = "pearson" then
      return Pearson;
    elsif To_Uppercase(S) = "S" or To_Lowercase(S) = "spearman" then
      return Spearman;
    else
      raise Unknown_Correlation_Error;
    end if;
  end Get_Correlation_Type;

  function Calculate_Dissimilarity(P1, P2: in PDoubles; Dissim: in Dissimilarity_Type; Mink_Param: in Double; Ct: in Correlation_Type) return Double is
    D, N1, N2, Num, Den: Double := 0.0;
    Diff, Corr: Double;
  begin
    case Dissim is
      when Euclidean_Distance =>
        for I in P1'Range loop
          if P1(I) /= P2(I) then
            D := D + (P1(I) - P2(I)) * (P1(I) - P2(I));
          end if;
        end loop;
        if D > 0.0 then
          D := Sqrt(D);
        end if;
      when Manhattan_Distance =>
        for I in P1'Range loop
          if P1(I) /= P2(I) then
            D := D + abs (P1(I) - P2(I));
          end if;
        end loop;
      when Chebyshev_Distance =>
        for I in P1'Range loop
          if P1(I) /= P2(I) then
            Diff := abs (P1(I) - P2(I));
            if Diff > D then
              D := Diff;
            end if;
          end if;
        end loop;
      when Minkowski_Distance =>
        if Mink_Param /= 0.0 then
          for I in P1'Range loop
            if P1(I) /= P2(I) then
              D := D + (abs (P1(I) - P2(I))) ** Mink_Param;
            end if;
          end loop;
          if D > 0.0 then
            D := D ** (1.0 / Mink_Param);
          end if;
        end if;
      when Canberra_Distance =>
        for I in P1'Range loop
          if P1(I) /= P2(I) then
            if P1(I) /= 0.0 and P2(I) /= 0.0 then
              D := D + (abs (P1(I) - P2(I))) / ((abs P1(I)) + (abs P2(I)));
            elsif P1(I) /= 0.0 or P2(I) /= 0.0 then
              D := D + 1.0;
            end if;
          end if;
        end loop;
      when Bray_Curtis_Dissimilarity =>
        for I in P1'Range loop
          if P1(I) /= P2(I) then
            Num := Num + (abs (P1(I) - P2(I)));
          end if;
          Den := Den + P1(I) + P2(I);
        end loop;
        if Den /= 0.0 then
          D := Num / Den;
        end if;
      when Correlation_Distance =>
        Corr := Correlation(P1, P2, Ct);
        D := Sqrt(2.0 * (1.0 - Corr));
      when Correlation_Dissimilarity =>
        Corr := Correlation(P1, P2, Ct);
        D := (1.0 - Corr) / 2.0;
      when Correlation_Abs_Dissimilarity =>
        Corr := Correlation(P1, P2, Ct);
        D := 1.0 - abs Corr;
      when Correlation_Sqr_Dissimilarity =>
        Corr := Correlation(P1, P2, Ct);
        Diff := 1.0 - (Corr * Corr);
        if Corr = 1.0 or Corr = -1.0 or Diff < 0.0 then
          D := 0.0;
        else
          D := Sqrt(Diff);
        end if;
      when Cosine_Dissimilarity =>
        for I in P1'Range loop
          if P1(I) /= 0.0 and P2(I) /= 0.0 then
            D := D + P1(I) * P2(I);
          end if;
          if P1(I) /= 0.0 then
            N1 := N1 + P1(I) * P1(I);
          end if;
          if P2(I) /= 0.0 then
            N2 := N2 + P2(I) * P2(I);
          end if;
        end loop;
        if N1 > 0.0 and N2 > 0.0 then
          D := (1.0 - D / (Sqrt(N1) * Sqrt(N2))) / 2.0;
        end if;
    end case;
    return D;
  end Calculate_Dissimilarity;

  function Calculate_Transform(D: in Double; Transform: in Transform_Type) return Double is
    Prox: Double := 0.0;
  begin
    case Transform is
      when No_Transform =>
        Prox := D;
      when One_Minus_Dissim =>
        Prox := 1.0 - D;
      when One_Minus_Two_Dissim =>
        Prox := 1.0 - 2.0 * D;
      when Inverse_Of_Dissim =>
        if D /= 0.0 then
          Prox := 1.0 / D;
        end if;
      when Exp_Of_Minus_Dissim =>
        Prox := Exp(-D);
      when One_If_Zero =>
        if D = 0.0 then
          Prox := 1.0;
        else
          Prox := 0.0;
        end if;
    end case;
    return Prox;
  end Calculate_Transform;

  Minkowski_Param_Default: constant Double := 2.0;
  Correlation_Type_Default: constant Correlation_Type := Pearson;

  Orientation: Orientation_Type;
  Scaling: Scaling_Type;
  Dissim: Dissimilarity_Type;
  Dissim_Param: Ustring;
  Transform: Transform_Type;
  Fn_In: Ustring;
  Fn_Out: Ustring;
  Aft: Field;
  F_In, F_Out: File_Type;
  U: Ustring;
  Nr, Nc, I, J, N: Natural;
  Name_R1C1, Names_Col1, Names_Row1: Boolean := False;
  Data: PDoubless;
  Gr: Graph;
  Vi: Vertex;
  El: Edges_List;
  E: Edge;
  P, P1, P2: PDoubles;
  Min, Max, Mean, Sigma, D, Prox: Double;
  Mink_Param: Double := Minkowski_Param_Default;
  Ct: Correlation_Type := Correlation_Type_Default;

begin
  Put_Info;

  if Argument_Count = 6 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Orientation := Get_Orientation_Type(Argument(3));
    Scaling := Get_Scaling_Type(Argument(4));
    Dissim := Get_Dissimilarity_Type(Argument(5));
    Dissim_Param := Null_Ustring;
    Transform := Get_Transform_Type(Argument(6));
    Aft := Default_Double_Aft;
  elsif Argument_Count = 7 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Orientation := Get_Orientation_Type(Argument(3));
    Scaling := Get_Scaling_Type(Argument(4));
    Dissim := Get_Dissimilarity_Type(Argument(5));
    if Is_Integer(Argument(7)) then
      Dissim_Param := Null_Ustring;
      Transform := Get_Transform_Type(Argument(6));
      Aft := S2I(Argument(7));
    else
      Dissim_Param := S2U(Argument(6));
      Transform := Get_Transform_Type(Argument(7));
      Aft := Default_Double_Aft;
    end if;
  elsif Argument_Count = 8 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Orientation := Get_Orientation_Type(Argument(3));
    Scaling := Get_Scaling_Type(Argument(4));
    Dissim := Get_Dissimilarity_Type(Argument(5));
    Dissim_Param := S2U(Argument(6));
    Transform := Get_Transform_Type(Argument(7));
    Aft := S2I(Argument(8));
  else
    Put_Line("Usage:  " & Command_Name & "  data_name  proximities_name  rows_or_columns  scaling_type  dissimilarity_type  [ dissimilarity_param ]  transform_type  [ decimal_digits ]");
    New_Line;
    Put_Line("   data_name           :  name of the data file");
    New_Line;
    Put_Line("   proximities_name    :  name of the file with the output proximities matrix");
    Put_Line("                            if name has .net extension, the output is a network file in Pajek format");
    New_Line;
    Put_Line("   rows_or_columns     :  R | C");
    Put_Line("                            also lowercase symbols");
    Put_Line("                            also case-insensitive full names (Rows, ...)");
    Put_Line("                            R = Rows");
    Put_Line("                            C = Cols = Columns");
    New_Line;
    Put_Line("   scaling_type        :  NS | S01 | SZS");
    Put_Line("                            also lowercase symbols");
    Put_Line("                            also case-insensitive full names (No_Scaling, ...)");
    Put_Line("                            NS  = No_Scaling");
    Put_Line("                                    x");
    Put_Line("                            S01 = Scaling_01");
    Put_Line("                                    (x - x_{min}) / (x_{max} - x_{min})");
    Put_Line("                            SZS = Scaling_Z_Score");
    Put_Line("                                    (x - <x>) / sigma_x");
    New_Line;
    Put_Line("   dissimilarity_type  :  EUCL | MANH | CHEB | MINK | CANB | BRAY | CORD | CODI | CABS | CSQR | COSI");
    Put_Line("                            also lowercase symbols");
    Put_Line("                            also case-insensitive full names (Euclidean_Distance, ...)");
    Put_Line("                            EUCL = Euclidean_Distance");
    Put_Line("                                     \sum_k (x_k - y_k)^2");
    Put_Line("                            MANH = Manhattan_Distance");
    Put_Line("                                     \sum_k |x_k - y_k|");
    Put_Line("                            CHEB = Chebyshev_Distance");
    Put_Line("                                     \max_k |x_k - y_k|");
    Put_Line("                            MINK = Minkowski_Distance");
    Put_Line("                                     [\sum_k (x_k - y_k)^p]^(1/p)");
    Put_Line("                            CANB = Canberra_Distance:");
    Put_Line("                                     \sum_k \frac{ |x_k - y_k| }{ |x_k| + |y_k| }");
    Put_Line("                            BRAY = Bray_Curtis_Dissimilarity:");
    Put_Line("                                     \frac{2 \sum_k |x_k - y_k| }{ \sum_k (x_k + y_k) }");
    Put_Line("                            CORD = Correlation_Distance:");
    Put_Line("                                     \sqrt{2 (1 - \rho)}");
    Put_Line("                            CODI = Correlation_Dissimilarity:");
    Put_Line("                                     \frac{1}{2} (1 - \rho)");
    Put_Line("                            CABS = Correlation_Abs_Dissimilarity:");
    Put_Line("                                     1 - |\rho|");
    Put_Line("                            CSQR = Correlation_Sqr_Dissimilarity:");
    Put_Line("                                     \sqrt(1 - \rho^2)");
    Put_Line("                            COSI = Cosine_Dissimilarity");
    Put_Line("                                     \frac{1}{2} (1 - \frac{x y}{|x| |y|})");
    New_Line;
    Put_Line("   dissimilarity_param :  parameter for some dissimilarity types, otherwise ignored");
    Put_Line("                            for Minkowski Distance: parameter p of the p-norm");
    Put_Line("                              integer or float number");
    Put_Line("                              default => " & I2S(Default_Double_Aft));
    Put_Line("                            for Correlation Distances: correlation type");
    Put_Line("                              P | S");
    Put_Line("                              also lowercase symbols");
    Put_Line("                              also case-insensitive full names (Pearson, ...)");
    Put_Line("                              P = Pearson");
    Put_Line("                              S = Spearman");
    Put_Line("                              default => " & Capitalize(Correlation_Type'Image(Correlation_Type_Default)));
    New_Line;
    Put_Line("   transform_type      :  NT | OMD | OM2D | IOD | EOMD");
    Put_Line("                            also lowercase symbols");
    Put_Line("                            also case-insensitive full names (No_Transform, ...)");
    Put_Line("                            NT   = No_Transform");
    Put_Line("                                     D");
    Put_Line("                            OMD  = One_Minus_Dissim");
    Put_Line("                                     1 - D");
    Put_Line("                            OM2D = One_Minus_Two_Dissim");
    Put_Line("                                     1 - 2 D");
    Put_Line("                            IOD  = Inverse_Of_Dissim");
    Put_Line("                                     \frac{1}{D}");
    Put_Line("                            EOMD = Exp_Of_Minus_Dissim");
    Put_Line("                                     \exp(-D)");
    Put_Line("                            OIZ  = One_If_Zero");
    Put_Line("                                     \delta(D,0)");
    New_Line;
    Put_Line("   decimal_digits      :  number of decimal digits for float values");
    Put_Line("                            default => " & I2S(Default_Double_Aft));
    return;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  if Dissim_Param /= Null_Ustring then
    if Dissim = Minkowski_Distance then
      Mink_Param := U2D(Dissim_Param);
    elsif Dissim in Correlation_Dissimilarity_Type then
      Ct := Get_Correlation_Type(U2S(Dissim_Param));
      Dissim_Param := S2U(Capitalize(Correlation_Type'Image(Ct)));
    else
      Dissim_Param := Null_Ustring;
    end if;
  else
    if Dissim = Minkowski_Distance then
      Dissim_Param := S2U(D2Se0(Mink_Param, Aft => 1));
    elsif Dissim in Correlation_Dissimilarity_Type then
      Dissim_Param := S2U(Capitalize(Correlation_Type'Image(Ct)));
    end if;
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
  Put_Line("  " & Capitalize(Orientation_Type'Image(Orientation)));
  Put_Line("  " & Capitalize(Scaling_Type'Image(Scaling)));
  if Dissim_Param = Null_Ustring then
    Put_Line("  " & Capitalize(Dissimilarity_Type'Image(Dissim)));
  else
    Put_Line("  " & Capitalize(Dissimilarity_Type'Image(Dissim)) & "  " & U2S(Dissim_Param));
  end if;
  Put_Line("  " & Capitalize(Transform_Type'Image(Transform)));

  -- Initialize network and data
  case Orientation is
    when By_Rows =>
      Initialize(Gr, Nr, Directed => False);
    when By_Columns =>
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
          if Orientation = By_Columns then
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
          if Orientation = By_Rows then
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
      Put_Line(NLine & "Current position at " & To_String(Fn_In) & ": " & I2S(Integer(Line(F_In))) & ", " & I2S(Integer(Col(F_In))));
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
        else
          for R in 1..Nr loop
            Data(R, C) := 0.5;
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
        else
          for R in 1..Nr loop
            Data(R, C) := 0.0;
          end loop;
        end if;
      end loop;
      Free(P);
  end case;

  -- Calculate proximities
  case Orientation is
    when By_Rows =>
      P1 := Alloc(1, Nc);
      P2 := Alloc(1, Nc);
      for R1 in 1..Nr loop
        for R2 in R1..Nr loop
          for C in 1..Nc loop
            P1(C) := Data(R1, C);
            P2(C) := Data(R2, C);
          end loop;
          D := Calculate_Dissimilarity(P1, P2, Dissim, Mink_Param, Ct);
          Prox := Calculate_Transform(D, Transform);
          Add_Edge(Get_Vertex(Gr, R1), Get_Vertex(Gr, R2), Prox);
        end loop;
      end loop;
      Free(P1);
      Free(P2);
    when By_Columns =>
      P1 := Alloc(1, Nr);
      P2 := Alloc(1, Nr);
      for C1 in 1..Nc loop
        for C2 in C1..Nc loop
          for R in 1..Nr loop
            P1(R) := Data(R, C1);
            P2(R) := Data(R, C2);
          end loop;
          D := Calculate_Dissimilarity(P1, P2, Dissim, Mink_Param, Ct);
          Prox := Calculate_Transform(D, Transform);
          Add_Edge(Get_Vertex(Gr, C1), Get_Vertex(Gr, C2), Prox);
        end loop;
      end loop;
      Free(P1);
      Free(P2);
  end case;

  -- Write proximities
  if To_Lowercase(File_Extension(U2S(Fn_Out))) = "net" then
    Put_Graph(To_String(Fn_Out), Gr, Aft => Aft);
  else
    Create(F_Out, Out_File, U2S(Fn_Out));
    N := Number_Of_Vertices(Gr);
    if (Names_Row1 and Orientation = By_Columns) or (Names_Col1 and Orientation = By_Rows) then
      for I in 1..N loop
        Put(F_Out, Get_Name(Get_Vertex(Gr, I)));
        if I < N then
          Put(F_Out, HTab);
        end if;
      end loop;
      New_Line(F_Out);
    end if;
    for I in 1..N loop
      Vi := Get_Vertex(Gr, I);
      El := Edges_From(Vi);
      Save(El);
      Reset(El);
      for J in 1..N loop
        E := Next(El);
        Put(F_Out, D2Sea(Value(E), Aft => Aft));
        if J < N then
          Put(F_Out, HTab);
        end if;
      end loop;
      Restore(El);
      New_Line(F_Out);
    end loop;
  end if;

  -- Free network and data
  Free(Gr);
  Free(Data);

end Data_To_Proximities;
