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


-- @filename Eps_Plots_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 04/04/2002
-- @revision 20/01/2015
-- @brief Test of Eps Plots

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Exceptions; use Ada.Exceptions;
with Eps_Plots; use Eps_Plots;
with Eps_Utils; use Eps_Utils;
with Utils; use Utils;

procedure Eps_Plots_Test is

  procedure Eps_Plots_Test_01 is
    Eps_File: constant String := "eps_plots_test_01.eps";
    Plot: Teps_Plot;
    Inum: constant := 3;
    Jnum: constant := 2;
    World_Min: constant T2dim := (0.0, 0.0);
    World_Max: constant T2dim := (10.0, 10.0);
    World_Pos: constant T2dim := (3 * World_Min + World_Max) / 4;
    R: Float;
    Xaxis, Yaxis: Taxis;
  begin
    Put_Line(Eps_File);
    Start_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Layout => (Inum, Jnum),
      Paper => Dina4,
      Orientation => Portrait,
      Shape => Squared,
      Paper_Margin => (100.0, 75.0),
      Plot_Ll_Margin => (40.0, 30.0),
      Plot_Ur_Margin => (40.0, 0.0),
      Separation => (40.0, 45.0),
      Top_Separation => True,
      Title_Height => 20.0,
      Color => Black,
      Font => (Helvetica, 10.0, False, False),
      Line_Width => 1.0,
      Dash_Pattern => Default_Dash_Pattern,
      Line_End => Default_Line_End,
      Line_Corner => Default_Line_Corner,
      Arrow_Head => Default_Arrow_Head,
      Aft => 2);

    Put_Bounding_Box(Plot);
    Put_Title(Plot, Change_Backslash(Eps_File));
    for I in 1..Inum loop
      Set_Line_Width(Plot, Float(I));
      for J in 1..Jnum loop
        Put_Figure_Frame(Plot, (I, J));
        Set_Figure_Scaling(Plot, (I, J), Xy_Independent, World_Min, World_Max);
      end loop;
    end loop;
    Set_Line_Width(Plot, 1.0);
    for I in 1..Inum loop
      for J in 1..Jnum loop
        R := I * J + 1.0;
        Set_Dash_Pattern(Plot, Solid_Line);
        Set_Color(Plot, Black);
        Set_Axis(Xaxis, Title=>"X axis", Title_Gap=>15.0, Ticks=>True, Ticks_Inc=>0.5,
                 Ticks_Group=>4, Labels=>True, Labels_Decimals=>1);
        Set_Axis(Yaxis, Title=>"Y axis", Title_Gap=>20.0, Ticks=>True, Ticks_Inc=>0.5,
                 Ticks_Group=>4, Labels=>True, Labels_Decimals=>1);
        Put_Figure_Axes(Plot, (I, J), Xaxis, Yaxis);
        Set_Color(Plot, Blue);
        Put_Figure_Title(Plot, (I, J), "Figure " & I2s(I) & "," & I2s(J));
        Set_Color(Plot, Green);
        Put_Figure_Text(Plot, (I, J), "r = " & F2ss(R) & " mm", (World_Pos.X, World_Max.Y));
        Set_Color(Plot, Red);
        Put_Figure_Symbol(Plot, (I, J), Circle, World_Pos, Mm_To_Pt(R), Filled);
        Set_Color(Plot, Navy);
        Put_Figure_Symbol(Plot, (I, J), Splat, World_Pos, Mm_To_Pt(R) + 5.0);
        Put_Figure_Symbol(Plot, (I, J), Plus, World_Min, 5.0);
        Put_Figure_Symbol(Plot, (I, J), Plus, World_Max, 5.0);
        R := 8.0;
        for Pat in Line_Patterns_List'Range loop
          Set_Dash_Pattern(Plot, Line_Patterns_List(Pat));
          Put_Figure_Symbol(Plot, (I, J), Line, (6.0, R), (10.0, R));
          R := R - 0.5;
        end loop;
      end loop;
    end loop;
    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_01;


  procedure Eps_Plots_Test_02 is
    Eps_File: constant String := "eps_plots_test_02.eps";
    Plot: Teps_Plot;
    Inum: constant := 2;
    Jnum: constant := 3;
    World_Min: constant T2dim := (0.0, 0.0);
    World_Max: constant T2dim := (10.0, 10.0);
    World_Pos: constant T2dim := (3 * World_Min + World_Max) / 4;
    W_Ll, W_Ur, Pos: T2dim;
    Corner: T2dims(1..5);
    R: Float;
    Xaxis, Yaxis: Taxis;
  begin
    Put_Line(Eps_File);
    Start_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Layout => (Inum, Jnum),
      Paper => Dina4,
      Orientation => Landscape,
      Shape => Autoshape,
      Paper_Margin => (100.0, 75.0),
      Plot_Ll_Margin => (40.0, 30.0),
      Plot_Ur_Margin => (40.0, 0.0),
      Separation => (40.0, 45.0),
      Top_Separation => True,
      Title_Height => 20.0,
      Color => Black,
      Font => (Helvetica, 10.0, False, False),
      Line_Width => 1.0,
      Dash_Pattern => Default_Dash_Pattern,
      Line_End => Default_Line_End,
      Line_Corner => Default_Line_Corner,
      Arrow_Head => Default_Arrow_Head,
      Aft => Default_Plot_Aft);

    Put_Bounding_Box(Plot);
    Put_Title(Plot, Change_Backslash(Eps_File));
    for I in 1..Inum loop
      for J in 1..Jnum loop
        Put_Figure_Frame(Plot, (I, J));
        if J mod 3 = 1 then
          W_Ll := World_Min;
          W_Ur := World_Max;
        elsif J mod 3 = 2 then
          W_Ll := World_Max;
          W_Ur := World_Min;
        else
          W_Ll := (World_Min.X, World_Max.Y);
          W_Ur := (World_Max.X, World_Min.Y);
        end if;
        Set_Figure_Scaling(Plot, (I, J), Xy_Independent, W_Ll, W_Ur, (10.0, 10.0) + (10.0 * I, 10.0 * J));
      end loop;
    end loop;
    for I in 1..Inum loop
      for J in 1..Jnum loop
        R := I * J + 1.0;
        Set_Color(Plot, Black);
        Set_Line_Width(Plot, 1.0);
        Set_Dash_Pattern(Plot, Solid_Line);
        Set_Axis(Xaxis, Title=>"X", Title_Gap=>12.0, Ticks=>True, Ticks_Inc=>0.5,
                 Ticks_Group=>4, Labels=>True, Labels_Decimals=>0);
        Set_Axis(Yaxis, Title=>"Y", Title_Gap=>12.0, Ticks=>True, Ticks_Inc=>0.5,
                 Ticks_Group=>4, Labels=>True, Labels_Decimals=>0);
        Put_Figure_Axes(Plot, (I, J), Xaxis, Yaxis);
        Set_Color(Plot, Blue);
        Put_Figure_Title(Plot, (I, J), "Figure " & I2s(I) & "," & I2s(J));
        Set_Color(Plot, Green);
        Put_Figure_Text(Plot, (I, J), "r = " & F2ss(R) & " mm", (World_Pos.X, World_Max.Y));
        Set_Color(Plot, Teal);
        Put_Figure_Symbol(Plot, (I, J), Plus, World_Min, 5.0);
        Put_Figure_Symbol(Plot, (I, J), Plus, World_Max, 5.0);
        Set_Color(Plot, Red);
        Set_Line_Width(Plot, Float(I + J) / 3.0);
        Set_Dash_Pattern(Plot, Dotted_Line);
        Put_Figure_Symbol(Plot, (I, J), Circle, World_Pos, Mm_To_Pt(R), Line);
        Set_Color(Plot, Navy);
        Put_Figure_Symbol(Plot, (I, J), Splat, World_Pos, Mm_To_Pt(R) + 5.0);
        if I = 1 and J = 1 then
          Set_Dash_Pattern(Plot, Solid_Line);
          Pos := (8.0, 2.0);
          for E in Tline_End'Range loop
            Set_Color(Plot, Gray);
            Set_Line_Width(Plot, 6.0);
            Set_Line_End(Plot, E);
            Put_Figure_Symbol(Plot, (I, J), Line, Pos, Pos + (0.0, 6.0), Line);
            Set_Color(Plot, White);
            Set_Line_Width(Plot, 1.0);
            Set_Line_End(Plot, Default_Line_End);
            Put_Figure_Symbol(Plot, (I, J), Line, Pos, Pos + (0.0, 6.0), Line);
            Pos := Pos + (-1.0, 0.0);
          end loop;
        elsif I = 2 and J = 1 then
          Set_Dash_Pattern(Plot, Solid_Line);
          Set_Color(Plot, Gray);
          Set_Line_Width(Plot, 6.0);
          Corner := ((6.0, 0.0), (6.0, 2.0), (8.0, 0.0), (9.0, 0.0), (9.0, 2.0));
          for C in Tline_Corner'Range loop
            Set_Line_Corner(Plot, C);
            Put_Figure_Symbol(Plot, (I, J), Polyline, Corner, Line);
            for K in Corner'Range loop
              Corner(K) := Corner(K) + (0.0, 4.0);
            end loop;
          end loop;
          Set_Line_Width(Plot, 1.0);
        end if;
      end loop;
    end loop;
    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_02;


  procedure Eps_Plots_Test_03 is
    Eps_File: constant String := "eps_plots_test_03.eps";
    Plot: Teps_Plot;
    Inum: constant := 3;
    Jnum: constant := 2;
    World_Min: constant T2dim := (0.0, 0.0);
    World_Max: constant T2dim := (10.0, 10.0);
    World_Pos: constant T2dim := (3 * World_Min + World_Max) / 4;
    Pos: T2dim;
    R: Float;
    Xaxis, Yaxis: Taxis;
  begin
    Put_Line(Eps_File);
    Start_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Layout => (Inum, Jnum),
      Paper => Dina4,
      Orientation => Portrait,
      Shape => Squared,
      Paper_Margin => (100.0, 75.0),
      Plot_Ll_Margin => (40.0, 30.0),
      Plot_Ur_Margin => (40.0, 0.0),
      Separation => (0.0, 0.0),
      Top_Separation => True,
      Title_Height => 20.0,
      Color => Black,
      Font => (Helvetica, 10.0, False, False),
      Line_Width => 1.0,
      Dash_Pattern => Default_Dash_Pattern,
      Line_End => Default_Line_End,
      Line_Corner => Default_Line_Corner,
      Arrow_Head => Default_Arrow_Head,
      Aft => Default_Plot_Aft);

    Put_Bounding_Box(Plot);
    Put_Title(Plot, Change_Backslash(Eps_File));
    Set_Line_Width(Plot, 1.0);
    for I in 1..Inum loop
      for J in 1..Jnum loop
        Put_Figure_Frame(Plot, (I, J));
        Set_Figure_Scaling(Plot, (I, J), Xy_Independent, World_Min, World_Max);
      end loop;
    end loop;
    for I in 1..Inum loop
      for J in 1..Jnum loop
        R := 1.0;
        Set_Color(Plot, Black);
        if I = 3 then
          Set_Axis(Xaxis, Title=>"X axis", Title_Gap=>12.0, Ticks=>True, Ticks_Inc=>0.5,
                   Ticks_Group=>4, Labels=>True, Labels_Decimals=>0);
        else
          Set_Axis(Xaxis, Ticks=>True, Ticks_Inc=>0.5, Ticks_Group=>4);
        end if;
        if J = 1 then
          Set_Axis(Yaxis, Title=>"Y axis", Title_Gap=>12.0, Ticks=>True, Ticks_Inc=>0.5,
                   Ticks_Group=>4, Labels=>True, Labels_Decimals=>0);
        else
          Set_Axis(Yaxis, Ticks=>True, Ticks_Inc=>0.5, Ticks_Group=>4);
        end if;
        Put_Figure_Axes(Plot, (I, J), Xaxis, Yaxis);
        Set_Color(Plot, Green);
        Put_Figure_Text(Plot, (I, J), "r = " & F2ss(R) & " mm", (World_Pos.X, World_Max.Y));
        Set_Color(Plot, Red);
--        Put_Figure_Symbol(Plot, (I, J), Circle, World_Pos, Mm_To_Pt(R), Filled);
        Set_Color(Plot, Navy);
--        Put_Figure_Symbol(Plot, (I, J), Splat, World_Pos, Mm_To_Pt(R) + 5.0);
        Put_Figure_Symbol(Plot, (I, J), Plus, World_Min, 5.0);
        Put_Figure_Symbol(Plot, (I, J), Plus, World_Max, 5.0);
        Set_Color(Plot, Red);
        Pos := World_Min;
        for Symbol in Txyr_Symbol loop
          Pos := Pos + (World_Max - World_Min) / 11.0;
          Put_Figure_Symbol(Plot, (I, J), Symbol, Pos, 5.0);
        end loop;
        Set_Color(Plot, Aqua);
        Put_Figure_Symbol(Plot, (I, J), Ellipse, (2.0, 7.0), 20.0,  5.0, Filled);
        Set_Color(Plot, Blue);
        Put_Figure_Symbol(Plot, (I, J), Ellipse, (2.0, 7.0), 25.0, 10.0);
        Put_Figure_Symbol(Plot, (I, J), Ellipse, (2.0, 7.0), 30.0, 15.0);
        Set_Color(Plot, Aqua);
        Put_Figure_Symbol(Plot, (I, J), Ellipse, (7.0, 2.0),  5.0, 20.0, Filled);
        Set_Color(Plot, Blue);
        Put_Figure_Symbol(Plot, (I, J), Ellipse, (7.0, 2.0), 10.0, 25.0);
        Put_Figure_Symbol(Plot, (I, J), Ellipse, (7.0, 2.0), 15.0, 30.0);
      end loop;
    end loop;
    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_03;


  procedure Eps_Plots_Test_04 is
    Eps_File: constant String := "eps_plots_test_04.eps";
    Plot: Teps_Plot;
    Inum: constant := 1;
    Jnum: constant := 1;
    World_Min: constant T2dim := (0.0, 45.0);
    World_Max: constant T2dim := (21.0, 1.0);
    Xaxis, Yaxis: Taxis;
    I, J: Positive;
    X, Y: Float;
  begin
    Put_Line(Eps_File);
    Start_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Layout => (Inum, Jnum),
      Paper => Dina4,
      Orientation => Portrait,
      Shape => Autoshape,
      Paper_Margin => Default_Paper_Margin,
      Plot_Ll_Margin => (40.0, 40.0),
      Plot_Ur_Margin => (40.0, 0.0),
      Separation => Default_Separation,
      Top_Separation => False,
      Title_Height => Default_Title_Height,
      Color => Black,
      Font => (Helvetica, 10.0, False, False),
      Line_Width => 1.0,
      Dash_Pattern => Default_Dash_Pattern,
      Line_End => Default_Line_End,
      Line_Corner => Default_Line_Corner,
      Arrow_Head => Default_Arrow_Head,
      Aft => Default_Plot_Aft);

    Put_Bounding_Box(Plot);
    Put_Title(Plot, Change_Backslash(Eps_File));
    I := Inum;
    J := Jnum;
    Put_Figure_Frame(Plot, (I, J));
    Set_Figure_Scaling(Plot, (I, J), Xy_Independent, World_Min, World_Max);
    Set_Axis(Xaxis, Title=>"X axis", Title_Gap=>20.0, Ticks=>True, Ticks_Inc=>0.2,
             Ticks_Group=>5, Labels=>True, Labels_Gap=>5.0, Labels_Decimals=>0);
    Set_Axis(Yaxis, Title=>"Y axis", Title_Gap=>20.0, Ticks=>True, Ticks_Inc=>0.2,
             Ticks_Group=>5, Labels=>True, Labels_Gap=>5.0, Labels_Decimals=>0);
    Put_Figure_Axes(Plot, (I, J), Xaxis, Yaxis);

    X := 0.0; Y := 1.0;
    for Font_Name in Tfont_Name loop
      for Bold in Boolean loop
        for Italics in Boolean loop
          if Font_Name in Tfont_Name_Prefix or else not (Bold or Italics) then
            Set_Font(Plot, (Font_Name, 9.0, Bold, Italics));
            Put_Figure_Text(Plot, (I, J), Capitalize(Tfont_Name'Image(Font_Name)), (X, Y), Middle_Left);
            Y := Y + 1.0;
            if Y > 45.0 then
              X := X + 7.0;
              Y := 1.0;
            end if;
          end if;
        end loop;
      end loop;
    end loop;
    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_04;


  procedure Eps_Plots_Test_05 is
    Eps_File: constant String := "eps_plots_test_05.eps";
    Plot: Teps_Plot;
    Inum: constant := 11;
    Jnum: constant := 11;
    World_Min: constant T2dim := (0.0, 0.0);
    World_Max: constant T2dim := (1.0, 1.0);
    Fig_Margin: constant T2dim := (0.0, 0.0);
    F: Float;
    Xaxis, Yaxis: Taxis;
  begin
    Put_Line(Eps_File);
    Start_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Layout => (Inum, Jnum),
      Paper => Dina4,
      Orientation => Portrait,
      Shape => Squared,
      Paper_Margin => (100.0, 75.0),
      Plot_Ll_Margin => (40.0, 30.0),
      Plot_Ur_Margin => (40.0, 0.0),
      Separation => (0.0, 0.0),
      Top_Separation => True,
      Title_Height => 20.0,
      Color => Black,
      Font => (Helvetica, 10.0, False, False),
      Line_Width => 1.0,
      Dash_Pattern => Default_Dash_Pattern,
      Line_End => Default_Line_End,
      Line_Corner => Default_Line_Corner,
      Arrow_Head => Default_Arrow_Head,
      Aft => Default_Plot_Aft);

    Put_Bounding_Box(Plot);
    Put_Title(Plot, Change_Backslash(Eps_File));
    Set_Line_Width(Plot, 1.0);
    for I in 1..Inum loop
      for J in 1..Jnum loop
        Set_Figure_Scaling(Plot, (I, J), Xy_Independent, World_Min, World_Max, Fig_Margin);
      end loop;
    end loop;
    for I in 1..Inum loop
      for J in 1..Jnum loop
        Set_Color(Plot, Black);
        if I = Inum then
          Set_Axis(Xaxis, Title=>I2S(J), Title_Gap=>12.0, Ticks=>False);
        else
          Set_Axis(Xaxis, Ticks=>False);
        end if;
        if J = 1 then
          Set_Axis(Yaxis, Title=>I2S(Inum-I+1), Title_Gap=>12.0, Ticks=>False);
        else
          Set_Axis(Yaxis, Ticks=>False);
        end if;
        Put_Figure_Axes(Plot, (I, J), Xaxis, Yaxis);
        --  -1.0 <= F <= 1.0
        F := Float(I-1) / Float(Inum-1) - Float(J-1) / Float(Jnum-1);
        Set_Color(Plot, Float_To_Color(F, Blue_White_Red));
        Put_Figure_Symbol(Plot, (I, J), Rectangle, World_Min, World_Max, Filled);
      end loop;
    end loop;
    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_05;


  procedure Eps_Plots_Test_06 is
    Eps_File:  constant String := "eps_plots_test_06.eps";
    Size:      constant T2dim  := (Cm2Pt(10.0), Cm2Pt(10.0));
    World_Min: constant T2dim  := (0.0, 0.0);
    World_Max: constant T2dim  := (10.0, 10.0);
    Scale_Size: constant T2dim  := (0.5, 5.0);
    Scale_Reso: constant Positive := 200;
    Scale_Min : constant Float := 0.0;
    Scale_Max : constant Float := 1.0;
    Scale_Tics: constant Positive := 5;
    Scale_Sep : constant Float := 0.7;
    Fig: constant Tfigure_Index := Default_Figure;
    Scale_Pos: T2dim;
    Plot: Teps_Plot;
    C: Tcolor;
    F, X1, X2, Y1, Y2, DY: Float;
    P1, P2: T2dim;
    K: Integer;

  begin
    Put_Line(Eps_File);
    Start_Simple_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Size => Size,
      Min => World_Min,
      Max => World_Max);

    Set_Aft(Plot, 3);
    Scale_Pos := (1.0, 4.0);
    K := -1;
    for Cs in Tcolor_Scheme loop
      DY := Scale_Size.Y / Scale_Reso;
      Set_Line_Width(Plot, 0.5);
      X1 := Scale_Pos.X;
      X2 := X1 + Scale_Size.X;
      for I in 1..Scale_Reso loop
        Y1 := Scale_Pos.Y + (I-1) * DY;
        Y2 := Scale_Pos.Y + I * DY;
        F := Scale_Min + (Scale_Max - Scale_Min) * I2F(I - 1) / I2F(Scale_Reso - 1);
        C := Float_To_Color(F, Cs, Scale_Min, Scale_Max);
        Set_Color(Plot, C);
        P1 := (X1, Y1);
        P2 := (X2, Y2);
        Put_Figure_Symbol(Plot, Fig, Rectangle, P1, P2, Filled);
      end loop;
      Set_Color(Plot, Black);
      for I in 0..Scale_Tics loop
        F := Scale_Min + (Scale_Max - Scale_Min) * I2F(I) / I2F(Scale_Tics);
        Y1 := Scale_Pos.Y + I * Scale_Size.Y / Scale_Tics;
        P1 := ((X1 + X2) / 2.0 + K * Scale_Sep, Y1);
        Put_Figure_Text(Plot, Fig, F2Se0(F, Aft => 1), P1, Middle_Middle);
      end loop;
      P1 := ((X1 + X2) / 2.0, Scale_Pos.Y - Scale_Sep / 2.0);
      Put_Figure_Text(Plot, Fig, Capitalize(Tcolor_Scheme'Image(Cs)), P1, Middle_Left, -45.0);
      Scale_Pos := Scale_Pos + (2.0, 0.0);
      if K < 1 then
        K := K + 1;
      end if;
    end loop;

    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_06;


  procedure Eps_Plots_Test_07 is
    Eps_File:  constant String := "eps_plots_test_07.eps";
    Size:      constant T2dim  := (Cm2Pt(7.0), Cm2Pt(3.0));
    World_Min: constant T2dim  := (0.5, 0.5);
    World_Max: constant T2dim  := (7.5, 3.5);
    Arrow_Head: constant Tarrow_Head := (10.0, 14.0, 10.0);
    Fig: constant Tfigure_Index := Default_Figure;
    Plot: Teps_Plot;
    R: Float;
    Hexa: T2dims(1..6);

    procedure Put_Node(Pos: in T2dim) is
    begin
      R := Global_X_Distance(Plot, Fig, 0.15);
      Put_Figure_Symbol(Plot, Fig, Circle, Pos, R, Opaque);
    end Put_Node;

  begin
    Put_Line(Eps_File);
    Start_Simple_Plot(
      Plot => Plot,
      File_Name => Eps_File,
      Size => Size,
      Min => World_Min,
      Max => World_Max);

    Set_Aft(Plot, Default_Plot_Aft);
    Set_Color(Plot, Red);
    for I in Hexa'Range loop
      Hexa(I).X := 2.0 + 1.0 * Sin((Float(I) * Pi) / 3.0);
      Hexa(I).Y := 2.0 + 1.0 * Cos((Float(I) * Pi) / 3.0);
    end loop;
    Put_Figure_Symbol(Plot, Fig, Polyline, Hexa, Line);
    Set_Color(Plot, Black);

    Hexa(4).X := Hexa(4).X + 4.0;
    Hexa(5).X := Hexa(5).X + 4.0;
    Set_Dash_Pattern(Plot, Dashed_Short_Line);
    Put_Figure_Symbol(Plot, Fig, Line, Hexa(1), Hexa(5), Line);
    Put_Figure_Symbol(Plot, Fig, Line, Hexa(2), Hexa(4), Line);
    Set_Dash_Pattern(Plot, Solid_Line);
    Hexa(4).X := Hexa(4).X - 4.0;
    Hexa(5).X := Hexa(5).X - 4.0;

    Set_Color(Plot, Black);
    for I in Hexa'Range loop
      Put_Node(Hexa(I));
    end loop;
    Set_Arrow_Head(Plot, Arrow_Head);
    Put_Figure_Symbol(Plot, Fig, Arrow, Hexa(3), Hexa(6), Opaque);

    Set_Color(Plot, (0.8, 0.8, 0.8));
    for I in Hexa'Range loop
      Hexa(I).X := 6.0 + 1.0 * Sin((Float(I) * Pi) / 3.0);
      Hexa(I).Y := 2.0 + 1.0 * Cos((Float(I) * Pi) / 3.0);
    end loop;
    Put_Figure_Symbol(Plot, Fig, Polygon, Hexa, Filled);
    Set_Color(Plot, Black);
    for I in Hexa'First..(Hexa'Last - 1) loop
      for J in (I + 1)..Hexa'Last loop
        Put_Figure_Symbol(Plot, Fig, Line, Hexa(I), Hexa(J), Line);
      end loop;
    end loop;
    Set_Color(Plot, Black);
    for I in Hexa'Range loop
      Put_Node(Hexa(I));
    end loop;

    End_Plot(Plot);
  exception
    when Eps_Plot_Error =>
      Put_Line("Exception Eps_Plot_Error raised!");
      End_Plot(Plot);
    when E: others =>
      Put("Exception raised: ");
      Put_Line(Exception_Name(E));
      Put_Line(Exception_Message(E));
  end Eps_Plots_Test_07;


begin
  Eps_Plots_Test_01;
  Eps_Plots_Test_02;
  Eps_Plots_Test_03;
  Eps_Plots_Test_04;
  Eps_Plots_Test_05;
  Eps_Plots_Test_06;
  Eps_Plots_Test_07;
end Eps_Plots_Test;
