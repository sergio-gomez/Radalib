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


-- @filename Eps_Plots.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 04/04/2002
-- @revision 19/01/2015
-- @brief Generation of Eps Plots

with Ada.Unchecked_Deallocation;
with Eps_Plots.Internals; use Eps_Plots.Internals;

package body Eps_Plots is

  ----------
  -- Free --
  ----------

  procedure Free is new Ada.Unchecked_Deallocation(Tfigures_Array, Pfigures_Array);

  ----------------
  -- Start_Plot --
  ----------------

  procedure Start_Plot(
    Plot: out Teps_Plot;
    File_Name: in String;
    Layout: in Tfigure_Index := Default_Layout;
    Paper: in T2dim := Default_Paper;
    Orientation: in Torientation := Default_Orientation;
    Shape: in Tshape := Default_Shape;
    Paper_Margin: in T2dim := Default_Paper_Margin;
    Plot_Ll_Margin: in T2dim := Default_Plot_Ll_Margin;
    Plot_Ur_Margin: in T2dim := Default_Plot_Ur_Margin;
    Separation: in T2dim := Default_Separation;
    Top_Separation: in Boolean := Default_Top_Separation;
    Title_Height: in Float := Default_Title_Height;
    Color: in Tcolor := Default_Color;
    Font: in Tfont := Default_Font;
    Line_Width: in Float := Default_Line_Width;
    Dash_Pattern: in Tdash_Pattern := Default_Dash_Pattern;
    Line_End: in Tline_End := Default_Line_End;
    Line_Corner: in Tline_Corner := Default_Line_Corner;
    Arrow_Head: in Tarrow_Head := Default_Arrow_Head;
    Aft: in Field := Default_Plot_Aft)
  is
  begin
    Create(Plot.File, Out_File, File_Name);
    Plot.Ori_Num := Layout;
    Plot.Ori_Paper := Paper;
    Plot.Ori_Orientation := Orientation;
    Plot.Ori_Shape := Shape;
    Plot.Ori_Margin := Paper_Margin;
    Plot.Ori_Plot_Ll_Margin := Plot_Ll_Margin;
    Plot.Ori_Plot_Ur_Margin := Plot_Ur_Margin;
    Plot.Ori_Separation := Separation;
    Plot.Ori_Top_Separation := Top_Separation;
    Plot.Ori_Title_Height := Title_Height;
    Plot.Ori_Color := Color;
    Plot.Ori_Font := Font;
    Plot.Ori_Line_Width := Line_Width;
    Plot.Ori_Dash_Pattern := Dash_Pattern;
    Plot.Ori_Line_End := Line_End;
    Plot.Ori_Line_Corner := Line_Corner;
    Plot.Ori_Arrow_Head := Arrow_Head;
    Plot.Ori_Aft := Aft;
    Recalculate_Plot(Plot);
    Put_Postscript_Header(Plot, File_Name);
    Put_Postscript_Definitions(Plot.File);
    if Orientation = Landscape then
      Change_Orientation(Plot);
    end if;
    Put_Color(Plot, Color);
    Put_Font(Plot, Font);
    Put_Line_Width(Plot, Line_Width);
    Put_Dash_Pattern(Plot, Dash_Pattern);
    Put_Line_End(Plot, Line_End);
    Put_Line_Corner(Plot, Line_Corner);
    Put_Arrow_Head(Plot, Arrow_Head);
  end Start_Plot;

  ----------------
  -- Start_Plot --
  ----------------

  procedure Start_Simple_Plot(
    Plot: out Teps_Plot;
    File_Name: in String;
    Size: in T2dim;
    Min: in T2dim := Default_Min;
    Max: in T2dim := Default_Max)
  is
  begin
    Start_Plot(
      Plot           => Plot,
      File_Name      => File_Name,
      Layout         => (1, 1),
      Paper          => Dina4,
      Orientation    => Portrait,
      Shape          => Autoshape,
      Paper_Margin   => (Dina4 - Size) / 2.0,
      Plot_Ll_Margin => (0.0, 0.0),
      Plot_Ur_Margin => (0.0, 0.0),
      Separation     => (0.0, 0.0),
      Top_Separation => False,
      Title_Height   => 0.0);

    Set_Figure_Scaling(
      Plot    => Plot,
      Figure  => (1, 1),
      Scaling => Xy_Independent,
      Min     => Min,
      Max     => Max,
      Margin  => (0.0, 0.0));
  end Start_Simple_Plot;

  --------------
  -- End_Plot --
  --------------

  procedure End_Plot(
    Plot: in out Teps_Plot)
  is
  begin
    Put_Postscript_Trailer(Plot.File);
    Close(Plot.File);
    Free(Plot.Figure);
    Plot.Ori_Paper := Default_Paper;
    Plot.Ori_Orientation := Default_Orientation;
    Plot.Ori_Shape := Default_Shape;
    Plot.Ori_Margin := Default_Paper_Margin;
    Plot.Ori_Plot_Ll_Margin := Default_Plot_Ll_Margin;
    Plot.Ori_Plot_Ur_Margin := Default_Plot_Ur_Margin;
    Plot.Ori_Separation := Default_Separation;
    Plot.Ori_Top_Separation := Default_Top_Separation;
    Plot.Ori_Title_Height := Default_Title_Height;
    Plot.Ori_Color := Default_Color;
    Plot.Ori_Font := Default_Font;
    Plot.Ori_Line_Width := Default_Line_Width;
    Plot.Ori_Dash_Pattern := Default_Dash_Pattern;
    Plot.Ori_Line_End := Default_Line_End;
    Plot.Ori_Line_Corner := Default_Line_Corner;
    Plot.Ori_Arrow_Head := Default_Arrow_Head;
    Plot.Ori_Aft := Default_Plot_Aft;
    Plot.Paper := Default_Paper;
    Plot.Orientation := Default_Orientation;
    Plot.Shape := Default_Shape;
    Plot.Margin := Default_Paper_Margin;
    Plot.Plot_Ll_Margin := Default_Plot_Ll_Margin;
    Plot.Plot_Ur_Margin := Default_Plot_Ur_Margin;
    Plot.Separation := Default_Separation;
    Plot.Top_Separation := Default_Top_Separation;
    Plot.Title_Height := Default_Title_Height;
    Plot.Color := Default_Color;
    Plot.Font := Default_Font;
    Plot.Line_Width := Default_Line_Width;
    Plot.Dash_Pattern := Default_Dash_Pattern;
    Plot.Line_End := Default_Line_End;
    Plot.Line_Corner := Default_Line_Corner;
    Plot.Arrow_Head := Default_Arrow_Head;
    Plot.Aft := Default_Plot_Aft;
    Plot.Figure := null;
  end End_Plot;

  ----------------
  -- Set_Layout --
  ----------------

  procedure Set_Layout(
    Plot: in out Teps_Plot;
    Layout: in Tfigure_Index := Default_Layout)
  is
  begin
    if Layout /= Plot.Ori_Num then
      Plot.Ori_Num := Layout;
      Recalculate_Plot(Plot);
    end if;
  end Set_Layout;

  ---------------
  -- Set_Paper --
  ---------------

  procedure Set_Paper(
    Plot: in out Teps_Plot;
    Paper: in T2dim := Default_Paper)
  is
  begin
    if Paper = Zero_2dim then
      raise Eps_Plot_Error;
    end if;
    if Paper /= Plot.Ori_Paper then
      Plot.Ori_Paper := Paper;
      Recalculate_Plot(Plot);
    end if;
  end Set_Paper;

  ---------------------
  -- Set_Orientation --
  ---------------------

  procedure Set_Orientation(
    Plot: in out Teps_Plot;
    Orientation: in Torientation := Default_Orientation)
  is
  begin
    if Orientation /= Plot.Ori_Orientation then
      Plot.Ori_Orientation := Orientation;
      Recalculate_Plot(Plot);
      Change_Orientation(Plot);
    end if;
  end Set_Orientation;

  ---------------
  -- Set_Shape --
  ---------------

  procedure Set_Shape(
    Plot: in out Teps_Plot;
    Shape: in Tshape := Default_Shape)
  is
  begin
    if Shape /= Plot.Ori_Shape then
      Plot.Ori_Shape := Shape;
      Recalculate_Plot(Plot);
    end if;
  end Set_Shape;

  ----------------------
  -- Set_Paper_Margin --
  ----------------------

  procedure Set_Paper_Margin(
    Plot: in out Teps_Plot;
    Paper_Margin: in T2dim := Default_Paper_Margin)
  is
  begin
    if Paper_Margin /= Plot.Ori_Margin then
      Plot.Ori_Margin := Paper_Margin;
      Recalculate_Plot(Plot);
    end if;
  end Set_Paper_Margin;

  ------------------------
  -- Set_Plot_Ll_Margin --
  ------------------------

  procedure Set_Plot_Ll_Margin(
    Plot: in out Teps_Plot;
    Plot_Ll_Margin: in T2dim := Default_Plot_Ll_Margin) is
  begin
    if Plot_Ll_Margin /= Plot.Ori_Plot_Ll_Margin then
      Plot.Ori_Plot_Ll_Margin := Plot_Ll_Margin;
      Recalculate_Plot(Plot);
    end if;
  end Set_Plot_Ll_Margin;

  ------------------------
  -- Set_Plot_Ur_Margin --
  ------------------------

  procedure Set_Plot_Ur_Margin(
    Plot: in out Teps_Plot;
    Plot_Ur_Margin: in T2dim := Default_Plot_Ur_Margin) is
  begin
    if Plot_Ur_Margin /= Plot.Ori_Plot_Ur_Margin then
      Plot.Ori_Plot_Ur_Margin := Plot_Ur_Margin;
      Recalculate_Plot(Plot);
    end if;
  end Set_Plot_Ur_Margin;

  --------------------
  -- Set_Separation --
  --------------------

  procedure Set_Separation(
    Plot: in out Teps_Plot;
    Separation: in T2dim := Default_Separation)
  is
  begin
    if Separation /= Plot.Ori_Separation then
      Plot.Ori_Separation := Separation;
      Recalculate_Plot(Plot);
    end if;
  end Set_Separation;

  ------------------------
  -- Set_Top_Separation --
  ------------------------

  procedure Set_Top_Separation(
    Plot: in out Teps_Plot;
    Top_Separation: in Boolean := Default_Top_Separation)
  is
  begin
    if Top_Separation /= Plot.Ori_Top_Separation then
      Plot.Ori_Top_Separation := Top_Separation;
      Recalculate_Plot(Plot);
    end if;
  end Set_Top_Separation;

  ----------------------
  -- Set_Title_Height --
  ----------------------

  procedure Set_Title_Height(
    Plot: in out Teps_Plot;
    Title_Height: in Float := Default_Title_Height)
  is
  begin
    if Title_Height /= Plot.Ori_Title_Height then
      Plot.Ori_Title_Height := Title_Height;
      Recalculate_Plot(Plot);
    end if;
  end Set_Title_Height;

  ---------------
  -- Set_Color --
  ---------------

  procedure Set_Color(
    Plot: in out Teps_Plot;
    Color: in Tcolor := Default_Color)
  is
  begin
    if Color /= Plot.Ori_Color then
      Plot.Ori_Color := Color;
      Recalculate_Plot(Plot);
      Put_Color(Plot, Color);
    end if;
  end Set_Color;

  --------------
  -- Set_Font --
  --------------

  procedure Set_Font(
    Plot: in out Teps_Plot;
    Font: in Tfont := Default_Font)
  is
  begin
    if Font /= Plot.Ori_Font then
      Plot.Ori_Font := Font;
      Recalculate_Plot(Plot);
      Put_Font(Plot, Font);
    end if;
  end Set_Font;

  --------------------
  -- Set_Line_Width --
  --------------------

  procedure Set_Line_Width(
    Plot: in out Teps_Plot;
    Line_Width: in Float := Default_Line_Width)
  is
  begin
    if Line_Width /= Plot.Ori_Line_Width then
      Plot.Ori_Line_Width := Line_Width;
      Recalculate_Plot(Plot);
      Put_Line_Width(Plot, Line_Width);
    end if;
  end Set_Line_Width;

  ----------------------
  -- Set_Dash_Pattern --
  ----------------------

  procedure Set_Dash_Pattern(
    Plot: in out Teps_Plot;
    Dash_Pattern: in Tdash_Pattern := Default_Dash_Pattern)
  is
  begin
    if Dash_Pattern /= Plot.Ori_Dash_Pattern then
      Plot.Ori_Dash_Pattern := Dash_Pattern;
      Recalculate_Plot(Plot);
      Put_Dash_Pattern(Plot, Dash_Pattern);
    end if;
  end Set_Dash_Pattern;

  ------------------
  -- Set_Line_End --
  ------------------

  procedure Set_Line_End(
    Plot: in out Teps_Plot;
    Line_End: in Tline_End := Default_Line_End)
  is
  begin
    if Line_End /= Plot.Ori_Line_End then
      Plot.Ori_Line_End := Line_End;
      Recalculate_Plot(Plot);
      Put_Line_End(Plot, Line_End);
    end if;
  end Set_Line_End;

  ---------------------
  -- Set_Line_Corner --
  ---------------------

  procedure Set_Line_Corner(
    Plot: in out Teps_Plot;
    Line_Corner: in Tline_Corner := Default_Line_Corner)
  is
  begin
    if Line_Corner /= Plot.Ori_Line_Corner then
      Plot.Ori_Line_Corner := Line_Corner;
      Recalculate_Plot(Plot);
      Put_Line_Corner(Plot, Line_Corner);
    end if;
  end Set_Line_Corner;

  --------------------
  -- Set_Arrow_Head --
  --------------------

  procedure Set_Arrow_Head(
    Plot: in out Teps_Plot;
    Arrow_Head: in Tarrow_Head := Default_Arrow_Head)
  is
  begin
    if Arrow_Head /= Plot.Ori_Arrow_Head then
      Plot.Ori_Arrow_Head := Arrow_Head;
      Recalculate_Plot(Plot);
      Put_Arrow_Head(Plot, Arrow_Head);
    end if;
  end Set_Arrow_Head;

  -------------
  -- Set_Aft --
  -------------

  procedure Set_Aft(
    Plot: in out Teps_Plot;
    Aft: in Field := Default_Plot_Aft)
  is
  begin
    if Aft /= Plot.Ori_Aft then
      Plot.Ori_Aft := Aft;
      Recalculate_Plot(Plot);
    end if;
  end Set_Aft;

  ----------------------
  -- Put_Bounding_Box --
  ----------------------

  procedure Put_Bounding_Box(
    Plot: in out Teps_Plot)
  is
    BB_Width: constant := 0.3;
    Ll, Ur: T2dim;
  begin
    Get_Bounding_Box(Plot, Ll, Ur);
    Put_Line(Plot.File, "gsave");
    Put_Line_Width(Plot, BB_Width);
    Put_Dash_Pattern(Plot, Dashed_Line);
    Put_Line_Corner(Plot, Miter);
    Put_Symbol(Plot, Rectangle, Ll, Ur, Line);
    Put_Line(Plot.File, "grestore");
  end Put_Bounding_Box;

  ---------------
  -- Put_Title --
  ---------------

  procedure Put_Title(
    Plot: in out Teps_Plot;
    Title: in String;
    Font: in Tfont := Default_Title_Font)
  is
    X, Y: Float;
  begin
    X := Plot.Paper.X / 2.0;
    Y := Plot.Paper.Y - Plot.Margin.Y - Plot.Title_Height / 2.0;
    Put_Line(Plot.File, "gsave");
    Put_Font(Plot, Font);
    Put_Text(Plot, Title, (X, Y), Middle_Middle);
    Put_Line(Plot.File, "grestore");
  end Put_Title;

  --------------
  -- Put_Text --
  --------------

  procedure Put_Text(
    Plot: in out Teps_Plot;
    Text: in String;
    Position: in T2dim;
    Justify: in Tjustify := Default_Justify;
    Angle: in Float := Default_Angle)
  is
  begin
    Put(Plot.File, F2ss(Position.X, Position.Y, Aft => Plot.Aft));
    if Angle /= 0.0 then
      Put(Plot.File, " " & F2ss(Angle, Aft => Plot.Aft));
    end if;
    Put(Plot.File, " (" & Text & ") ss");
    if Angle /= 0.0 then
      Put(Plot.File, "a");
    end if;
    case Justify is
      when Lower_Left    => Put(Plot.File, "ll");
      when Lower_Middle  => Put(Plot.File, "lm");
      when Lower_Right   => Put(Plot.File, "lr");
      when Middle_Left   => Put(Plot.File, "ml");
      when Middle_Middle => Put(Plot.File, "mm");
      when Middle_Right  => Put(Plot.File, "mr");
      when Upper_Left    => Put(Plot.File, "ul");
      when Upper_Middle  => Put(Plot.File, "um");
      when Upper_Right   => Put(Plot.File, "ur");
      when Bottom_Left   => Put(Plot.File, "bl");
      when Bottom_Middle => Put(Plot.File, "bm");
      when Bottom_Right  => Put(Plot.File, "br");
      when Center_Left   => Put(Plot.File, "cl");
      when Center_Middle => Put(Plot.File, "cm");
      when Center_Right  => Put(Plot.File, "cr");
      when Top_Left      => Put(Plot.File, "tl");
      when Top_Middle    => Put(Plot.File, "tm");
      when Top_Right     => Put(Plot.File, "tr");
    end case;
    New_Line(Plot.File);
  end Put_Text;

  ----------------
  -- Put_Symbol --
  ----------------

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txyr_Symbol;
    Center: in T2dim;
    Radius: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
  begin
    Put(Plot.File, F2ss(Center.X, Center.Y, Radius, Aft => Plot.Aft));
    case Style is
      when Line   => Put(Plot.File, " d");
      when Filled => Put(Plot.File, " f");
      when Opaque => Put(Plot.File, " o");
    end case;
    case Symbol is
      when Circle     => Put_Line(Plot.File, "ci");
      when Square     => Put_Line(Plot.File, "sq");
      when Triangle_N => Put_Line(Plot.File, "tn");
      when Triangle_W => Put_Line(Plot.File, "tw");
      when Triangle_S => Put_Line(Plot.File, "ts");
      when Triangle_E => Put_Line(Plot.File, "te");
      when Diamond    => Put_Line(Plot.File, "di");
      when Plus       => Put_Line(Plot.File, "pl");
      when Times      => Put_Line(Plot.File, "xx");
      when Splat      => Put_Line(Plot.File, "sp");
    end case;
  end Put_Symbol;

  ----------------
  -- Put_Symbol --
  ----------------

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txyrr_Symbol;
    Center: in T2dim;
    Radius_X: in Float;
    Radius_Y: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
  begin
    Put(Plot.File, F2ss(Center.X, Center.Y, Radius_X, Radius_Y, Aft => Plot.Aft));
    case Style is
      when Line   => Put(Plot.File, " d");
      when Filled => Put(Plot.File, " f");
      when Opaque => Put(Plot.File, " o");
    end case;
    case Symbol is
      when Ellipse => Put_Line(Plot.File, "el");
    end case;
  end Put_Symbol;

  ----------------
  -- Put_Symbol --
  ----------------

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txyxy_Symbol;
    Ll: in T2dim;
    Ur: in T2dim;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
  begin
    Put(Plot.File, F2ss(Ll.X, Ll.Y, Ur.X, Ur.Y, Aft => Plot.Aft));
    case Style is
      when Line   => Put(Plot.File, " d");
      when Filled => Put(Plot.File, " f");
      when Opaque => Put(Plot.File, " o");
    end case;
    case Symbol is
      when Line      => Put_Line(Plot.File, "li");
      when Arrow     => Put_Line(Plot.File, "ar");
      when Rectangle => Put_Line(Plot.File, "rc");
    end case;
  end Put_Symbol;

  ----------------
  -- Put_Symbol --
  ----------------

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txys_Symbol;
    Vertices: in T2dims;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
  begin
    if Vertices'Length >= 2 then
      Put_Line(Plot.File, F2ss(Vertices(Vertices'First).X, Vertices(Vertices'First).Y, Aft => Plot.Aft) & " m");
      for I in (Vertices'First + 1)..Vertices'Last loop
        Put_Line(Plot.File, F2ss(Vertices(I).X, Vertices(I).Y, Aft => Plot.Aft) & " l");
      end loop;
      case Symbol is
        when Polyline => null;
        when Polygon  => Put(Plot.File, "closepath ");
      end case;
      case Style is
        when Line   => Put_Line(Plot.File, "s");
        when Filled => Put_Line(Plot.File, "f");
        when Opaque => Put_Line(Plot.File, "o");
      end case;
    end if;
  end Put_Symbol;

  --------------------
  -- Put_Postscript --
  --------------------

  procedure Put_Postscript(
    Plot: in out Teps_Plot;
    Text: in String)
  is
  begin
    Put_Line(Plot.File, Text);
  end Put_Postscript;

  ------------------------
  -- Set_Figure_Scaling --
  ------------------------

  procedure Set_Figure_Scaling(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Scaling: in Tscaling := Default_Scaling;
    Min: in T2dim := Default_Min;
    Max: in T2dim := Default_Max;
    Margin: in T2dim := Default_Figure_Margin)
  is
    Fig: Peps_Figure;
  begin
    if Min = Max then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    Fig.Ori_Scaling := Scaling;
    Fig.Ori_Min := Min;
    Fig.Ori_Max := Max;
    Fig.Ori_Margin := Margin;
    Recalculate_Figure(Fig.all);
  end Set_Figure_Scaling;

  ------------------------
  -- Global_Coordinates --
  ------------------------

  function Global_Coordinates(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Position: in T2dim) return T2dim
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    return Fig.Offset + (Position - Fig.Min) * Fig.Ratio;
  end Global_Coordinates;

  -------------------------
  -- Global_X_Coordinate --
  -------------------------

  function Global_X_Coordinate(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    X_Position: in Float) return Float
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    return Fig.Offset.X + (X_Position - Fig.Min.X) * Fig.Ratio.X;
  end Global_X_Coordinate;

  -------------------------
  -- Global_Y_Coordinate --
  -------------------------

  function Global_Y_Coordinate(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Y_Position: in Float) return Float
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    return Fig.Offset.Y + (Y_Position - Fig.Min.Y) * Fig.Ratio.Y;
  end Global_Y_Coordinate;

  ----------------------
  -- Global_Distances --
  ----------------------

  function Global_Distances(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Distance: in T2dim) return T2dim
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    return Distance * Fig.Ratio;
  end Global_Distances;

  -----------------------
  -- Global_X_Distance --
  -----------------------

  function Global_X_Distance(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    X_Distance: in Float) return Float
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    return X_Distance * Fig.Ratio.X;
  end Global_X_Distance;

  -----------------------
  -- Global_Y_Distance --
  -----------------------

  function Global_Y_Distance(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Y_Distance: in Float) return Float
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    return Y_Distance * Fig.Ratio.Y;
  end Global_Y_Distance;

  ----------------------
  -- Put_Figure_Frame --
  ----------------------

  procedure Put_Figure_Frame(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure)
  is
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    Put_Line(Plot.File, F2ss(Fig.Ll.X, Fig.Ll.Y, Fig.Ur.X, Fig.Ur.Y, Aft => Plot.Aft) & " drc");
  end Put_Figure_Frame;

  ---------------------
  -- Put_Figure_Axes --
  ---------------------

  procedure Put_Figure_Axes(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    X_Axis: in Taxis;
    Y_Axis: in Taxis)
  is
    X, Y, Inc, Size, Max, Gaps: Float;
    Xy1, Xy2: T2dim;
    Fig: Peps_Figure;
    N: Natural;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;

    -- X Ticks and Labels
    if X_Axis.Ticks or X_Axis.Labels then
      Put_Line(Plot.File, "gsave");
      Put_Font(Plot, X_Axis.Labels_Font);
      Inc := X_Axis.Ticks_Inc;
      if Inc * (Fig.Max.X - Fig.Min.X) < 0.0 then
        Inc := -Inc;
      end if;
      Gaps := (Fig.Max.X - Fig.Min.X) / Inc;
      Max := Fig.Max.X + Inc / 100.0;
      N := 0;
      X := Fig.Min.X;
      while (Inc > 0.0 and X < Max) or (Inc < 0.0 and X > Max) loop
        Xy1 := (Global_X_Coordinate(Plot, Figure, X), Fig.Ll.Y);
        if N mod X_Axis.Ticks_Group = 0 then
          Size := X_Axis.Ticks_Size;
          if X_Axis.Labels then
            Xy2 := (Xy1.X, Xy1.Y - X_Axis.Labels_Gap);
            Put_Text(Plot, F2ss(X, Aft => X_Axis.Labels_Decimals), Xy2, Upper_Middle);
          end if;
        else
          Size := X_Axis.Ticks_Size / 2.0;
        end if;
        Xy2 := (Xy1.X, Xy1.Y + Size);
        if X_Axis.Ticks then
          Put_Symbol(Plot, Line, Xy1, Xy2);
        end if;
        N := N + 1;
        X := Fig.Min.X + N * (Fig.Max.X - Fig.Min.X) / Gaps;
      end loop;
      Put_Line(Plot.File, "grestore");
    end if;

    -- X Title
    if Length(X_Axis.Title) > 0 then
      Put_Line(Plot.File, "gsave");
      Put_Font(Plot, X_Axis.Title_Font);
      Xy1 := ((Fig.Ll.X + Fig.Ur.X) / 2.0, Fig.Ll.Y - X_Axis.Title_Gap);
      Put_Text(Plot, To_String(X_Axis.Title), Xy1, Top_Middle);
      Put_Line(Plot.File, "grestore");
    end if;

    -- Y Ticks and Labels
    if Y_Axis.Ticks or Y_Axis.Labels then
      Put_Line(Plot.File, "gsave");
      Put_Font(Plot, Y_Axis.Labels_Font);
      Inc := Y_Axis.Ticks_Inc;
      if Inc * (Fig.Max.Y - Fig.Min.Y) < 0.0 then
        Inc := -Inc;
      end if;
      Gaps := (Fig.Max.Y - Fig.Min.Y) / Inc;
      Max := Fig.Max.Y + Inc / 100.0;
      N := 0;
      Y := Fig.Min.Y;
      while (Inc > 0.0 and Y < Max) or (Inc < 0.0 and Y > Max) loop
        Xy1 := (Fig.Ll.X, Global_Y_Coordinate(Plot, Figure, Y));
        if N mod Y_Axis.Ticks_Group = 0 then
          Size := Y_Axis.Ticks_Size;
          if Y_Axis.Labels then
            Xy2 := (Xy1.X - Y_Axis.Labels_Gap, Xy1.Y);
            Put_Text(Plot, F2ss(Y, Aft => Y_Axis.Labels_Decimals), Xy2, Middle_Right);
          end if;
        else
          Size := Y_Axis.Ticks_Size / 2.0;
        end if;
        Xy2 := (Xy1.X + Size, Xy1.Y);
        if Y_Axis.Ticks then
          Put_Symbol(Plot, Line, Xy1, Xy2);
        end if;
        N := N + 1;
        Y := Fig.Min.Y + N * (Fig.Max.Y - Fig.Min.Y) / Gaps;
      end loop;
      Put_Line(Plot.File, "grestore");
    end if;

    -- Y Title
    if Length(Y_Axis.Title) > 0 then
      Put_Line(Plot.File, "gsave");
      Put_Font(Plot, Y_Axis.Title_Font);
      Xy1 := (Fig.Ll.X - Y_Axis.Title_Gap, (Fig.Ll.Y + Fig.Ur.Y) / 2.0);
      Put_Text(Plot, To_String(Y_Axis.Title), Xy1, Bottom_Middle, 90.0);
      Put_Line(Plot.File, "grestore");
    end if;
  end Put_Figure_Axes;

  ----------------------
  -- Put_Figure_Title --
  ----------------------

  procedure Put_Figure_Title(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Title: in String;
    Gap: in Float := Default_Figure_Title_Gap)
  is
    X, Y: Float;
    Fig: Peps_Figure;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Fig := Plot.Figure(Figure.I, Figure.J)'access;
    X := Fig.Ll.X + Plot.Side.X / 2.0;
    Y := Fig.Ur.Y + Gap;
    Put_Text(Plot, Title, (X, Y), Bottom_Middle);
  end Put_Figure_Title;

  ---------------------
  -- Put_Figure_Text --
  ---------------------

  procedure Put_Figure_Text(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Text: in String;
    Position: in T2dim;
    Justify: in Tjustify := Default_Justify;
    Angle: in Float := Default_Angle)
  is
    Pos: T2dim;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Pos := Global_Coordinates(Plot, Figure, Position);
    Put_Text(Plot, Text, Pos, Justify, Angle);
  end Put_Figure_Text;

  -----------------------
  -- Put_Figure_Symbol --
  -----------------------

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txyr_Symbol;
    Center: in T2dim;
    Radius: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
    Pos: T2dim;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Pos := Global_Coordinates(Plot, Figure, Center);
    Put_Symbol(Plot, Symbol, Pos, Radius, Style);
  end Put_Figure_Symbol;

  -----------------------
  -- Put_Figure_Symbol --
  -----------------------

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txyrr_Symbol;
    Center: in T2dim;
    Radius_X: in Float;
    Radius_Y: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
    Pos: T2dim;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Pos := Global_Coordinates(Plot, Figure, Center);
    Put_Symbol(Plot, Symbol, Pos, Radius_X, Radius_Y, Style);
  end Put_Figure_Symbol;

  -----------------------
  -- Put_Figure_Symbol --
  -----------------------

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txyxy_Symbol;
    Ll: in T2dim;
    Ur: in T2dim;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
    Xy1, Xy2: T2dim;
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    Xy1 := Global_Coordinates(Plot, Figure, Ll);
    Xy2 := Global_Coordinates(Plot, Figure, Ur);
    Put_Symbol(Plot, Symbol, Xy1, Xy2, Style);
  end Put_Figure_Symbol;

  -----------------------
  -- Put_Figure_Symbol --
  -----------------------

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txys_Symbol;
    Vertices: in T2dims;
    Style: in Tsymbol_Style := Default_Symbol_Style)
  is
    Xy: T2dims(Vertices'Range);
  begin
    if not Figure_In_Plot(Plot, Figure) then
      raise Eps_Plot_Error;
    end if;
    for I in Vertices'Range loop
      Xy(I) := Global_Coordinates(Plot, Figure, Vertices(I));
    end loop;
    Put_Symbol(Plot, Symbol, Xy, Style);
  end Put_Figure_Symbol;

  --------------
  -- Set_Axis --
  --------------

  procedure Set_Axis(
    Axis: in out Taxis;
    Title: in String := Default_Axis_Title;
    Title_Font: in Tfont := Default_Axis_Title_Font;
    Title_Gap: in Float := Default_Axis_Title_Gap;
    Ticks: in Boolean := Default_Axis_Ticks;
    Ticks_Inc: in Float := Default_Axis_Ticks_Inc;
    Ticks_Group: in Positive := Default_Axis_Ticks_Group;
    Ticks_Size: in Float := Default_Axis_Ticks_Size;
    Labels: in Boolean := Default_Axis_Labels;
    Labels_Font: in Tfont := Default_Axis_Labels_Font;
    Labels_Gap: in Float := Default_Axis_Labels_Gap;
    Labels_Decimals: in Natural := Default_Axis_Labels_Decimals)
  is
  begin
    Axis.Title := To_Str(Title);
    Axis.Title_Font := Title_Font;
    Axis.Title_Gap := Title_Gap;
    Axis.Ticks := Ticks;
    Axis.Ticks_Inc := Ticks_Inc;
    Axis.Ticks_Group := Ticks_Group;
    Axis.Ticks_Size := Ticks_Size;
    Axis.Labels := Labels;
    Axis.Labels_Font := Labels_Font;
    Axis.Labels_Gap := Labels_Gap;
    Axis.Labels_Decimals := Labels_Decimals;
    if Ticks_Inc = 0.0 then
      raise Eps_Plot_Error;
    end if;
  end Set_Axis;

  --------------------
  -- Set_Axis_Title --
  --------------------

  procedure Set_Axis_Title(
    Axis: in out Taxis;
    Title: in String := Default_Axis_Title;
    Title_Font: in Tfont := Default_Axis_Title_Font;
    Title_Gap: in Float := Default_Axis_Title_Gap)
  is
  begin
    Axis.Title := To_Str(Title);
    Axis.Title_Font := Title_Font;
    Axis.Title_Gap := Title_Gap;
  end Set_Axis_Title;

  --------------------
  -- Set_Axis_Ticks --
  --------------------

  procedure Set_Axis_Ticks(
    Axis: in out Taxis;
    Ticks: in Boolean := Default_Axis_Ticks;
    Ticks_Inc: in Float := Default_Axis_Ticks_Inc;
    Ticks_Group: in Positive := Default_Axis_Ticks_Group;
    Ticks_Size: in Float := Default_Axis_Ticks_Size)
  is
  begin
    Axis.Ticks := Ticks;
    Axis.Ticks_Inc := Ticks_Inc;
    Axis.Ticks_Group := Ticks_Group;
    Axis.Ticks_Size := Ticks_Size;
  end Set_Axis_Ticks;

  ---------------------
  -- Set_Axis_Labels --
  ---------------------

  procedure Set_Axis_Labels(
    Axis: in out Taxis;
    Labels: in Boolean := Default_Axis_Labels;
    Labels_Font: in Tfont := Default_Axis_Labels_Font;
    Labels_Gap: in Float := Default_Axis_Labels_Gap;
    Labels_Decimals: in Natural := Default_Axis_Labels_Decimals)
  is
  begin
    Axis.Labels := Labels;
    Axis.Labels_Font := Labels_Font;
    Axis.Labels_Gap := Labels_Gap;
    Axis.Labels_Decimals := Labels_Decimals;
  end Set_Axis_Labels;

end Eps_Plots;
