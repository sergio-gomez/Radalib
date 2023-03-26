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


-- @filename Eps_Plots.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 04/04/2002
-- @revision 19/01/2015
-- @brief Generation of Eps Plots

with Ada.Text_Io; use Ada.Text_Io;
with Eps_Utils; use Eps_Utils;
with Utils; use Utils;

package Eps_Plots is

  -- Plot type
  type Teps_Plot is limited private;

  -- Option types
  type Tfigure_Index is record
    I, J: Positive;
  end record;
  type Torientation is (Portrait, Landscape);
  type Tshape is (Autoshape, Squared);
  type Tscaling is (Noscale, Xy_Independent, Xy_Comparable);

  -- Font types
  type Tfont_Name is (Arial, Avant_Garde, Bodoni, Bookman, Courier, Gill_Sans, Goudy,
    Helvetica, Helvetica_Condensed, Helvetica_Narrow, Hoefler, Joanna, Letter_Gothic,
    Lubalin_Graph, New_Century, Optima, Palatino, Stempel_Garamond, Times, Times_New,
    Univers, Univers_Condensed, Univers_Extended,
    Albertus_Light, Albertus, Albertus_Italic, Antique_Olive_Roman, Antique_Olive_Italic,
    Antique_Olive_Bold, Antique_Olive_Compact, Apple_Chancery, Bodoni_Poster,
    Bodoni_Poster_Compressed, Carta, Chicago, Clarendon_Light, Clarendon, Clarendon_Bold,
    Cooper_Black, Cooper_Black_Italic, Copperplate_32BC, Copperplate_33BC, Coronet,
    Eurostile, Eurostile_Bold, Eurostile_Extended2, Eurostile_Bold_Extended2, Geneva,
    Gill_Sans_Light, Gill_Sans_Light_Italic, Gill_Sans_ExtraBold, Gill_Sans_Condensed,
    Gill_Sans_Condensed_Bold, Goudy_Extra_Bold, Hoefler_Ornaments, Marigold, Monaco,
    Mona_Lisa_Recut, New_York, Oxford, Symbol, Tekton, Wingdings_Regular,
    Zapf_Chancery_Medium_Italic, Zapf_Dingbats);
  subtype Tfont_Name_Prefix is Tfont_Name range Arial..Univers_Extended;
  type Tfont is record
    Name: Tfont_Name;
    Size: Float;
    Bold: Boolean;
    Italic: Boolean;
  end record;

  -- Dash Pattern type
  Max_Dash_Pattern: constant := 8;
  type Tdash_Pattern is array(1..Max_Dash_Pattern) of Float;

  -- Line End Type
  type Tline_End is (Blutt, Round, Square);

  -- Line Corner Type
  type Tline_Corner is (Miter, Round, Bevel);

  -- Arrow Head type
  type Tarrow_Head is record
    Width, Length_Outside, Length_Inside: Float;
  end record;

  -- Symbol types
  type Tsymbol is (Circle, Square, Triangle_N, Triangle_W,
    Triangle_S, Triangle_E, Diamond, Plus, Times, Splat,
    Ellipse, Line, Arrow, Rectangle, Polyline, Polygon);
  subtype Txyr_Symbol is Tsymbol range Circle..Splat;
  subtype Txyrr_Symbol is Tsymbol range Ellipse..Ellipse;
  subtype Txyxy_Symbol is Tsymbol range Line..Rectangle;
  subtype Txys_Symbol is Tsymbol range Polyline..Polygon;
  type Tsymbol_Style is (Line, Filled, Opaque);

  -- Text types
  type Tjustify is (Lower_Left, Lower_Middle, Lower_Right,
    Middle_Left, Middle_Middle, Middle_Right,
    Upper_Left, Upper_Middle, Upper_Right,
    Bottom_Left, Bottom_Middle, Bottom_Right,
    Center_Left, Center_Middle, Center_Right,
    Top_Left, Top_Middle, Top_Right);

  -- Axis type
  type Taxis is limited private;

  -- Constants
  Dina3: constant T2dim := (X=>842.75, Y=>1191.82);
  Dina4: constant T2dim := (X=>595.91, Y=>842.75);
  Dina5: constant T2dim := (X=>421.37, Y=>595.91);

  Black:   constant Tcolor := (R=>0.0, G=>0.0, B=>0.0);
  Red:     constant Tcolor := (R=>1.0, G=>0.0, B=>0.0);
  Lime:    constant Tcolor := (R=>0.0, G=>1.0, B=>0.0);
  Blue:    constant Tcolor := (R=>0.0, G=>0.0, B=>1.0);
  Aqua:    constant Tcolor := (R=>0.0, G=>1.0, B=>1.0);
  Fuchsia: constant Tcolor := (R=>1.0, G=>0.0, B=>1.0);
  Yellow:  constant Tcolor := (R=>1.0, G=>1.0, B=>0.0);
  White:   constant Tcolor := (R=>1.0, G=>1.0, B=>1.0);
  Navy:    constant Tcolor := (R=>0.0, G=>0.0, B=>0.5);
  Green:   constant Tcolor := (R=>0.0, G=>0.5, B=>0.0);
  Maroon:  constant Tcolor := (R=>0.5, G=>0.0, B=>0.0);
  Teal:    constant Tcolor := (R=>0.0, G=>0.5, B=>0.5);
  Purple:  constant Tcolor := (R=>0.5, G=>0.0, B=>0.5);
  Olive:   constant Tcolor := (R=>0.5, G=>0.5, B=>0.0);
  Gray:    constant Tcolor := (R=>0.5, G=>0.5, B=>0.5);
  Colors_List: constant array(1..15) of Tcolor :=
    (Black, Red, Lime, Blue, Aqua, Fuchsia, Yellow, White,
    Navy, Green, Maroon, Teal, Purple, Olive, Gray);

  Solid_Line:        constant Tdash_Pattern := (others=>0.0);
  Dotted_Line:       constant Tdash_Pattern := (2.0, 3.0, others=>0.0);
  Dotted_Short_Line: constant Tdash_Pattern := (2.0, 2.0, others=>0.0);
  Dotted_Long_Line:  constant Tdash_Pattern := (2.0, 4.0, others=>0.0);
  Dashed_Line:       constant Tdash_Pattern := (6.0, 6.0, others=>0.0);
  Dashed_Short_Line: constant Tdash_Pattern := (4.0, 4.0, others=>0.0);
  Dashed_Long_Line:  constant Tdash_Pattern := (8.0, 8.0, others=>0.0);
  Dash_Dot_Line:     constant Tdash_Pattern := (6.0, 4.0, 2.0, 4.0, others=>0.0);
  Line_Patterns_List: constant array(1..8) of Tdash_Pattern :=
    (Solid_Line, Dotted_Line, Dotted_Short_Line, Dotted_Long_Line,
    Dashed_Line, Dashed_Short_Line, Dashed_Long_Line, Dash_Dot_Line);

  -- Default values
  Default_Paper: constant T2dim := Dina4;
  Default_Layout: constant Tfigure_Index := (I=>1, J=>1);
  Default_Figure: constant Tfigure_Index := (I=>1, J=>1);
  Default_Plot_Aft: constant Field := Default_Float_Aft;
  Default_Orientation: constant Torientation := Portrait;
  Default_Shape: constant Tshape := Autoshape;
  Default_Paper_Margin: constant T2dim := (X=>75.0, Y=>75.0);
  Default_Plot_Ll_Margin: constant T2dim := (X=>0.0, Y=>0.0);
  Default_Plot_Ur_Margin: constant T2dim := (X=>0.0, Y=>0.0);
  Default_Separation: constant T2dim := (X=>30.0, Y=>30.0);
  Default_Top_Separation: constant Boolean := True;
  Default_Title_Height: constant Float := 30.0;
  Default_Scaling: constant Tscaling := Noscale;
  Default_Min: constant T2dim := (X=>0.0, Y=>0.0);
  Default_Max: constant T2dim := (X=>1.0, Y=>1.0);
  Default_Figure_Margin: constant T2dim := (X=>15.0, Y=>15.0);
  Default_Ratio: constant T2dim := (X=>1.0, Y=>1.0);
  Default_Offset: constant T2dim := (X=>0.0, Y=>0.0);
  Default_Figure_Title_Gap: constant Float := 5.0;

  Default_Color: constant Tcolor := Black;
  Default_Font: constant Tfont := (Name=>Helvetica, Size=>10.0, Bold=>False, Italic=>False);
  Default_Title_Font: constant Tfont := (Name=>Helvetica, Size=>14.0, Bold=>True, Italic=>False);
  Default_Line_Width: constant Float := 1.0;
  Default_Dash_Pattern: constant Tdash_Pattern := Solid_Line;
  Default_Line_End: constant Tline_End := Blutt;
  Default_Line_Corner: constant Tline_Corner := Miter;
  Default_Arrow_Head: constant Tarrow_Head := (Width => 10.0, Length_Outside => 10.0, Length_Inside => 7.0);

  Default_Symbol_Style: constant Tsymbol_Style := Line;
  Default_Justify: constant Tjustify := Middle_Middle;
  Default_Angle: constant Float := 0.0;

  Default_Axis_Title: constant String := "";
  Default_Axis_Title_Font: constant Tfont := (Name=>Helvetica, Size=>10.0, Bold=>False, Italic=>False);
  Default_Axis_Title_Gap: constant Float := 15.0;
  Default_Axis_Ticks: constant Boolean := False;
  Default_Axis_Ticks_Inc: constant Float := 0.1;
  Default_Axis_Ticks_Group: constant Positive := 2;
  Default_Axis_Ticks_Size: constant Float := 5.0;
  Default_Axis_Labels: constant Boolean := False;
  Default_Axis_Labels_Font: constant Tfont := (Name=>Helvetica, Size=>9.0, Bold=>False, Italic=>False);
  Default_Axis_Labels_Gap: constant Float := 3.0;
  Default_Axis_Labels_Decimals: constant Natural := 1;


  -- Plot exception
  Eps_Plot_Error: exception;


  -- Simple plot and figure
  procedure Start_Simple_Plot(
    Plot: out Teps_Plot;
    File_Name: in String;
    Size: in T2dim;
    Min: in T2dim := Default_Min;
    Max: in T2dim := Default_Max);

  -- Plot subprograms
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
    Aft: in Field := Default_Plot_Aft);

  procedure End_Plot(
    Plot: in out Teps_Plot);

  procedure Set_Layout(
    Plot: in out Teps_Plot;
    Layout: in Tfigure_Index := Default_Layout);

  procedure Set_Paper(
    Plot: in out Teps_Plot;
    Paper: in T2dim := Default_Paper);

  procedure Set_Orientation(
    Plot: in out Teps_Plot;
    Orientation: in Torientation := Default_Orientation);

  procedure Set_Shape(
    Plot: in out Teps_Plot;
    Shape: in Tshape := Default_Shape);

  procedure Set_Paper_Margin(
    Plot: in out Teps_Plot;
    Paper_Margin: in T2dim := Default_Paper_Margin);

  procedure Set_Plot_Ll_Margin(
    Plot: in out Teps_Plot;
    Plot_Ll_Margin: in T2dim := Default_Plot_Ll_Margin);

  procedure Set_Plot_Ur_Margin(
    Plot: in out Teps_Plot;
    Plot_Ur_Margin: in T2dim := Default_Plot_Ur_Margin);

  procedure Set_Separation(
    Plot: in out Teps_Plot;
    Separation: in T2dim := Default_Separation);

  procedure Set_Top_Separation(
    Plot: in out Teps_Plot;
    Top_Separation: in Boolean := Default_Top_Separation);

  procedure Set_Title_Height(
    Plot: in out Teps_Plot;
    Title_Height: in Float := Default_Title_Height);

  procedure Set_Color(
    Plot: in out Teps_Plot;
    Color: in Tcolor := Default_Color);

  procedure Set_Font(
    Plot: in out Teps_Plot;
    Font: in Tfont := Default_Font);

  procedure Set_Line_Width(
    Plot: in out Teps_Plot;
    Line_Width: in Float := Default_Line_Width);

  procedure Set_Dash_Pattern(
    Plot: in out Teps_Plot;
    Dash_Pattern: in Tdash_Pattern := Default_Dash_Pattern);

  procedure Set_Line_End(
    Plot: in out Teps_Plot;
    Line_End: in Tline_End := Default_Line_End);

  procedure Set_Line_Corner(
    Plot: in out Teps_Plot;
    Line_Corner: in Tline_Corner := Default_Line_Corner);

  procedure Set_Arrow_Head(
    Plot: in out Teps_Plot;
    Arrow_Head: in Tarrow_Head := Default_Arrow_Head);

  procedure Set_Aft(
    Plot: in out Teps_Plot;
    Aft: in Field := Default_Plot_Aft);

  procedure Put_Bounding_Box(
    Plot: in out Teps_Plot);

  procedure Put_Title(
    Plot: in out Teps_Plot;
    Title: in String;
    Font: in Tfont := Default_Title_Font);

  procedure Put_Text(
    Plot: in out Teps_Plot;
    Text: in String;
    Position: in T2dim;
    Justify: in Tjustify := Default_Justify;
    Angle: in Float := Default_Angle);

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txyr_Symbol;
    Center: in T2dim;
    Radius: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txyrr_Symbol;
    Center: in T2dim;
    Radius_X: in Float;
    Radius_Y: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txyxy_Symbol;
    Ll: in T2dim;
    Ur: in T2dim;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Symbol(
    Plot: in out Teps_Plot;
    Symbol: in Txys_Symbol;
    Vertices: in T2dims;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Postscript(
    Plot: in out Teps_Plot;
    Text: in String);


  -- Figure subprograms
  procedure Set_Figure_Scaling(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Scaling: in Tscaling := Default_Scaling;
    Min: in T2dim := Default_Min;
    Max: in T2dim := Default_Max;
    Margin: in T2dim := Default_Figure_Margin);

  function Global_Coordinates(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Position: in T2dim) return T2dim;

  function Global_X_Coordinate(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    X_Position: in Float) return Float;

  function Global_Y_Coordinate(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Y_Position: in Float) return Float;

  function Global_Distances(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Distance: in T2dim) return T2dim;

  function Global_X_Distance(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    X_Distance: in Float) return Float;

  function Global_Y_Distance(
    Plot: Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Y_Distance: in Float) return Float;

  procedure Put_Figure_Frame(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure);

  procedure Put_Figure_Axes(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    X_Axis: in Taxis;
    Y_Axis: in Taxis);

  procedure Put_Figure_Title(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Title: in String;
    Gap: in Float := Default_Figure_Title_Gap);

  procedure Put_Figure_Text(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Text: in String;
    Position: in T2dim;
    Justify: in Tjustify := Default_Justify;
    Angle: in Float := Default_Angle);

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txyr_Symbol;
    Center: in T2dim;
    Radius: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txyrr_Symbol;
    Center: in T2dim;
    Radius_X: in Float;
    Radius_Y: in Float;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txyxy_Symbol;
    Ll: in T2dim;
    Ur: in T2dim;
    Style: in Tsymbol_Style := Default_Symbol_Style);

  procedure Put_Figure_Symbol(
    Plot: in out Teps_Plot;
    Figure: in Tfigure_Index := Default_Figure;
    Symbol: in Txys_Symbol;
    Vertices: in T2dims;
    Style: in Tsymbol_Style := Default_Symbol_Style);


  -- Axis subprograms
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
    Labels_Decimals: in Natural := Default_Axis_Labels_Decimals);

  procedure Set_Axis_Title(
    Axis: in out Taxis;
    Title: in String := Default_Axis_Title;
    Title_Font: in Tfont := Default_Axis_Title_Font;
    Title_Gap: in Float := Default_Axis_Title_Gap);

  procedure Set_Axis_Ticks(
    Axis: in out Taxis;
    Ticks: in Boolean := Default_Axis_Ticks;
    Ticks_Inc: in Float := Default_Axis_Ticks_Inc;
    Ticks_Group: in Positive := Default_Axis_Ticks_Group;
    Ticks_Size: in Float := Default_Axis_Ticks_Size);

  procedure Set_Axis_Labels(
    Axis: in out Taxis;
    Labels: in Boolean := Default_Axis_Labels;
    Labels_Font: in Tfont := Default_Axis_Labels_Font;
    Labels_Gap: in Float := Default_Axis_Labels_Gap;
    Labels_Decimals: in Natural := Default_Axis_Labels_Decimals);


private

  -- Axis type
  type Taxis is record
    Title: Str := To_Str(Default_Axis_Title);
    Title_Font: Tfont := Default_Axis_Title_Font;
    Title_Gap: Float := Default_Axis_Title_Gap;
    Ticks: Boolean := Default_Axis_Ticks;
    Ticks_Inc: Float := Default_Axis_Ticks_Inc;
    Ticks_Group: Positive := Default_Axis_Ticks_Group;
    Ticks_Size: Float := Default_Axis_Ticks_Size;
    Labels: Boolean := Default_Axis_Labels;
    Labels_Font: Tfont := Default_Axis_Labels_Font;
    Labels_Gap: Float := Default_Axis_Labels_Gap;
    Labels_Decimals: Natural := Default_Axis_Labels_Decimals;
  end record;


  -- Figure types
  type Teps_Figure is record
    Ll: T2dim;
    Ur: T2dim;
    Ori_Scaling: Tscaling := Default_Scaling;
    Ori_Min: T2dim := Default_Min;
    Ori_Max: T2dim := Default_Max;
    Ori_Margin: T2dim := Default_Figure_Margin;
    Scaling: Tscaling := Default_Scaling;
    Min: T2dim := Default_Min;
    Max: T2dim := Default_Max;
    Margin: T2dim := Default_Figure_Margin;
    Ratio: T2dim := Default_Ratio;
    Offset: T2dim := Default_Offset;
  end record;

  type Peps_Figure is access all Teps_Figure;

  type Tfigures_Array is
    array(Positive range <>, Positive range <>)
    of aliased Teps_Figure;

  type Pfigures_Array is access Tfigures_Array;


  -- Plot type
  type Teps_Plot is record
    File: File_Type;
    Ori_Paper: T2dim := Default_Paper;
    Ori_Orientation: Torientation := Default_Orientation;
    Ori_Shape: Tshape := Default_Shape;
    Ori_Margin: T2dim := Default_Paper_Margin;
    Ori_Plot_Ll_Margin: T2dim := Default_Plot_Ll_Margin;
    Ori_Plot_Ur_Margin: T2dim := Default_Plot_Ur_Margin;
    Ori_Separation: T2dim := Default_Separation;
    Ori_Top_Separation: Boolean := Default_Top_Separation;
    Ori_Title_Height: Float := Default_Title_Height;
    Ori_Num: Tfigure_Index;
    Ori_Color: Tcolor := Default_Color;
    Ori_Font: Tfont := Default_Font;
    Ori_Line_Width: Float := Default_Line_Width;
    Ori_Dash_Pattern: Tdash_Pattern := Default_Dash_Pattern;
    Ori_Line_End: Tline_End := Default_Line_End;
    Ori_Line_Corner: Tline_Corner := Default_Line_Corner;
    Ori_Arrow_Head: Tarrow_Head := Default_Arrow_Head;
    Ori_Aft: Field := Default_Plot_Aft;
    Paper: T2dim := Default_Paper;
    Orientation: Torientation := Default_Orientation;
    Shape: Tshape := Default_Shape;
    Margin: T2dim := Default_Paper_Margin;
    Plot_Ll_Margin: T2dim := Default_Plot_Ll_Margin;
    Plot_Ur_Margin: T2dim := Default_Plot_Ur_Margin;
    Separation: T2dim := Default_Separation;
    Top_Separation: Boolean := Default_Top_Separation;
    Title_Height: Float := Default_Title_Height;
    Num: Tfigure_Index;
    Color: Tcolor := Default_Color;
    Font: Tfont := Default_Font;
    Line_Width: Float := Default_Line_Width;
    Dash_Pattern: Tdash_Pattern := Default_Dash_Pattern;
    Line_End: Tline_End := Default_Line_End;
    Line_Corner: Tline_Corner := Default_Line_Corner;
    Arrow_Head: Tarrow_Head := Default_Arrow_Head;
    Aft: Field := Default_Plot_Aft;
    Side: T2dim;
    Figure: Pfigures_Array := null;
  end record;

end Eps_Plots;
