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


-- @filename Eps_Plots-Internals.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 04/04/2002
-- @revision 08/06/2012
-- @brief Internal utils for the implementation of Eps Plots

with Ada.Calendar; use Ada.Calendar;

package body Eps_Plots.Internals is

  ----------
  -- F2ss --
  ----------

  function F2ss(F1, F2: in Float; Aft: in Field := Default_Float_Aft) return String is
  begin
    return F2ss(F1, Aft => Aft) & " " & F2ss(F2, Aft => Aft);
  end F2ss;

  ----------
  -- F2ss --
  ----------

  function F2ss(F1, F2, F3: in Float; Aft: in Field := Default_Float_Aft) return String is
  begin
    return F2ss(F1, F2, Aft => Aft) & " " & F2ss(F3, Aft => Aft);
  end F2ss;

  ----------
  -- F2ss --
  ----------

  function F2ss(F1, F2, F3, F4: in Float; Aft: in Field := Default_Float_Aft) return String is
  begin
    return F2ss(F1, F2, Aft => Aft) & " " & F2ss(F3, F4, Aft => Aft);
  end F2ss;

  --------------------
  -- Figure_In_Plot --
  --------------------

  function Figure_In_Plot(Plot: in Teps_Plot; Figure: in Tfigure_Index) return Boolean is
  begin
    return (Figure.I <= Plot.Num.I) and (Figure.J <= Plot.Num.J);
  end Figure_In_Plot;

  ----------------------
  -- Recalculate_Plot --
  ----------------------

  procedure Recalculate_Plot(Plot: in out Teps_Plot) is
    A: Integer range 0..1;
    X, Y, Dx, Dy: Float;
  begin
    Plot.Paper := Plot.Ori_Paper;
    Plot.Orientation := Plot.Ori_Orientation;
    Plot.Shape := Plot.Ori_Shape;
    Plot.Margin := Plot.Ori_Margin;
    Plot.Plot_Ll_Margin := Plot.Ori_Plot_Ll_Margin;
    Plot.Plot_Ur_Margin := Plot.Ori_Plot_Ur_Margin;
    Plot.Separation := Plot.Ori_Separation;
    Plot.Top_Separation := Plot.Ori_Top_Separation;
    Plot.Title_Height := Plot.Ori_Title_Height;
    Plot.Color := Plot.Ori_Color;
    Plot.Font := Plot.Ori_Font;
    Plot.Line_Width := Plot.Ori_Line_Width;
    Plot.Dash_Pattern := Plot.Ori_Dash_Pattern;
    Plot.Arrow_Head := Plot.Ori_Arrow_Head;
    Plot.Aft := Plot.Ori_Aft;
    Plot.Num := Plot.Ori_Num;
    case Plot.Orientation is
      when Portrait =>
        if Plot.Paper.X > Plot.Paper.Y then
          Swap(Plot.Paper);
        end if;
      when Landscape =>
        if Plot.Paper.X < Plot.Paper.Y then
          Swap(Plot.Paper);
        end if;
    end case;
    if Plot.Figure = null then
      Plot.Figure := new Tfigures_Array(1..Plot.Num.I, 1..Plot.Num.J);
    end if;
    if Plot.Top_Separation then
      A := 0;
    else
      A := 1;
    end if;
    Plot.Side.X := (Plot.Paper.X - 2.0 * Plot.Margin.X -
      Plot.Plot_Ll_Margin.X - Plot.Plot_Ur_Margin.X -
      (Plot.Num.J - 1) * Plot.Separation.X) / Plot.Num.J;
    Plot.Side.Y := (Plot.Paper.Y - 2.0 * Plot.Margin.Y -
      Plot.Plot_Ll_Margin.Y - Plot.Plot_Ur_Margin.Y -
      (Plot.Num.I - A) * Plot.Separation.Y - Plot.Title_Height) / Plot.Num.I;
    if Plot.Shape = Squared then
      if Plot.Side.X > Plot.Side.Y then
        Plot.Side.X := Plot.Side.Y;
        Plot.Margin.X := (Plot.Paper.X - Plot.Plot_Ll_Margin.X -
          Plot.Plot_Ur_Margin.X - Plot.Num.J * Plot.Side.X -
          (Plot.Num.J - 1) * Plot.Separation.X) / 2.0;
      else
        Plot.Side.Y := Plot.Side.X;
        Plot.Margin.Y := (Plot.Paper.Y - Plot.Plot_Ll_Margin.Y -
          Plot.Plot_Ur_Margin.Y - Plot.Num.I * Plot.Side.Y -
          (Plot.Num.I - A) * Plot.Separation.Y - Plot.Title_Height) / 2.0;
      end if;
    end if;
    Dx := Plot.Side.X + Plot.Separation.X;
    Dy := Plot.Side.Y + Plot.Separation.Y;
    Y := Plot.Margin.Y + Plot.Plot_Ll_Margin.Y;
    for I in reverse Plot.Figure'Range(1) loop
      X := Plot.Margin.X + Plot.Plot_Ll_Margin.X;
      for J in Plot.Figure'Range(2) loop
        Plot.Figure(I, J).Ll := (X, Y);
        Plot.Figure(I, J).Ur := (X + Plot.Side.X, Y + Plot.Side.Y);
        X := X + Dx;
      end loop;
      Y := Y + Dy;
    end loop;
  end Recalculate_Plot;

  ------------------------
  -- Recalculate_Figure --
  ------------------------

  procedure Recalculate_Figure(Figure: in out Teps_Figure) is
  begin
    Figure.Scaling := Figure.Ori_Scaling;
    Figure.Min := Figure.Ori_Min;
    Figure.Max := Figure.Ori_Max;
    Figure.Margin := Figure.Ori_Margin;
    case Figure.Scaling is
      when Noscale =>
        Figure.Min := Figure.Ll + Figure.Margin;
        Figure.Max := Figure.Ur - Figure.Margin;
      when Xy_Independent =>
        null;
      when Xy_Comparable =>
        Figure.Ratio := (Figure.Ur - Figure.Ll -
          2.0 * Figure.Margin) / (Figure.Max - Figure.Min);
        if Figure.Ratio.X < Figure.Ratio.Y then
          Figure.Margin.Y := Figure.Margin.Y +
            (Figure.Ratio.Y - Figure.Ratio.X) *
            (Figure.Max.Y - Figure.Min.Y) / 2.0;
        elsif Figure.Ratio.X > Figure.Ratio.Y then
          Figure.Margin.X := Figure.Margin.X +
            (Figure.Ratio.X - Figure.Ratio.Y) *
            (Figure.Max.X - Figure.Min.X) / 2.0;
        end if;
    end case;
    Figure.Ratio := (Figure.Ur - Figure.Ll - 2.0 * Figure.Margin) /
      (Figure.Max - Figure.Min);
    Figure.Offset := Figure.Ll + Figure.Margin;
  exception
    when others => raise Eps_Plot_Error;
  end Recalculate_Figure;

  ----------------------
  -- Get_Bounding_Box --
  ----------------------

  procedure Get_Bounding_Box(Plot: in Teps_Plot; Ll, Ur: out T2dim) is
    Bb_Margin: constant := 3.0;
  begin
    Ll := (Floor(Plot.Margin.X - Bb_Margin),
           Floor(Plot.Margin.Y - Bb_Margin));
    Ur := (Ceiling(Plot.Paper.X - Plot.Margin.X + Bb_Margin),
           Ceiling(Plot.Paper.Y - Plot.Margin.Y + Bb_Margin));
  end Get_Bounding_Box;

  ---------------------------
  -- Put_Postscript_Header --
  ---------------------------

  procedure Put_Postscript_Header(Plot: in out Teps_Plot; Name: in String) is
    Ll, Ur: T2dim;
  begin
    Put_Line(Plot.File, "%!PS-Adobe-3.0 EPSF-3.0");
    Put_Line(Plot.File, "%%Title: " & Name);
    Put_Line(Plot.File, "%%Creator: Ada Library by Sergio Gomez");
    Put_Line(Plot.File, "%%CreationDate: " & T2S(Clock));
    Get_Bounding_Box(Plot, Ll, Ur);
    if Plot.Orientation = Landscape then
      Swap(Ll);
      Swap(Ur);
    end if;
    Put_Line(Plot.File, "%%BoundingBox: " & F2ss(Ll.X, Ll.Y, Ur.X, Ur.Y, Aft => 0));
    case Plot.Orientation is
      when Portrait => Put_Line(Plot.File, "%%Orientation: Portrait");
      when Landscape => Put_Line(Plot.File, "%%Orientation: Landscape");
    end case;
    Put_Line(Plot.File, "%%Pages: 1");
    Put_Line(Plot.File, "%%EndComments");
    New_Line(Plot.File);
  end Put_Postscript_Header;

  --------------------------------
  -- Put_Postscript_Definitions --
  --------------------------------

  procedure Put_Postscript_Definitions(F: in out File_Type) is
  begin
    Put_Line(F, "%% BeginProlog");
    New_Line(F);
    Put_Line(F, "% Begin definitions summary");
    Put_Line(F, "%   Variables");
    Put_Line(F, "%     n: Natural");
    Put_Line(F, "%     i: Natural range 0..Max");
    Put_Line(F, "%     x*, y*, dx, dy, r*, a: Float");
    Put_Line(F, "%     rr, gg, bb, ww: Float range 0.0..1.0");
    Put_Line(F, "%     ff: AnyFontName");
    Put_Line(F, "%     fn: FontNameAbreviation(Cour, Helv, Helvn, Tim, Sym)");
    Put_Line(F, "%     str: String_Without_Quotation_Marks");
    Put_Line(F, "%   Functions and their usage");
    Put_Line(F, "%     SetLineWidth: r slw");
    Put_Line(F, "%     SetDashPattern: x1 x2 ... xn n sdp");
    Put_Line(F, "%     SetLineEnd: i sle");
    Put_Line(F, "%     SetLineCorner: i slc");
    Put_Line(F, "%     SetArrowHead: rw rlo rli sahw");
    Put_Line(F, "%     SetRGBColor: rr gg bb c");
    Put_Line(F, "%     SetGray: ww g");
    Put_Line(F, "%     Stroke: s");
    Put_Line(F, "%     MoveTo: x y m");
    Put_Line(F, "%     LineTo: x y l");
    Put_Line(F, "%     RMoveTo: dx dy rm");
    Put_Line(F, "%     RLineTo: dx dy rl");
    Put_Line(F, "%     DupDup: x y dup2  ->  x y x y");
    Put_Line(F, "%     DupDupDup: x y z dup3  ->  x y z x y z");
    Put_Line(F, "%     VectorSubstract: x1 y1 x2 y2 vsub  ->  x2-x1 y2-y1");
    Put_Line(F, "%     Circle    : x y r dci");
    Put_Line(F, "%     = (filled): x y r fci");
    Put_Line(F, "%     = (opaque): x y r oci");
    Put_Line(F, "%     Square    : x y r dsq");
    Put_Line(F, "%     = (filled): x y r fsq");
    Put_Line(F, "%     = (opaque): x y r osq");
    Put_Line(F, "%     Triangle N: x y r dtn");
    Put_Line(F, "%     = (filled): x y r ftn");
    Put_Line(F, "%     = (opaque): x y r otn");
    Put_Line(F, "%     Triangle W: x y r dtw");
    Put_Line(F, "%     = (filled): x y r ftw");
    Put_Line(F, "%     = (opaque): x y r otw");
    Put_Line(F, "%     Triangle S: x y r dts");
    Put_Line(F, "%     = (filled): x y r fts");
    Put_Line(F, "%     = (opaque): x y r ots");
    Put_Line(F, "%     Triangle E: x y r dte");
    Put_Line(F, "%     = (filled): x y r fte");
    Put_Line(F, "%     = (opaque): x y r ote");
    Put_Line(F, "%     Diamond   : x y r ddi");
    Put_Line(F, "%     = (filled): x y r fdi");
    Put_Line(F, "%     = (opaque): x y r odi");
    Put_Line(F, "%     Plus      : x y r dpl");
    Put_Line(F, "%     = (filled): x y r fpl");
    Put_Line(F, "%     = (opaque): x y r opl");
    Put_Line(F, "%     Times     : x y r dxx");
    Put_Line(F, "%     = (filled): x y r fxx");
    Put_Line(F, "%     = (opaque): x y r oxx");
    Put_Line(F, "%     Splat     : x y r dsp");
    Put_Line(F, "%     = (filled): x y r fsp");
    Put_Line(F, "%     = (opaque): x y r osp");
    Put_Line(F, "%     Ellipse   : x y rx ry del");
    Put_Line(F, "%     = (filled): x y rx ry fel");
    Put_Line(F, "%     = (opaque): x y rx ry oel");
    Put_Line(F, "%     Line      : x1 y1 x2 y2 dli");
    Put_Line(F, "%     = (filled): x1 y1 x2 y2 fli");
    Put_Line(F, "%     = (opaque): x1 y1 x2 y2 oli");
    Put_Line(F, "%     Arrow     : x1 y1 x2 y2 dar");
    Put_Line(F, "%     = (filled): x1 y1 x2 y2 far");
    Put_Line(F, "%     = (opaque): x1 y1 x2 y2 oar");
    Put_Line(F, "%     Rectangle : x1 y1 x2 y2 drc");
    Put_Line(F, "%     = (filled): x1 y1 x2 y2 frc");
    Put_Line(F, "%     = (opaque): x1 y1 x2 y2 orc");
    Put_Line(F, "%     ScaleSetFont   : fn r fss | ff r fss");
    Put_Line(F, "%     = (italic)     : fnem r fss");
    Put_Line(F, "%     = (bold)       : fnbf r fss");
    Put_Line(F, "%     = (bold-italic): fnbfem r fss");
    Put_Line(F, "%     Text:");
    Put_Line(F, "%     = BoundingBox        : x y (str) ssbb -> lx ly rx uy");
    Put_Line(F, "%     = LowercaseBottomTop : x y (str) ssbt -> by ty");
    Put_Line(F, "%     = Lower: Referred to BoundingBox");
    Put_Line(F, "%     =   (lower left)    : x y (str) ssll");
    Put_Line(F, "%     =   (lower middle)  : x y (str) sslm");
    Put_Line(F, "%     =   (lower right)   : x y (str) sslr");
    Put_Line(F, "%     = Middle: Referred to BoundingBox");
    Put_Line(F, "%     =   (middle left)   : x y (str) ssml");
    Put_Line(F, "%     =   (middle middle) : x y (str) ssmm");
    Put_Line(F, "%     =   (middle right)  : x y (str) ssmr");
    Put_Line(F, "%     = Upper: Referred to BoundingBox");
    Put_Line(F, "%     =   (upper left)    : x y (str) ssul");
    Put_Line(F, "%     =   (upper middle)  : x y (str) ssum");
    Put_Line(F, "%     =   (upper right)   : x y (str) ssur");
    Put_Line(F, "%     = Bottom: Referred to LowercaseBottomTop");
    Put_Line(F, "%     =   (bottom left)   : x y (str) ssbl");
    Put_Line(F, "%     =   (bottom middle) : x y (str) ssbm");
    Put_Line(F, "%     =   (bottom right)  : x y (str) ssbr");
    Put_Line(F, "%     = Center: Referred to LowercaseBottomTop");
    Put_Line(F, "%     =   (center left)   : x y (str) sscl");
    Put_Line(F, "%     =   (center middle) : x y (str) sscm");
    Put_Line(F, "%     =   (center right)  : x y (str) sscr");
    Put_Line(F, "%     = Top: Referred to LowercaseBottomTop");
    Put_Line(F, "%     =   (top left)      : x y (str) sstl");
    Put_Line(F, "%     =   (top middle)    : x y (str) sstm");
    Put_Line(F, "%     =   (top right)     : x y (str) sstr");
    Put_Line(F, "%     Text Rotated:");
    Put_Line(F, "%     = Lower: Referred to BoundingBox");
    Put_Line(F, "%     =   (lower left)    : x y a (str) ssall");
    Put_Line(F, "%     =   (lower middle)  : x y a (str) ssalm");
    Put_Line(F, "%     =   (lower right)   : x y a (str) ssalr");
    Put_Line(F, "%     = Middle: Referred to BoundingBox");
    Put_Line(F, "%     =   (middle left)   : x y a (str) ssaml");
    Put_Line(F, "%     =   (middle middle) : x y a (str) ssamm");
    Put_Line(F, "%     =   (middle right)  : x y a (str) ssamr");
    Put_Line(F, "%     = Upper: Referred to BoundingBox");
    Put_Line(F, "%     =   (upper left)    : x y a (str) ssaul");
    Put_Line(F, "%     =   (upper middle)  : x y a (str) ssaum");
    Put_Line(F, "%     =   (upper right)   : x y a (str) ssaur");
    Put_Line(F, "%     = Bottom: Referred to LowercaseBottomTop");
    Put_Line(F, "%     =   (bottom left)   : x y a (str) ssabl");
    Put_Line(F, "%     =   (bottom middle) : x y a (str) ssabm");
    Put_Line(F, "%     =   (bottom right)  : x y a (str) ssabr");
    Put_Line(F, "%     = Center: Referred to LowercaseBottomTop");
    Put_Line(F, "%     =   (center left)   : x y a (str) ssacl");
    Put_Line(F, "%     =   (center middle) : x y a (str) ssacm");
    Put_Line(F, "%     =   (center right)  : x y a (str) ssacr");
    Put_Line(F, "%     = Top: Referred to LowercaseBottomTop");
    Put_Line(F, "%     =   (top left)      : x y a (str) ssatl");
    Put_Line(F, "%     =   (top middle)    : x y a (str) ssatm");
    Put_Line(F, "%     =   (top right)     : x y a (str) ssatr");
    Put_Line(F, "% End definitions summary");
    New_Line(F);
    Put_Line(F, "% Begin definitions");
    Put_Line(F, "% Basic");
    Put_Line(F, "/slw {setlinewidth} bind def");
    Put_Line(F, "/sdp {array astore 0 setdash} bind def");
    Put_Line(F, "/sle {setlinecap} bind def");
    Put_Line(F, "/slc {setlinejoin} bind def");
    Put_Line(F, "/sah {/cg currentglobal def true setglobal /ahli exch def /ahlo exch def /ahw exch 2 div def cg setglobal} bind def");
    Put_Line(F, "/c {setrgbcolor} bind def");
    Put_Line(F, "/g {setgray} bind def");
    Put_Line(F, "/s {stroke} bind def");
    Put_Line(F, "/m {moveto} bind def");
    Put_Line(F, "/l {lineto} bind def");
    Put_Line(F, "/rm {rmoveto} bind def");
    Put_Line(F, "/rl {rlineto} bind def");
    Put_Line(F, "/dup2 {dup 3 -1 roll dup 4 1 roll exch} bind def");
    Put_Line(F, "/dup3 {dup 4 2 roll dup 5 1 roll 3 1 roll dup 6 1 roll 3 1 roll} bind def");
    Put_Line(F, "/vsub {4 2 roll 3 -1 roll exch sub 3 1 roll sub exch} bind def");
    Put_Line(F, "% Fill");
    Put_Line(F, "/f { gsave fill grestore s} bind def");
    Put_Line(F, "% Opaque");
    Put_Line(F, "/o { gsave 1.0 1.0 1.0 c fill grestore s} bind def");
    Put_Line(F, "% Circle");
    Put_Line(F, "%/ci { /r exch def /y exch def /x exch def x y m newpath x y r 0 360 arc s } bind def");
    Put_Line(F, "/ci { dup3 3 -1 roll add exch m 0 360 arc } bind def");
    Put_Line(F, "/dci { ci s } bind def");
    Put_Line(F, "/fci { ci f } bind def");
    Put_Line(F, "/oci { ci o } bind def");
    Put_Line(F, "% Square");
    Put_Line(F, "%/sq { /r exch def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r neg r neg rm r2 0 rl 0 r2 rl r2 neg 0 rl closepath } bind def");
    Put_Line(F, "/sq { 3 1 roll m dup dup rm 2 mul dup neg 0 rl dup neg 0 exch rl 0 rl closepath } bind def");
    Put_Line(F, "/dsq { sq s } bind def");
    Put_Line(F, "/fsq { sq f } bind def");
    Put_Line(F, "/osq { sq o } bind def");
    Put_Line(F, "% Triangle N");
    Put_Line(F, "%/tn { /r exch def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r neg r neg rm r2 0 rl r neg r2 rl closepath } bind def");
    Put_Line(F, "/tn { 3 1 roll m dup 0 exch rm dup neg dup 2 mul rl 2 mul 0 rl closepath } bind def");
    Put_Line(F, "/dtn { tn s } bind def");
    Put_Line(F, "/ftn { tn f } bind def");
    Put_Line(F, "/otn { tn o } bind def");
    Put_Line(F, "% Triangle W");
    Put_Line(F, "%/tw { /r exch def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r neg 0 rm r2 r rl 0 r2 neg rl closepath } bind def");
    Put_Line(F, "/tw { 3 1 roll m dup neg 0 rm dup dup 2 mul exch neg rl 2 mul 0 exch rl closepath } bind def");
    Put_Line(F, "/dtw { tw s } bind def");
    Put_Line(F, "/ftw { tw f } bind def");
    Put_Line(F, "/otw { tw o } bind def");
    Put_Line(F, "% Triangle S");
    Put_Line(F, "%/ts { /r exch def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r neg r rm r2 0 rl r neg r2 neg rl closepath } bind def");
    Put_Line(F, "/ts { 3 1 roll m dup neg 0 exch rm dup dup 2 mul rl neg 2 mul 0 rl closepath } bind def");
    Put_Line(F, "/dts { ts s } bind def");
    Put_Line(F, "/fts { ts f } bind def");
    Put_Line(F, "/ots { ts o } bind def");
    Put_Line(F, "% Triangle E");
    Put_Line(F, "%/te { /r exch def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r 0 rm r2 neg r rl 0 r2 neg rl closepath } bind def");
    Put_Line(F, "/te { 3 1 roll m dup 0 rm dup dup -2 mul exch rl -2 mul 0 exch rl closepath } bind def");
    Put_Line(F, "/dte { te s } bind def");
    Put_Line(F, "/fte { te f } bind def");
    Put_Line(F, "/ote { te o } bind def");
    Put_Line(F, "% Diamond");
    Put_Line(F, "%/di { /r exch def /y1 exch def /x1 exch def x1 y1 m r neg 0 rm r r neg rl r r rl r neg r rl closepath } bind def");
    Put_Line(F, "/di { 3 1 roll m dup 0 exch rm dup neg dup rl dup dup neg rl dup rl closepath } bind def");
    Put_Line(F, "/ddi { di s } bind def");
    Put_Line(F, "/fdi { di f } bind def");
    Put_Line(F, "/odi { di o } bind def");
    Put_Line(F, "% Plus symbol");
    Put_Line(F, "%/pl { /r exch def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r neg 0 rm r2 0 rl r neg r neg rm 0 r2 rl } bind def");
    Put_Line(F, "/pl { 3 1 roll m dup 0 rm dup -2 mul 0 rl dup dup rm -2 mul 0 exch rl } bind def");
    Put_Line(F, "/dpl { pl s } bind def");
    Put_Line(F, "/fpl { pl f } bind def");
    Put_Line(F, "/opl { pl o } bind def");
    Put_Line(F, "% Times symbol");
    Put_Line(F, "%/xx { /r exch 45 cos mul def /y1 exch def /x1 exch def /r2 r 2 mul def x1 y1 m r neg r neg rm r2 r2 rl r2 neg 0 rm r2 r2 neg rl } bind def");
    Put_Line(F, "/xx { 3 1 roll m 45 cos mul dup dup rm dup -2 mul dup rl 2 mul dup 0 rm dup neg exch rl } bind def");
    Put_Line(F, "/dxx { xx s } bind def");
    Put_Line(F, "/fxx { xx f } bind def");
    Put_Line(F, "/oxx { xx o } bind def");
    Put_Line(F, "% Splat symbol");
    Put_Line(F, "%/sp { /r exch def /y1 exch def /x1 exch def x1 y1 r pl x1 y1 r xx } bind def");
    Put_Line(F, "/sp { dup 4 2 roll dup2 6 -1 roll pl 3 -1 roll xx } bind def");
    Put_Line(F, "/dsp { sp s } bind def");
    Put_Line(F, "/fsp { sp f } bind def");
    Put_Line(F, "/osp { sp o } bind def");
    Put_Line(F, "% Ellipse symbol");
    Put_Line(F, "/el { /ry exch def /rx exch def /y exch def /x exch def /savematrix matrix currentmatrix def x y translate rx ry scale 0 0 1 0 360 arc savematrix setmatrix } bind def");
    Put_Line(F, "/del { el s } bind def");
    Put_Line(F, "/fel { el f } bind def");
    Put_Line(F, "/oel { el o } bind def");
    Put_Line(F, "% Line");
    Put_Line(F, "%/li {/y2 exch def /x2 exch def /y1 exch def /x1 exch def x1 y1 m x2 y2 l } bind def");
    Put_Line(F, "/li { 4 2 roll m l } bind def");
    Put_Line(F, "/dli { li s } bind def");
    Put_Line(F, "/fli { li f } bind def");
    Put_Line(F, "/oli { li o } bind def");
    Put_Line(F, "% Arrow");
    Put_Line(F, "/dar {/y2 exch def /x2 exch def /y1 exch def /x1 exch def /dy y2 y1 sub def /dx x2 x1 sub def /dist dx dx mul dy dy mul add sqrt def");
    Put_Line(F, "      gsave x1 y1 translate dy dx atan rotate currentdash /dashoffset exch def /dasharray exch def [] 0 setdash");
    Put_Line(F, "      newpath dist 0 m dist ahlo sub ahw l dist ahli sub 0 l dist ahlo sub ahw neg l closepath s dasharray dashoffset setdash 0 0 m dist ahli sub 0 l s grestore} bind def");
    Put_Line(F, "/far {/y2 exch def /x2 exch def /y1 exch def /x1 exch def /dy y2 y1 sub def /dx x2 x1 sub def /dist dx dx mul dy dy mul add sqrt def");
    Put_Line(F, "      gsave x1 y1 translate dy dx atan rotate currentdash /dashoffset exch def /dasharray exch def [] 0 setdash");
    Put_Line(F, "      newpath dist 0 m dist ahlo sub ahw l dist ahli sub 0 l dist ahlo sub ahw neg l closepath f dasharray dashoffset setdash 0 0 m dist ahli sub 0 l f grestore} bind def");
    Put_Line(F, "/oar {/y2 exch def /x2 exch def /y1 exch def /x1 exch def /dy y2 y1 sub def /dx x2 x1 sub def /dist dx dx mul dy dy mul add sqrt def");
    Put_Line(F, "      gsave x1 y1 translate dy dx atan rotate currentdash /dashoffset exch def /dasharray exch def [] 0 setdash");
    Put_Line(F, "      newpath dist 0 m dist ahlo sub ahw l dist ahli sub 0 l dist ahlo sub ahw neg l closepath o dasharray dashoffset setdash 0 0 m dist ahli sub 0 l o grestore} bind def");
    Put_Line(F, "% Rectangle");
    Put_Line(F, "%/rc {/y2 exch def /x2 exch def /y1 exch def /x1 exch def x1 y1 m x2 y1 l x2 y2 l x1 y2 l closepath } bind def");
    Put_Line(F, "/rc { dup2 m vsub dup neg 0 exch rl exch neg 0 rl 0 exch rl closepath } bind def");
    Put_Line(F, "/drc { rc s } bind def");
    Put_Line(F, "/frc { rc f } bind def");
    Put_Line(F, "/orc { rc o } bind def");
    Put_Line(F, "% Fonts");
    Put_Line(F, "/fss { selectfont } bind def");
    Put_Line(F, "/Ari { /ArialMT } bind def");
    Put_Line(F, "/Ariem { /Arial-ItalicMT } bind def");
    Put_Line(F, "/Aribf { /Arial-BoldMT } bind def");
    Put_Line(F, "/Aribfem { /Arial-BoldItalicMT } bind def");
    Put_Line(F, "/Avant { /AvantGarde-Book } bind def");
    Put_Line(F, "/Avantem { /AvantGarde-BookOblique } bind def");
    Put_Line(F, "/Avantbf { /AvantGarde-Demi } bind def");
    Put_Line(F, "/Avantbfem { /AvantGarde-DemiOblique } bind def");
    Put_Line(F, "/Bodo { /Bodoni } bind def");
    Put_Line(F, "/Bodoem { /Bodoni-Italic } bind def");
    Put_Line(F, "/Bodobf { /Bodoni-Bold } bind def");
    Put_Line(F, "/Bodobfem { /Bodoni-BoldItalic } bind def");
    Put_Line(F, "/Bookm { /Bookman-Light } bind def");
    Put_Line(F, "/Bookmem { /Bookman-LightItalic } bind def");
    Put_Line(F, "/Bookmbf { /Bookman-Demi } bind def");
    Put_Line(F, "/Bookmbfem { /Bookman-DemiItalic } bind def");
    Put_Line(F, "/Cour { /Courier } bind def");
    Put_Line(F, "/Courem { /Courier-Oblique } bind def");
    Put_Line(F, "/Courbf { /Courier-Bold } bind def");
    Put_Line(F, "/Courbfem { /Courier-BoldOblique } bind def");
    Put_Line(F, "/Gill { /GillSans } bind def");
    Put_Line(F, "/Gillem { /GillSans-Italic } bind def");
    Put_Line(F, "/Gillbf { /GillSans-Bold } bind def");
    Put_Line(F, "/Gillbfem { /GillSans-BoldItalic } bind def");
    Put_Line(F, "/Goud { /Goudy } bind def");
    Put_Line(F, "/Goudem { /Goudy-Italic } bind def");
    Put_Line(F, "/Goudbf { /Goudy-Bold } bind def");
    Put_Line(F, "/Goudbfem { /Goudy-BoldItalic } bind def");
    Put_Line(F, "/Helv { /Helvetica } bind def");
    Put_Line(F, "/Helvem { /Helvetica-Oblique } bind def");
    Put_Line(F, "/Helvbf { /Helvetica-Bold } bind def");
    Put_Line(F, "/Helvbfem { /Helvetica-BoldOblique } bind def");
    Put_Line(F, "/Helvc { /Helvetica-Condensed } bind def");
    Put_Line(F, "/Helvcem { /Helvetica-Condensed_Oblique } bind def");
    Put_Line(F, "/Helvcbf { /Helvetica-Condensed_Bold } bind def");
    Put_Line(F, "/Helvcbfem { /Helvetica-Condensed_BoldOblique } bind def");
    Put_Line(F, "/Helvn { /Helvetica-Narrow } bind def");
    Put_Line(F, "/Helvnem { /Helvetica-Narrow-Oblique } bind def");
    Put_Line(F, "/Helvnbf { /Helvetica-Narrow-Bold } bind def");
    Put_Line(F, "/Helvnbfem { /Helvetica-Narrow-BoldOblique } bind def");
    Put_Line(F, "/Hoef { /HoeflerText-Regular } bind def");
    Put_Line(F, "/Hoefem { /HoeflerText-Italic } bind def");
    Put_Line(F, "/Hoefbf { /HoeflerText-Black } bind def");
    Put_Line(F, "/Hoefbfem { /HoeflerText-BlackItalic } bind def");
    Put_Line(F, "/Joan { /JoannaMT } bind def");
    Put_Line(F, "/Joanem { /JoannaMT-Italic } bind def");
    Put_Line(F, "/Joanbf { /JoannaMT-Bold } bind def");
    Put_Line(F, "/Joanbfem { /JoannaMT-BoldItalic } bind def");
    Put_Line(F, "/Goth { /LetterGothic } bind def");
    Put_Line(F, "/Gothem { /LetterGothic-Italic } bind def");
    Put_Line(F, "/Gothbf { /LetterGothic-Bold } bind def");
    Put_Line(F, "/Gothbfem { /LetterGothic-BoldItalic } bind def");
    Put_Line(F, "/Luba { /LubalinGraph-Book } bind def");
    Put_Line(F, "/Lubaem { /LubalinGraph-BookOblique } bind def");
    Put_Line(F, "/Lubabf { /LubalinGraph-Bold } bind def");
    Put_Line(F, "/Lubabfem { /LubalinGraph-BoldOblique } bind def");
    Put_Line(F, "/Cent { /NewCenturySchlbk-Roman } bind def");
    Put_Line(F, "/Centem { /NewCenturySchlbk-Italic } bind def");
    Put_Line(F, "/Centbf { /NewCenturySchlbk-Bold } bind def");
    Put_Line(F, "/Centbfem { /NewCenturySchlbk-BoldItalic } bind def");
    Put_Line(F, "/Opti { /Optima } bind def");
    Put_Line(F, "/Optiem { /Optima-Italic } bind def");
    Put_Line(F, "/Optibf { /Optima-Bold } bind def");
    Put_Line(F, "/Optibfem { /Optima-BoldItalic } bind def");
    Put_Line(F, "/Palat { /Palatino-Roman } bind def");
    Put_Line(F, "/Palatem { /Palatino-Italic } bind def");
    Put_Line(F, "/Palatbf { /Palatino-Bold } bind def");
    Put_Line(F, "/Palatbfem { /Palatino-BoldItalic } bind def");
    Put_Line(F, "/Gara { /StempelGaramond-Roman } bind def");
    Put_Line(F, "/Garaem { /StempelGaramond-Italic } bind def");
    Put_Line(F, "/Garabf { /StempelGaramond-Bold } bind def");
    Put_Line(F, "/Garabfem { /StempelGaramond-BoldItalic } bind def");
    Put_Line(F, "/Tim { /Times-Roman } bind def");
    Put_Line(F, "/Timem { /Times-Italic } bind def");
    Put_Line(F, "/Timbf { /Times-Bold } bind def");
    Put_Line(F, "/Timbfem { /Times-BoldItalic } bind def");
    Put_Line(F, "/Timn { /TimesNewRomanPSMT } bind def");
    Put_Line(F, "/Timnem { /TimesNewRomanPS-ItalicMT } bind def");
    Put_Line(F, "/Timnbf { /TimesNewRomanPS-BoldMT } bind def");
    Put_Line(F, "/Timnbfem { /TimesNewRomanPS-BoldItalicMT } bind def");
    Put_Line(F, "/Uni { /Univers } bind def");
    Put_Line(F, "/Uniem { /Univers-Oblique } bind def");
    Put_Line(F, "/Unibf { /Univers-Bold } bind def");
    Put_Line(F, "/Unibfem { /Univers-BoldOblique } bind def");
    Put_Line(F, "/Unic { /Univers-Condensed } bind def");
    Put_Line(F, "/Unicem { /Univers-CondensedOblique } bind def");
    Put_Line(F, "/Unicbf { /Univers-CondensedBold } bind def");
    Put_Line(F, "/Unicbfem { /Univers-CondensedBoldOblique } bind def");
    Put_Line(F, "/Unie { /Univers-Extended } bind def");
    Put_Line(F, "/Unieem { /Univers-ExtendedOblique } bind def");
    Put_Line(F, "/Uniebf { /Univers-BoldExt } bind def");
    Put_Line(F, "/Uniebfem { /Univers-BoldExtOblique } bind def");
    Put_Line(F, "% Text");
    Put_line(F, "/ssbb {gsave 3 1 roll m false charpath pathbbox clippath grestore} bind def");
    Put_line(F, "/ssbt {pop (a) ssbb exch pop 3 -1 roll pop} bind def");
    Put_line(F, "/sslx {ssbb pop pop pop} bind def");
    Put_line(F, "/ssrx {ssbb pop exch pop exch pop} bind def");
    Put_line(F, "/ssmx {dup3 sslx 4 1 roll ssrx add 2 div} bind def");
    Put_line(F, "/ssly {ssbb pop pop exch pop} bind def");
    Put_line(F, "/ssuy {ssbb exch pop exch pop exch pop} bind def");
    Put_line(F, "/ssmy {dup3 ssly 4 1 roll ssuy add 2 div} bind def");
    Put_line(F, "/ssby {ssbt pop} bind def");
    Put_line(F, "/ssty {ssbt exch pop} bind def");
    Put_line(F, "/sscy {dup3 ssby 4 1 roll ssty add 2 div} bind def");
    Put_line(F, "/ssll {dup3 dup3 sslx 4 1 roll ssly 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sslm {dup3 dup3 ssmx 4 1 roll ssly 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sslr {dup3 dup3 ssrx 4 1 roll ssly 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssml {dup3 dup3 sslx 4 1 roll ssmy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssmm {dup3 dup3 ssmx 4 1 roll ssmy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssmr {dup3 dup3 ssrx 4 1 roll ssmy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssul {dup3 dup3 sslx 4 1 roll ssuy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssum {dup3 dup3 ssmx 4 1 roll ssuy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssur {dup3 dup3 ssrx 4 1 roll ssuy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssbl {dup3 dup3 sslx 4 1 roll ssby 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssbm {dup3 dup3 ssmx 4 1 roll ssby 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssbr {dup3 dup3 ssrx 4 1 roll ssby 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sscl {dup3 dup3 sslx 4 1 roll sscy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sscm {dup3 dup3 ssmx 4 1 roll sscy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sscr {dup3 dup3 ssrx 4 1 roll sscy 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sstl {dup3 dup3 sslx 4 1 roll ssty 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sstm {dup3 dup3 ssmx 4 1 roll ssty 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/sstr {dup3 dup3 ssrx 4 1 roll ssty 5 -2 roll dup2 m vsub rm show} bind def");
    Put_line(F, "/ssall {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 sslx 4 1 roll ssly 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssalm {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssmx 4 1 roll ssly 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssalr {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssrx 4 1 roll ssly 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssaml {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 sslx 4 1 roll ssmy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssamm {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssmx 4 1 roll ssmy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssamr {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssrx 4 1 roll ssmy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssaul {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 sslx 4 1 roll ssuy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssaum {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssmx 4 1 roll ssuy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssaur {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssrx 4 1 roll ssuy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssabl {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 sslx 4 1 roll ssby 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssabm {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssmx 4 1 roll ssby 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssabr {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssrx 4 1 roll ssby 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssacl {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 sslx 4 1 roll sscy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssacm {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssmx 4 1 roll sscy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssacr {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssrx 4 1 roll sscy 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssatl {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 sslx 4 1 roll ssty 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssatm {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssmx 4 1 roll ssty 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_line(F, "/ssatr {gsave 4 2 roll translate 0 0 m 0 0 3 -1 roll dup3 dup3 ssrx 4 1 roll ssty 5 -2 roll vsub 4 -1 roll rotate rm show grestore} bind def");
    Put_Line(F, "% End definitions");
    New_Line(F);
    Put_Line(F, "%%EndProlog");
    New_Line(F);
    Put_Line(F, "%%Page: 1 1");
  end Put_Postscript_Definitions;

  ----------------------------
  -- Put_Postscript_Trailer --
  ----------------------------

  procedure Put_Postscript_Trailer(F: in out File_Type) is
  begin
    Put_Line(F, "showpage");
    New_Line(F);
    Put_Line(F, "%%EOF");
  end Put_Postscript_Trailer;

  ------------------------
  -- Change_Orientation --
  ------------------------

  procedure Change_Orientation(Plot: in out Teps_Plot) is
  begin
    case Plot.Orientation is
      when Portrait =>
        Put_Line(Plot.File, F2ss(0.0, Plot.Paper.X, Aft => Plot.Aft) & " translate");
        Put_Line(Plot.File, "-90 rotate");
      when Landscape =>
        Put_Line(Plot.File, "90 rotate");
        Put_Line(Plot.File, F2ss(0.0, -Plot.Paper.Y, Aft => Plot.Aft) & " translate");
    end case;
  end Change_Orientation;

  ---------------
  -- Put_Color --
  ---------------

  procedure Put_Color(Plot: in out Teps_Plot; Color: in Tcolor) is
  begin
    Put_Line(Plot.File, F2ss(Color.R, Color.G, Color.B, Aft => Plot.Aft) & " c");
  end Put_Color;

  --------------
  -- Put_Font --
  --------------

  procedure Put_Font(Plot: in out Teps_Plot; Font: in Tfont) is
  begin
    case Font.Name is
      when Arial                       => Put(Plot.File, "Ari");
      when Avant_Garde                 => Put(Plot.File, "Avant");
      when Bodoni                      => Put(Plot.File, "Bodo");
      when Bookman                     => Put(Plot.File, "Bookm");
      when Courier                     => Put(Plot.File, "Cour");
      when Gill_Sans                   => Put(Plot.File, "Gill");
      when Goudy                       => Put(Plot.File, "Goud");
      when Helvetica                   => Put(Plot.File, "Helv");
      when Helvetica_Condensed         => Put(Plot.File, "Helvc");
      when Helvetica_Narrow            => Put(Plot.File, "Helvn");
      when Hoefler                     => Put(Plot.File, "Hoef");
      when Joanna                      => Put(Plot.File, "Joan");
      when Letter_Gothic               => Put(Plot.File, "Goth");
      when Lubalin_Graph               => Put(Plot.File, "Luba");
      when New_Century                 => Put(Plot.File, "Cent");
      when Optima                      => Put(Plot.File, "Opti");
      when Palatino                    => Put(Plot.File, "Palat");
      when Stempel_Garamond            => Put(Plot.File, "Gara");
      when Times                       => Put(Plot.File, "Tim");
      when Times_New                   => Put(Plot.File, "Timn");
      when Univers                     => Put(Plot.File, "Uni");
      when Univers_Condensed           => Put(Plot.File, "Unic");
      when Univers_Extended            => Put(Plot.File, "Unie");
      when Albertus_Light              => Put(Plot.File, "/AlbertusMT-Light");
      when Albertus                    => Put(Plot.File, "/AlbertusMT");
      when Albertus_Italic             => Put(Plot.File, "/AlbertusMT-Italic");
      when Antique_Olive_Roman         => Put(Plot.File, "/AntiqueOlive-Roman");
      when Antique_Olive_Italic        => Put(Plot.File, "/AntiqueOlive-Italic");
      when Antique_Olive_Bold          => Put(Plot.File, "/AntiqueOlive-Bold");
      when Antique_Olive_Compact       => Put(Plot.File, "/AntiqueOlive-Compact");
      when Apple_Chancery              => Put(Plot.File, "/Apple-Chancery");
      when Bodoni_Poster               => Put(Plot.File, "/Bodoni-Poster");
      when Bodoni_Poster_Compressed    => Put(Plot.File, "/Bodoni-PosterCompressed");
      when Carta                       => Put(Plot.File, "/Carta");
      when Chicago                     => Put(Plot.File, "/Chicago");
      when Clarendon_Light             => Put(Plot.File, "/Clarendon-Light");
      when Clarendon                   => Put(Plot.File, "/Clarendon");
      when Clarendon_Bold              => Put(Plot.File, "/Clarendon-Bold");
      when Cooper_Black                => Put(Plot.File, "/CooperBlack");
      when Cooper_Black_Italic         => Put(Plot.File, "/CooperBlack-Italic");
      when Copperplate_32BC            => Put(Plot.File, "/Copperplate-ThirtyTwoBC");
      when Copperplate_33BC            => Put(Plot.File, "/Copperplate-ThirtyThreeBC");
      when Coronet                     => Put(Plot.File, "/Coronet-Regular");
      when Eurostile                   => Put(Plot.File, "/Eurostile");
      when Eurostile_Bold              => Put(Plot.File, "/Eurostile-Bold");
      when Eurostile_Extended2         => Put(Plot.File, "/Eurostile-ExtendedTwo");
      when Eurostile_Bold_Extended2    => Put(Plot.File, "/Eurostile-BoldExtendedTwo");
      when Geneva                      => Put(Plot.File, "/Geneva");
      when Gill_Sans_Light             => Put(Plot.File, "/GillSans-Light");
      when Gill_Sans_Light_Italic      => Put(Plot.File, "/GillSans-LightItalic");
      when Gill_Sans_ExtraBold         => Put(Plot.File, "/GillSans-ExtraBold");
      when Gill_Sans_Condensed         => Put(Plot.File, "/GillSans-Condensed");
      when Gill_Sans_Condensed_Bold    => Put(Plot.File, "/GillSans-BoldCondensed");
      when Goudy_Extra_Bold            => Put(Plot.File, "/Goudy-ExtraBold");
      when Hoefler_Ornaments           => Put(Plot.File, "/HoeflerText-Ornaments");
      when Marigold                    => Put(Plot.File, "/Marigold");
      when Monaco                      => Put(Plot.File, "/Monaco");
      when Mona_Lisa_Recut             => Put(Plot.File, "/MonaLisa-Recut");
      when New_York                    => Put(Plot.File, "/NewYork");
      when Oxford                      => Put(Plot.File, "/Oxford");
      when Symbol                      => Put(Plot.File, "/Symbol");
      when Tekton                      => Put(Plot.File, "/Tekton");
      when Wingdings_Regular           => Put(Plot.File, "/Wingdings-Regular");
      when Zapf_Chancery_Medium_Italic => Put(Plot.File, "/ZapfChancery-MediumItalic");
      when Zapf_Dingbats               => Put(Plot.File, "/ZapfDingbats");
    end case;
    if Font.Name in Tfont_Name_Prefix then
      if Font.Bold then Put(Plot.File, "bf"); end if;
      if Font.Italic then Put(Plot.File, "em"); end if;
    end if;
    Put_Line(Plot.File, " " & F2ss(Font.Size, Aft => Plot.Aft) & " fss");
  end Put_Font;

  --------------------
  -- Put_Line_Width --
  --------------------

  procedure Put_Line_Width(Plot: in out Teps_Plot; Line_Width: in Float) is
  begin
    Put_Line(Plot.File, F2ss(Line_Width, Aft => Plot.Aft) & " slw");
  end Put_Line_Width;

  ----------------------
  -- Put_Dash_Pattern --
  ----------------------

  procedure Put_Dash_Pattern(Plot: in out Teps_Plot; Dash_Pattern: in Tdash_Pattern) is
  begin
    Put(Plot.File, "[");
    for i in Dash_Pattern'Range loop
      if Dash_Pattern(i) > 0.0 then
        if i > Dash_Pattern'First then
          Put(Plot.File, " ");
        end if;
        Put(Plot.File, F2ss(Dash_Pattern(i), Aft => Plot.Aft));
      else
        exit;
      end if;
    end loop;
    Put_Line(Plot.File, "] 0 setdash");
  end Put_Dash_Pattern;

  ------------------
  -- Put_Line_End --
  ------------------

  procedure Put_Line_End(Plot: in out Teps_Plot; Line_End: in Tline_End) is
  begin
    case Line_End is
      when Blutt  => Put_Line(Plot.File, "0 sle");
      when Round  => Put_Line(Plot.File, "1 sle");
      when Square => Put_Line(Plot.File, "2 sle");
    end case;
  end Put_Line_End;

  ---------------------
  -- Put_Line_Corner --
  ---------------------

  procedure Put_Line_Corner(Plot: in out Teps_Plot; Line_Corner: in Tline_Corner) is
  begin
    case Line_Corner is
      when Miter => Put_Line(Plot.File, "0 slc");
      when Round => Put_Line(Plot.File, "1 slc");
      when Bevel => Put_Line(Plot.File, "2 slc");
    end case;
  end Put_Line_Corner;

  --------------------
  -- Put_Arrow_Head --
  --------------------

  procedure Put_Arrow_Head(Plot: in out Teps_Plot; Arrow_Head: in Tarrow_Head) is
  begin
    Put_Line(Plot.File, F2ss(Arrow_Head.Width,          Aft => Plot.Aft) & " " &
                        F2ss(Arrow_Head.Length_Outside, Aft => Plot.Aft) & " " &
                        F2ss(Arrow_Head.Length_Inside,  Aft => Plot.Aft) & " sah");
  end Put_Arrow_Head;

end Eps_Plots.Internals;

