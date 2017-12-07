-- Radalib, Copyright (c) 2017 by
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


-- @filename Eps_Plots-Internals.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 04/04/2002
-- @revision 08/06/2012
-- @brief Internal utils for the implementation of Eps Plots

private package Eps_Plots.Internals is

  -- Strings
  function F2ss(F1, F2: in Float; Aft: in Field := Default_Float_Aft) return String;
  function F2ss(F1, F2, F3: in Float; Aft: in Field := Default_Float_Aft) return String;
  function F2ss(F1, F2, F3, F4: in Float; Aft: in Field := Default_Float_Aft) return String;

  function Figure_In_Plot(Plot: in Teps_Plot; Figure: in Tfigure_Index) return Boolean;

  procedure Recalculate_Plot(Plot: in out Teps_Plot);
  procedure Recalculate_Figure(Figure: in out Teps_Figure);

  procedure Get_Bounding_Box(Plot: in Teps_Plot; Ll, Ur: out T2dim);

  procedure Put_Postscript_Header(Plot: in out Teps_Plot; Name: in String);
  procedure Put_Postscript_Definitions(F: in out File_Type);
  procedure Put_Postscript_Trailer(F: in out File_Type);

  procedure Change_Orientation(Plot: in out Teps_Plot);
  procedure Put_Color(Plot: in out Teps_Plot; Color: in Tcolor);
  procedure Put_Font(Plot: in out Teps_Plot; Font: in Tfont);
  procedure Put_Line_Width(Plot: in out Teps_Plot; Line_Width: in Float);
  procedure Put_Dash_Pattern(Plot: in out Teps_Plot; Dash_Pattern: in Tdash_Pattern);
  procedure Put_Line_End(Plot: in out Teps_Plot; Line_End: in Tline_End);
  procedure Put_Line_Corner(Plot: in out Teps_Plot; Line_Corner: in Tline_Corner);
  procedure Put_Arrow_Head(Plot: in out Teps_Plot; Arrow_Head: in Tarrow_Head);

end Eps_Plots.Internals;
