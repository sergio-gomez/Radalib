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


-- @filename Eps_Utils.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/04/2002
-- @revision 30/01/2015
-- @brief Utils for the Eps Plots package

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Eps_Utils is

  -- Twodim
  type T2dim is record
    X, Y: Float;
  end record;

  procedure Swap(T: in out T2dim);
  function "+"(Left, Right: in T2dim) return T2dim;
  function "-"(Left, Right: in T2dim) return T2dim;
  function "*"(Left, Right: in T2dim) return T2dim;
  function "/"(Left, Right: in T2dim) return T2dim;
  function "*"(Left: in Float; Right: in T2dim) return T2dim;
  function "/"(Left: in T2dim; Right: in Float) return T2dim;
  function "*"(Left: in Integer; Right: in T2dim) return T2dim;
  function "/"(Left: in T2dim; Right: in Integer) return T2dim;
  function "="(Left, Right: in T2dim) return Boolean;
  function "<"(Left, Right: in T2dim) return Boolean;
  function "<="(Left, Right: in T2dim) return Boolean;
  function ">"(Left, Right: in T2dim) return Boolean;
  function ">="(Left, Right: in T2dim) return Boolean;

  Zero_2dim: constant T2dim := (0.0, 0.0);

  -- Twodims
  type T2dims is array(Integer range <>) of T2dim;
  type Pt2dims is access T2dims;

  function Alloc(First, Last: in Integer) return Pt2dims;
  pragma Inline(Alloc);
  procedure Free(P: in out Pt2dims);

  -- TColor
  subtype Trgb_Value is Float range 0.0..1.0;
  subtype Trgb_255_Value is Float range 0.0..255.0;
  type Tcolor is record
    R, G, B: Trgb_Value;
  end record;

  function To_Rgb(R, G, B: in Trgb_255_Value) return Tcolor;

  -- TColors
  type Tcolors is array (Integer range <>) of TColor;
  type Ptcolors is access Tcolors;

  type Tcolor_Scheme is (
    White_Black,            -- White =>  0.0, Black  =>  1.0
    Blue_Red,               -- Blue  =>  0.0, Red    =>  1.0
    Blue_White_Red,         -- Blue  => -1.0, White  =>  0.0, Red =>  1.0
    Green_Yellow_Red        -- Green =>  0.0, Yellow =>  0.5, Red =>  1.0
  );

  function Alloc(First, Last: in Integer) return Ptcolors;
  pragma Inline(Alloc);
  procedure Free(P: in out Ptcolors);

  function Float_To_Color(F: in Float; Colors: in Tcolors; Min: in Float := 0.0; Max: in Float := 1.0) return Tcolor;
  function Float_To_Color(F: in Float; Cs: in Tcolor_Scheme) return Tcolor;
  function Float_To_Color(F: in Float; Cs: in Tcolor_Scheme; Min, Max: in Float) return Tcolor;

  -- Strings
  function Change_Backslash(S: in String) return String;

  -- Unbounded strings
  type Str is new Unbounded_String;
  Null_Str: constant Str := Str(Null_Unbounded_String);
  function To_Str(Source: in String) return Str;
  function To_Str(Length: in Natural) return Str;

  -- Measure units
  function Cm_To_Pt(Cm: in Float) return Float;
  function Pt_To_Cm(Pt: in Float) return Float;
  function Mm_To_Pt(Mm: in Float) return Float;
  function Pt_To_Mm(Pt: in Float) return Float;

  function Cm2Pt(Cm: in Float) return Float renames Cm_To_Pt;
  function Pt2Cm(Pt: in Float) return Float renames Pt_To_Cm;
  function Mm2Pt(Mm: in Float) return Float renames Mm_To_Pt;
  function Pt2Mm(Pt: in Float) return Float renames Pt_To_Mm;

  function Deg_To_Rad(D: in Float) return Float;
  function Rad_To_Deg(R: in Float) return Float;

end Eps_Utils;
