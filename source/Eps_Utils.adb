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


-- @filename Eps_Utils.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/04/2002
-- @revision 30/01/2015
-- @brief Utils for the Eps Plots package

with Ada.Unchecked_Deallocation;
with Ada.Numerics; use Ada.Numerics;
with Utils; use Utils;

package body Eps_Utils is

  ------------
  -- To_Rgb --
  ------------

  function To_Rgb(R, G, B: in Trgb_255_Value) return Tcolor is
  begin
    return (R => R / 255.0, G => G / 255.0, B => B / 255.0);
  end To_Rgb;

  -------------------
  -- Color_Schemes --
  -------------------

  Scheme_White_Black: constant Tcolors := (
    1  => To_Rgb(R=>240.0, G=>240.0, B=>240.0),
    2  => To_Rgb(R=> 40.0, G=> 40.0, B=> 40.0)
  );

  Scheme_Blue_Red: constant Tcolors := (
--    1  => To_Rgb(R=>  0.0, G=>  0.0, B=>255.0),
--    2  => To_Rgb(R=>255.0, G=>  0.0, B=>  0.0)
    1  => To_Rgb(R=>  8.0, G=> 48.0, B=>107.0),
    2  => To_Rgb(R=>  8.0, G=> 81.0, B=>156.0),
    3  => To_Rgb(R=> 33.0, G=>113.0, B=>181.0),
    4  => To_Rgb(R=> 66.0, G=>146.0, B=>198.0),
    5  => To_Rgb(R=>107.0, G=>174.0, B=>214.0),
    6  => To_Rgb(R=>158.0, G=>202.0, B=>225.0),
    7  => To_Rgb(R=>198.0, G=>219.0, B=>239.0),
    8  => To_Rgb(R=>216.0, G=>209.0, B=>213.0),
    9  => To_Rgb(R=>234.0, G=>197.0, B=>187.0),
    10 => To_Rgb(R=>252.0, G=>187.0, B=>161.0),
    11 => To_Rgb(R=>252.0, G=>146.0, B=>114.0),
    12 => To_Rgb(R=>251.0, G=>106.0, B=> 74.0),
    13 => To_Rgb(R=>239.0, G=> 59.0, B=> 44.0),
    14 => To_Rgb(R=>203.0, G=> 24.0, B=> 29.0),
    15 => To_Rgb(R=>165.0, G=> 15.0, B=> 21.0),
    16 => To_Rgb(R=>103.0, G=>  0.0, B=> 13.0)
  );

  Scheme_Blue_White_Red: constant Tcolors := (
    -- cold (blueish) colors, for negative values
    1  => To_Rgb(R=>  8.0, G=> 48.0, B=>107.0),
    2  => To_Rgb(R=>  8.0, G=> 81.0, B=>156.0),
    3  => To_Rgb(R=> 33.0, G=>113.0, B=>181.0),
    4  => To_Rgb(R=> 66.0, G=>146.0, B=>198.0),
    5  => To_Rgb(R=>107.0, G=>174.0, B=>214.0),
    6  => To_Rgb(R=>158.0, G=>202.0, B=>225.0),
    7  => To_Rgb(R=>198.0, G=>219.0, B=>239.0),
    8  => To_Rgb(R=>222.0, G=>235.0, B=>247.0),
    9  => To_Rgb(R=>240.0, G=>244.0, B=>255.0),
    -- no color (white), for close-to-null values
    10 => To_Rgb(R=>255.0, G=>255.0, B=>255.0),
    --
    11 => To_Rgb(R=>255.0, G=>245.0, B=>240.0),
    12 => To_Rgb(R=>254.0, G=>224.0, B=>210.0),
    13 => To_Rgb(R=>252.0, G=>187.0, B=>161.0),
    14 => To_Rgb(R=>252.0, G=>146.0, B=>114.0),
    15 => To_Rgb(R=>251.0, G=>106.0, B=> 74.0),
    16 => To_Rgb(R=>239.0, G=> 59.0, B=> 44.0),
    17 => To_Rgb(R=>203.0, G=> 24.0, B=> 29.0),
    18 => To_Rgb(R=>165.0, G=> 15.0, B=> 21.0),
    19 => To_Rgb(R=>103.0, G=>  0.0, B=> 13.0)
    -- hot (redish) colors, for positive values
  );

  Scheme_Green_Yellow_Red: constant Tcolors := (
    1  => To_Rgb(R=>  0.0, G=>200.0, B=>  0.0),
    2  => To_Rgb(R=>158.0, G=>255.0, B=>  0.0),
    3  => To_Rgb(R=>255.0, G=>255.0, B=>  0.0),
    4  => To_Rgb(R=>255.0, G=>158.0, B=>  0.0),
    5  => To_Rgb(R=>200.0, G=>  0.0, B=>  0.0)
  );

  ----------
  -- Swap --
  ----------

  procedure Swap(T: in out T2dim) is
    Tmp: Float;
  begin
    Tmp := T.X;
    T.X := T.Y;
    T.Y := Tmp;
  end Swap;

  ---------
  -- "+" --
  ---------

  function "+"(Left, Right: in T2dim) return T2dim is
  begin
    return (X => Left.X + Right.X, Y => Left.Y + Right.Y);
  end "+";

  ---------
  -- "-" --
  ---------

  function "-"(Left, Right: in T2dim) return T2dim is
  begin
    return (X => Left.X - Right.X, Y => Left.Y - Right.Y);
  end "-";

  ---------
  -- "*" --
  ---------

  function "*"(Left, Right: in T2dim) return T2dim is
  begin
    return (X => Left.X * Right.X, Y => Left.Y * Right.Y);
  end "*";

  ---------
  -- "/" --
  ---------

  function "/"(Left, Right: in T2dim) return T2dim is
  begin
    return (X => Left.X / Right.X, Y => Left.Y / Right.Y);
  end "/";

  ---------
  -- "*" --
  ---------

  function "*"(Left: in Float; Right: in T2dim) return T2dim is
  begin
    return (X => Left * Right.X, Y => Left * Right.Y);
  end "*";

  ---------
  -- "/" --
  ---------

  function "/"(Left: in T2dim; Right: in Float) return T2dim is
  begin
    return (X => Left.X / Right, Y => Left.Y / Right);
  end "/";

  ---------
  -- "*" --
  ---------

  function "*"(Left: in Integer; Right: in T2dim) return T2dim is
  begin
    return (X => Left * Right.X, Y => Left * Right.Y);
  end "*";

  ---------
  -- "/" --
  ---------

  function "/"(Left: in T2dim; Right: in Integer) return T2dim is
  begin
    return (X => Left.X / Right, Y => Left.Y / Right);
  end "/";

  ---------
  -- "=" --
  ---------

  function "="(Left, Right: in T2dim) return Boolean is
  begin
    return (Left.X = Right.X) and (Left.Y = Right.Y);
  end "=";

  ---------
  -- "<" --
  ---------

  function "<"(Left, Right: in T2dim) return Boolean is
  begin
    return (Left.X < Right.X) and (Left.Y < Right.Y);
  end "<";

  ----------
  -- "<=" --
  ----------

  function "<="(Left, Right: in T2dim) return Boolean is
  begin
    return (Left.X <= Right.X) and (Left.Y <= Right.Y);
  end "<=";

  ---------
  -- ">" --
  ---------

  function ">"(Left, Right: in T2dim) return Boolean is
  begin
    return (Left.X > Right.X) and (Left.Y > Right.Y);
  end ">";

  ----------
  -- ">=" --
  ----------

  function ">="(Left, Right: in T2dim) return Boolean is
  begin
    return (Left.X >= Right.X) and (Left.Y >= Right.Y);
  end ">=";

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(T2dims, Pt2dims);

  -----------
  -- Alloc --
  -----------

  function Alloc(First, Last: in Integer) return Pt2dims is
  begin
    return new T2dims(First..Last);
  end Alloc;

  ----------
  -- Free --
  ----------

  procedure Free(P: in out Pt2dims) is
  begin
    if P /= null then
      Dispose(P);
      P := null;
    end if;
  end Free;

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Tcolors, Ptcolors);

  -----------
  -- Alloc --
  -----------

  function Alloc(First, Last: in Integer) return Ptcolors is
  begin
    return new Tcolors(First..Last);
  end Alloc;

  ----------
  -- Free --
  ----------

  procedure Free(P: in out Ptcolors) is
  begin
    if P /= null then
      Dispose(P);
      P := null;
    end if;
  end Free;

  --------------------
  -- Float_To_Color --
  --------------------

  function Float_To_Color(F: in Float; Colors: in Tcolors; Min: in Float := 0.0; Max: in Float := 1.0) return Tcolor is
    C: TColor;
    I: Integer;
    Pos: Float;
  begin
    Pos := I2F(Colors'First) + (F - Min) / (Max - Min) * (Colors'Length - 1);
    I := F2I(Floor(Pos));
    if I < Colors'First then
      return Colors(Colors'First);
    elsif I >= Colors'Last then
      return Colors(Colors'Last);
    end if;
    C.R := Colors(I).R + (Colors(I + 1).R - Colors(I).R) * (Pos - I2F(I));
    C.G := Colors(I).G + (Colors(I + 1).G - Colors(I).G) * (Pos - I2F(I));
    C.B := Colors(I).B + (Colors(I + 1).B - Colors(I).B) * (Pos - I2F(I));
    return C;
  end Float_To_Color;

  --------------------
  -- Float_To_Color --
  --------------------

  function Float_To_Color(F: in Float; Cs: in Tcolor_Scheme) return Tcolor is
  begin
    case Cs is
      when White_Black =>
        return Float_To_Color(F, Scheme_White_Black, 0.0, 1.0);
      when Blue_Red =>
        return Float_To_Color(F, Scheme_Blue_Red, 0.0, 1.0);
      when Blue_White_Red =>
        return Float_To_Color(F, Scheme_Blue_White_Red, -1.0, 1.0);
      when Green_Yellow_Red =>
        return Float_To_Color(F, Scheme_Green_Yellow_Red, 0.0, 1.0);
    end case;
  end Float_To_Color;

  --------------------
  -- Float_To_Color --
  --------------------

  function Float_To_Color(F: in Float; Cs: in Tcolor_Scheme; Min, Max: in Float) return Tcolor is
  begin
    case Cs is
      when White_Black =>
        return Float_To_Color(F, Scheme_White_Black, Min, Max);
      when Blue_Red =>
        return Float_To_Color(F, Scheme_Blue_Red, Min, Max);
      when Blue_White_Red =>
        return Float_To_Color(F, Scheme_Blue_White_Red, Min, Max);
      when Green_Yellow_Red =>
        return Float_To_Color(F, Scheme_Green_Yellow_Red, Min, Max);
    end case;
  end Float_To_Color;

  ----------------------
  -- Change_Backslash --
  ----------------------

  function Change_Backslash(S: in String) return String is
    Res: String := S;
  begin
    for I in Res'Range loop
      if Res(I) = '\' then
        Res(I) := '/';
      end if;
    end loop;
    return Res;
  end Change_Backslash;

  ------------
  -- To_Str --
  ------------

  function To_Str(Source: in String) return Str renames To_Unbounded_String;

  ------------
  -- To_Str --
  ------------

  function To_Str(Length: in Natural) return Str renames To_Unbounded_String;

  --------------
  -- Cm_To_Pt --
  --------------

  function Cm_To_Pt(Cm: in Float) return Float is
  begin
    return 72.0 * Cm / 2.54;
  end Cm_To_Pt;

  --------------
  -- Pt_To_Cm--
  --------------

  function Pt_To_Cm(Pt: in Float) return Float is
  begin
    return 2.54 * Pt / 72.0;
  end Pt_To_Cm;

  --------------
  -- Mm_To_Pt --
  --------------

  function Mm_To_Pt(Mm: in Float) return Float is
  begin
    return Cm_To_Pt(Mm / 10.0);
  end Mm_To_Pt;

  --------------
  -- Pt_To_Mm--
  --------------

  function Pt_To_Mm(Pt: in Float) return Float is
  begin
    return 10.0 * Pt_To_Cm(Pt);
  end Pt_To_Mm;

  ----------------
  -- Deg_To_Rad --
  ----------------

  function Deg_To_Rad(D: in Float) return Float is
  begin
    return (D * Pi) / 180.0;
  end Deg_To_Rad;

  ----------------
  -- Rad_To_Deg --
  ----------------

  function Rad_To_Deg(R: in Float) return Float is
  begin
    return (R * 180.0) / Pi;
  end Rad_To_Deg;


end Eps_Utils;
