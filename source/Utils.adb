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


-- @filename Utils.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/05/2005
-- @revision 13/10/2016
-- @brief Several Utils

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions; use Ada.Numerics.Long_Elementary_Functions;


package body Utils is

  ----------------
  -- To_Type_Id --
  ----------------

  function To_Type_Id(S: in String) return Basic_Type_Id is
  begin
    if    To_Uppercase(S) = "I" or To_Lowercase(S) = "integer"   or To_Lowercase(S) = "int"  then
      return Integer_Id;
    elsif To_Uppercase(S) = "L" or To_Lowercase(S) = "longint"   or To_Lowercase(S) = "long" then
      return Integer_Id;
    elsif To_Uppercase(S) = "F" or To_Lowercase(S) = "float"                                 then
      return Float_Id;
    elsif To_Uppercase(S) = "D" or To_Lowercase(S) = "double"                                then
      return Double_Id;
    elsif To_Uppercase(S) = "B" or To_Lowercase(S) = "boolean"   or To_Lowercase(S) = "bool" then
      return Boolean_Id;
    elsif To_Uppercase(S) = "C" or To_Lowercase(S) = "character" or To_Lowercase(S) = "char" then
      return Character_Id;
    elsif To_Uppercase(S) = "S" or To_Lowercase(S) = "string"    or To_Lowercase(S) = "str"  then
      return String_Id;
    elsif To_Uppercase(S) = "U" or To_Lowercase(S) = "ustring"   or To_Lowercase(S) = "ustr" then
      return Ustring_Id;
    else
      raise Unknown_Type_Id_Error;
    end if;
  end To_Type_Id;

  -----------
  -- Round --
  -----------

  function Round(F: in Float) return Integer is
    R: Float;
  begin
    R := Round(F);
    return Integer(R);
  end Round;

  -----------
  -- Round --
  -----------

  function Round(D: in Double) return Integer is
    R: Double;
  begin
    R := Round(D);
    return Integer(R);
  end Round;

  -----------
  -- Round --
  -----------

  function Round(F: in Float; Decimals: in Natural := Default_Round_Decimals) return Float is
    Epsilon: constant Float := 1.0E-4;
    F_Round, Diff, Factor, F_Scal, X, X_Scal, X_Factor, R, R_Shifted: Float;
  begin
    if Decimals = 0 then
      if (abs F) < Float(Integer'Last - 1) then
        R := Float(Integer(F));
        if R < F then
          R_Shifted := Float(Integer(F + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        F_Round := R;
      else
        Diff := Float'Ceiling(Log(abs F, 10.0)) - Float'Floor(Log(Float(Integer'Last), 10.0));
        Factor := 10.0 ** Diff;
        F_Scal := F / Factor;
        R := Float(Integer(F_Scal));
        if R < F_Scal then
          R_Shifted := Float(Integer(F_Scal + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        F_Round := R * Factor;
      end if;
    else
      X_Factor := 10.0 ** Decimals;
      X := F * X_Factor;
      if (abs X) < Float(Integer'Last - 1) then
        R := Float(Integer(X));
        if R < X then
          R_Shifted := Float(Integer(X + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        F_Round := R;
      else
        Diff := Float'Ceiling(Log(abs X, 10.0)) - Float'Floor(Log(Float(Integer'Last), 10.0));
        Factor := 10.0 ** Diff;
        X_Scal := X / Factor;
        R := Float(Integer(X_Scal));
        if R < X_Scal then
          R_Shifted := Float(Integer(X_Scal + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        F_Round := R * Factor;
      end if;
      F_Round := F_Round / X_Factor;
    end if;
    return F_Round;
  end Round;

  -----------
  -- Round --
  -----------

  function Round(D: in Double; Decimals: in Natural := Default_Round_Decimals) return Double is
    Epsilon: constant Double := 1.0E-4;
    D_Round, Diff, Factor, D_Scal, X, X_Scal, X_Factor, R, R_Shifted: Double;
  begin
    if Decimals = 0 then
      if (abs D) < Double(Integer'Last - 1) then
        R := Double(Integer(D));
        if R < D then
          R_Shifted := Double(Integer(D + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        D_Round := R;
      else
        Diff := Double'Ceiling(Log(abs D, 10.0)) - Double'Floor(Log(Double(Integer'Last), 10.0));
        Factor := 10.0 ** Diff;
        D_Scal := D / Factor;
        R := Double(Integer(D_Scal));
        if R < D_Scal then
          R_Shifted := Double(Integer(D_Scal + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        D_Round := R * Factor;
      end if;
    else
      X_Factor := 10.0 ** Decimals;
      X := D * X_Factor;
      if (abs X) < Double(Integer'Last - 1) then
        R := Double(Integer(X));
        if R < X then
          R_Shifted := Double(Integer(X + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        D_Round := R;
      else
        Diff := Double'Ceiling(Log(abs X, 10.0)) - Double'Floor(Log(Double(Integer'Last), 10.0));
        Factor := 10.0 ** Diff;
        X_Scal := X / Factor;
        R := Double(Integer(X_Scal));
        if R < X_Scal then
          R_Shifted := Double(Integer(X_Scal + Epsilon));
          if R_Shifted - R > 0.5 then
            R := R_Shifted;
          end if;
        end if;
        D_Round := R * Factor;
      end if;
      D_Round := D_Round / X_Factor;
    end if;
    return D_Round;
  end Round;

  -----------
  -- Floor --
  -----------

  function Floor(F: in Float) return Float is
  begin
    return Float'Floor(F);
  end Floor;

  -----------
  -- Floor --
  -----------

  function Floor(F: in Float) return Integer is
  begin
    return Integer(Float'Floor(F));
  end Floor;

  -----------
  -- Floor --
  -----------

  function Floor(D: in Double) return Double is
  begin
    return Double'Floor(D);
  end Floor;

  -----------
  -- Floor --
  -----------

  function Floor(D: in Double) return Integer is
  begin
    return Integer(Double'Floor(D));
  end Floor;

  -------------
  -- Ceiling --
  -------------

  function Ceiling(F: in Float) return Float is
  begin
    return Float'Ceiling(F);
  end Ceiling;

  -------------
  -- Ceiling --
  -------------

  function Ceiling(F: in Float) return Integer is
  begin
    return Integer(Float'Ceiling(F));
  end Ceiling;

  -------------
  -- Ceiling --
  -------------

  function Ceiling(D: in Double) return Double is
  begin
    return Double'Ceiling(D);
  end Ceiling;

  -------------
  -- Ceiling --
  -------------

  function Ceiling(D: in Double) return Integer is
  begin
    return Integer(Double'Ceiling(D));
  end Ceiling;

  ---------
  -- Min --
  ---------

  function Min(Left, Right: Integer) return Integer is
  begin
    if Left <= Right then
      return Left;
    else
      return Right;
    end if;
  end Min;

  ---------
  -- Min --
  ---------

  function Min(Left, Right: Longint) return Longint is
  begin
    if Left <= Right then
      return Left;
    else
      return Right;
    end if;
  end Min;

  ---------
  -- Min --
  ---------

  function Min(Left, Right: Float) return Float is
  begin
    if Left <= Right then
      return Left;
    else
      return Right;
    end if;
  end Min;

  ---------
  -- Min --
  ---------

  function Min(Left, Right: Double) return Double is
  begin
    if Left <= Right then
      return Left;
    else
      return Right;
    end if;
  end Min;

  ---------
  -- Max --
  ---------

  function Max(Left, Right: Integer) return Integer is
  begin
    if Left >= Right then
      return Left;
    else
      return Right;
    end if;
  end Max;

  ---------
  -- Max --
  ---------

  function Max(Left, Right: Longint) return Longint is
  begin
    if Left >= Right then
      return Left;
    else
      return Right;
    end if;
  end Max;

  ---------
  -- Max --
  ---------

  function Max(Left, Right: Float) return Float is
  begin
    if Left >= Right then
      return Left;
    else
      return Right;
    end if;
  end Max;

  ---------
  -- Max --
  ---------

  function Max(Left, Right: Double) return Double is
  begin
    if Left >= Right then
      return Left;
    else
      return Right;
    end if;
  end Max;

  ----------------
  -- Is_Integer --
  ----------------

  function Is_Integer(S: in String) return Boolean is
    L: Longint;
    Len: Positive;
  begin
    Get(S, L, Len);
    return S'Length = Len;
  exception
    when others => return False;
  end Is_Integer;

  ----------------
  -- Is_Integer --
  ----------------

  function Is_Integer(U: in Ustring) return Boolean is
  begin
    return Is_Integer(To_String(U));
  end Is_Integer;

  -------------
  -- Is_Real --
  -------------

  function Is_Real(S: in String) return Boolean is
    D: Double;
    Len: Positive;
  begin
    Get(S, D, Len);
    return S'Length = Len;
  exception
    when others => return False;
  end Is_Real;

  -------------
  -- Is_Real --
  -------------

  function Is_Real(U: in Ustring) return Boolean is
  begin
    return Is_Real(To_String(U));
  end Is_Real;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(C: in Character) return Integer is
  begin
    return Character'Pos(C);
  end To_Integer;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(S: in String) return Integer is
    I: Integer;
    Len: Positive;
  begin
    Get(S, I, Len);
    return I;
  end To_Integer;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(U: in Ustring) return Integer is
  begin
    return To_Integer(U2S(U));
  end To_Integer;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(I: in Integer) return Integer is
  begin
    return I;
  end To_Integer;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(L: in Longint) return Integer is
  begin
    return Integer(L);
  end To_Integer;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(F: in Float) return Integer is
  begin
    return Integer(F);
  end To_Integer;

  ----------------
  -- To_Integer --
  ----------------

  function To_Integer(D: in Double) return Integer is
  begin
    return Integer(D);
  end To_Integer;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(C: in Character) return Longint is
  begin
    return Longint(Character'Pos(C));
  end To_Longint;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(S: in String) return Longint is
    L: Longint;
    Len: Positive;
  begin
    Get(S, L, Len);
    return L;
  end To_Longint;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(U: in Ustring) return Longint is
  begin
    return To_Longint(U2S(U));
  end To_Longint;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(I: in Integer) return Longint is
  begin
    return Longint(I);
  end To_Longint;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(L: in Longint) return Longint is
  begin
    return L;
  end To_Longint;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(F: in Float) return Longint is
  begin
    return Longint(F);
  end To_Longint;

  ----------------
  -- To_Longint --
  ----------------

  function To_Longint(D: in Double) return Longint is
  begin
    return Longint(D);
  end To_Longint;

  --------------
  -- To_Float --
  --------------

  function To_Float(C: in Character) return Float is
  begin
    return Float(Character'Pos(C));
  end To_Float;

  --------------
  -- To_Float --
  --------------

  function To_Float(S: in String) return Float is
    F: Float;
    L: Positive;
  begin
    Get(S, F, L);
    return F;
  end To_Float;

  --------------
  -- To_Float --
  --------------

  function To_Float(U: in Ustring) return Float is
  begin
    return To_Float(U2S(U));
  end To_Float;

  --------------
  -- To_Float --
  --------------

  function To_Float(I: in Integer) return Float is
  begin
    return Float(I);
  end To_Float;

  --------------
  -- To_Float --
  --------------

  function To_Float(L: in Longint) return Float is
  begin
    return Float(L);
  end To_Float;

  --------------
  -- To_Float --
  --------------

  function To_Float(F: in Float) return Float is
  begin
    return F;
  end To_Float;

  --------------
  -- To_Float --
  --------------

  function To_Float(D: in Double) return Float is
  begin
    return Float(D);
  end To_Float;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(C: in Character) return Double is
  begin
    return Double(Character'Pos(C));
  end To_Double;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(S: in String) return Double is
    D: Double;
    L: Positive;
  begin
    Get(S, D, L);
    return D;
  end To_Double;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(U: in Ustring) return Double is
  begin
    return To_Double(U2S(U));
  end To_Double;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(I: in Integer) return Double is
  begin
    return Double(I);
  end To_Double;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(L: in Longint) return Double is
  begin
    return Double(L);
  end To_Double;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(F: in Float) return Double is
  begin
    return Double(F);
  end To_Double;

  ---------------
  -- To_Double --
  ---------------

  function To_Double(D: in Double) return Double is
  begin
    return D;
  end To_Double;

  ---------------
  -- To_String --
  ---------------

  function To_String(I: in Integer; Base: in Number_Base := Ada.Integer_Text_IO.Default_Base) return String is
    S: String(1..16);
  begin
    Put(S, I, Base => Base);
    return Trim(S, Both);
  end To_String;

  ---------------
  -- To_String --
  ---------------

  function To_String(L: in Longint; Base: in Number_Base := Ada.Long_Long_Integer_Text_IO.Default_Base) return String is
    S: String(1..32);
  begin
    Put(S, L, Base => Base);
    return Trim(S, Both);
  end To_String;

  ---------------
  -- To_String --
  ---------------

  function To_String(F: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) return String is
    S: String(1..30);
  begin
    if Aft = 0 and Exp = 0 then
      return To_String(Integer(F));
    else
      Put(S, F, Aft => Aft, Exp => Exp);
      return Trim(S, Both);
    end if;
  end To_String;

  ---------------
  -- To_String --
  ---------------

  function To_String(D: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) return String is
    S: String(1..40);
  begin
    if Aft = 0 and Exp = 0 then
      return To_String(Integer(D));
    else
      Put(S, D, Aft => Aft, Exp => Exp);
      return Trim(S, Both);
    end if;
  end To_String;

  ---------------
  -- To_String --
  ---------------

  function To_String(D: in Duration; Aft: in Field := Default_Float_Aft) return String is
    Hh, Mm, Ss: Natural;
    Sf: Float;
  begin
    Ss := Natural(Float'Floor(Float(D)));
    Mm := Ss / 60;
    Sf := Float(Ss mod 60) + Float(D - Duration(Ss));
    Hh := Mm / 60;
    Mm := Mm mod 60;
    if Hh > 0 then
      return I2S(Hh) & "h " & I2S(Mm) & "m " & F2S(Sf, Aft => Aft, Exp => 0) & "s";
    elsif Mm > 0 then
      return I2S(Mm) & "m " & F2S(Sf, Aft => Aft, Exp => 0) & "s";
    else
      return F2S(Sf, Aft => Aft, Exp => 0) & "s";
    end if;
  end To_String;

  ---------------
  -- To_String --
  ---------------

  function To_String(T: in Time) return String is

    function Rj(I: in Integer) return String is
    begin
      return Right_Justify(I2s(I), 2, '0');
    end Rj;

    Year: Year_Number;
    Month: Month_Number;
    Day: Day_Number;
    Seconds: Day_Duration;
    Hour: Integer range 0..23;
    Minute: Integer range 0..59;
    Second: Integer range 0..59;
  begin
    Split(T, Year, Month, Day, Seconds);
    Hour := Integer(Seconds) / 3600;
    Minute := (Integer(Seconds) - Hour * 3600) / 60;
    Second := Integer(Seconds) mod 60;
    return  Rj(Day) & "/" & Rj(Month) & "/" & I2s(Year) & " " &
            Rj(Hour) & ":" & Rj(Minute) & ":" & Rj(Second) & "";
  end To_String;

  --------------------
  -- To_String_Exp0 --
  --------------------

  function To_String_Exp0(F: in Float; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String is
    S: String(1..30);
    J, K: Integer;
  begin
    if Aft = 0 then
      return To_String(Integer(F));
    else
      Put(S, F, Aft => Aft, Exp => 0);
      if Short then
        J := S'First;
        while S(J) = ' ' loop J := J + 1; end loop;
        K := S'Last;
        while S(K) = '0' loop K := K - 1; end loop;
        if S(K) = '.' then K := K + 1; end if;
        return S(J..K);
      else
        return Trim(S, Both);
      end if;
    end if;
  end To_String_Exp0;

  --------------------
  -- To_String_Exp0 --
  --------------------

  function To_String_Exp0(D: in Double; Aft: in Field := Default_Double_Aft; Short: in Boolean := False) return String is
    S: String(1..30);
    J, K: Integer;
  begin
    if Aft = 0 then
      return To_String(Integer(D));
    else
      Put(S, D, Aft => Aft, Exp => 0);
      if Short then
        J := S'First;
        while S(J) = ' ' loop J := J + 1; end loop;
        K := S'Last;
        while S(K) = '0' loop K := K - 1; end loop;
        if S(K) = '.' then K := K + 1; end if;
        return S(J..K);
      else
        return Trim(S, Both);
      end if;
    end if;
  end To_String_Exp0;

  ---------------------
  -- To_String_Short --
  ---------------------

  function To_String_Short(F: in Float; Aft: in Field := Default_Float_Aft) return String is
  begin
    return To_String_Exp0(F, Aft => Aft, Short => True);
  end To_String_Short;

  ---------------------
  -- To_String_Short --
  ---------------------

  function To_String_Short(D: in Double; Aft: in Field := Default_Double_Aft) return String is
  begin
    return To_String_Exp0(D, Aft => Aft, Short => True);
  end To_String_Short;

  ------------------------
  -- To_String_Exp_Auto --
  ------------------------

  function To_String_Exp_Auto(F: in Float; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String is
    Low : constant Floats(Field'Range) := (0 => 1.0  , 1 => 1.0e-1, 2 => 1.0e-2, 3..5 => 1.0e-3, others => 0.0);
    High: constant Floats(Field'Range) := (0 => 1.0e9, 1 => 1.0e9 , 2 => 1.0e9 , 3..5 => 1.0e7 , others => 0.0);
    S: String(1..30);
    J, K: Integer;
    Exp: Field := 2;
  begin
    if F = 0.0 or else (abs F) in Low(Aft)..High(Aft) then
      Exp := 0;
    end if;
    Put(S, F, Aft => Aft, Exp => Exp);
    if Short and Exp = 0 then
      J := S'First;
      while S(J) = ' ' loop J := J + 1; end loop;
      K := S'Last;
      while S(K) = '0' loop K := K - 1; end loop;
      if S(K) = '.' then K := K + 1; end if;
      return S(J..K);
    else
      return Trim(S, Both);
    end if;
  end To_String_Exp_Auto;

  ------------------------
  -- To_String_Exp_Auto --
  ------------------------

  function To_String_Exp_Auto(D: in Double; Aft: in Field := Default_Double_Aft; Short: in Boolean := False) return String is
    Low : constant Doubles(Field'Range) := (0 => 1.0   , 1 => 1.0e-1, 2 => 1.0e-2, 3..5 => 1.0e-3, 6..7 => 1.0e-4, others => 0.0);
    High: constant Doubles(Field'Range) := (0 => 1.0e15, 1 => 1.0e15, 2 => 1.0e15, 3..5 => 1.0e15, 6..7 => 1.0e15, others => 0.0);
    S: String(1..30);
    J, K: Integer;
    Exp: Field := 2;
  begin
    if D = 0.0 or else (abs D) in Low(Aft)..High(Aft) then
      Exp := 0;
    end if;
    Put(S, D, Aft => Aft, Exp => Exp);
    if Short and Exp = 0 then
      J := S'First;
      while S(J) = ' ' loop J := J + 1; end loop;
      K := S'Last;
      while S(K) = '0' loop K := K - 1; end loop;
      if S(K) = '.' then K := K + 1; end if;
      return S(J..K);
    else
      return Trim(S, Both);
    end if;
  end To_String_Exp_Auto;

  ------------------
  -- Left_Justify --
  ------------------

  function Left_Justify(S: in String; Width: in Natural; Pad: in Character := ' ') return String is
    T: constant String := Trim(S, Both);
    B: constant String(1..Width-T'Length) := (others => Pad);
  begin
    return T & B;
  end Left_Justify;

  -------------------
  -- Right_Justify --
  -------------------

  function Right_Justify(S: in String; Width: in Natural; Pad: in Character := ' ') return String is
    T: constant String := Trim(S, Both);
    B: constant String(1..Width-T'Length) := (others => Pad);
  begin
    return B & T;
  end Right_Justify;

  ---------------
  -- Translate --
  ---------------

  function Translate(S: in String; From, To: in String) return String is
  begin
    return Translate(S, To_Mapping(From, To));
  end Translate;

  ------------------
  -- To_Uppercase --
  ------------------

  function To_Uppercase(S: in String) return String is
  begin
    return To_Upper(S);
  end To_Uppercase;

  ------------------
  -- To_Uppercase --
  ------------------

  function To_Uppercase(U: in Ustring) return Ustring is
  begin
    return S2U(To_Upper(U2S(U)));
  end To_Uppercase;

  ------------------
  -- To_Lowercase --
  ------------------

  function To_Lowercase(S: in String) return String is
  begin
    return To_Lower(S);
  end To_Lowercase;

  ------------------
  -- To_Lowercase --
  ------------------

  function To_Lowercase(U: in Ustring) return Ustring is
  begin
    return S2U(To_Lower(U2S(U)));
  end To_Lowercase;

  ----------------
  -- Capitalize --
  ----------------

  function Capitalize(S: in String) return String is
    T: String := To_Lower(S);
    First: Boolean;
  begin
    First := True;
    for I in T'Range loop
      if Is_Letter(T(I)) then
        if First then
          First := False;
          T(I) := To_Upper(T(I));
        end if;
      else
        First := True;
      end if;
    end loop;
    return T;
  end Capitalize;

  ----------------
  -- Capitalize --
  ----------------

  function Capitalize(U: in Ustring) return Ustring is
  begin
    return S2U(Capitalize(U2S(U)));
  end Capitalize;

  -----------------
  -- Trim_Spaces --
  -----------------

  function Trim_Spaces(S: in String) return String is
    Left, Right: Integer;
  begin
    Left := Integer'Last;
    for I in S'Range loop
      if not Is_Space(S(I)) then
        Left := I;
        exit;
      end if;
    end loop;
    Right := Integer'First;
    for I in reverse S'Range loop
      if not Is_Space(S(I)) then
        Right := I;
        exit;
      end if;
    end loop;
    if Left <= Right then
      return S(Left..Right);
    else
      return "";
    end if;
  end Trim_Spaces;

  -----------------
  -- Trim_Spaces --
  -----------------

  function Trim_Spaces(U: in Ustring) return Ustring is
  begin
    return S2U(Trim_Spaces(U2S(U)));
  end Trim_Spaces;

  ----------------------
  -- Is_In_Characters --
  ----------------------

  function Is_In_Characters(C: in Character; L: Characters) return Boolean is
  begin
    for I in L'Range loop
      if C = L(I) then
        return True;
      end if;
    end loop;
    return False;
  end Is_In_Characters;

  ----------------------
  -- Is_In_Characters --
  ----------------------

  function Is_In_Characters(C: in Character; L: PCharacters) return Boolean is
  begin
    for I in L'Range loop
      if C = L(I) then
        return True;
      end if;
    end loop;
    return False;
  end Is_In_Characters;

  --------------
  -- Is_Space --
  --------------

  function Is_Space(C: in Character) return Boolean is
  begin
    return Is_In_Characters(C, Spaces);
  end Is_Space;

  ----------------
  -- Is_Comment --
  ----------------

  function Is_Comment(C: in Character) return Boolean is
  begin
    return Is_In_Characters(C, Comments);
  end Is_Comment;

  ------------------
  -- Is_Separator --
  ------------------

  function Is_Separator(C: in Character) return Boolean is
  begin
    return Is_In_Characters(C, Separators);
  end Is_Separator;

  -------------------
  -- Is_Open_Quote --
  -------------------

  function Is_Open_Quote(C: in Character) return Boolean is
  begin
    return Is_In_Characters(C, Quotes_Open);
  end Is_Open_Quote;

  --------------------
  -- Is_Close_Quote --
  --------------------

  function Is_Close_Quote(C: in Character) return Boolean is
  begin
    return Is_In_Characters(C, Quotes_Close);
  end Is_Close_Quote;

  ----------------
  -- Get_Spaces --
  ----------------

  function Get_Spaces return Characters is
  begin
    return Spaces.all;
  end Get_Spaces;

  ------------------
  -- Get_Comments --
  ------------------

  function Get_Comments return Characters is
  begin
    return Comments.all;
  end Get_Comments;

  --------------------
  -- Get_Separators --
  --------------------

  function Get_Separators return Characters is
  begin
    return Separators.all;
  end Get_Separators;

  ---------------------
  -- Get_Quotes_Open --
  ---------------------

  function Get_Quotes_Open return Characters is
  begin
    return Quotes_Open.all;
  end Get_Quotes_Open;

  ----------------------
  -- Get_Quotes_Close --
  ----------------------

  function Get_Quotes_Close return Characters is
  begin
    return Quotes_Close.all;
  end Get_Quotes_Close;

  ----------------
  -- Set_Spaces --
  ----------------

  procedure Set_Spaces(L: in Characters := Standard_Spaces) is
  begin
    Free(Spaces);
    Spaces := new Characters(L'Range);
    Spaces.all := L;
  end Set_Spaces;

  ------------------
  -- Set_Comments --
  ------------------

  procedure Set_Comments(L: in Characters := Standard_Comments) is
  begin
    Free(Comments);
    Comments := new Characters(L'Range);
    Comments.all := L;
  end Set_Comments;

  --------------------
  -- Set_Separators --
  --------------------

  procedure Set_Separators(L: in Characters := Standard_Separators) is
  begin
    Free(Separators);
    Separators := new Characters(L'Range);
    Separators.all := L;
  end Set_Separators;

  ----------------
  -- Set_Quotes --
  ----------------

  procedure Set_Quotes(L_Open: in Characters := Standard_Quotes_Open; L_Close: in Characters := Standard_Quotes_Close) is
  begin
    if L_Open'First /= L_Close'First or L_Open'Length /= L_Close'Length then
      raise Quotes_Error with "Incompatible open and close quote characters";
    end if;
    Free(Quotes_Open);
    Free(Quotes_Close);
    Quotes_Open  := new Characters(L_Open'Range);
    Quotes_Close := new Characters(L_Close'Range);
    Quotes_Open.all  := L_Open;
    Quotes_Close.all := L_Close;
  end Set_Quotes;

  ---------------
  -- Add_Space --
  ---------------

  procedure Add_Space(C: in Character) is
    L: PCharacters;
  begin
    if not Is_Space(C) then
      L := Spaces;
      Spaces := Alloc(L'First, L'Last + 1);
      Spaces(L'Range) := L.all;
      Spaces(L'Last + 1) := C;
      Free(L);
    end if;
  end Add_Space;

  -----------------
  -- Add_Comment --
  -----------------

  procedure Add_Comment(C: in Character) is
    L: PCharacters;
  begin
    if not Is_Comment(C) then
      L := Comments;
      Comments := Alloc(L'First, L'Last + 1);
      Comments(L'Range) := L.all;
      Comments(L'Last + 1) := C;
      Free(L);
    end if;
  end Add_Comment;

  -------------------
  -- Add_Separator --
  -------------------

  procedure Add_Separator(C: in Character) is
    L: PCharacters;
  begin
    if not Is_Separator(C) then
      L := Separators;
      Separators := Alloc(L'First, L'Last + 1);
      Separators(L'Range) := L.all;
      Separators(L'Last + 1) := C;
      Free(L);
    end if;
  end Add_Separator;

  ----------------
  -- Add_Quotes --
  ----------------

  procedure Add_Quotes(C_Open, C_Close: in Character) is
    L: PCharacters;
  begin
    L := Quotes_Open;
    Quotes_Open := Alloc(L'First, L'Last + 1);
    Quotes_Open(L'Range) := L.all;
    Quotes_Open(L'Last + 1) := C_Open;
    Free(L);
    L := Quotes_Close;
    Quotes_Close := Alloc(L'First, L'Last + 1);
    Quotes_Close(L'Range) := L.all;
    Quotes_Close(L'Last + 1) := C_Close;
    Free(L);
  end Add_Quotes;

  ---------
  -- "+" --
  ---------

  function "+"(Left, Right: in Character) return Character is
  begin
    return Character'Max(Left, Right);
  end "+";

  ---------
  -- "*" --
  ---------

  function "*"(Left, Right: in Character) return Character is
  begin
    return Character'Max(Left, Right);
  end "*";

  ---------
  -- "*" --
  ---------

  function "*"(Left, Right: in Ustring) return Ustring is
  begin
    if Left >= Right then
      return Left;
    else
      return Right;
    end if;
  end "*";

  ----------
  -- "**" --
  ----------

  function "**"(C: in Character; Power: in Natural) return String is
    B: constant String(1..Power) := (others => C);
  begin
    return B;
  end "**";

  ----------
  -- "**" --
  ----------

  function "**"(S: in String; Power: in Natural) return String is
    B: String(1..S'Length * Power);
    F, T: Positive;
  begin
    for I in 1..Power loop
      F := (I - 1) * S'Length + 1;
      T := F + S'Length - 1;
      B(F..T) := S;
    end loop;
    return B;
  end "**";

  ----------
  -- "**" --
  ----------

  function "**"(U: in Ustring; Power: in Natural) return Ustring is
    B: Ustring;
  begin
    if Power = 0 then
      B := Null_Ustring;
    else
      B := U;
      for I in 2..Power loop
        B := B & U;
      end loop;
    end if;
    return B;
  end "**";

  ---------
  -- "+" --
  ---------

  function "+"(Left: in Integer; Right: in Float) return Float is
  begin
    return Float(Left) + Right;
  end "+";

  ---------
  -- "+" --
  ---------

  function "+"(Left: in Float; Right: in Integer) return Float is
  begin
    return Left + Float(Right);
  end "+";

  ---------
  -- "-" --
  ---------

  function "-"(Left: in Integer; Right: in Float) return Float is
  begin
    return Float(Left) - Right;
  end "-";

  ---------
  -- "-" --
  ---------

  function "-"(Left: in Float; Right: in Integer) return Float is
  begin
    return Left - Float(Right);
  end "-";

  ---------
  -- "*" --
  ---------

  function "*"(Left: in Integer; Right: in Float) return Float is
  begin
    return Float(Left) * Right;
  end "*";

  ---------
  -- "*" --
  ---------

  function "*"(Left: in Float; Right: in Integer) return Float is
  begin
    return Left * Float(Right);
  end "*";

  ---------
  -- "/" --
  ---------

  function "/"(Left: in Float; Right: in Integer) return Float is
  begin
    return Left / Float(Right);
  end "/";

  ---------
  -- "+" --
  ---------

  function "+"(Left: in Integer; Right: in Double) return Double is
  begin
    return Double(Left) + Right;
  end "+";

  ---------
  -- "+" --
  ---------

  function "+"(Left: in Double; Right: in Integer) return Double is
  begin
    return Left + Double(Right);
  end "+";

  ---------
  -- "-" --
  ---------

  function "-"(Left: in Integer; Right: in Double) return Double is
  begin
    return Double(Left) - Right;
  end "-";

  ---------
  -- "-" --
  ---------

  function "-"(Left: in Double; Right: in Integer) return Double is
  begin
    return Left - Double(Right);
  end "-";

  ---------
  -- "*" --
  ---------

  function "*"(Left: in Integer; Right: in Double) return Double is
  begin
    return Double(Left) * Right;
  end "*";

  ---------
  -- "*" --
  ---------

  function "*"(Left: in Double; Right: in Integer) return Double is
  begin
    return Left * Double(Right);
  end "*";

  ---------
  -- "/" --
  ---------

  function "/"(Left: in Double; Right: in Integer) return Double is
  begin
    return Left / Double(Right);
  end "/";


begin

  Spaces       := new Characters(Standard_Spaces'Range);
  Comments     := new Characters(Standard_Comments'Range);
  Separators   := new Characters(Standard_Separators'Range);
  Quotes_Open  := new Characters(Standard_Quotes_Open'Range);
  Quotes_Close := new Characters(Standard_Quotes_Close'Range);

  Spaces.all       := Standard_Spaces;
  Comments.all     := Standard_Comments;
  Separators.all   := Standard_Separators;
  Quotes_Open.all  := Standard_Quotes_Open;
  Quotes_Close.all := Standard_Quotes_Close;

end Utils;
