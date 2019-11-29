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


-- @filename Statistics.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/11/2007
-- @revision 28/01/2018
-- @brief Statistics of Numerical Arrays

with Ada.Numerics.Generic_Elementary_Functions;

with Random_Numbers; use Random_Numbers;
with Queues_Integer; use Queues_Integer;
with Utils; use Utils;
with Arrays_Utils;
with Minheaps;

package body Statistics is

  package Num_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Num);
  use Num_Elementary_Functions;

  ----------
  -- Size --
  ----------

  function Size(V: in Nums) return Natural is
  begin
    return V'Length;
  end Size;

  function Size(P: in PNums) return Natural is
  begin
    if P = null then
      raise Statistics_Error;
    end if;

    return P'Length;
  end Size;

  -------------
  -- Minimum --
  -------------

  function Minimum(V: in Nums) return Num is
    Min: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    Min := Num'Last;
    for I in V'Range loop
      if V(I) < Min then
        Min := V(I);
      end if;
    end loop;
    return Min;
  end Minimum;

  function Minimum(P: in PNums) return Num is
    Min: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    Min := Num'Last;
    for I in P'Range loop
      if P(I) < Min then
        Min := P(I);
      end if;
    end loop;
    return Min;
  end Minimum;

  -------------
  -- Maximum --
  -------------

  function Maximum(V: in Nums) return Num is
    Max: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    Max := Num'First;
    for I in V'Range loop
      if V(I) > Max then
        Max := V(I);
      end if;
    end loop;
    return Max;
  end Maximum;

  function Maximum(P: in PNums) return Num is
    Max: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    Max := Num'First;
    for I in P'Range loop
      if P(I) > Max then
        Max := P(I);
      end if;
    end loop;
    return Max;
  end Maximum;

  ------------------
  -- Has_Negative --
  ------------------

  function Has_Negative(V: in Nums) return Boolean is
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    for I in V'Range loop
      if V(I) < 0.0 then
        return True;
      end if;
    end loop;
    return False;
  end Has_Negative;

  function Has_Negative(P: in PNums) return Boolean is
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    for I in P'Range loop
      if P(I) < 0.0 then
        return True;
      end if;
    end loop;
    return False;
  end Has_Negative;

  ---------
  -- Sum --
  ---------

  function Sum(V: in Nums; Degree: in Natural := 1) return Num is
    Total: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    Total := 0.0;
    if Degree = 0 then
      return Num(V'Length);
    elsif Degree = 1 then
      for I in V'Range loop
        Total := Total + V(I);
      end loop;
    else
      for I in V'Range loop
        Total := Total + V(I) ** Degree;
      end loop;
    end if;
    return Total;
  end Sum;

  function Sum(P: in PNums; Degree: in Natural := 1) return Num is
    Total: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    Total := 0.0;
    if Degree = 0 then
      return Num(P'Length);
    elsif Degree = 1 then
      for I in P'Range loop
        Total := Total + P(I);
      end loop;
    else
      for I in P'Range loop
        Total := Total + P(I) ** Degree;
      end loop;
    end if;
    return Total;
  end Sum;

  function Sum(V, Wh: in Nums; Degree: in Natural := 1) return Num is
    Total: Num;
    I, Iw: Integer;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    Total := 0.0;
    if Degree = 0 then
      return Sum(Wh);
    elsif Degree = 1 then
      for C in 0..(V'Length-1) loop
        I := V'First + C;
        Iw := Wh'First + C;
        Total := Total + Wh(Iw) * V(I);
      end loop;
    else
      for C in 0..(V'Length-1) loop
        I := V'First + C;
        Iw := Wh'First + C;
        Total := Total + Wh(Iw) * (V(I) ** Degree);
      end loop;
    end if;
    return Total;
  end Sum;

  function Sum(P, Wh: in PNums; Degree: in Natural := 1) return Num is
    Total: Num;
    I, Iw: Integer;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    Total := 0.0;
    if Degree = 0 then
      return Sum(Wh);
    elsif Degree = 1 then
      for C in 0..(P'Length-1) loop
        I := P'First + C;
        Iw := Wh'First + C;
        Total := Total + Wh(Iw) * P(I);
      end loop;
    else
      for C in 0..(P'Length-1) loop
        I := P'First + C;
        Iw := Wh'First + C;
        Total := Total + Wh(Iw) * (P(I) ** Degree);
      end loop;
    end if;
    return Total;
  end Sum;

  ---------------------
  -- Arithmetic_Mean --
  ---------------------

  function Arithmetic_Mean(V: in Nums) return Num is
    N: Natural;
    Sm, X: Num;
    All_Equal: Boolean;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    N := V'Length;
    All_Equal := True;
    X := V(V'First);
    Sm := 0.0;
    for I in V'Range loop
      if V(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + V(I);
    end loop;
    if All_Equal then
      return X;
    end if;
    return Sm / Num(N);
  end Arithmetic_Mean;

  function Arithmetic_Mean(P: in PNums) return Num is
    N: Natural;
    Sm, X: Num;
    All_Equal: Boolean;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    All_Equal := True;
    X := P(P'First);
    Sm := 0.0;
    for I in P'Range loop
      if P(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + P(I);
    end loop;
    if All_Equal then
      return X;
    end if;
    return Sm / Num(N);
  end Arithmetic_Mean;

  function Arithmetic_Mean(V, Wh: in Nums) return Num is
    Sm, W, X: Num;
    All_Equal: Boolean;
    I, Iw: Integer;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Arithmetic_Mean(V);
    end if;

    All_Equal := True;
    X := V(V'First);
    Sm := 0.0;
    for C in 0..(V'Length-1) loop
      I := V'First + C;
      Iw := Wh'First + C;
      if V(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + Wh(Iw) * V(I);
    end loop;
    if All_Equal then
      return X;
    end if;
    return Sm / W;
  end Arithmetic_Mean;

  function Arithmetic_Mean(P, Wh: in PNums) return Num is
    Sm, W, X: Num;
    All_Equal: Boolean;
    I, Iw: Integer;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Arithmetic_Mean(P);
    end if;

    All_Equal := True;
    X := P(P'First);
    Sm := 0.0;
    for C in 0..(P'Length-1) loop
      I := P'First + C;
      Iw := Wh'First + C;
      if P(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + Wh(Iw) * P(I);
    end loop;
    if All_Equal then
      return X;
    end if;
    return Sm / W;
  end Arithmetic_Mean;

  --------------------
  -- Geometric_Mean --
  --------------------

  function Geometric_Mean(V: in Nums) return Num is
    N: Natural;
    Min_Val, Max_Val, Sm: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    if Has_Negative(V) then
      raise Negative_Value_Error;
    end if;

    Min_Val := Min(V);
    Max_Val := Max(V);
    if Min_Val = 0.0 then
      return 0.0;
    elsif Min_Val = Max_Val then
      return Min_Val;
    end if;

    N := V'Length;
    Sm := 0.0;
    for I in V'Range loop
      Sm := Sm + Log(V(I));
    end loop;
    return Exp(Sm / Num(N));
  end Geometric_Mean;

  function Geometric_Mean(P: in PNums) return Num is
    N: Natural;
    Min_Val, Max_Val, Sm: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    if Has_Negative(P) then
      raise Negative_Value_Error;
    end if;

    Min_Val := Min(P);
    Max_Val := Max(P);
    if Min_Val = 0.0 then
      return 0.0;
    elsif Min_Val = Max_Val then
      return Min_Val;
    end if;

    N := P'Length;
    Sm := 0.0;
    for I in P'Range loop
      Sm := Sm + Log(P(I));
    end loop;
    return Exp(Sm / Num(N));
  end Geometric_Mean;

  function Geometric_Mean(V, Wh: in Nums) return Num is
    Min_Val, Max_Val, Sm, W: Num;
    I, Iw: Integer;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(V) then
      raise Negative_Value_Error;
    end if;
    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Geometric_Mean(V);
    end if;

    Min_Val := Min(V);
    Max_Val := Max(V);
    if Min_Val = 0.0 then
      return 0.0;
    elsif Min_Val = Max_Val then
      return Min_Val;
    end if;

    Sm := 0.0;
    for C in 0..(V'Length-1) loop
      I := V'First + C;
      Iw := Wh'First + C;
      Sm := Sm + Wh(Iw) * Log(V(I));
    end loop;
    return Exp(Sm / W);
  end Geometric_Mean;

  function Geometric_Mean(P, Wh: in PNums) return Num is
    Min_Val, Max_Val, Sm, W: Num;
    I, Iw: Integer;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(P) then
      raise Negative_Value_Error;
    end if;
    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Geometric_Mean(P);
    end if;

    Min_Val := Min(P);
    Max_Val := Max(P);
    if Min_Val = 0.0 then
      return 0.0;
    elsif Min_Val = Max_Val then
      return Min_Val;
    end if;

    Sm := 0.0;
    for C in 0..(P'Length-1) loop
      I := P'First + C;
      Iw := Wh'First + C;
      Sm := Sm + Wh(Iw) * Log(P(I));
    end loop;
    return Exp(Sm / W);
  end Geometric_Mean;

  -------------------
  -- Harmonic_Mean --
  -------------------

  function Harmonic_Mean(V: in Nums) return Num is
    N: Natural;
    Sm, X: Num;
    All_Equal: Boolean;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    if Minimum(V) < 0.0 then
      raise Statistics_Error;
    end if;

    N := V'Length;
    All_Equal := True;
    X := V(V'First);
    Sm := 0.0;
    for I in V'Range loop
      if V(I) = 0.0 then
        Sm := Num'Last;
        exit;
      end if;
      if V(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + 1.0 / V(I);
    end loop;
    if Sm = Num'Last then
      return 0.0;
    elsif All_Equal then
      return X;
    end if;
    if Sm <= 0.0 then
      raise Statistics_Error;
    end if;
    return Num(N) / Sm;
  end Harmonic_Mean;

  function Harmonic_Mean(P: in PNums) return Num is
    N: Natural;
    Sm, X: Num;
    All_Equal: Boolean;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    if Minimum(P) < 0.0 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    All_Equal := True;
    X := P(P'First);
    Sm := 0.0;
    for I in P'Range loop
      if P(I) = 0.0 then
        Sm := Num'Last;
        exit;
      end if;
      if P(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + 1.0 / P(I);
    end loop;
    if Sm = Num'Last then
      return 0.0;
    elsif All_Equal then
      return X;
    end if;
    if Sm <= 0.0 then
      raise Statistics_Error;
    end if;
    return Num(N) / Sm;
  end Harmonic_Mean;

  function Harmonic_Mean(V, Wh: in Nums) return Num is
    Sm, W, X: Num;
    All_Equal: Boolean;
    I, Iw: Integer;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(V) then
      raise Negative_Value_Error;
    end if;
    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Harmonic_Mean(V);
    end if;

    All_Equal := True;
    X := V(V'First);
    Sm := 0.0;
    for C in 0..(V'Length-1) loop
      I := V'First + C;
      Iw := Wh'First + C;
      if V(I) = 0.0 then
        Sm := Num'Last;
        exit;
      end if;
      if V(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + Wh(Iw) / V(I);
    end loop;
    if Sm = Num'Last then
      return 0.0;
    elsif All_Equal then
      return X;
    end if;
    if Sm <= 0.0 then
      raise Statistics_Error;
    end if;
    return W / Sm;
  end Harmonic_Mean;

  function Harmonic_Mean(P, Wh: in PNums) return Num is
    Sm, W, X: Num;
    All_Equal: Boolean;
    I, Iw: Integer;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(P) then
      raise Negative_Value_Error;
    end if;
    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Harmonic_Mean(P);
    end if;

    All_Equal := True;
    X := P(P'First);
    Sm := 0.0;
    for C in 0..(P'Length-1) loop
      I := P'First + C;
      Iw := Wh'First + C;
      if P(I) = 0.0 then
        Sm := Num'Last;
        exit;
      end if;
      if P(I) /= X then
        All_Equal := False;
      end if;
      Sm := Sm + Wh(Iw) / P(I);
    end loop;
    if Sm = Num'Last then
      return 0.0;
    elsif All_Equal then
      return X;
    end if;
    if Sm <= 0.0 then
      raise Statistics_Error;
    end if;
    return W / Sm;
  end Harmonic_Mean;

  --------------------
  -- Central_Moment --
  --------------------

  function Central_Moment(V: in Nums; Degree: in Natural) return Num is
    N: Natural;
    Am, Sm: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    N := V'Length;
    if N = 1 or Degree = 1 then
      return 0.0;
    elsif Degree = 0 then
      return 1.0;
    end if;

    Am := Arithmetic_Mean(V);
    Sm := 0.0;
    for I in V'Range loop
      if V(I) /= Am then
        Sm := Sm + (V(I) - Am) ** Degree;
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return Sm / Num(N);
  end Central_Moment;

  function Central_Moment(P: in PNums; Degree: in Natural) return Num is
    N: Natural;
    Am, Sm: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    if N = 1 or Degree = 1 then
      return 0.0;
    elsif Degree = 0 then
      return 1.0;
    end if;

    Am := Arithmetic_Mean(P);
    Sm := 0.0;
    for I in P'Range loop
      if P(I) /= Am then
        Sm := Sm + (P(I) - Am) ** Degree;
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return Sm / Num(N);
  end Central_Moment;

  function Central_Moment(V, Wh: in Nums; Degree: in Natural) return Num is
    N: Natural;
    Am, Sm, W: Num;
    I, Iw: Integer;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Central_Moment(V, Degree);
    end if;

    N := V'Length;
    if N = 1 or Degree = 1 then
      return 0.0;
    elsif Degree = 0 then
      return 1.0;
    end if;

    Am := Arithmetic_Mean(V, Wh);
    Sm := 0.0;
    for C in 0..(V'Length-1) loop
      I := V'First + C;
      Iw := Wh'First + C;
      if V(I) /= Am and Wh(Iw) /= 0.0 then
        Sm := Sm + Wh(Iw) * ((V(I) - Am) ** Degree);
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return Sm / W;
  end Central_Moment;

  function Central_Moment(P, Wh: in PNums; Degree: in Natural) return Num is
    N: Natural;
    Am, Sm, W: Num;
    I, Iw: Integer;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W := Sum(Wh);
    if W = 0.0 then
      return Central_Moment(P, Degree);
    end if;

    N := P'Length;
    if N = 1 or Degree = 1 then
      return 0.0;
    elsif Degree = 0 then
      return 1.0;
    end if;

    Am := Arithmetic_Mean(P, Wh);
    Sm := 0.0;
    for C in 0..(P'Length-1) loop
      I := P'First + C;
      Iw := Wh'First + C;
      if P(I) /= Am then
        Sm := Sm + Wh(Iw) * ((P(I) - Am) ** Degree);
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return Sm / W;
  end Central_Moment;

  --------------
  -- Variance --
  --------------

  function Variance(V: in Nums) return Num is
    N: Natural;
    Cm2: Num;
  begin
    Cm2 := Central_Moment(V, 2);

    if Cm2 = 0.0 then
      return 0.0;
    end if;

    N := V'Length;
    return Num(N) / Num(N - 1) * Cm2;
  end Variance;

  function Variance(P: in PNums) return Num is
    N: Natural;
    Cm2: Num;
  begin
    Cm2 := Central_Moment(P, 2);

    if Cm2 = 0.0 then
      return 0.0;
    end if;

    N := P'Length;
    return Num(N) / Num(N - 1) * Cm2;
  end Variance;

  function Variance(V, Wh: in Nums) return Num is
    Cm2, W1, W2: Num;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    if W1 = 0.0 then
      return Variance(V);
    end if;

    W2 := Sum(Wh, 2);
    Cm2 := Central_Moment(V, Wh, 2);
    if Cm2 = 0.0 then
      return 0.0;
    end if;
    return (W1 * W1) / (W1 * W1 - W2) * Cm2;
  end Variance;

  function Variance(P, Wh: in PNums) return Num is
    Cm2, W1, W2: Num;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    if W1 = 0.0 then
      return Variance(P);
    end if;

    W2 := Sum(Wh, 2);
    Cm2 := Central_Moment(P, Wh, 2);
    if Cm2 = 0.0 then
      return 0.0;
    end if;
    return (W1 * W1) / (W1 * W1 - W2) * Cm2;
  end Variance;

  ------------------------
  -- Standard_Deviation --
  ------------------------

  function Standard_Deviation(V: in Nums) return Num is
    Var: Num;
  begin
    Var := Variance(V);
    if Var <= 0.0 then
      return 0.0;
    end if;
    return Sqrt(Var);
  end Standard_Deviation;

  function Standard_Deviation(P: in PNums) return Num is
    Var: Num;
  begin
    Var := Variance(P);
    if Var <= 0.0 then
      return 0.0;
    end if;
    return Sqrt(Var);
  end Standard_Deviation;

  function Standard_Deviation(V, Wh: in Nums) return Num is
    Var: Num;
  begin
    Var := Variance(V, Wh);
    if Var <= 0.0 then
      return 0.0;
    end if;
    return Sqrt(Var);
  end Standard_Deviation;

  function Standard_Deviation(P, Wh: in PNums) return Num is
    Var: Num;
  begin
    Var := Variance(P, Wh);
    if Var <= 0.0 then
      return 0.0;
    end if;
    return Sqrt(Var);
  end Standard_Deviation;

  --------------
  -- Skewness --
  --------------

  function Skewness(V: in Nums) return Num is
    N: Natural;
    Cm2, Cm3, K2, K3: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    N := V'Length;
    if N <= 2 then
      return 0.0;
    end if;

    Cm2 := Central_Moment(V, 2);
    Cm3 := Central_Moment(V, 3);
    if Cm3 = 0.0 then
      return 0.0;
    end if;
    K2 := Num(N) / Num(N - 1) * Cm2;
    K3 := (Num(N) / Num(N - 1)) * (Num(N) / Num(N - 2)) * Cm3;
    return K3 / (K2 ** (3.0 / 2.0));
  end Skewness;

  function Skewness(P: in PNums) return Num is
    N: Natural;
    Cm2, Cm3, K2, K3: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    if N <= 2 then
      return 0.0;
    end if;

    Cm2 := Central_Moment(P, 2);
    Cm3 := Central_Moment(P, 3);
    if Cm3 = 0.0 then
      return 0.0;
    end if;
    K2 := (Num(N) / Num(N - 1)) * Cm2;
    K3 := (Num(N) / Num(N - 1)) * (Num(N) / Num(N - 2)) * Cm3;
    return K3 / (K2 ** (3.0 / 2.0));
  end Skewness;

  function Skewness(V, Wh: in Nums) return Num is
    N: Natural;
    Cm2, Cm3, K2, K3, W1, W2, W3: Num;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    if W1 = 0.0 then
      return Skewness(V);
    end if;

    N := V'Length;
    if N <= 2 then
      return 0.0;
    end if;

    W2 := Sum(Wh, 2);
    W3 := Sum(Wh, 3);
    Cm2 := Central_Moment(V, Wh, 2);
    Cm3 := Central_Moment(V, Wh, 3);
    if Cm3 = 0.0 then
      return 0.0;
    end if;
    K2 := W1 * W1 / (W1 * W1 - W2) * Cm2;
    K3 := (W1 ** 3 / (W1 ** 3 - 3.0 * W1 * W2 + 2.0 * W3)) * Cm3;
    return K3 / (K2 ** (3.0 / 2.0));
  end Skewness;

  function Skewness(P, Wh: in PNums) return Num is
    N: Natural;
    Cm2, Cm3, K2, K3, W1, W2, W3: Num;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    if W1 = 0.0 then
      return Skewness(P);
    end if;

    N := P'Length;
    if N <= 2 then
      return 0.0;
    end if;

    W2 := Sum(Wh, 2);
    W3 := Sum(Wh, 3);
    Cm2 := Central_Moment(P, Wh, 2);
    Cm3 := Central_Moment(P, Wh, 3);
    if Cm3 = 0.0 then
      return 0.0;
    end if;
    K2 := W1 * W1 / (W1 * W1 - W2) * Cm2;
    K3 := (W1 ** 3 / (W1 ** 3 - 3.0 * W1 * W2 + 2.0 * W3)) * Cm3;
    return K3 / (K2 ** (3.0 / 2.0));
  end Skewness;

  --------------
  -- Kurtosis --
  --------------

  function Kurtosis(V: in Nums) return Num is
    N: Natural;
    Cm2, Cm4, K2, K4: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    N := V'Length;
    if N <= 3 then
      return 0.0;
    end if;

    Cm2 := Central_Moment(V, 2);
    Cm4 := Central_Moment(V, 4);
    if Cm4 = 0.0 then
      return 0.0;
    end if;
    K2 := Num(N) / Num(N - 1) * Cm2;
    K4 := (Num(N) / Num(N - 1)) * (Num(N) / Num(N - 2)) * (Num(N + 1) / Num(N - 3)) * Cm4;
    K4 := K4 - 3.0 * (Num(N) / Num(N - 2)) * (Num(N) / Num(N - 3)) * Cm2 * Cm2;
    return K4 / (K2 * K2);
  end Kurtosis;

  function Kurtosis(P: in PNums) return Num is
    N: Natural;
    Cm2, Cm4, K2, K4: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    if N <= 3 then
      return 0.0;
    end if;

    Cm2 := Central_Moment(P, 2);
    Cm4 := Central_Moment(P, 4);
    if Cm4 = 0.0 then
      return 0.0;
    end if;
    K2 := Num(N) / Num(N - 1) * Cm2;
    K4 := (Num(N) / Num(N - 1)) * (Num(N) / Num(N - 2)) * (Num(N + 1) / Num(N - 3)) * Cm4;
    K4 := K4 - 3.0 * (Num(N) / Num(N - 2)) * (Num(N) / Num(N - 3)) * Cm2 * Cm2;
    return K4 / (K2 * K2);
  end Kurtosis;

  function Kurtosis(V, Wh: in Nums) return Num is
    N: Natural;
    Cm2, Cm4, K2, K4, W1, W2, W3, W4: Num;
  begin
    if V'Length < 1 or V'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    if W1 = 0.0 then
      return Kurtosis(V);
    end if;

    N := V'Length;
    if N <= 3 then
      return 0.0;
    end if;

    W2 := Sum(Wh, 2);
    W3 := Sum(Wh, 3);
    W4 := Sum(Wh, 4);
    Cm2 := Central_Moment(V, Wh, 2);
    Cm4 := Central_Moment(V, Wh, 4);
    if Cm4 = 0.0 then
      return 0.0;
    end if;
    K2 := W1 * W1 / (W1 * W1 - W2) * Cm2;
    K4 := (W1 ** 4 - 4.0 * W1 * W3 + 3.0 * W2 * W2) * Cm4;
    K4 := K4 - 3.0 * (W1 ** 4 - 2.0 * W1 * W1 * W2 + 4.0 * W1 * W3 - 3.0 * W2 * W2) * Cm2 * Cm2;
    K4 := K4 / (W1 ** 4 - 6.0 * W1 * W1 * W2 + 8.0 * W1 * W3 + 3.0 * W2 * W2 - 6.0 * W4);
    K4 := K4 * (W1 * W1) / (W1 * W1 - W2);
    return K4 / (K2 * K2);
  end Kurtosis;

  function Kurtosis(P, Wh: in PNums) return Num is
    N: Natural;
    Cm2, Cm4, K2, K4, W1, W2, W3, W4: Num;
  begin
    if (P = null or Wh = null) or else (P'Length < 1 or P'Length /= Wh'Length) then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    if W1 = 0.0 then
      return Kurtosis(P);
    end if;

    N := P'Length;
    if N <= 3 then
      return 0.0;
    end if;

    W2 := Sum(Wh, 2);
    W3 := Sum(Wh, 3);
    W4 := Sum(Wh, 4);
    Cm2 := Central_Moment(P, Wh, 2);
    Cm4 := Central_Moment(P, Wh, 4);
    if Cm4 = 0.0 then
      return 0.0;
    end if;
    K2 := W1 * W1 / (W1 * W1 - W2) * Cm2;
    K4 := (W1 ** 4 - 4.0 * W1 * W3 + 3.0 * W2 * W2) * Cm4;
    K4 := K4 - 3.0 * (W1 ** 4 - 2.0 * W1 * W1 * W2 + 4.0 * W1 * W3 - 3.0 * W2 * W2) * Cm2 * Cm2;
    K4 := K4 / (W1 ** 4 - 6.0 * W1 * W1 * W2 + 8.0 * W1 * W3 + 3.0 * W2 * W2 - 6.0 * W4);
    K4 := K4 * (W1 * W1) / (W1 * W1 - W2);
    return K4 / (K2 * K2);
  end Kurtosis;

  ----------------
  -- Covariance --
  ----------------

  function Covariance(V1, V2: in Nums) return Num is
    N: Natural;
    Am1, Am2, Sm: Num;
    I1, I2: Integer;
  begin
    if V1'Length < 1 or V1'Length /= V2'Length then
      raise Statistics_Error;
    end if;

    N := V1'Length;
    if N = 1 then
      return 0.0;
    end if;

    Am1 := Arithmetic_Mean(V1);
    Am2 := Arithmetic_Mean(V2);
    Sm := 0.0;
    for C in 0..(V1'Length-1) loop
      I1 := V1'First + C;
      I2 := V2'First + C;
      if V1(I1) /= Am1 and V2(I2) /= Am2 then
        Sm := Sm + (V1(I1) - Am1) * (V2(I2) - Am2);
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return Sm / Num(N - 1);
  end Covariance;

  function Covariance(P1, P2: in PNums) return Num is
    N: Natural;
    Am1, Am2, Sm: Num;
    I1, I2: Integer;
  begin
    if P1 = null or else P1'Length < 1 then
      raise Statistics_Error;
    end if;
    if P2 = null or else P2'Length < 1 then
      raise Statistics_Error;
    end if;
    if P1'Length /= P2'Length then
      raise Statistics_Error;
    end if;

    N := P1'Length;
    if N = 1 then
      return 0.0;
    end if;

    Am1 := Arithmetic_Mean(P1);
    Am2 := Arithmetic_Mean(P2);
    Sm := 0.0;
    for C in 0..(P1'Length-1) loop
      I1 := P1'First + C;
      I2 := P2'First + C;
      if P1(I1) /= Am1 and P2(I2) /= Am2 then
        Sm := Sm + (P1(I1) - Am1) * (P2(I2) - Am2);
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return Sm / Num(N - 1);
  end Covariance;

  function Covariance(V1, V2, Wh: in Nums) return Num is
    N: Natural;
    Am1, Am2, Sm, W1, W2: Num;
    I1, I2, Iw: Integer;
  begin
    if V1'Length < 1 or V1'Length /= V2'Length or V1'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    if Has_Negative(Wh) then
      raise Negative_Value_Error;
    end if;

    W1 := Sum(Wh);
    W2 := Sum(Wh, 2);
    if W1 = 0.0 then
      return Covariance(V1, V2);
    end if;

    N := V1'Length;
    if N = 1 then
      return 0.0;
    end if;

    Am1 := Arithmetic_Mean(V1, Wh);
    Am2 := Arithmetic_Mean(V2, Wh);
    Sm := 0.0;
    for C in 0..(V1'Length-1) loop
      I1 := V1'First + C;
      I2 := V2'First + C;
      Iw := Wh'First + C;
      if Wh(Iw) /= 0.0 and V1(I1) /= Am1 and V2(I2) /= Am2 then
        Sm := Sm + Wh(Iw) * (V1(I1) - Am1) * (V2(I2) - Am2);
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return W1 / (W1 * W1 - W2) * Sm;
  end Covariance;

  function Covariance(P1, P2, Wh: in PNums) return Num is
    N: Natural;
    Am1, Am2, Sm, W1, W2: Num;
    I1, I2, Iw: Integer;
  begin
    if P1 = null or else P1'Length < 1 then
      raise Statistics_Error;
    end if;
    if P2 = null or else P2'Length < 1 then
      raise Statistics_Error;
    end if;
    if Wh = null or else Wh'Length < 1 then
      raise Statistics_Error;
    end if;
    if P1'Length /= P2'Length or P1'Length /= Wh'Length then
      raise Statistics_Error;
    end if;

    W1 := Sum(Wh);
    W2 := Sum(Wh, 2);
    if W1 = 0.0 then
      return Covariance(P1, P2);
    end if;

    N := P1'Length;
    if N = 1 then
      return 0.0;
    end if;

    Am1 := Arithmetic_Mean(P1, Wh);
    Am2 := Arithmetic_Mean(P2, Wh);
    Sm := 0.0;
    for C in 0..(P1'Length-1) loop
      I1 := P1'First + C;
      I2 := P2'First + C;
      Iw := Wh'First + C;
      if Wh(Iw) /= 0.0 and P1(I1) /= Am1 and P2(I2) /= Am2 then
        Sm := Sm + Wh(Iw) * (P1(I1) - Am1) * (P2(I2) - Am2);
      end if;
    end loop;
    if Sm = 0.0 then
      return 0.0;
    end if;
    return W1 / (W1 * W1 - W2) * Sm;
  end Covariance;

  -----------------
  -- Correlation --
  -----------------

  function Correlation(V1, V2: in Nums; Ct: in Correlation_Type) return Num is
  begin
    case Ct is
      when Pearson =>
        return Pearson_Correlation(V1, V2);
      when Spearman =>
        return Spearman_Correlation(V1, V2);
      when Kendall =>
        return Kendall_Correlation(V1, V2);
    end case;
  end Correlation;

  function Correlation(P1, P2: in PNums; Ct: in Correlation_Type) return Num is
  begin
    case Ct is
      when Pearson =>
        return Pearson_Correlation(P1, P2);
      when Spearman =>
        return Spearman_Correlation(P1, P2);
      when Kendall =>
        return Kendall_Correlation(P1, P2);
    end case;
  end Correlation;

  function Correlation(V1, V2, Wh: in Nums; Ct: in Correlation_Type) return Num is
  begin
    case Ct is
      when Pearson =>
        return Pearson_Correlation(V1, V2, Wh);
      when Spearman =>
        return Spearman_Correlation(V1, V2, Wh);
      when Kendall =>
        return Kendall_Correlation(V1, V2);
    end case;
  end Correlation;

  function Correlation(P1, P2, Wh: in PNums; Ct: in Correlation_Type) return Num is
  begin
    case Ct is
      when Pearson =>
        return Pearson_Correlation(P1, P2, Wh);
      when Spearman =>
        return Spearman_Correlation(P1, P2, Wh);
      when Kendall =>
        return Kendall_Correlation(P1, P2);
    end case;
  end Correlation;

  -------------------------
  -- Pearson_Correlation --
  -------------------------

  function Pearson_Correlation(V1, V2: in Nums) return Num is
    Corr, Cov, Std1, Std2: Num;
  begin
    Cov := Covariance(V1, V2);
    Std1 := Standard_Deviation(V1);
    Std2 := Standard_Deviation(V2);
    if Std1 = 0.0 or Std2 = 0.0 then
      return 1.0;
    elsif Cov = 0.0 then
      return 0.0;
    end if;
    Corr := Cov / (Std1 * Std2);
    if Corr >= 1.0 then
      return 1.0;
    elsif Corr <= -1.0 then
      return -1.0;
    end if;
    return Corr;
  end Pearson_Correlation;

  function Pearson_Correlation(P1, P2: in PNums) return Num is
    Corr, Cov, Std1, Std2: Num;
  begin
    Cov := Covariance(P1, P2);
    Std1 := Standard_Deviation(P1);
    Std2 := Standard_Deviation(P2);
    if Std1 = 0.0 or Std2 = 0.0 then
      return 1.0;
    elsif Cov = 0.0 then
      return 0.0;
    end if;
    Corr := Cov / (Std1 * Std2);
    if Corr >= 1.0 then
      return 1.0;
    elsif Corr <= -1.0 then
      return -1.0;
    end if;
    return Corr;
  end Pearson_Correlation;

  function Pearson_Correlation(V1, V2, Wh: in Nums) return Num is
    Corr, Cov, Std1, Std2: Num;
  begin
    Cov := Covariance(V1, V2, Wh);
    Std1 := Standard_Deviation(V1, Wh);
    Std2 := Standard_Deviation(V2, Wh);
    if Std1 = 0.0 or Std2 = 0.0 then
      return 1.0;
    elsif Cov = 0.0 then
      return 0.0;
    end if;
    Corr := Cov / (Std1 * Std2);
    if Corr >= 1.0 then
      return 1.0;
    elsif Corr <= -1.0 then
      return -1.0;
    end if;
    return Corr;
  end Pearson_Correlation;

  function Pearson_Correlation(P1, P2, Wh: in PNums) return Num is
    Corr, Cov, Std1, Std2: Num;
  begin
    Cov := Covariance(P1, P2, Wh);
    Std1 := Standard_Deviation(P1, Wh);
    Std2 := Standard_Deviation(P2, Wh);
    if Std1 = 0.0 or Std2 = 0.0 then
      return 1.0;
    elsif Cov = 0.0 then
      return 0.0;
    end if;
    Corr := Cov / (Std1 * Std2);
    if Corr >= 1.0 then
      return 1.0;
    elsif Corr <= -1.0 then
      return -1.0;
    end if;
    return Corr;
  end Pearson_Correlation;

  --------------------------
  -- Spearman_Correlation --
  --------------------------

  function Spearman_Correlation(V1, V2: in Nums) return Num is
  begin
    return Pearson_Correlation(Ranks(V1, Mean_Of_Tied => True), Ranks(V2, Mean_Of_Tied => True));
  end Spearman_Correlation;

  function Spearman_Correlation(P1, P2: in PNums) return Num is
    Rank1, Rank2: PNums;
    Corr: Num;
  begin
    Rank1 := Ranks(P1, Mean_Of_Tied => True);
    Rank2 := Ranks(P2, Mean_Of_Tied => True);
    Corr := Pearson_Correlation(Rank1, Rank2);
    Free(Rank1);
    Free(Rank2);
    return Corr;
  end Spearman_Correlation;

  function Spearman_Correlation(V1, V2, Wh: in Nums) return Num is
  begin
    return Pearson_Correlation(Ranks(V1, Mean_Of_Tied => True), Ranks(V2, Mean_Of_Tied => True), Wh);
  end Spearman_Correlation;

  function Spearman_Correlation(P1, P2, Wh: in PNums) return Num is
    Rank1, Rank2: PNums;
    Corr: Num;
  begin
    Rank1 := Ranks(P1, Mean_Of_Tied => True);
    Rank2 := Ranks(P2, Mean_Of_Tied => True);
    Corr := Pearson_Correlation(Rank1, Rank2, Wh);
    Free(Rank1);
    Free(Rank2);
    return Corr;
  end Spearman_Correlation;

  -------------------------
  -- Kendall_Correlation --
  -------------------------

  function Kendall_Correlation(V1, V2: in Nums) return Num is
    Off2: Integer;
    Perm, Temp: PIntegers;

    -- Utils for sorting
    function Lower_Than(Left, Right: in Integer) return Boolean is
    begin
      return V1(Left) < V1(Right) or else (V1(Left) = V1(Right) and then V2(Off2 + Left) < V2(Off2 + Right));
    end Lower_Than;

    package Arrays_Index is new Arrays_Utils(Integer, Integers, Integerss,
                                             PIntegers, PIntegerss, PsIntegers, PPsIntegers,
                                             Alloc, Alloc, Alloc, Lower_Than);
    use Arrays_Index;

    -- MergeSort with counting of Exchanges
    function Number_Of_Exchanges(First, Length: in Integer) return Longint is
      N_Exch: Longint;
      T, Length1, Length2, Middle, I, J, K, D: Integer;
    begin
      N_Exch := 0;
      if Length = 1 then
        return 0;
      elsif Length = 2 then
        if V2(Off2 + Perm(First)) <= V2(Off2 + Perm(First + 1)) then
          return 0;
        else
          T := Perm(First);
          Perm(First) := Perm(First + 1);
          Perm(First + 1) := T;
          return 1;
        end if;
      end if;
      -- Recursive division
      Length1 := Length / 2;
      Length2 := Length - Length1;
      Middle := First + Length1;
      N_Exch := N_Exch + Number_Of_Exchanges(First, Length1);
      N_Exch := N_Exch + Number_Of_Exchanges(Middle, Length2);
      if V2(Off2 + Perm(Middle - 1)) < V2(Off2 + Perm(Middle)) then
        return N_Exch;
      end if;
      -- Merge
      I := 0;
      J := 0;
      K := 0;
      while J < Length1 or else K < Length2 loop
        if K >= Length2 or else (J < Length1 and then V2(Off2 + Perm(First + J)) <= V2(Off2 + Perm(Middle + K))) then
          Temp(I) := Perm(First + J);
          D := I - J;
          J := J + 1;
        else
          Temp(I) := Perm(Middle + K);
          D := (First + I) - (Middle + K);
          K := K + 1;
        end if;
        if D > 0 then
          N_Exch := N_Exch + Longint(D);
        end if;
        I := I + 1;
      end loop;
      Perm(First..(First + Length - 1)) := Temp(0..(Length - 1));
      return N_Exch;
    end Number_Of_Exchanges;

    -- Cominatorial number
    function Combi2(M: in Natural) return Longint is
    begin
      if M <= 1 then
        return 0;
      elsif M mod 2 = 0 then
        return Longint(M / 2) * Longint(M - 1);
      else
        return Longint(M) * Longint((M - 1) / 2);
      end if;
    end Combi2;

    N, First: Integer;
    Joint_Ties, Ties_V1, Ties_V2, Exchanges, Total: Longint;
    Denom, Tau: Num;
  begin
    if V1'Length < 1 or V1'Length /= V2'Length then
      raise Statistics_Error;
    end if;

    N := V1'Length;
    Off2 := V2'First - V1'First;

    Perm := Alloc(V1'First, V1'Last);
    Temp := Alloc(0, N - 1);

    for I in Perm'Range loop
      Perm(I) := I;
    end loop;

    Sort(Perm);

    -- Calculate joint ties
    First := V1'First;
    Joint_Ties := 0;
    for I in (First + 1)..V1'Last loop
      if V1(Perm(First)) /= V1(Perm(I)) or else V2(Off2 + Perm(First)) /= V2(Off2 + Perm(I)) then
        Joint_Ties := Joint_Ties + Combi2(I - First);
        First := I;
      end if;
    end loop;
    Joint_Ties := Joint_Ties + Combi2(V1'Last + 1 - First);

    -- Calculate ties in V1
    First := V1'First;
    Ties_V1 := 0;
    for I in (First + 1)..V1'Last loop
      if V1(Perm(First)) /= V1(Perm(I)) then
        Ties_V1 := Ties_V1 + Combi2(I - First);
        First := I;
      end if;
    end loop;
    Ties_V1 := Ties_V1 + Combi2(V1'Last + 1 - First);

    -- Calculate exchanges while MergeSort
    Exchanges := Number_Of_Exchanges(V1'First, N);

    -- Calculate ties in V2
    First := V1'First;
    Ties_V2 := 0;
    for I in (First + 1)..V1'Last loop
      if V2(Off2 + Perm(First)) /= V2(Off2 + Perm(I)) then
        Ties_V2 := Ties_V2 + Combi2(I - First);
        First := I;
      end if;
    end loop;
    Ties_V2 := Ties_V2 + Combi2(V1'Last + 1 - First);

    -- Calculation of Kendell Tau
    Total := Combi2(N);
    if Total = Ties_V1 or Total = Ties_V2 then
      Tau := 1.0;
    else
      Denom := Exp(0.5 * (Log(Num(Total - Ties_V1)) + Log(Num(Total - Ties_V2))));
      Tau := Num(Total - (Ties_V1 + Ties_V2 - Joint_Ties) - 2 * Exchanges) / Denom;
    end if;

    Free(Temp);
    Free(Perm);

    return Tau;
  end Kendall_Correlation;

  function Kendall_Correlation(P1, P2: in PNums) return Num is
    Off2: Integer;
    Perm, Temp: PIntegers;

    -- Utils for sorting
    function Lower_Than(Left, Right: in Integer) return Boolean is
    begin
      return P1(Left) < P1(Right) or else (P1(Left) = P1(Right) and then P2(Off2 + Left) < P2(Off2 + Right));
    end Lower_Than;

    package Arrays_Index is new Arrays_Utils(Integer, Integers, Integerss,
                                             PIntegers, PIntegerss, PsIntegers, PPsIntegers,
                                             Alloc, Alloc, Alloc, Lower_Than);
    use Arrays_Index;

    -- MergeSort with counting of Exchanges
    function Number_Of_Exchanges(First, Length: in Integer) return Longint is
      N_Exch: Longint;
      T, Length1, Length2, Middle, I, J, K, D: Integer;
    begin
      N_Exch := 0;
      if Length = 1 then
        return 0;
      elsif Length = 2 then
        if P2(Off2 + Perm(First)) <= P2(Off2 + Perm(First + 1)) then
          return 0;
        else
          T := Perm(First);
          Perm(First) := Perm(First + 1);
          Perm(First + 1) := T;
          return 1;
        end if;
      end if;
      -- Recursive division
      Length1 := Length / 2;
      Length2 := Length - Length1;
      Middle := First + Length1;
      N_Exch := N_Exch + Number_Of_Exchanges(First, Length1);
      N_Exch := N_Exch + Number_Of_Exchanges(Middle, Length2);
      if P2(Off2 + Perm(Middle - 1)) < P2(Off2 + Perm(Middle)) then
        return N_Exch;
      end if;
      -- Merge
      I := 0;
      J := 0;
      K := 0;
      while J < Length1 or else K < Length2 loop
        if K >= Length2 or else (J < Length1 and then P2(Off2 + Perm(First + J)) <= P2(Off2 + Perm(Middle + K))) then
          Temp(I) := Perm(First + J);
          D := I - J;
          J := J + 1;
        else
          Temp(I) := Perm(Middle + K);
          D := (First + I) - (Middle + K);
          K := K + 1;
        end if;
        if D > 0 then
          N_Exch := N_Exch + Longint(D);
        end if;
        I := I + 1;
      end loop;
      Perm(First..(First + Length - 1)) := Temp(0..(Length - 1));
      return N_Exch;
    end Number_Of_Exchanges;

    -- Cominatorial number
    function Combi2(M: in Natural) return Longint is
    begin
      if M <= 1 then
        return 0;
      elsif M mod 2 = 0 then
        return Longint(M / 2) * Longint(M - 1);
      else
        return Longint(M) * Longint((M - 1) / 2);
      end if;
    end Combi2;

    N, First: Integer;
    Joint_Ties, Ties_P1, Ties_P2, Exchanges, Total: Longint;
    Denom, Tau: Num;
  begin
    if (P1 = null or P2 = null) or else (P1'Length < 1 or P1'Length /= P2'Length) then
      raise Statistics_Error;
    end if;

    N := P1'Length;
    Off2 := P2'First - P1'First;

    Perm := Alloc(P1'First, P1'Last);
    Temp := Alloc(0, N - 1);

    for I in Perm'Range loop
      Perm(I) := I;
    end loop;

    Sort(Perm);

    -- Calculate joint ties
    First := P1'First;
    Joint_Ties := 0;
    for I in (First + 1)..P1'Last loop
      if P1(Perm(First)) /= P1(Perm(I)) or else P2(Off2 + Perm(First)) /= P2(Off2 + Perm(I)) then
        Joint_Ties := Joint_Ties + Combi2(I - First);
        First := I;
      end if;
    end loop;
    Joint_Ties := Joint_Ties + Combi2(P1'Last + 1 - First);

    -- Calculate ties in P1
    First := P1'First;
    Ties_P1 := 0;
    for I in (First + 1)..P1'Last loop
      if P1(Perm(First)) /= P1(Perm(I)) then
        Ties_P1 := Ties_P1 + Combi2(I - First);
        First := I;
      end if;
    end loop;
    Ties_P1 := Ties_P1 + Combi2(P1'Last + 1 - First);

    -- Calculate exchanges while MergeSort
    Exchanges := Number_Of_Exchanges(P1'First, N);

    -- Calculate ties in P2
    First := P1'First;
    Ties_P2 := 0;
    for I in (First + 1)..P1'Last loop
      if P2(Off2 + Perm(First)) /= P2(Off2 + Perm(I)) then
        Ties_P2 := Ties_P2 + Combi2(I - First);
        First := I;
      end if;
    end loop;
    Ties_P2 := Ties_P2 + Combi2(P1'Last + 1 - First);

    -- Calculation of Kendell Tau
    Total := Combi2(N);
    if Total = Ties_P1 or Total = Ties_P2 then
      Tau := 1.0;
    else
      Denom := Exp(0.5 * (Log(Num(Total - Ties_P1)) + Log(Num(Total - Ties_P2))));
      Tau := Num(Total - (Ties_P1 + Ties_P2 - Joint_Ties) - 2 * Exchanges) / Denom;
    end if;

    Free(Temp);
    Free(Perm);

    return Tau;
  end Kendall_Correlation;

  -----------------------
  -- Correlation_Error --
  -----------------------

  function Correlation_Error(V1, V2: in Nums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num is
    type Calculation is access function(V1, V2: in Nums) return Num;
    Correlation_Calculation: Calculation;
    N: Natural;
    Cest: Correlation_Error_Subtype;
    Corr, Err: Num;
  begin
    case Ct is
      when Pearson =>
        Correlation_Calculation := Pearson_Correlation'Access;
      when Spearman =>
        Correlation_Calculation := Spearman_Correlation'Access;
      when Kendall =>
        Correlation_Calculation := Kendall_Correlation'Access;
    end case;

    Corr := Correlation_Calculation(V1, V2);

    N := V1'Length;
    if N <= 2 then
      return 0.0;
    end if;

    if Cet = Auto then
      Cest := Select_Correlation_Error_Subtype(N);
    else
      Cest := Cet;
    end if;

    case Cest is
      when Jackknife =>
        declare
          Vi1: Nums((V1'First+1)..V1'Last);
          Vi2: Nums((V2'First+1)..V2'Last);
          Corri: Num;
          I1, I2: Integer;
        begin
          Vi1 := V1(Vi1'Range);
          Vi2 := V2(Vi2'Range);
          Err := 0.0;
          for C in 0..(N-1) loop
            if C > 0 then
              I1 := V1'First + C;
              I2 := V2'First + C;
              Vi1(I1) := V1(I1 - 1);
              Vi2(I2) := V2(I2 - 1);
            end if;
            Corri := Correlation_Calculation(Vi1, Vi2);
            if Corri /= Corr then
              Err := Err + (Corri - Corr) ** 2;
            end if;
          end loop;
          if Err > 0.0 then
            Err := Sqrt(Num(N - 1) / Num(N) * Err);
          end if;
        end;

      when Bootstrap =>
        declare
          Vi1: Nums(V1'Range);
          Vi2: Nums(V2'Range);
          Corrs: Nums(1..Bootstrap_Size);
          Percentile: Nums(1..100);
          Cj: Natural;
          I1, I2, J1, J2: Integer;
        begin
          Err := 0.0;
          for B in Corrs'Range loop
            for Ci in 0..(N-1) loop
              Cj := Random_Uniform(Bootstrap_G, 0, N - 1);
              I1 := Vi1'First + Ci;
              I2 := Vi2'First + Ci;
              J1 := V1'First + Cj;
              J2 := V2'First + Cj;
              Vi1(I1) := V1(J1);
              Vi2(I2) := V2(J2);
            end loop;
            Corrs(B) := Correlation_Calculation(Vi1, Vi2);
          end loop;
          Percentile := Percentiles(Corrs);
          Err := (Percentile(84) - Percentile(16)) / 2.0;
        end;

      when Fisher_Transform =>
        Err := Fisher_Tranform_Sigma(Corr, N);
    end case;

    return Err;
  end Correlation_Error;

  function Correlation_Error(P1, P2: in PNums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num is
    type Calculation is access function(P1, P2: in PNums) return Num;
    Correlation_Calculation: Calculation;
    N: Natural;
    Cest: Correlation_Error_Subtype;
    Corr, Err: Num;
  begin
    case Ct is
      when Pearson =>
        Correlation_Calculation := Pearson_Correlation'Access;
      when Spearman =>
        Correlation_Calculation := Spearman_Correlation'Access;
      when Kendall =>
        Correlation_Calculation := Kendall_Correlation'Access;
    end case;

    Corr := Correlation_Calculation(P1, P2);

    N := P1'Length;
    if N <= 2 then
      return 0.0;
    end if;

    if Cet = Auto then
      Cest := Select_Correlation_Error_Subtype(N);
    else
      Cest := Cet;
    end if;

    case Cest is
      when Jackknife =>
        declare
          Pi1: PNums;
          Pi2: PNums;
          Corri: Num;
          I1, I2: Integer;
        begin
          Pi1 := Alloc(P1'First + 1, P1'Last);
          Pi2 := Alloc(P2'First + 1, P2'Last);
          Pi1.all := P1(Pi1'Range);
          Pi2.all := P2(Pi2'Range);
          Err := 0.0;
          for C in 0..(N-1) loop
            if C > 0 then
              I1 := P1'First + C;
              I2 := P2'First + C;
              Pi1(I1) := P1(I1 - 1);
              Pi2(I2) := P2(I2 - 1);
            end if;
            Corri := Correlation_Calculation(Pi1, Pi2);
            if Corri /= Corr then
              Err := Err + (Corri - Corr) ** 2;
            end if;
          end loop;
          if Err > 0.0 then
            Err := Sqrt(Num(N - 1) / Num(N) * Err);
          end if;
          Free(Pi1);
          Free(Pi2);
        end;

      when Bootstrap =>
        declare
          Pi1: PNums;
          Pi2: PNums;
          Corrs: PNums;
          Percentile: PNums;
          Cj: Natural;
          I1, I2, J1, J2: Integer;
        begin
          Pi1 := Alloc(P1'First, P1'Last);
          Pi2 := Alloc(P2'First, P2'Last);
          Corrs := Alloc(1, Bootstrap_Size);
          for B in Corrs'Range loop
            for Ci in 0..(N-1) loop
              Cj := Random_Uniform(Bootstrap_G, 0, N - 1);
              I1 := Pi1'First + Ci;
              I2 := Pi2'First + Ci;
              J1 := P1'First + Cj;
              J2 := P2'First + Cj;
              Pi1(I1) := P1(J1);
              Pi2(I2) := P2(J2);
            end loop;
            Corrs(B) := Correlation_Calculation(Pi1, Pi2);
          end loop;
          Percentile := Percentiles(Corrs);
          Err := (Percentile(84) - Percentile(16)) / 2.0;
          Free(Percentile);
          Free(Corrs);
          Free(Pi1);
          Free(Pi2);
        end;

      when Fisher_Transform =>
        Err := Fisher_Tranform_Sigma(Corr, N);
    end case;

    return Err;
  end Correlation_Error;

  function Correlation_Error(V1, V2, Wh: in Nums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num is
    N: Natural;
    Cest: Correlation_Error_Subtype;
    Corr, Err: Num;
  begin
    Corr := Correlation(V1, V2, Wh, Ct);

    N := V1'Length;
    if N <= 2 then
      return 0.0;
    end if;

    if Cet = Auto then
      Cest := Select_Correlation_Error_Subtype(N);
    else
      Cest := Cet;
    end if;

    case Cest is
      when Jackknife =>
        declare
          Vi1: Nums((V1'First+1)..V1'Last);
          Vi2: Nums((V2'First+1)..V2'Last);
          Whi: Nums((Wh'First+1)..Wh'Last);
          Corri: Num;
          I1, I2, Iw: Integer;
        begin
          Vi1 := V1(Vi1'Range);
          Vi2 := V2(Vi2'Range);
          Whi := Wh(Whi'Range);
          Err := 0.0;
          for C in 0..(N-1) loop
            if C > 0 then
              I1 := V1'First + C;
              I2 := V2'First + C;
              Iw := Wh'First + C;
              Vi1(I1) := V1(I1 - 1);
              Vi2(I2) := V2(I2 - 1);
              Whi(Iw) := Wh(Iw - 1);
            end if;
            Corri := Correlation(Vi1, Vi2, Whi, Ct);
            if Corri /= Corr then
              Err := Err + (Corri - Corr) ** 2;
            end if;
          end loop;
          if Err > 0.0 then
            Err := Sqrt(Num(N - 1) / Num(N) * Err);
          end if;
        end;

      when Bootstrap =>
        declare
          Vi1: Nums(V1'Range);
          Vi2: Nums(V2'Range);
          Whi: Nums(Wh'Range);
          Corrs: Nums(1..Bootstrap_Size);
          Percentile: Nums(1..100);
          Cj: Natural;
          I1, I2, Iw, J1, J2, Jw: Integer;
        begin
          Err := 0.0;
          for B in Corrs'Range loop
            for Ci in 0..(N-1) loop
              Cj := Random_Uniform(Bootstrap_G, 0, N - 1);
              I1 := Vi1'First + Ci;
              I2 := Vi2'First + Ci;
              Iw := Whi'First + Ci;
              J1 := V1'First + Cj;
              J2 := V2'First + Cj;
              Jw := Wh'First + Cj;
              Vi1(I1) := V1(J1);
              Vi2(I2) := V2(J2);
              Whi(Iw) := Wh(Jw);
            end loop;
            Corrs(B) := Correlation(Vi1, Vi2, Whi, Ct);
          end loop;
          Percentile := Percentiles(Corrs);
          Err := (Percentile(84) - Percentile(16)) / 2.0;
        end;

      when Fisher_Transform =>
        Err := Fisher_Tranform_Sigma(Corr, N);
    end case;

    return Err;
  end Correlation_Error;

  function Correlation_Error(P1, P2, Wh: in PNums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num is
    N: Natural;
    Cest: Correlation_Error_Subtype;
    Corr, Err: Num;
  begin
    Corr := Correlation(P1, P2, Wh, Ct);

    N := P1'Length;
    if N <= 2 then
      return 0.0;
    end if;

    if Cet = Auto then
      Cest := Select_Correlation_Error_Subtype(N);
    else
      Cest := Cet;
    end if;

    case Cest is
      when Jackknife =>
        declare
          Pi1: PNums;
          Pi2: PNums;
          Whi: PNums;
          Corri: Num;
          I1, I2, Iw: Integer;
        begin
          Pi1 := Alloc(P1'First + 1, P1'Last);
          Pi2 := Alloc(P2'First + 1, P2'Last);
          Whi := Alloc(Wh'First + 1, Wh'Last);
          Pi1.all := P1(Pi1'Range);
          Pi2.all := P2(Pi2'Range);
          Whi.all := Wh(Whi'Range);
          Err := 0.0;
          for C in 0..(N-1) loop
            if C > 0 then
              I1 := P1'First + C;
              I2 := P2'First + C;
              Iw := Wh'First + C;
              Pi1(I1) := P1(I1 - 1);
              Pi2(I2) := P2(I2 - 1);
              Whi(Iw) := Wh(Iw - 1);
            end if;
            Corri := Correlation(Pi1, Pi2, Whi, Ct);
            if Corri /= Corr then
              Err := Err + (Corri - Corr) ** 2;
            end if;
          end loop;
          if Err > 0.0 then
            Err := Sqrt(Num(N - 1) / Num(N) * Err);
          end if;
          Free(Pi1);
          Free(Pi2);
          Free(Whi);
        end;

      when Bootstrap =>
        declare
          Pi1: PNums;
          Pi2: PNums;
          Whi: PNums;
          Corrs: PNums;
          Percentile: PNums;
          Cj: Natural;
          I1, I2, Iw, J1, J2, Jw: Integer;
        begin
          Pi1 := Alloc(P1'First, P1'Last);
          Pi2 := Alloc(P2'First, P2'Last);
          Whi := Alloc(Wh'First, Wh'Last);
          Corrs := Alloc(1, Bootstrap_Size);
          for B in Corrs'Range loop
            for Ci in 0..(N-1) loop
              Cj := Random_Uniform(Bootstrap_G, 0, N - 1);
              I1 := Pi1'First + Ci;
              I2 := Pi2'First + Ci;
              Iw := Whi'First + Ci;
              J1 := P1'First + Cj;
              J2 := P2'First + Cj;
              Jw := Wh'First + Cj;
              Pi1(I1) := P1(J1);
              Pi2(I2) := P2(J2);
              Whi(Iw) := Wh(Jw);
            end loop;
            Corrs(B) := Correlation(Pi1, Pi2, Whi, Ct);
          end loop;
          Percentile := Percentiles(Corrs);
          Err := (Percentile(84) - Percentile(16)) / 2.0;
          Free(Percentile);
          Free(Corrs);
          Free(Pi1);
          Free(Pi2);
          Free(Whi);
        end;

      when Fisher_Transform =>
        Err := Fisher_Tranform_Sigma(Corr, N);
    end case;

    return Err;
  end Correlation_Error;

  -------------------------------
  -- Pearson_Correlation_Error --
  -------------------------------

  function Pearson_Correlation_Error(V1, V2: in Nums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(V1, V2, Pearson, Cet);
  end Pearson_Correlation_Error;

  function Pearson_Correlation_Error(P1, P2: in PNums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(P1, P2, Pearson, Cet);
  end Pearson_Correlation_Error;

  function Pearson_Correlation_Error(V1, V2, Wh: in Nums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(V1, V2, Wh, Pearson, Cet);
  end Pearson_Correlation_Error;

  function Pearson_Correlation_Error(P1, P2, Wh: in PNums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(P1, P2, Wh, Pearson, Cet);
  end Pearson_Correlation_Error;

  --------------------------------
  -- Spearman_Correlation_Error --
  --------------------------------

  function Spearman_Correlation_Error(V1, V2: in Nums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(V1, V2, Spearman, Cet);
  end Spearman_Correlation_Error;

  function Spearman_Correlation_Error(P1, P2: in PNums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(P1, P2, Spearman, Cet);
  end Spearman_Correlation_Error;

  function Spearman_Correlation_Error(V1, V2, Wh: in Nums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(V1, V2, Wh, Spearman, Cet);
  end Spearman_Correlation_Error;

  function Spearman_Correlation_Error(P1, P2, Wh: in PNums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(P1, P2, Wh, Spearman, Cet);
  end Spearman_Correlation_Error;

  --------------------------------
  -- Kendall_Correlation_Error --
  --------------------------------

  function Kendall_Correlation_Error(V1, V2: in Nums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(V1, V2, Kendall, Cet);
  end Kendall_Correlation_Error;

  function Kendall_Correlation_Error(P1, P2: in PNums; Cet: in Correlation_Error_Type := Auto) return Num is
  begin
    return Correlation_Error(P1, P2, Kendall, Cet);
  end Kendall_Correlation_Error;

  ------------------------------
  -- Simple_Linear_Regression --
  ------------------------------

  procedure Simple_Linear_Regression(X, Y: in Nums; Slope, Intercept: out Num) is
    Cov_XY, Var_X, Am_X, Am_Y: Num;
  begin
    Cov_XY := Covariance(X, Y);
    Var_X := Variance(X);
    Am_X := Arithmetic_Mean(X);
    Am_Y := Arithmetic_Mean(Y);
    if Var_X = 0.0 then
      Slope := 0.0;
      Intercept := Am_Y;
    else
      Slope := Cov_XY / Var_X;
      Intercept := Am_Y - Slope * Am_X;
    end if;
  end Simple_Linear_Regression;

  procedure Simple_Linear_Regression(X, Y: in PNums; Slope, Intercept: out Num) is
    Cov_XY, Var_X, Am_X, Am_Y: Num;
  begin
    Cov_XY := Covariance(X, Y);
    Var_X := Variance(X);
    Am_X := Arithmetic_Mean(X);
    Am_Y := Arithmetic_Mean(Y);
    if Var_X = 0.0 then
      Slope := 0.0;
      Intercept := Am_Y;
    else
      Slope := Cov_XY / Var_X;
      Intercept := Am_Y - Slope * Am_X;
    end if;
  end Simple_Linear_Regression;

  -----------------
  -- Percentiles --
  -----------------

  function Percentiles(V: in Nums) return Nums is
    package Minheaps_Num is new Minheaps(Num); use Minheaps_Num;
    N, Pos: Positive;
    Sorted, Percent: Nums(1..V'Length);
    Percentils: Nums(1..100);
    Minh: Minheap;
    V_K, V_K1, P_K: Num;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    N := V'Length;

    Initialize(Minh, N);
    for I in V'Range loop
      Add(V(I), Minh);
    end loop;
    for I in Sorted'Range loop
      Sorted(I) := Delete_Minimum(Minh);
    end loop;
    Free(Minh);

    for I in Percent'Range loop
      Percent(I) := 100.0 * (Num(I) - 0.5) / Num(N);
    end loop;

    for I in Percentils'Range loop
      if Num(I) <= Percent(1) then
        Percentils(I) := Sorted(1);
      elsif Num(I) >= Percent(N) then
        Percentils(I) := Sorted(N);
      else
        Pos:= 1;
        while Pos in Percent'Range loop
          if Num(I) < Percent(Pos + 1) then
            exit;
          end if;
          Pos := Pos + 1;
        end loop;
        V_K := Sorted(Pos);
        V_K1 := Sorted(Pos + 1);
        P_K := Percent(Pos);
        Percentils(I) := V_K + (Num(N) * (Num(I) - P_K) * (V_K1 - V_K)) / 100.0;
      end if;
    end loop;

    return Percentils;
  end Percentiles;

  function Percentiles(P: in PNums) return PNums is
    package Minheaps_Num is new Minheaps(Num); use Minheaps_Num;
    N, Pos: Positive;
    Sorted, Percent, Percentils: PNums;
    Minh: Minheap;
    V_K, V_K1, P_K: Num;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    Sorted := Alloc(1, N);
    Percent := Alloc(1, N);

    Initialize(Minh, N);
    for I in P'Range loop
      Add(P(I), Minh);
    end loop;
    for I in Sorted'Range loop
      Sorted(I) := Delete_Minimum(Minh);
    end loop;
    Free(Minh);

    for I in Percent'Range loop
      Percent(I) := 100.0 * (Num(I) - 0.5) / Num(N);
    end loop;

    Percentils := Alloc(1, 100);
    for I in Percentils'Range loop
      if Num(I) <= Percent(1) then
        Percentils(I) := Sorted(1);
      elsif Num(I) >= Percent(N) then
        Percentils(I) := Sorted(N);
      else
        Pos:= 1;
        while Pos in Percent'Range loop
          if Num(I) < Percent(Pos + 1) then
            exit;
          end if;
          Pos := Pos + 1;
        end loop;
        V_K := Sorted(Pos);
        V_K1 := Sorted(Pos + 1);
        P_K := Percent(Pos);
        Percentils(I) := V_K + (Num(N) * (Num(I) - P_K) * (V_K1 - V_K)) / 100.0;
      end if;
    end loop;

    Free(Sorted);
    Free(Percent);
    return Percentils;
  end Percentiles;

  -----------
  -- Ranks --
  -----------

  function Ranks(V: in Nums; Mean_Of_Tied: in Boolean := False) return Nums is
    type Pair is record
      Val: Num;
      Pos: Integer;
    end record;

    function "<"(E1: in Pair; E2: in Pair) return Boolean is
    begin
      return E1.Val < E2.Val;
    end "<";

    package Minheaps_Pair is new Minheaps(Pair); use Minheaps_Pair;

    N, Rk: Positive;
    Nqe: Natural;
    Pos: Integer;
    Rank: Nums(V'Range);
    Pr: Pair;
    Vprev, Rknum: Num;
    Minh: Minheap;
    Qe: Queue;
  begin
    if V'Length < 1 then
      raise Statistics_Error;
    end if;

    N := V'Length;

    Initialize(Qe);
    Initialize(Minh, N);
    for I in V'Range loop
      Pr := (V(I), I);
      Add(Pr, Minh);
    end loop;

    Vprev := 0.0;
    if Minimum(Minh).Val = 0.0 then
      Vprev := 1.0;
    end if;
    Nqe := 0;
    Rk := 1;
    for I in V'Range loop
      Pr := Delete_Minimum(Minh);
      Nqe := Size(Qe);
      if Pr.Val /= Vprev and Nqe > 0 then
        Rknum := Num(Rk);
        if Mean_Of_Tied then
          if Nqe mod 2 = 0 then
            Rknum := Num(Rk + (Nqe - 1) / 2) + 0.5;
          else
            Rknum := Num(Rk + (Nqe - 1) / 2);
          end if;
        end if;
        while not Is_Empty(Qe) loop
          Pos := Dequeue(Qe);
          Rank(Pos) := Rknum;
        end loop;
        Rk := Rk + Nqe;
      end if;
      Enqueue(Pr.Pos, Qe);
      Vprev := Pr.Val;
    end loop;
    Nqe := Size(Qe);
    Rknum := Num(Rk);
    if Mean_Of_Tied then
      if Nqe mod 2 = 0 then
        Rknum := Num(Rk + (Nqe - 1) / 2) + 0.5;
      else
        Rknum := Num(Rk + (Nqe - 1) / 2);
      end if;
    end if;
    while not Is_Empty(Qe) loop
      Pos := Dequeue(Qe);
      Rank(Pos) := Rknum;
    end loop;
    Free(Minh);
    Free(Qe);

    return Rank;
  end Ranks;

  function Ranks(P: in PNums; Mean_Of_Tied: in Boolean := False) return PNums is
    type Pair is record
      Val: Num;
      Pos: Integer;
    end record;

    function "<"(E1: in Pair; E2: in Pair) return Boolean is
    begin
      return E1.Val < E2.Val;
    end "<";

    package Minheaps_Pair is new Minheaps(Pair); use Minheaps_Pair;

    N, Rk: Positive;
    Nqe: Natural;
    Pos: Integer;
    Rank: PNums;
    Pr: Pair;
    Vprev, Rknum: Num;
    Minh: Minheap;
    Qe: Queue;
  begin
    if P = null or else P'Length < 1 then
      raise Statistics_Error;
    end if;

    N := P'Length;
    Rank := Alloc(P'First, P'Last);

    Initialize(Qe);
    Initialize(Minh, N);
    for I in P'Range loop
      Pr := (P(I), I);
      Add(Pr, Minh);
    end loop;

    Vprev := 0.0;
    if Minimum(Minh).Val = 0.0 then
      Vprev := 1.0;
    end if;
    Nqe := 0;
    Rk := 1;
    for I in P'Range loop
      Pr := Delete_Minimum(Minh);
      Nqe := Size(Qe);
      if Pr.Val /= Vprev and Nqe > 0 then
        Rknum := Num(Rk);
        if Mean_Of_Tied then
          if Nqe mod 2 = 0 then
            Rknum := Num(Rk + (Nqe - 1) / 2) + 0.5;
          else
            Rknum := Num(Rk + (Nqe - 1) / 2);
          end if;
        end if;
        while not Is_Empty(Qe) loop
          Pos := Dequeue(Qe);
          Rank(Pos) := Rknum;
        end loop;
        Rk := Rk + Nqe;
      end if;
      Enqueue(Pr.Pos, Qe);
      Vprev := Pr.Val;
    end loop;
    Nqe := Size(Qe);
    Rknum := Num(Rk);
    if Mean_Of_Tied then
      if Nqe mod 2 = 0 then
        Rknum := Num(Rk + (Nqe - 1) / 2) + 0.5;
      else
        Rknum := Num(Rk + (Nqe - 1) / 2);
      end if;
    end if;
    while not Is_Empty(Qe) loop
      Pos := Dequeue(Qe);
      Rank(Pos) := Rknum;
    end loop;
    Free(Minh);
    Free(Qe);

    return Rank;
  end Ranks;

  ------------------------
  -- Get_Bootstrap_Size --
  ------------------------

  function Get_Bootstrap_Size return Positive is
  begin
    return Bootstrap_Size;
  end Get_Bootstrap_Size;

  ------------------------
  -- Set_Bootstrap_Size --
  ------------------------

  procedure Set_Bootstrap_Size(Bs: in Positive := Default_Bootstrap_Size) is
  begin
    Bootstrap_Size := Bs;
  end Set_Bootstrap_Size;

  --------------------------------------
  -- Select_Correlation_Error_Subtype --
  --------------------------------------

  function Select_Correlation_Error_Subtype(N: in Natural) return Correlation_Error_Subtype is
  begin
    if N < 10_000_000 then
      return Bootstrap;
    else
      return Fisher_Transform;
    end if;
  end Select_Correlation_Error_Subtype;

  ---------------------------
  -- Fisher_Tranform_Sigma --
  ---------------------------

  function Fisher_Tranform_Sigma(Corr: in Num; N: in Natural) return Num is
    Fr, Dfr, Cp, Cm: Num;
  begin
    if Corr >= 1.0 or Corr <= -1.0 or N <= 3 then
      return 0.0;
    end if;
    Fr := Arctanh(Corr);
    Dfr := 1.0 / Sqrt(Num(N - 3));
    Cp := Tanh(Fr + Dfr);
    Cm := Tanh(Fr - Dfr);
    return (Cp - Cm) / 2.0;
  end Fisher_Tranform_Sigma;

begin
  Reset(Bootstrap_G);
end Statistics;
