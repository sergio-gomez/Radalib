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


-- @filename Histograms.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 7/02/2005
-- @revision 08/04/2012
-- @brief Treatment of Histograms

with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Histograms is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Histogram_Rec, Histogram);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(H: out Histogram; Lower_Limit, Upper_Limit: in Float; Num_Bins: in Positive; Logarithmic: Boolean := False) is
  begin
    if Lower_Limit >= Upper_Limit then
      raise Incompatible_Parameters_Error;
    end if;
    if Logarithmic and Lower_Limit <= 0.0 then
      raise Incompatible_Parameters_Error;
    end if;

    H := new Histogram_Rec(Num_Bins);

    H.Accumulated := False;
    H.Logarithmic := Logarithmic;
    if Logarithmic then
      H.Lower_Limit := Log(Lower_Limit);
      H.Upper_Limit := Log(Upper_Limit);
    else
      H.Lower_Limit := Lower_Limit;
      H.Upper_Limit := Upper_Limit;
    end if;
    for I in H.Bin'Range loop
      H.Bin(I) := 0;
    end loop;
    H.Under_Lower_Limit := 0;
    H.Over_Upper_Limit := 0;
    H.Num_Values := 0;

  end Initialize;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(H: out Histogram; Lower_Limit: in Float; Num_Bins: in Positive; Bin_Size: in Float) is
  begin
    if Bin_Size <= 0.0 then
      raise Incompatible_Parameters_Error;
    end if;

    H := new Histogram_Rec(Num_Bins);

    H.Accumulated := False;
    H.Logarithmic := False;
    H.Lower_Limit := Lower_Limit;
    H.Upper_Limit := Lower_Limit + Float(Num_Bins) * Bin_Size;
    for I in H.Bin'Range loop
      H.Bin(I) := 0;
    end loop;
    H.Under_Lower_Limit := 0;
    H.Over_Upper_Limit := 0;
    H.Num_Values := 0;

  end Initialize;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(H: out Histogram; Lower_Value: in Integer; Num_Bins, Bin_Size: in Positive) is
  begin
    H := new Histogram_Rec(Num_Bins);

    H.Accumulated := False;
    H.Logarithmic := False;
    H.Lower_Limit := Float(Lower_Value) - 0.5;
    H.Upper_Limit := H.Lower_Limit + Float(Num_Bins * Bin_Size);
    for I in H.Bin'Range loop
      H.Bin(I) := 0;
    end loop;
    H.Under_Lower_Limit := 0;
    H.Over_Upper_Limit := 0;
    H.Num_Values := 0;

  end Initialize;

  ----------
  -- Free --
  ----------

  procedure Free(H: in out Histogram) is
  begin
    if H /= null then
      Dispose(H);
      H := null;
    end if;
  end Free;

  --------------------
  -- Is_Logarithmic --
  --------------------

  function Is_Logarithmic(H: in Histogram) return Boolean is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    return H.Logarithmic;
  end Is_Logarithmic;

  ------------------
  -- Get_Num_Bins --
  ------------------

  function Get_Num_Bins(H: in Histogram) return Positive is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    return H.Num_Bins;
  end Get_Num_Bins;

  ---------------
  -- Get_Limit --
  ---------------

  function Get_Limit(H: in Histogram; I: in Natural) return Float is
    Limit: Float;
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;
    if I > H.Num_Bins then
      raise Index_Out_Of_Bounds_Error;
    end if;

    Limit := H.Lower_Limit + (Float(I) / Float(H.Num_Bins)) * (H.Upper_Limit - H.Lower_Limit);
    if H.Logarithmic then
      Limit := Exp(Limit);
    end if;

    return Limit;
  end Get_Limit;

  ---------------------
  -- Get_Lower_Limit --
  ---------------------

  function Get_Lower_Limit(H: in Histogram) return Float is
    Limit: Float;
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    Limit := H.Lower_Limit;
    if H.Logarithmic then
      Limit := Exp(Limit);
    end if;

    return Limit;
  end Get_Lower_Limit;

  ---------------------
  -- Get_Upper_Limit --
  ---------------------

  function Get_Upper_Limit(H: in Histogram) return Float is
    Limit: Float;
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    Limit := H.Upper_Limit;
    if H.Logarithmic then
      Limit := Exp(Limit);
    end if;

    return Limit;
  end Get_Upper_Limit;

  ---------
  -- Add --
  ---------

  procedure Add(H: in Histogram; V: in Float) is
    W, X: Float;
    I: Natural;
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;
    if H.Accumulated then
      raise Accumulated_Histogram_Error;
    end if;

    W := V;
    if H.Logarithmic then
      if W <= 0.0 then
        W := H.Lower_Limit - 1.0;
      else
        W := Log(W);
      end if;
    end if;
    if W < H.Lower_Limit then
      H.Under_Lower_Limit := H.Under_Lower_Limit + 1;
    elsif W >= H.Upper_Limit then
      H.Over_Upper_Limit := H.Over_Upper_Limit + 1;
    else
      X := Float(H.Num_Bins) * (W - H.Lower_Limit) / (H.Upper_Limit - H.Lower_Limit);
      I := 1 + Integer(Float'Floor(X));
      if I = 0 then
        H.Under_Lower_Limit := H.Under_Lower_Limit + 1;
      elsif I > H.Num_Bins then
        H.Over_Upper_Limit := H.Over_Upper_Limit + 1;
      else
        H.Bin(I) := H.Bin(I) + 1;
      end if;
    end if;
    H.Num_Values := H.Num_Values + 1;
  end Add;

  ---------
  -- Add --
  ---------

  procedure Add(H: in Histogram; V: in Integer) is
  begin
    Add(H, Float(V));
  end Add;

  -------------
  -- Get_Bin --
  -------------

  function Get_Bin(H: in Histogram; I: in Positive) return Natural is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;
    if I > H.Num_Bins then
      raise Index_Out_Of_Bounds_Error;
    end if;

    return H.Bin(I);
  end Get_Bin;

  ---------------------------
  -- Get_Under_Lower_Limit --
  ---------------------------

  function Get_Under_Lower_Limit(H: in Histogram) return Natural is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    return H.Under_Lower_Limit;
  end Get_Under_Lower_Limit;

  --------------------------
  -- Get_Over_Upper_Limit --
  --------------------------

  function Get_Over_Upper_Limit(H: in Histogram) return Natural is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    return H.Over_Upper_Limit;
  end Get_Over_Upper_Limit;

  --------------------
  -- Get_Num_Values --
  --------------------

  function Get_Num_Values(H: in Histogram) return Natural is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    return H.Num_Values;
  end Get_Num_Values;


  function Accumulate_Histogram(H: in Histogram; Acc: in Accumulation_Type) return Histogram is
    Ch: Histogram;
    Sum: Natural;
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    Ch := new Histogram_Rec(H.Num_Bins);
    Ch.all := H.all;
    Ch.Accumulated := True;

    case Acc is
      when Add_Left_Bins =>
        Sum := Ch.Under_Lower_Limit;
        for I in Ch.Bin'Range loop
          Ch.Bin(I) := Ch.Bin(I) + Sum;
          Sum := Ch.Bin(I);
        end loop;
        Ch.Over_Upper_Limit := Ch.Over_Upper_Limit + Sum;
      when Add_Right_Bins =>
        Sum := Ch.Over_Upper_Limit;
        for I in reverse Ch.Bin'Range loop
          Ch.Bin(I) := Ch.Bin(I) + Sum;
          Sum := Ch.Bin(I);
        end loop;
        Ch.Under_Lower_Limit := Ch.Under_Lower_Limit + Sum;
    end case;

    return Ch;
  end Accumulate_Histogram;

  --------------------
  -- Is_Accumulated --
  --------------------

  function Is_Accumulated(H: in Histogram) return Boolean is
  begin
    if H = null then
      raise Uninitialized_Histogram_Error;
    end if;

    return H.Accumulated;
  end Is_Accumulated;

end Histograms;
