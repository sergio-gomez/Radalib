-- Radalib, Copyright (c) 2018 by
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


-- @filename Contingency_Tables.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 1/04/2005
-- @revision 10/04/2015
-- @brief Treatment of Contingency Tables

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;

with Utils; use Utils;

package body Contingency_Tables is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Lists, PLists);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Contingencies, PContingencies);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Contingency_Rec, Contingency_Table);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Ct: out Contingency_Table; Lol1, Lol2: in List_Of_Lists) is
  begin
    if Number_Of_Elements(Lol1) /= Number_Of_Elements(Lol2) then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;

    Ct := new Contingency_Rec;

    Ct.Lol1 := Lol1;
    Ct.Lol2 := Lol2;
    Update(Ct);
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Ct: in Contingency_Table) return Boolean is
  begin
    return Ct /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(Ct: in out Contingency_Table) is
  begin
    if Ct /= null then
      Dispose(Ct.Ls1);
      Dispose(Ct.Ls2);
      Dispose(Ct.Num12);
      Dispose(Ct);
      Ct := null;
    end if;
  end Free;

  ------------
  -- Combi2 --
  ------------

  function Combi2(M: in Natural) return Natural is
  begin
    if M <= 1 then
      return 0;
    elsif M mod 2 = 0 then
      return (M / 2) * (M - 1);
    else
      return M * ((M - 1) / 2);
    end if;
  end Combi2;

  ----------------
  -- Find_Index --
  ----------------

  function Find_Index(Ls: in Plists; L: in List) return Natural is
  begin
    for I in Ls'Range loop
      if Ls(I) = L then
        return I;
      end if;
    end loop;
    raise List_Not_Found_Error;
  end Find_Index;

  ------------
  -- Update --
  ------------

  procedure Update(Ct: in Contingency_Table) is
    I1, I2: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    Dispose(Ct.Ls1);
    Dispose(Ct.Ls2);
    Dispose(Ct.Num12);
    Ct.Ls1 := new Lists(0..Number_Of_Lists(Ct.Lol1));
    Ct.Ls2 := new Lists(0..Number_Of_Lists(Ct.Lol2));
    Ct.Num12 := new Contingencies(Ct.Ls1'Range, Ct.Ls2'Range);

    Ct.Ls1(0) := Unassigned_List(Ct.Lol1);
    Save(Ct.Lol1);
    Reset(Ct.Lol1);
    for I in 1..Ct.Ls1'Last loop
      Ct.Ls1(I) := Next_List(Ct.Lol1);
    end loop;
    Restore(Ct.Lol1);

    Ct.Ls2(0) := Unassigned_List(Ct.Lol2);
    Save(Ct.Lol2);
    Reset(Ct.Lol2);
    for I in 1..Ct.Ls2'Last loop
      Ct.Ls2(I) := Next_List(Ct.Lol2);
    end loop;
    Restore(Ct.Lol2);

    for I1 in Ct.Num12'Range(1) loop
      for I2 in Ct.Num12'Range(2) loop
        Ct.Num12(I1, I2) := 0;
      end loop;
    end loop;

    for E in 1..Number_Of_Elements(Ct.Lol1) loop
      I1 := Find_Index(Ct.Ls1, List_Of(Get_Element(Ct.Lol1, E)));
      I2 := Find_Index(Ct.Ls2, List_Of(Get_Element(Ct.Lol2, E)));
      Ct.Num12(I1, I2) := Ct.Num12(I1, I2) + 1;
    end loop;
  end Update;

  ------------------------
  -- Get_Lists_Of_Lists --
  ------------------------

  procedure Get_Lists_Of_Lists(Ct: in Contingency_Table; Lol1, Lol2: out List_Of_Lists) is
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    Lol1 := Ct.Lol1;
    Lol2 := Ct.Lol2;
  end Get_Lists_Of_Lists;

  ------------------------
  -- Number_Of_Elements --
  ------------------------

  function Number_Of_Elements(Ct: in Contingency_Table; L1, L2: in List) return Natural is
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    return Ct.Num12(Find_Index(Ct.Ls1, L1), Find_Index(Ct.Ls2, L2));
  end Number_Of_Elements;

  ---------------
  -- Transpose --
  ---------------

  procedure Transpose(Ct: in Contingency_Table) is
    Lol: List_Of_Lists;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    Lol := Ct.Lol1;
    Ct.Lol1 := Ct.Lol2;
    Ct.Lol2 := Lol;
    Update(Ct);
  end Transpose;

  ---------------------
  -- Number_Of_Pairs --
  ---------------------

  function Number_Of_Pairs(Ct: in Contingency_Table) return Natural is
    N: Positive;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    N := Number_Of_Elements(Ct.Lol1);
    return Combi2(N);
  end Number_Of_Pairs;

  -------------------------------------
  -- Number_Of_Same_Class_Agreements --
  -------------------------------------

  function Number_Of_Same_Class_Agreements(Ct: in Contingency_Table) return Natural is
    S12, M: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    S12 := 0;
    for I1 in 1..Ct.Ls1'Last loop
      for I2 in 1..Ct.Ls2'Last loop
        M := Ct.Num12(I1, I2);
        S12 := S12 + Combi2(M);
      end loop;
    end loop;

    return S12;
  end Number_Of_Same_Class_Agreements;

  --------------------------
  -- Number_Of_Agreements --
  --------------------------

  function Number_Of_Agreements(Ct: in Contingency_Table) return Natural is
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    return Number_Of_Pairs(Ct) - Number_Of_Disagreements(Ct);
  end Number_Of_Agreements;

  -----------------------------
  -- Number_Of_Disagreements --
  -----------------------------

  procedure Number_Of_Disagreements(Ct: in Contingency_Table; D1, D2: out Natural) is
    S1, S2, S12, M: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    S1 := 0;
    for I1 in 1..Ct.Ls1'Last loop
      M := Number_Of_Elements(Ct.Ls1(I1));
      S1 := S1 + Combi2(M);
    end loop;

    S2 := 0;
    for I2 in 1..Ct.Ls2'Last loop
      M := Number_Of_Elements(Ct.Ls2(I2));
      S2 := S2 + Combi2(M);
    end loop;

    S12 := Number_Of_Same_Class_Agreements(Ct);

    D1 := S1 - S12;
    D2 := S2 - S12;
  end Number_Of_Disagreements;

  -----------------------------
  -- Number_Of_Disagreements --
  -----------------------------

  function Number_Of_Disagreements(Ct: in Contingency_Table) return Natural is
    D1, D2: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    Number_Of_Disagreements(Ct, D1, D2);

    return D1 + D2;
  end Number_Of_Disagreements;

  ----------------
  -- Rand_Index --
  ----------------

  function Rand_Index(Ct: in Contingency_Table) return Float is
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    return Float(Number_Of_Agreements(Ct)) / Float(Number_Of_Pairs(Ct));
  end Rand_Index;

  -------------------------
  -- Adjusted_Rand_Index --
  -------------------------

  function Adjusted_Rand_Index(Ct: in Contingency_Table) return Float is
    S1, S2, S12, M: Natural;
    E: Float;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    S1 := 0;
    for I1 in 1..Ct.Ls1'Last loop
      M := Number_Of_Elements(Ct.Ls1(I1));
      S1 := S1 + Combi2(M);
    end loop;

    S2 := 0;
    for I2 in 1..Ct.Ls2'Last loop
      M := Number_Of_Elements(Ct.Ls2(I2));
      S2 := S2 + Combi2(M);
    end loop;

    S12 := Number_Of_Same_Class_Agreements(Ct);

    E := Float(S1 * S2) / Float(Number_Of_Pairs(Ct));

    if S1 + S2 = 2 * S12 then
      return 1.0;
    else
      return (Float(S12) - E) / (Float(S1 + S2) / 2.0 - E);
    end if;
  end Adjusted_Rand_Index;

  -------------------
  -- Jaccard_Index --
  -------------------

  function Jaccard_Index(Ct: in Contingency_Table) return Float is
    A, D: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    A := Number_Of_Same_Class_Agreements(Ct);
    D := Number_Of_Disagreements(Ct);

    if A + D = 0 then
      return 1.0;
    else
      return Float(A) / Float(A + D);
    end if;
  end Jaccard_Index;

  ------------------------------
  -- Asymmetric_Wallace_Index --
  ------------------------------

  procedure Asymmetric_Wallace_Index(Ct: in Contingency_Table; Awi1, Awi2: out Float) is
    D1, D2, A: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    Number_Of_Disagreements(Ct, D1, D2);
    A := Number_Of_Same_Class_Agreements(Ct);

    if D1 = 0 or A + D1 = 0 then
      Awi1 := 1.0;
    else
      Awi1 := Float(A) / Float(A + D1);
    end if;
    if D2 = 0 or A + D2 = 0 then
      Awi2 := 1.0;
    else
      Awi2 := Float(A) / Float(A + D2);
    end if;
  end Asymmetric_Wallace_Index;

  ---------------------------
  -- Fowlkes_Mallows_Index --
  ---------------------------

  function Fowlkes_Mallows_Index(Ct: in Contingency_Table) return Float is
    S1, S2, S12, M: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    S1 := 0;
    for I1 in 1..Ct.Ls1'Last loop
      M := Number_Of_Elements(Ct.Ls1(I1));
      S1 := S1 + Combi2(M);
    end loop;

    S2 := 0;
    for I2 in 1..Ct.Ls2'Last loop
      M := Number_Of_Elements(Ct.Ls2(I2));
      S2 := S2 + Combi2(M);
    end loop;

    S12 := Number_Of_Same_Class_Agreements(Ct);

    if S1 = 0 and S2 = 0 then
      return 1.0;
    elsif S1 = 0 xor S2 = 0 then
      return 0.0;
    else
      return Float(S12) / Sqrt(Float(S1) * Float(S2));
    end if;
  end Fowlkes_Mallows_Index;

  -----------------------------------------
  -- Normalized_Mutual_Information_Index --
  -----------------------------------------

  function Normalized_Mutual_Information_Index(Ct: in Contingency_Table; Mean: in Normalization_Method := Geometric_Mean) return Float is
    N: Positive;
    M, N1, N2: Natural;
    S1, S2, S12, NMI: Float;
    L: List;
    I: Positive;
    I1, I2: Natural;
  begin
    pragma Warnings(Off, L);
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    N := Number_Of_Elements(Ct.Lol1);

    S1 := 0.0;
    for I1 in 1..Ct.Ls1'Last loop
      M := Number_Of_Elements(Ct.Ls1(I1));
      if M > 0 and M /= N then
        S1 := S1 + Float(M) * Log(Float(M) / Float(N), 2.0);
      end if;
    end loop;
    M := Number_Of_Elements(Ct.Ls1(0));
    if M > 0 and N /= 1 then
      S1 := S1 + Float(M) * Log(1.0 / Float(N), 2.0);
    end if;

    S2 := 0.0;
    for I2 in 1..Ct.Ls2'Last loop
      M := Number_Of_Elements(Ct.Ls2(I2));
      if M > 0 and M /= N then
        S2 := S2 + Float(M) * Log(Float(M) / Float(N), 2.0);
      end if;
    end loop;
    M := Number_Of_Elements(Ct.Ls2(0));
    if M > 0 and N /= 1 then
      S2 := S2 + Float(M) * Log(1.0 / Float(N), 2.0);
    end if;

    S12 := 0.0;
    for I1 in 1..Ct.Ls1'Last loop
      N1 := Number_Of_Elements(Ct.Ls1(I1));
      for I2 in 1..Ct.Ls2'Last loop
        N2 := Number_Of_Elements(Ct.Ls2(I2));
        M := Ct.Num12(I1, I2);
        if M > 0 and M * N /= N1 * N2 then
          S12 := S12 + Float(M) * Log(Float(M * N) / Float(N1 * N2), 2.0);
        end if;
      end loop;
    end loop;
    L := Ct.Ls1(0);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      I2 := Find_Index(Ct.Ls2, List_Of(Get_Element(Ct.Lol2, I)));
      if I2 = 0 then
        if N /= 1 then
          S12 := S12 + Log(Float(N), 2.0) / 2.0;
        end if;
      else
        N2 := Number_Of_Elements(Ct.Ls2(I2));
        if N /= N2 then
          S12 := S12 + Log(Float(N) / Float(N2), 2.0);
        end if;
      end if;
    end loop;
    Restore(L);
    L := Ct.Ls2(0);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      I1 := Find_Index(Ct.Ls1, List_Of(Get_Element(Ct.Lol1, I)));
      if I1 = 0 then
        if N /= 1 then
          S12 := S12 + Log(Float(N), 2.0) / 2.0;
        end if;
      else
        N1 := Number_Of_Elements(Ct.Ls1(I1));
        if N /= N1 then
          S12 := S12 + Log(Float(N) / Float(N1), 2.0);
        end if;
      end if;
    end loop;
    Restore(L);

    S1 := -S1;
    S2 := -S2;
    case Mean is
      when Maximum =>
        if S1 = 0.0 and S2 = 0.0 and S12 = 0.0 then
          NMI := 1.0;
        elsif Max(S1, S2) <= 0.0 then
          NMI := 0.0;
        else
          NMI := S12 / Max(S1, S2);
        end if;
      when Arithmetic_Mean =>
        if S1 = 0.0 and S2 = 0.0 and S12 = 0.0 then
          NMI := 1.0;
        else
          NMI := 2.0 * S12 / (S1 + S2);
        end if;
      when Geometric_Mean =>
        if S1 = 0.0 and S2 = 0.0 and S12 = 0.0 then
          NMI := 1.0;
        elsif S1 * S2 <= 0.0 then
          NMI := 0.0;
        else
          NMI := S12 / Sqrt(S1 * S2);
        end if;
      when Minimum =>
        if S1 = 0.0 and S2 = 0.0 and S12 = 0.0 then
          NMI := 1.0;
        elsif Min(S1, S2) <= 0.0 then
          NMI := 0.0;
        else
          NMI := S12 / Min(S1, S2);
        end if;
    end case;
    return NMI;
  end Normalized_Mutual_Information_Index;

  -------------------
  -- Mirkin_Metric --
  -------------------

  function Mirkin_Metric(Ct: in Contingency_Table) return Float is
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    return Float(2 * Number_Of_Disagreements(Ct));
  end Mirkin_Metric;

  ------------------------------
  -- Normalized_Mirkin_Metric --
  ------------------------------

  function Normalized_Mirkin_Metric(Ct: in Contingency_Table) return Float is
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    return 1.0 - Rand_Index(Ct);
  end Normalized_Mirkin_Metric;

  -----------------------
  -- Van_Dongen_Metric --
  -----------------------

  function Van_Dongen_Metric(Ct: in Contingency_Table) return Float is
    N: Positive;
    S1, S2, M, Mx: Natural;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    N := Number_Of_Elements(Ct.Lol1);

    S1 := 0;
    for I1 in 1..Ct.Ls1'Last loop
      Mx := 1;
      for I2 in 1..Ct.Ls2'Last loop
        M := Ct.Num12(I1, I2);
        if M > Mx then
          Mx := M;
        end if;
      end loop;
      S1 := S1 + Mx;
    end loop;
    M := Number_Of_Elements(Ct.Ls1(0));
    if M > 0 then
      S1 := S1 + M;
    end if;

    S2 := 0;
    for I2 in 1..Ct.Ls2'Last loop
      Mx := 1;
      for I1 in 1..Ct.Ls1'Last loop
        M := Ct.Num12(I1, I2);
        if M > Mx then
          Mx := M;
        end if;
      end loop;
      S2 := S2 + Mx;
    end loop;
    M := Number_Of_Elements(Ct.Ls2(0));
    if M > 0 then
      S2 := S2 + M;
    end if;

    return Float(2 * N - S1 - S2);
  end Van_Dongen_Metric;

  ----------------------------------
  -- Normalized_Van_Dongen_Metric --
  ----------------------------------

  function Normalized_Van_Dongen_Metric(Ct: in Contingency_Table) return Float is
    N: Positive;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    N := Number_Of_Elements(Ct.Lol1);

    return Van_Dongen_Metric(Ct) / Float(2 * N);
  end Normalized_Van_Dongen_Metric;

  -------------------------------------
  -- Variation_Of_Information_Metric --
  -------------------------------------

  function Variation_Of_Information_Metric(Ct: in Contingency_Table) return Float is
    N: Positive;
    M, N1, N2: Natural;
    S12: Float;
    L: List;
    I: Positive;
    I1, I2: Natural;
  begin
    pragma Warnings(Off, L);
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    N := Number_Of_Elements(Ct.Lol1);

    S12 := 0.0;
    for I1 in 1..Ct.Ls1'Last loop
      N1 := Number_Of_Elements(Ct.Ls1(I1));
      for I2 in 1..Ct.Ls2'Last loop
        N2 := Number_Of_Elements(Ct.Ls2(I2));
        M := Ct.Num12(I1, I2);
        if M > 0 then
          S12 := S12 - Float(M) * Log(Float(M * M) / Float(N1 * N2), 2.0);
        end if;
      end loop;
    end loop;
    L := Ct.Ls1(0);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      I2 := Find_Index(Ct.Ls2, List_Of(Get_Element(Ct.Lol2, I)));
      if I2 > 0 then
        N2 := Number_Of_Elements(Ct.Ls2(I2));
        if N2 > 1 then
          S12 := S12 - Log(1.0 / Float(N2), 2.0);
        end if;
      end if;
    end loop;
    Restore(L);
    L := Ct.Ls2(0);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      I1 := Find_Index(Ct.Ls1, List_Of(Get_Element(Ct.Lol1, I)));
      if I1 > 0 then
        N1 := Number_Of_Elements(Ct.Ls1(I1));
        if N1 > 1 then
          S12 := S12 - Log(1.0 / Float(N1), 2.0);
        end if;
      end if;
    end loop;
    Restore(L);

    if S12 < 0.0 then
      S12 := 0.0;
    end if;

    return S12 / Float(N);
  end Variation_Of_Information_Metric;

  ------------------------------------------------
  -- Normalized_Variation_Of_Information_Metric --
  ------------------------------------------------

  function Normalized_Variation_Of_Information_Metric(Ct: in Contingency_Table) return Float is
    N: Positive;
  begin
    if Ct = null then
      raise Uninitialized_Contingency_Table_Error;
    end if;

    N := Number_Of_Elements(Ct.Lol1);

    return Variation_Of_Information_Metric(Ct) / Log(Float(N), 2.0);
  end Normalized_Variation_Of_Information_Metric;

end Contingency_Tables;

