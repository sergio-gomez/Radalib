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


-- @filename Graphs-Properties.adb
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 24/10/2007
-- @revision 03/11/2014
-- @brief Calculation of Properties of Graphs

with Ada.Numerics.Generic_Elementary_Functions;

with Stacks_Integer; use Stacks_Integer;
with Queues_Integer; use Queues_Integer;
with Graphs_Double; use Graphs_Double;

package body Graphs.Properties is

  package Num_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Num);
  use Num_Elementary_Functions;


  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(Wh: in Num; Ls: in Links_Subset) return Boolean is
  begin
    case Ls is
      when All_Links =>
        return True;
      when Positive_Links =>
        return Wh >= 0.0;
      when Negative_Links =>
        return Wh < 0.0;
    end case;
  end Belongs_To;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(E: in Edge; Ls: in Links_Subset) return Boolean is
  begin
    return Belongs_To(To_Num(E.Value), Ls);
  end Belongs_To;

  -----------
  -- Value --
  -----------

  function Value(E: in Edge; Ls: in Links_Subset) return Num is
    Wh: Num;
  begin
    Wh := To_Num(E.Value);
    case Ls is
      when All_Links =>
        return Wh;
      when Positive_Links =>
        return Num'Max(Wh, 0.0);
      when Negative_Links =>
        return Num'Max(-Wh, 0.0);
    end case;
  end Value;

  --------------
  -- Has_Next --
  --------------

  function Has_Next(El: in Edges_List; Ls: in Links_Subset) return Boolean is
    E: Edge;
  begin
    while Has_Next(El) loop
      E := Get(El);
      if Belongs_To(E, Ls) then
        return True;
      end if;
      Next(El);
    end loop;
    return False;
  end Has_Next;

  ---------------
  -- Has_Links --
  ---------------

  function Has_Links(V: in Vertex; Ls: in Links_Subset := All_Links) return Boolean is
    Found: Boolean;
    El: Edges_List;
  begin
    El := Edges_From(V);
    Save(El);
    Reset(El);
    Found := Has_Next(El, Ls);
    Restore(El);
    return Found;
  end Has_Links;

  ---------------
  -- Has_Links --
  ---------------

  function Has_Links(Gr: in Graph; Ls: in Links_Subset := All_Links) return Boolean is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      if Has_Links(Vertex'(Gr, I), Ls) then
        return True;
      end if;
    end loop;
    return False;
  end Has_Links;

  -------------------
  -- Has_Self_Loop --
  -------------------

  function Has_Self_Loop(V: in Vertex; Ls: in Links_Subset) return Boolean is
    E: Edge;
  begin
    E := Get_Self_Loop_Or_No_Edge(V);
    if E /= No_Edge and then Belongs_To(E, Ls) then
      return True;
    end if;
    return False;
  end Has_Self_Loop;

  ---------------------
  -- Number_Of_Edges --
  ---------------------

  function Number_Of_Edges(Gr: in Graph; Ls: in Links_Subset) return Num is
    Ne: Natural;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Ne := 0;
    for I in Gr.Vertices'Range loop
      Ne := Ne + Natural(Degree_From(Vertex'(Gr, I), Ls));
    end loop;
    return Num(Ne);
  end Number_Of_Edges;

  --------------------------
  -- Number_Of_Self_Loops --
  --------------------------

  function Number_Of_Self_Loops(Gr: in Graph; Ls: in Links_Subset) return Num is
    Nsl: Natural;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Nsl := 0;
    for I in Gr.Vertices'Range loop
      if Has_Self_Loop(Vertex'(Gr, I), Ls) then
        Nsl := Nsl + 1;
      end if;
    end loop;
    return Num(Nsl);
  end Number_Of_Self_Loops;

  ------------
  -- Degree --
  ------------

  function Degree(V: in Vertex; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    El: Edges_List;
    N: Natural;
  begin
    case Ld is
      when From_Links =>
        El := Edges_From(V);
      when To_Links =>
        El := Edges_To(V);
    end case;
    N := 0;
    Save(El);
    Reset(El);
    while Has_Next(El, Ls) loop
      Next(El);
      N := N + 1;
    end loop;
    Restore(El);
    return Num(N);
  end Degree;

  -----------------
  -- Degree_From --
  -----------------

  function Degree_From(V: in Vertex; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Degree(V, From_Links, Ls);
  end Degree_From;

  ---------------
  -- Degree_To --
  ---------------

  function Degree_To(V: in Vertex; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Degree(V, To_Links, Ls);
  end Degree_To;

  ------------
  -- Degree --
  ------------

  function Degree(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums is
    P: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    P := Alloc(1, Gr.Size);
    for I in P'Range loop
      P(I) := Degree(Vertex'(Gr, I), Ld, Ls);
    end loop;
    return P;
  end Degree;

  -----------------
  -- Degree_From --
  -----------------

  function Degree_From(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums is
  begin
    return Degree(Gr, From_Links, Ls);
  end Degree_From;

  ---------------
  -- Degree_To --
  ---------------

  function Degree_To(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums is
  begin
    return Degree(Gr, To_Links, Ls);
  end Degree_To;

  ------------------
  -- Total_Degree --
  ------------------

  function Total_Degree(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
    Total: Natural;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Total := 0;
    for I in Gr.Vertices'Range loop
      Total := Total + Natural(Degree_From(Vertex'(Gr, I), Ls));
    end loop;
    return Num(Total);
  end Total_Degree;

  --------------------
  -- Average Degree --
  --------------------

  function Average_Degree(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Total_Degree(Gr, Ls) / Num(Gr.Size);
  end Average_Degree;

  --------------------
  -- Minimum_Degree --
  --------------------

  function Minimum_Degree(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    Min, K: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Min := Num'Last;
    for I in Gr.Vertices'Range loop
      K := Degree(Vertex'(Gr, I), Ld, Ls);
      if K < Min then
        Min := K;
      end if;
    end loop;
    return Min;
  end Minimum_Degree;

  --------------------
  -- Maximum_Degree --
  --------------------

  function Maximum_Degree(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    Max, K: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Max := Num'First;
    for I in Gr.Vertices'Range loop
      K := Degree(Vertex'(Gr, I), Ld, Ls);
      if K > Max then
        Max := K;
      end if;
    end loop;
    return Max;
  end Maximum_Degree;

  ----------------------
  -- Values_Statistic --
  ----------------------

  function Values_Statistic(V: in Vertex; Vst: in Values_Statistic_Type; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    El: Edges_List;
    St: Num;
  begin
    case Ld is
      when From_Links =>
        El := Edges_From(V);
      when To_Links =>
        El := Edges_To(V);
    end case;
    St := 0.0;
    declare
      Vals: Nums(1..Number_Of_Edges(El));
      E: Edge;
      Wh: Num;
      N: Natural := 0;
    begin
      Save(El);
      Reset(El);
      while Has_Next(El, Ls) loop
        E := Next(El);
        Wh := Value(E, Ls);
        N := N + 1;
        Vals(N) := Wh;
      end loop;
      Restore(El);
      if N > 0 then
        case Vst is
          when Size_Value =>
            St := Num(N);
          when Min_Value =>
            St := Min(Vals(1..N));
          when Max_Value =>
            St := Max(Vals(1..N));
          when Sum_Value =>
            St := Sum(Vals(1..N));
          when Avg_Value =>
            St := Average(Vals(1..N));
          when Std_Dev_Value =>
            St := Standard_Deviation(Vals(1..N));
        end case;
      end if;
    end;
    return St;
  end Values_Statistic;

  ---------------------------
  -- Values_Statistic_From --
  ---------------------------

  function Values_Statistic_From(V: in Vertex; Vst: in Values_Statistic_Type; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Values_Statistic(V, Vst, From_Links, Ls);
  end Values_Statistic_From;

  -------------------------
  -- Values_Statistic_To --
  -------------------------

  function Values_Statistic_To(V: in Vertex; Vst: in Values_Statistic_Type; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Values_Statistic(V, Vst, To_Links, Ls);
  end Values_Statistic_To;

  ----------------------
  -- Values_Statistic --
  ----------------------

  function Values_Statistic(Gr: in Graph; Vst: in Values_Statistic_Type; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums is
    P: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    P := Alloc(1, Gr.Size);
    for I in P'Range loop
      P(I) := Values_Statistic(Vertex'(Gr, I), Vst, Ld, Ls);
    end loop;
    return P;
  end Values_Statistic;

  -------------------
  -- Minimum_Value --
  -------------------

  function Minimum_Value(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
    Min_Val, Wh: Num;
    V: Vertex;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Min_Val := Num'Last;
    for I in 1..Gr.Size loop
      V := Get_Vertex(Gr, I);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El, Ls) loop
        E := Next(El);
        Wh := Value(E, Ls);
        if Wh < Min_Val then
          Min_Val := Wh;
        end if;
      end loop;
      Restore(El);
    end loop;
    return Min_Val;
  end Minimum_Value;

  -------------------
  -- Maximum_Value --
  -------------------

  function Maximum_Value(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
    Max_Val, Wh: Num;
    V: Vertex;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Max_Val := Num'First;
    for I in 1..Gr.Size loop
      V := Get_Vertex(Gr, I);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El, Ls) loop
        E := Next(El);
        Wh := Value(E, Ls);
        if Wh > Max_Val then
          Max_Val := Wh;
        end if;
      end loop;
      Restore(El);
    end loop;
    return Max_Val;
  end Maximum_Value;

  --------------
  -- Strength --
  --------------

  function Strength(V: in Vertex; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    El: Edges_List;
    E: Edge;
    Wi, Wh: Num;
  begin
    case Ld is
      when From_Links =>
        El := Edges_From(V);
      when To_Links =>
        El := Edges_To(V);
    end case;
    Wi := 0.0;
    Save(El);
    Reset(El);
    while Has_Next(El, Ls) loop
      E := Next(El);
      Wh := Value(E, Ls);
      Wi := Wi + Wh;
    end loop;
    Restore(El);
    return Wi;
  end Strength;

  -------------------
  -- Strength_From --
  -------------------

  function Strength_From(V: in Vertex; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Strength(V, From_Links, Ls);
  end Strength_From;

  -----------------
  -- Strength_To --
  -----------------

  function Strength_To(V: in Vertex; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Strength(V, To_Links, Ls);
  end Strength_To;

  --------------
  -- Strength --
  --------------

  function Strength(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums is
    P: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    P := Alloc(1, Gr.Size);
    for I in P'Range loop
      P(I) := Strength(Vertex'(Gr, I), Ld, Ls);
    end loop;
    return P;
  end Strength;

  -------------------
  -- Strength_From --
  -------------------

  function Strength_From(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums is
  begin
    return Strength(Gr, From_Links, Ls);
  end Strength_From;

  -----------------
  -- Strength_To --
  -----------------

  function Strength_To(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums is
  begin
    return Strength(Gr, To_Links, Ls);
  end Strength_To;

  --------------------
  -- Total_Strength --
  --------------------

  function Total_Strength(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
    Total: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Total := 0.0;
    for I in Gr.Vertices'Range loop
      Total := Total + Strength_From(Vertex'(Gr, I), Ls);
    end loop;
    return Total;
  end Total_Strength;

  ----------------------
  -- Average Strength --
  ----------------------

  function Average_Strength(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    return Total_Strength(Gr, Ls) / Num(Gr.Size);
  end Average_Strength;

  ----------------------
  -- Minimum_Strength --
  ----------------------

  function Minimum_Strength(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    Min, W: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Min := Num'Last;
    for I in Gr.Vertices'Range loop
      W := Strength(Vertex'(Gr, I), Ld, Ls);
      if W < Min then
        Min := W;
      end if;
    end loop;
    return Min;
  end Minimum_Strength;

  ----------------------
  -- Maximum_Strength --
  ----------------------

  function Maximum_Strength(Gr: in Graph; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    Max, W: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Max := Num'First;
    for I in Gr.Vertices'Range loop
      W := Strength(Vertex'(Gr, I), Ld, Ls);
      if W > Max then
        Max := W;
      end if;
    end loop;
    return Max;
  end Maximum_Strength;

  ---------------
  -- Self_Loop --
  ---------------

  function Self_Loop(V: in Vertex; Ls: in Links_Subset := All_Links) return Num is
    Sl: Num;
    El: Edges_List;
    E: Edge;
  begin
    Sl := 0.0;
    El := Edges_From(V);
    Save(El);
    Reset(El);
    while Has_Next(El, Ls) loop
      E := Next(El);
      if To(E) = V then
        Sl := Value(E, Ls);
        exit;
      end if;
    end loop;
    Restore(El);
    return Sl;
  end Self_Loop;

  ---------------
  -- Self_Loop --
  ---------------

  function Self_Loop(Gr: in Graph; Ls: in Links_Subset := All_Links) return PNums is
    P: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    P := Alloc(1, Gr.Size);
    for I in P'Range loop
      P(I) := Self_Loop(Vertex'(Gr, I), Ls);
    end loop;
    return P;
  end Self_Loop;

  -------------------------------
  -- Total_Self_Loops_Strength --
  -------------------------------

  function Total_Self_Loops_Strength(Gr: in Graph; Ls: in Links_Subset := All_Links) return Num is
    Total: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Total := 0.0;
    for I in Gr.Vertices'Range loop
      Total := Total + Self_Loop(Vertex'(Gr, I), Ls);
    end loop;
    return Total;
  end Total_Self_Loops_Strength;

  ---------------
  -- Asymmetry --
  ---------------

  function Asymmetry(Gr: in Graph; Ignore_Self_Loops: in Boolean := False; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num is
    Diff, Total, Asym: Num;
    V: Vertex;
    Elf, Elt, El: Edges_List;
    Ef, Et, E: Edge;
    Jt, Jf: Positive;
    Whf, Wht, Wh: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if not Is_Directed(Gr) then
      return 0.0;
    end if;

    Diff := 0.0;
    Total := 0.0;
    for I in 1..Gr.Size loop
      V := Get_Vertex(Gr, I);
      Elf := Edges_From(V);
      Elt := Edges_To(V);
      Save(Elf);
      Save(Elt);
      Reset(Elf);
      Reset(Elt);
      while Has_Next(Elf, Ls) and Has_Next(Elt, Ls) loop
        Ef := Get(Elf);
        Et := Get(Elt);
        if Weighted then
          Whf := Value(Ef, Ls);
          Wht := Value(Et, Ls);
        else
          Whf := 1.0;
          Wht := 1.0;
        end if;
        Jf := Index_Of(To(Ef));
        Jt := Index_Of(From(Et));
        if Jf < Jt then
          Diff := Diff + abs (Whf);
          Total := Total + abs (Whf);
          Next(Elf);
        elsif Jf > Jt then
          Diff := Diff + abs (Wht);
          Total := Total + abs (Wht);
          Next(Elt);
        elsif Jf = Jt then
          if Jf /= I or else not Ignore_Self_Loops then
            if Whf /= Wht then
              Diff := Diff + abs (Whf - Wht);
            end if;
            Total := Total + abs (Whf) + abs (Wht);
          end if;
          Next(Elf);
          Next(Elt);
        end if;
      end loop;
      if Has_Next(Elf, Ls) or Has_Next(Elt, Ls) then
        if Has_Next(Elf, Ls) then
          El := Elf;
        else
          El := Elt;
        end if;
        while Has_Next(El, Ls) loop
          E := Next(El);
          if Weighted then
            Wh := Value(E, Ls);
          else
            Wh := 1.0;
          end if;
          Diff := Diff + abs (Wh);
          Total := Total + abs (Wh);
        end loop;
      end if;
      Restore(Elf);
      Restore(Elt);
    end loop;

    if Diff = 0.0 or Total = 0.0 then
      Asym := 0.0;
    elsif Diff >= Total then
      Asym := 1.0;
    else
      Asym := Diff / Total;
    end if;
    return Asym;
  end Asymmetry;

  -----------------
  -- Reciprocity --
  -----------------

  function Reciprocity(Gr: in Graph; Ignore_Self_Loops: in Boolean := False; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num is
    Asym, Recip: Num;
  begin
    Asym := Asymmetry(Gr, Ignore_Self_Loops, Weighted, Ls);
    if Asym = 0.0 then
      Recip := 1.0;
    elsif Asym = 1.0 then
      Recip := 0.0;
    else
      Recip := 1.0 - Asym;
    end if;
    return Recip;
  end Reciprocity;

  -------------
  -- Entropy --
  -------------

  function Entropy(V: in Vertex; Ld: in Links_Direction := From_Links; Weighted: in Boolean := True; Normalized: in Boolean := False) return Num is
    El: Edges_List;
    E: Edge;
    Ki: Natural;
    Hi, Wi, Wh: Num;
  begin
    if Weighted and then Has_Links(V, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    case Ld is
      when From_Links =>
        El := Edges_From(V);
      when To_Links =>
        El := Edges_To(V);
    end case;
    Ki := Number_Of_Edges(El);
    if Ki <= 1 then
      return 0.0;
    end if;
    if Weighted then
      Wi := 0.0;
      Hi := 0.0;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Wh := To_Num(Value(E));
        if Wh > 0.0 then
          Wi := Wi + Wh;
          if Wh /= 1.0 then
            Hi := Hi - Wh * Log(Wh, 2.0);
          end if;
        end if;
      end loop;
      Restore(El);
      if Wi = 0.0 then
        if Normalized then
          Hi := 1.0;
        else
          Hi := Log(Num(Ki), 2.0);
        end if;
      else
        Hi := Log(Wi, 2.0) + Hi / Wi;
        if Normalized then
          Hi := Hi / Log(Num(Ki), 2.0);
        end if;
      end if;
    else
      if Normalized then
        Hi := 1.0;
      else
        Hi := Log(Num(Ki), 2.0);
      end if;
    end if;
    return Hi;
  end Entropy;

  -------------
  -- Entropy --
  -------------

  function Entropy(Gr: in Graph; Ld: in Links_Direction := From_Links; Weighted: in Boolean := True; Normalized: in Boolean := False) return PNums is
    H: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    H := Alloc(1, Gr.Size);
    if (not Weighted) and Normalized then
      H.all := (others => 1.0);
    else
      for I in Gr.Vertices'Range loop
        H(I) := Entropy(Vertex'(Gr, I), Ld, Weighted, Normalized);
      end loop;
    end if;
    return H;
  end Entropy;

  ---------------------
  -- Average_Entropy --
  ---------------------

  function Average_Entropy(Gr: in Graph; Ld: in Links_Direction := From_Links; Weighted: in Boolean := True; Normalized: in Boolean := False) return Num is
    Sum: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    if (not Weighted) and Normalized then
      return 1.0;
    end if;

    Sum := 0.0;
    for I in Gr.Vertices'Range loop
      Sum := Sum + Entropy(Vertex'(Gr, I), Ld, Weighted, Normalized);
    end loop;
    return Sum / Num(Gr.Size);
  end Average_Entropy;

  --------------------
  -- Shortest_Paths --
  --------------------

  procedure Shortest_Paths(Gr: in Graph; Dists: out PNumss; Preds: out PIntegerss; Allow_Infinite_Distances: in Boolean := False; Weighted: in Boolean := True) is
    V: Vertex;
    El: Edges_List;
    E: Edge;
    N, J: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    N := Gr.Size;
    Dists := Alloc(1, N, 1, N);
    Preds := Alloc(1, N, 1, N);

    -- Weights matrix
    Dists.all := (others => (others => Plus_Infinity));
    for I in 1..N loop
      V := Get_Vertex(Gr, I);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Weighted then
          Dists(I, J) := To_Num(Value(E));
        else
          Dists(I, J) := 1.0;
        end if;
      end loop;
      Restore(El);
      Dists(I, I) := 0.0;
    end loop;

    for I in 1..N loop
      for J in 1..N loop
        Preds(I, J) := I;
      end loop;
    end loop;

    -- Shortest Paths
    for K in 1..N loop
      for I in 1..N loop
        for J in 1..N loop
          if I /= K then
            if Dists(I, K) < Plus_Infinity and Dists(K, J) < Plus_Infinity then
              if Weighted then
                if Dists(I, K) + Dists(K, J) < Dists(I, J) then
                  Dists(I, J) := Dists(I, K) + Dists(K, J);
                  Preds(I, J) := Preds(K, J);
                end if;
              else
                if Dists(I, J) = Plus_Infinity or else Integer(Dists(I, K) + Dists(K, J)) < Integer(Dists(I, J)) then
                  Dists(I, J) := Num(Integer(Dists(I, K) + Dists(K, J)));
                  Preds(I, J) := Preds(K, J);
                end if;
              end if;
            end if;
          end if;
        end loop;
      end loop;
    end loop;

    -- Check Connectedness
    if not Allow_Infinite_Distances then
      for I in 1..N loop
        for J in 1..N loop
          if Dists(I, J) = Plus_Infinity then
            Free(Dists);
            Free(Preds);
            raise Disconnected_Graph_Error;
          end if;
        end loop;
      end loop;
    end if;
  end Shortest_Paths;

  -------------------
  -- Shortest_Path --
  -------------------

  function Shortest_Path(V_From, V_To: in Vertex; Dists: in PNumss; Preds: in PIntegerss) return Graph_Path is
    Path: Graph_Path;
    F, T, I: Positive;
    E: Edge;
  begin
    if V_From.Gr /= V_To.Gr then
      raise Incompatible_Graphs_Error;
    end if;

    Linked_Edges.Initialize(Path);
    F := V_From.Index;
    T := V_To.Index;
    if F /= T and then Dists(F, T) /= Plus_Infinity then
      E.Gr    := V_From.Gr;
      E.From  := T;
      E.To    := T;
      E.Value := To_Value(Plus_Infinity);
      I := Preds(F, T);
      while I /= F loop
        E.To    := E.From;
        E.From  := I;
        E.Value := To_Value(Dists(E.From, E.To));
        Linked_Edges.Add_First(E, Path);
        I := Preds(F, I);
      end loop;
      E.To    := E.From;
      E.From  := F;
      E.Value := To_Value(Dists(E.From, E.To));
      Linked_Edges.Add_First(E, Path);
    end if;
    return Path;
  end Shortest_Path;

  --------------------------
  -- Shortest_Path_Length --
  --------------------------

  function Shortest_Path_Length(V: in Vertex; Allow_Infinite_Distances: in Boolean := False; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums is
    Distances: PNums;
    Gr: constant Graph := V.Gr;
    Visited: array(1..Gr.Size) of Boolean;
    M, N, Index, Minimum: Positive;
    W, Min_Dist: Num;
    El: Edges_List;
    E: Edge;
    Vn: Vertex;
    Q: Queue;
  begin
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Index := Index_Of(V);
    Minimum := Index;

    Distances := Alloc(1, Gr.Size);
    Distances.all := (others => Plus_Infinity);
    Distances(Index) := 0.0;
    Visited := (others => False);

    if Weighted then
      -- Dijkstra's Algorithm
      while not Visited(Minimum) loop
        M := Minimum;
        Visited(M) := True;
        case Ld is
          when From_Links =>
            El := Edges_From(Vertex'(Gr, M));
          when To_Links =>
            El := Edges_To(Vertex'(Gr, M));
        end case;
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          case Ld is
            when From_Links =>
              Vn := To(E);
            when To_Links =>
              Vn := From(E);
          end case;
          N := Index_Of(Vn);
          if M /= N then -- Skip self-loops
            W := To_Num(E.Value);
            if Distances(M) + W < Distances(N) then
              Distances(N) := Distances(M) + W;
            end if;
          end if;
        end loop;
        Restore(El);
        -- Recalculate Minimum
        Min_Dist := Plus_Infinity;
        for I in 1..Gr.Size loop
          if not Visited(I) and then Distances(I) < Min_Dist then
            Min_Dist := Distances(I);
            Minimum := I;
          end if;
        end loop;
      end loop;
    else
      -- Breadth First Search Algorithm
      Initialize(Q);
      Visited(Index) := True;
      Enqueue(Index, Q);
      while not Is_Empty(Q) loop
        M := Dequeue(Q);
        case Ld is
          when From_Links =>
            El := Edges_From(Vertex'(Gr, M));
          when To_Links =>
            El := Edges_To(Vertex'(Gr, M));
        end case;
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          case Ld is
            when From_Links =>
              Vn := To(E);
            when To_Links =>
              Vn := From(E);
          end case;
          N := Index_Of(Vn);
          if M /= N then -- Skip self-loops
            if not Visited(N) then
              Visited(N) := True;
              Distances(N) := Distances(M) + 1.0;
              Enqueue(N, Q);
            end if;
          end if;
        end loop;
        Restore(El);
      end loop;
      Free(Q);
    end if;

    -- Check Connectedness
    if not Allow_Infinite_Distances then
      for I in 1..Gr.Size loop
        if not Visited(I) then
          Free(Distances);
          raise Disconnected_Graph_Error;
        end if;
      end loop;
    end if;

    return Distances;
  end Shortest_Path_Length;

  -------------------------
  -- Average_Path_Length --
  -------------------------

  function Average_Path_Length(V: in Vertex; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return Num is
    Sum: Num;
    Distances: PNums;
  begin
    if V.Gr.Size = 1 then
      return 0.0;
    end if;
    Distances := Shortest_Path_Length(V, False, Weighted, Ld);
    Sum := 0.0;
    for I in 1..V.Gr.Size loop
      Sum := Sum + Distances(I);
    end loop;
    Free(Distances);
    return Sum / Num(V.Gr.Size - 1);
  end Average_Path_Length;

  -------------------------
  -- Average_Path_Length --
  -------------------------

  function Average_Path_Length(Gr: in Graph; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums is
    Apl: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Apl := Alloc(1, Gr.Size);
    for I in Gr.Vertices'Range loop
      Apl(I) := Average_Path_Length(Vertex'(Gr, I), Weighted, Ld);
    end loop;
    return Apl;
  end Average_Path_Length;

  -------------------------
  -- Average_Path_Length --
  -------------------------

  function Average_Path_Length(Gr: in Graph; Weighted: in Boolean := True) return Num is
    Sum: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    if Gr.Size = 1 then
      return 0.0;
    end if;
    Sum := 0.0;
    for I in Gr.Vertices'Range loop
      Sum := Sum + Average_Path_Length(Vertex'(Gr, I), Weighted);
    end loop;
    return Sum / Num(Gr.Size);
  end Average_Path_Length;

  -------------------------
  -- Maximum_Path_Length --
  -------------------------

  function Maximum_Path_Length(V: in Vertex; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return Num is
    Max: Num;
    Distances: PNums;
  begin
    Distances := Shortest_Path_Length(V, False, Weighted, Ld);
    Max := 0.0;
    for I in 1..V.Gr.Size loop
      if Distances(I) > Max then
        Max := Distances(I);
      end if;
    end loop;
    Free(Distances);
    return Max;
  end Maximum_Path_Length;

  -------------------------
  -- Maximum_Path_Length --
  -------------------------

  function Maximum_Path_Length(Gr: in Graph; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums is
    Mpl: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Mpl := Alloc(1, Gr.Size);
    for I in Gr.Vertices'Range loop
      Mpl(I) := Maximum_Path_Length(Vertex'(Gr, I), Weighted, Ld);
    end loop;
    return Mpl;
  end Maximum_Path_Length;

  --------------
  -- Diameter --
  --------------

  function Diameter(Gr: in Graph; Weighted: in Boolean := True) return Num is
    Max, Diam: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Diam := 0.0;
    for I in Gr.Vertices'Range loop
      Max := Maximum_Path_Length(Vertex'(Gr, I), Weighted);
      if Max > Diam then
        Diam := Max;
      end if;
    end loop;
    return Diam;
  end Diameter;

  ----------------
  -- Efficiency --
  ----------------

  function Efficiency(V: in Vertex; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return Num is
    Index: Positive;
    Sum: Num;
    Distances: PNums;
  begin
    if V.Gr.Size = 1 then
      return 1.0;
    end if;
    Index := Index_Of(V);
    Distances := Shortest_Path_Length(V, True, Weighted, Ld);
    Sum := 0.0;
    for I in 1..V.Gr.Size loop
      if I /= Index then
        if Distances(I) /= Plus_Infinity then
          Sum := Sum + 1.0 / Distances(I);
        end if;
      end if;
    end loop;
    Free(Distances);
    return Sum / Num(V.Gr.Size - 1);
  end Efficiency;

  ----------------
  -- Efficiency --
  ----------------

  function Efficiency(Gr: in Graph; Weighted: in Boolean := True; Ld: in Links_Direction := From_Links) return PNums is
    Eff: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Eff := Alloc(1, Gr.Size);
    for I in Gr.Vertices'Range loop
      Eff(I) := Efficiency(Vertex'(Gr, I), Weighted, Ld);
    end loop;
    return Eff;
  end Efficiency;

  ----------------
  -- Efficiency --
  ----------------

  function Efficiency(Gr: in Graph; Weighted: in Boolean := True) return Num is
    Sum: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    if Gr.Size = 1 then
      return 1.0;
    end if;
    Sum := 0.0;
    for I in Gr.Vertices'Range loop
      Sum := Sum + Efficiency(Vertex'(Gr, I), Weighted);
    end loop;
    return Sum / Num(Gr.Size);
  end Efficiency;

  ----------------------------
  -- Clustering_Coefficient --
  ----------------------------

  function Clustering_Coefficient(V: in Vertex; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num is

    type Triangle_Pattern is (Cycle, Middle, Source, Sink);

    type Cc_Rec is record
      El1, El2: Edges_List;
      Arc12: Boolean;
      Num_Tri, Num_Pairs: Natural := 0;
      Sum_Tri, Sum_Pairs: Num := 0.0;
    end record;

    Deg_In, Deg_Out, Deg_Total, Deg_Bilateral: Natural := 0;
    Num_Tri_Total, Num_Pairs_Total: Natural := 0;
    Sum_Tri_Total, Sum_Pairs_Total: Num := 0.0;
    Cc: Num := 0.0;
    Cc_Info: array(Triangle_Pattern) of Cc_Rec;

    El, El1, El2: Edges_List;
    V1, V2: Vertex;
    E, E1, E2: Edge;
    W, W1, W2: Num;
  begin
    if not Is_Directed(V.Gr) then

      -- Undirected Graph
      Deg_Total := Natural(Degree_From(V, Ls));
      if Self_Loop(V, Ls) /= 0.0 then
        Deg_Total := Deg_Total - 1;
      end if;
      Num_Pairs_Total := Deg_Total * (Deg_Total - 1) / 2;

      Num_Tri_Total := 0;
      Sum_Tri_Total := 0.0;
      Sum_Pairs_Total := 0.0;
      if Num_Pairs_Total > 0 then
        El := Edges_From(V);
        Save(El);
        Reset(El);
        while Has_Next(El, Ls) loop
          V1 := Get(El);
          E1 := Next(El);
          W1 := Value(E1, Ls);
          if V1 /= V then
            Save(El);
            Reset(El);
            while Has_Next(El, Ls) loop
              V2 := Get(El);
              E2 := Next(El);
              W2 := Value(E2, Ls);
              if V2 /= V and V2 /= V1 then
                Sum_Pairs_Total := Sum_Pairs_Total + W1 * W2;
                E := Get_Edge_Or_No_Edge(V1, V2);
                if E /= No_Edge and then Belongs_To(E, Ls) then
                  W := Value(E, Ls);
                  Num_Tri_Total := Num_Tri_Total + 1;
                  Sum_Tri_Total := Sum_Tri_Total + W1 * W2 * W;
                end if;
              end if;
            end loop;
            Restore(El);
          end if;
        end loop;
        Restore(El);
        Num_Tri_Total := Num_Tri_Total / 2;  -- Each triangle has been counted twice
        Sum_Tri_Total := Sum_Tri_Total / 2.0;
        Sum_Pairs_Total := Sum_Pairs_Total / 2.0;
      end if;

    else

      -- Directed Graph
      Deg_In := Natural(Degree_To(V, Ls));
      Deg_Out := Natural(Degree_From(V, Ls));
      if Self_Loop(V, Ls) /= 0.0 then
        Deg_In := Deg_In - 1;
        Deg_Out := Deg_Out - 1;
      end if;
      Deg_Total := Deg_In + Deg_Out;

      Deg_Bilateral := 0;
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El, Ls) loop
        V1 := Next(El);
        if V1 /= V then
          E := Get_Edge_Or_No_Edge(V1, V);
          if E /= No_Edge and then Belongs_To(E, Ls) then
            Deg_Bilateral := Deg_Bilateral + 1;
          end if;
        end if;
      end loop;
      Restore(El);

      Num_Pairs_Total := Deg_Total * (Deg_Total - 1) - 2 * Deg_Bilateral;

      Cc_Info(Cycle)  := (El1 => Edges_From(V), El2 => Edges_To(V)  , Arc12 => True ,
                          Num_Tri => 0, Num_Pairs => Deg_In * Deg_Out - Deg_Bilateral,
                          Sum_Tri => 0.0, Sum_Pairs => 0.0);
      Cc_Info(Middle) := (El1 => Edges_From(V), El2 => Edges_To(V)  , Arc12 => False,
                          Num_Tri => 0, Num_Pairs => Deg_In * Deg_Out - Deg_Bilateral,
                          Sum_Tri => 0.0, Sum_Pairs => 0.0);
      Cc_Info(Source) := (El1 => Edges_From(V), El2 => Edges_From(V), Arc12 => True ,
                          Num_Tri => 0, Num_Pairs => Deg_Out * (Deg_Out - 1),
                          Sum_Tri => 0.0, Sum_Pairs => 0.0);
      Cc_Info(Sink)   := (El1 => Edges_To(V)  , El2 => Edges_To(V)  , Arc12 => True ,
                          Num_Tri => 0, Num_Pairs => Deg_In * (Deg_In - 1),
                          Sum_Tri => 0.0, Sum_Pairs => 0.0);

      Num_Tri_Total := 0;
      Sum_Tri_Total := 0.0;
      Sum_Pairs_Total := 0.0;
      for P in Triangle_Pattern loop
        Cc_Info(P).Num_Tri := 0;
        Cc_Info(P).Sum_Tri := 0.0;
        Cc_Info(P).Sum_Pairs := 0.0;
        if Cc_Info(P).Num_Pairs > 0 then
          El1 := Cc_Info(P).El1;
          Save(El1);
          Reset(El1);
          while Has_Next(El1, Ls) loop
            V1 := Get(El1);
            E1 := Next(El1);
            W1 := Value(E1, Ls);
            if V1 /= V then
              El2 := Cc_Info(P).El2;
              Save(El2);
              Reset(El2);
              while Has_Next(El2, Ls) loop
                V2 := Get(El2);
                E2 := Next(El2);
                W2 := Value(E2, Ls);
                if V2 /= V and V2 /= V1 then
                  if Cc_Info(P).Arc12 then    -- Triangles are not counted twice
                    Cc_Info(P).Sum_Pairs := Cc_Info(P).Sum_Pairs + W1 * W2;
                    E := Get_Edge_Or_No_Edge(V1, V2);
                    if E /= No_Edge and then Belongs_To(E, Ls) then
                      W := Value(E, Ls);
                      Cc_Info(P).Num_Tri := Cc_Info(P).Num_Tri + 1;
                      Cc_Info(P).Sum_Tri := Cc_Info(P).Sum_Tri + W1 * W2 * W;
                    end if;
                  else
                    Cc_Info(P).Sum_Pairs := Cc_Info(P).Sum_Pairs + W1 * W2;
                    E := Get_Edge_Or_No_Edge(V2, V1);
                    if E /= No_Edge and then Belongs_To(E, Ls) then
                      W := Value(E, Ls);
                      Cc_Info(P).Num_Tri := Cc_Info(P).Num_Tri + 1;
                      Cc_Info(P).Sum_Tri := Cc_Info(P).Sum_Tri + W1 * W2 * W;
                    end if;
                  end if;
                end if;
              end loop;
              Restore(El2);
            end if;
          end loop;
          Restore(El1);
        end if;
        Num_Tri_Total := Num_Tri_Total + Cc_Info(P).Num_Tri;
        Sum_Tri_Total := Sum_Tri_Total + Cc_Info(P).Sum_Tri;
        Sum_Pairs_Total := Sum_Pairs_Total + Cc_Info(P).Sum_Pairs;
      end loop;

    end if;

    if Num_Pairs_Total > 0 then
      if Weighted then
        if Sum_Pairs_Total /= 0.0 then
          Cc := Sum_Tri_Total / Sum_Pairs_Total;
        else
          Cc := 0.0;
        end if;
      else
        Cc := Num(Num_Tri_Total) / Num(Num_Pairs_Total);
      end if;
    else
      Cc := 0.0;
    end if;

    return Cc;
  end Clustering_Coefficient;

  ----------------------------
  -- Clustering_Coefficient --
  ----------------------------

  function Clustering_Coefficient(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return PNums is
    Cc: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Cc := Alloc(1, Gr.Size);
    for I in Gr.Vertices'Range loop
      Cc(I) := Clustering_Coefficient(Vertex'(Gr, I), Weighted, Ls);
    end loop;
    return Cc;
  end Clustering_Coefficient;

  ------------------------------------
  -- Average_Clustering_Coefficient --
  ------------------------------------

  function Average_Clustering_Coefficient(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num is
    Sum: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Sum := 0.0;
    for I in Gr.Vertices'Range loop
      Sum := Sum + Clustering_Coefficient(Vertex'(Gr, I), Weighted, Ls);
    end loop;
    return Sum / Num(Gr.Size);
  end Average_Clustering_Coefficient;

  ------------------------
  -- Vertex_Betweenness --
  ------------------------

  function Vertex_Betweenness(Gr: in Graph; Weighted: in Boolean := True; Normalized: in Boolean := True) return PNums is

    package Linked_Lists_Pos is new Linked_Lists(Positive); use Linked_Lists_Pos;

    Bet: PNums;
    P_Dist, P_Sigma, P_Delta: PNums;
    St: Stack;
    Visited: array(1..Gr.Size) of Boolean;
    P: array(1..Gr.Size) of Linked_Lists_Pos.Linked_List;

    Nums_Tolerance: constant Num := 1.0e-10;
    N, V, W, Min_V: Positive;
    Ver_V: Vertex;
    El: Edges_List;
    E: Edge;
    Dist, Min_Dist: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    N := Gr.Size;
    Bet := Alloc(1, N);
    P_Dist := Alloc(1, N);
    P_Sigma := Alloc(1, N);
    P_Delta := Alloc(1, N);
    Initialize(St);
    for V in 1..N loop
      Initialize(P(V));
      Bet(V) := 0.0;
    end loop;

    if N > 2 then
      for S in 1..N loop
        Clear(St);

        for T in 1..N loop
          Clear(P(T));
          P_Dist(T) := Plus_Infinity;
          P_Sigma(T) := 0.0;
          P_Delta(T) := 0.0;
          Visited(T) := False;
        end loop;
        P_Dist(S) := 0.0;
        P_Sigma(S) := 1.0;

        Min_V := S;
        Min_Dist := 0.0;
        while Min_Dist /= Plus_Infinity loop
          V := Min_V;
          Visited(V) := True;
          Push(V, St);
          Ver_V := Vertex'(Gr, V);
          El := Edges_From(Ver_V);
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            E := Next(El);
            W := Index_Of(To(E));
            if V /= W then
              if Weighted then
                Dist := To_Num(E.Value);
              else
                Dist := 1.0;
              end if;
              -- Relax
              if P_Dist(W) - P_Dist(V) - Dist > Nums_Tolerance then
                -- found new shortest path
                P_Dist(W) := P_Dist(V) + Dist;
                P_Sigma(W) := P_Sigma(V);
                Clear(P(W));
                Add_Last(V, P(W));
              elsif abs (P_Dist(W) - P_Dist(V) - Dist) < Nums_Tolerance then
                -- found another shortest path
                P_Sigma(W) := P_Sigma(W) + P_Sigma(V);
                Add_Last(V, P(W));
              end if;
            end if;
          end loop;
          Restore(El);

          -- Recalculate Minimum
          Min_Dist := Plus_Infinity;
          for T in 1..N loop
            if (not Visited(T)) and then P_Dist(T) < Min_Dist then
              Min_Dist := P_Dist(T);
              Min_V := T;
            end if;
          end loop;
        end loop;

        while not Is_Empty(St) loop
          W := Pop(St);
          Save(P(W));
          Reset(P(W));
          while Has_Next(P(W)) loop
            V := Next(P(W));
            P_Delta(V) := P_Delta(V) + P_Sigma(V) * (1.0 + P_Delta(W)) / P_Sigma(W);
          end loop;
          Restore(P(W));
          if W /= S then
            Bet(W) := Bet(W) + P_Delta(W);
          end if;
        end loop;
      end loop;

      if Normalized then
        for S in 1..N loop
          Bet(S) := Bet(S) / Num((N - 1) * (N - 2));
        end loop;
      end if;
    end if;

    Free(St);
    Free(P_Dist);
    Free(P_Sigma);
    Free(P_Delta);
    for V in 1..N loop
      Free(P(V));
    end loop;

    return Bet;
  end Vertex_Betweenness;

  ----------------------
  -- Edge_Betweenness --
  ----------------------

  function Edge_Betweenness(Gr: in Graph; Weighted: in Boolean := True; Normalized: in Boolean := True) return Graphs_Double.Graph is

    procedure Normalize_Betweenness(Gr_Bet: in Graphs_Double.Graph) is
      N, T: Positive;
      Vf: Graphs_Double.Vertex;
      El: Graphs_Double.Edges_List;
      E: Graphs_Double.Edge;
      Norm: Double;
    begin
      pragma Warnings(Off, El);
      N := Number_Of_Vertices(Gr_Bet);
      if N > 1 then
        Norm := Double(N * (N - 1));
        for F in 1..N loop
          Vf := Get_Vertex(Gr_Bet, F);
          El := Edges_From(Vf);
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            E := Next(El);
            T := Index_Of(To(E));
            if Is_Directed(Gr_Bet) or else F < T then
              Set_Value(E, Value(E) / Norm);
            end if;
          end loop;
          Restore(El);
        end loop;
      end if;
    end Normalize_Betweenness;

    package Linked_Lists_Pos is new Linked_Lists(Positive); use Linked_Lists_Pos;

    N: constant Positive := Gr.Size;
    Gr_Bet: Graphs_Double.Graph;
    P_Dist, P_Sigma, P_Delta: PNums;
    St: Stack;
    Visited: array(1..N) of Boolean;
    P: array(1..N) of Linked_Lists_Pos.Linked_List;

    Nums_Tolerance: constant Num := 1.0e-10;
    T, V, W, Min_V: Positive;
    Vx: Vertex;
    Vf, Vt, Vv, Vw: Graphs_Double.Vertex;
    El: Edges_List;
    E: Edge;
    Evw: Graphs_Double.Edge;
    Dist, Min_Dist, Inc: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Initialize(Gr_Bet, N, Is_Directed(Gr));
    for F in 1..N loop
      Vf := Get_Vertex(Gr_Bet, F);
      Vx := Get_Vertex(Gr, F);
      Set_Name(Vf, Get_Name(Vx));
      Set_Tag(Vf, Get_Tag(Vx));
      El := Edges_From(Vx);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        T := Index_Of(To(Next(El)));
        Vt := Get_Vertex(Gr_Bet, T);
        Add_Edge(Vf, Vt, 0.0);
      end loop;
      Restore(El);
    end loop;

    P_Dist := Alloc(1, N);
    P_Sigma := Alloc(1, N);
    P_Delta := Alloc(1, N);
    Initialize(St);
    for V in 1..N loop
      Initialize(P(V));
    end loop;

    if N > 2 then
      for S in 1..N loop
        Clear(St);

        for T in 1..N loop
          Clear(P(T));
          P_Dist(T) := Plus_Infinity;
          P_Sigma(T) := 0.0;
          P_Delta(T) := 0.0;
          Visited(T) := False;
        end loop;
        P_Dist(S) := 0.0;
        P_Sigma(S) := 1.0;

        Min_V := S;
        Min_Dist := 0.0;
        while Min_Dist /= Plus_Infinity loop
          V := Min_V;
          Visited(V) := True;
          Push(V, St);
          Vx := Vertex'(Gr, V);
          El := Edges_From(Vx);
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            E := Next(El);
            W := Index_Of(To(E));
            if V /= W then
              if Weighted then
                Dist := To_Num(E.Value);
              else
                Dist := 1.0;
              end if;
              -- Relax
              if P_Dist(W) - P_Dist(V) - Dist > Nums_Tolerance then
                -- found new shortest path
                P_Dist(W) := P_Dist(V) + Dist;
                P_Sigma(W) := P_Sigma(V);
                Clear(P(W));
                Add_Last(V, P(W));
              elsif abs (P_Dist(W) - P_Dist(V) - Dist) < Nums_Tolerance then
                -- found another shortest path
                P_Sigma(W) := P_Sigma(W) + P_Sigma(V);
                Add_Last(V, P(W));
              end if;
            end if;
          end loop;
          Restore(El);

          -- Recalculate Minimum
          Min_Dist := Plus_Infinity;
          for T in 1..N loop
            if (not Visited(T)) and then P_Dist(T) < Min_Dist then
              Min_Dist := P_Dist(T);
              Min_V := T;
            end if;
          end loop;
        end loop;

        while not Is_Empty(St) loop
          W := Pop(St);
          Vw := Get_Vertex(Gr_Bet, W);
          Save(P(W));
          Reset(P(W));
          while Has_Next(P(W)) loop
            V := Next(P(W));
            Vv := Get_Vertex(Gr_Bet, V);
            Inc := P_Sigma(V) * (1.0 + P_Delta(W)) / P_Sigma(W);
            P_Delta(V) := P_Delta(V) + Inc;
            Evw := Get_Edge(Vv, Vw);
            Set_Value(Evw, Value(Evw) + Double(Inc));
          end loop;
          Restore(P(W));
        end loop;
      end loop;

      if Normalized then
        Normalize_Betweenness(Gr_Bet);
      end if;
    end if;

    Free(St);
    Free(P_Dist);
    Free(P_Sigma);
    Free(P_Delta);
    for V in 1..N loop
      Free(P(V));
    end loop;

    return Gr_Bet;
  end Edge_Betweenness;

  -----------------
  -- Betweenness --
  -----------------

  procedure Betweenness(Gr: in Graph; Vertex_Bet: out PNums; Edge_Bet: out Graphs_Double.Graph; Weighted: in Boolean := True; Normalized: in Boolean := True) is

    procedure Normalize_Betweenness(Gr_Bet: in Graphs_Double.Graph) is
      N, T: Positive;
      Vf: Graphs_Double.Vertex;
      El: Graphs_Double.Edges_List;
      E: Graphs_Double.Edge;
      Norm: Double;
    begin
      pragma Warnings(Off, El);
      N := Number_Of_Vertices(Gr_Bet);
      if N > 1 then
        Norm := Double(N * (N - 1));
        for F in 1..N loop
          Vf := Get_Vertex(Gr_Bet, F);
          El := Edges_From(Vf);
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            E := Next(El);
            T := Index_Of(To(E));
            if Is_Directed(Gr_Bet) or else F < T then
              Set_Value(E, Value(E) / Norm);
            end if;
          end loop;
          Restore(El);
        end loop;
      end if;
    end Normalize_Betweenness;

    package Linked_Lists_Pos is new Linked_Lists(Positive); use Linked_Lists_Pos;

    N: constant Positive := Gr.Size;
    P_Dist, P_Sigma, P_Delta: PNums;
    St: Stack;
    Visited: array(1..N) of Boolean;
    P: array(1..N) of Linked_Lists_Pos.Linked_List;

    Nums_Tolerance: constant Num := 1.0e-10;
    T, V, W, Min_V: Positive;
    Vx: Vertex;
    Vf, Vt, Vv, Vw: Graphs_Double.Vertex;
    El: Edges_List;
    E: Edge;
    Evw: Graphs_Double.Edge;
    Dist, Min_Dist, Inc: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Weighted and then Has_Links(Gr, Negative_Links) then
      raise Negative_Weights_Error;
    end if;

    Initialize(Edge_Bet, N, Is_Directed(Gr));
    for F in 1..N loop
      Vf := Get_Vertex(Edge_Bet, F);
      Vx := Get_Vertex(Gr, F);
      Set_Name(Vf, Get_Name(Vx));
      Set_Tag(Vf, Get_Tag(Vx));
      El := Edges_From(Vx);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        T := Index_Of(To(Next(El)));
        Vt := Get_Vertex(Edge_Bet, T);
        Add_Edge(Vf, Vt, 0.0);
      end loop;
      Restore(El);
    end loop;

    Vertex_Bet := Alloc(1, N);
    P_Dist := Alloc(1, N);
    P_Sigma := Alloc(1, N);
    P_Delta := Alloc(1, N);
    Initialize(St);
    for V in 1..N loop
      Initialize(P(V));
      Vertex_Bet(V) := 0.0;
    end loop;

    if N > 2 then
      for S in 1..N loop
        Clear(St);

        for T in 1..N loop
          Clear(P(T));
          P_Dist(T) := Plus_Infinity;
          P_Sigma(T) := 0.0;
          P_Delta(T) := 0.0;
          Visited(T) := False;
        end loop;
        P_Dist(S) := 0.0;
        P_Sigma(S) := 1.0;

        Min_V := S;
        Min_Dist := 0.0;
        while Min_Dist /= Plus_Infinity loop
          V := Min_V;
          Visited(V) := True;
          Push(V, St);
          Vx := Vertex'(Gr, V);
          El := Edges_From(Vx);
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            E := Next(El);
            W := Index_Of(To(E));
            if V /= W then
              if Weighted then
                Dist := To_Num(E.Value);
              else
                Dist := 1.0;
              end if;
              -- Relax
              if P_Dist(W) - P_Dist(V) - Dist > Nums_Tolerance then
                -- found new shortest path
                P_Dist(W) := P_Dist(V) + Dist;
                P_Sigma(W) := P_Sigma(V);
                Clear(P(W));
                Add_Last(V, P(W));
              elsif abs (P_Dist(W) - P_Dist(V) - Dist) < Nums_Tolerance then
                -- found another shortest path
                P_Sigma(W) := P_Sigma(W) + P_Sigma(V);
                Add_Last(V, P(W));
              end if;
            end if;
          end loop;
          Restore(El);

          -- Recalculate Minimum
          Min_Dist := Plus_Infinity;
          for T in 1..N loop
            if (not Visited(T)) and then P_Dist(T) < Min_Dist then
              Min_Dist := P_Dist(T);
              Min_V := T;
            end if;
          end loop;
        end loop;

        while not Is_Empty(St) loop
          W := Pop(St);
          Vw := Get_Vertex(Edge_Bet, W);
          Save(P(W));
          Reset(P(W));
          while Has_Next(P(W)) loop
            V := Next(P(W));
            Vv := Get_Vertex(Edge_Bet, V);
            Inc := P_Sigma(V) * (1.0 + P_Delta(W)) / P_Sigma(W);
            P_Delta(V) := P_Delta(V) + Inc;
            Evw := Get_Edge(Vv, Vw);
            Set_Value(Evw, Value(Evw) + Double(Inc));
          end loop;
          Restore(P(W));
          if W /= S then
            Vertex_Bet(W) := Vertex_Bet(W) + P_Delta(W);
          end if;
        end loop;
      end loop;

      if Normalized then
        for S in 1..N loop
          Vertex_Bet(S) := Vertex_Bet(S) / Num((N - 1) * (N - 2));
        end loop;
        Normalize_Betweenness(Edge_Bet);
      end if;
    end if;

    Free(St);
    Free(P_Dist);
    Free(P_Sigma);
    Free(P_Delta);
    for V in 1..N loop
      Free(P(V));
    end loop;

  end Betweenness;

  ------------------------------
  -- Linked_Nodes_Correlation --
  ------------------------------

  function Linked_Nodes_Correlation(Gr: in Graph; Pfrom, Pto: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson) return Num is
    Pf, Pt, Wh: PNums;
    Corr: Num;
    Ne, A: Natural;
    T: Positive;
    V: Vertex;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Pfrom = null or else Pfrom'Length /= Gr.Size then
      raise Incompatible_Values_Error;
    end if;
    if Pto = null or else Pto'Length /= Gr.Size then
      raise Incompatible_Values_Error;
    end if;

    Ne := Integer(Number_Of_Edges(Gr, Ls));
    if Ne <= 2 then
      return 1.0;
    end if;

    Pf := Alloc(1, Ne);
    Pt := Alloc(1, Ne);
    if Weighted then
      Wh := Alloc(1, Ne);
    end if;

    A := 0;
    for F in 1..Gr.Size loop
      V := Vertex'(Gr, F);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El, Ls) loop
        E := Next(El);
        T := Index_Of(To(E));
        A := A + 1;
        Pf(A) := Pfrom(F);
        Pt(A) := Pto(T);
        if Weighted then
          Wh(A) := abs Value(E, Ls);
        end if;
      end loop;
      Restore(El);
    end loop;

    if not Weighted then
      Corr := Correlation(Pf, Pt, Ct);
    else
      Corr := Correlation(Pf, Pt, Wh, Ct);
    end if;

    Free(Pf);
    Free(Pt);
    if Weighted then
      Free(Wh);
    end if;

    return Corr;
  end Linked_Nodes_Correlation;

  ------------------------------------
  -- Linked_Nodes_Correlation_Error --
  ------------------------------------

  function Linked_Nodes_Correlation_Error(Gr: in Graph; Pfrom, Pto: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson; Cet: in Stats.Correlation_Error_Type := Auto) return Num is
    Pf, Pt, Wh: PNums;
    Err: Num;
    Ne, A: Natural;
    T: Positive;
    V: Vertex;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Pfrom = null or else Pfrom'Length /= Gr.Size then
      raise Incompatible_Values_Error;
    end if;
    if Pto = null or else Pto'Length /= Gr.Size then
      raise Incompatible_Values_Error;
    end if;

    Ne := Integer(Number_Of_Edges(Gr, Ls));
    if Ne <= 2 then
      return 0.0;
    end if;

    Pf := Alloc(1, Ne);
    Pt := Alloc(1, Ne);
    if Weighted then
      Wh := Alloc(1, Ne);
    end if;

    A := 0;
    for F in 1..Gr.Size loop
      V := Vertex'(Gr, F);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El, Ls) loop
        E := Next(El);
        T := Index_Of(To(E));
        A := A + 1;
        Pf(A) := Pfrom(F);
        Pt(A) := Pto(T);
        if Weighted then
          Wh(A) := abs Value(E, Ls);
        end if;
      end loop;
      Restore(El);
    end loop;

    if not Weighted then
      Err := Correlation_Error(Pf, Pt, Ct, Cet);
    else
      Err := Correlation_Error(Pf, Pt, Wh, Ct, Cet);
    end if;

    Free(Pf);
    Free(Pt);
    if Weighted then
      Free(Wh);
    end if;

    return Err;
  end Linked_Nodes_Correlation_Error;

  -------------------
  -- Assortativity --
  -------------------

  function Assortativity(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson) return Num is
    Wf, Wt: PNums;
    A: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if not Weighted then
      Wf := Degree_From(Gr, Ls);
      Wt := Degree_To(Gr, Ls);
    else
      Wf := Strength_From(Gr, Ls);
      Wt := Strength_To(Gr, Ls);
    end if;
    A := Linked_Nodes_Correlation(Gr, Wf, Wt, Weighted, Ls, Ct);
    Free(Wf);
    Free(Wt);
    return A;
  end Assortativity;

  -------------------------
  -- Assortativity_Error --
  -------------------------

  function Assortativity_Error(Gr: in Graph; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links; Ct: in Stats.Correlation_Type := Pearson; Cet: in Stats.Correlation_Error_Type := Auto) return Num is
    Wf, Wt: PNums;
    Ae: Num;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if not Weighted then
      Wf := Degree_From(Gr, Ls);
      Wt := Degree_To(Gr, Ls);
    else
      Wf := Strength_From(Gr, Ls);
      Wt := Strength_To(Gr, Ls);
    end if;
    Ae := Linked_Nodes_Correlation_Error(Gr, Wf, Wt, Weighted, Ls, Ct, Cet);
    Free(Wf);
    Free(Wt);
    return Ae;
  end Assortativity_Error;

  -------------------------------
  -- Nearest_Neighbors_Average --
  -------------------------------

  function Nearest_Neighbors_Average(V: in Vertex; P: in PNums; Weighted: in Boolean := True; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return Num is
    Avg, Norm, Wij: Num;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
  begin
    if P = null or else P'Length /= V.Gr.Size then
      raise Incompatible_Values_Error;
    end if;

    I := Index_Of(V);
    Avg := 0.0;
    Norm := 0.0;

    case Ld is
      when From_Links =>
        El := Edges_From(V);
      when To_Links =>
        El := Edges_To(V);
    end case;
    Save(El);
    Reset(El);
    while Has_Next(El, Ls) loop
      E := Get(El);
      J := Index_Of(Next(El));
      if J /= I then
        if Weighted then
          Wij := Value(E, Ls);
          Avg := Avg + (abs Wij) * P(J);
          Norm := Norm + (abs Wij);
        else
          Avg := Avg + P(J);
          Norm := Norm + 1.0;
        end if;
      end if;
    end loop;
    Restore(El);

    if Norm /= 0.0 then
      return Avg / Norm;
    else
      return 0.0;
    end if;
  end Nearest_Neighbors_Average;

  ------------------------------------
  -- Nearest_Neighbors_Average_From --
  ------------------------------------

  function Nearest_Neighbors_Average_From(V: in Vertex; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Nearest_Neighbors_Average(V, P, Weighted, From_Links, Ls);
  end Nearest_Neighbors_Average_From;

  ----------------------------------
  -- Nearest_Neighbors_Average_To --
  ----------------------------------

  function Nearest_Neighbors_Average_To(V: in Vertex; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return Num is
  begin
    return Nearest_Neighbors_Average(V, P, Weighted, To_Links, Ls);
  end Nearest_Neighbors_Average_To;

  -------------------------------
  -- Nearest_Neighbors_Average --
  -------------------------------

  function Nearest_Neighbors_Average(Gr: in Graph; P: in PNums; Weighted: in Boolean := True; Ld: in Links_Direction; Ls: in Links_Subset := All_Links) return PNums is
    Pn: PNums;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Pn := Alloc(1, Gr.Size);
    for I in P'Range loop
      Pn(I) := Nearest_Neighbors_Average(Vertex'(Gr, I), P, Weighted, Ld, Ls);
    end loop;
    return Pn;
  end Nearest_Neighbors_Average;

  ------------------------------------
  -- Nearest_Neighbors_Average_From --
  ------------------------------------

  function Nearest_Neighbors_Average_From(Gr: in Graph; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return PNums is
  begin
    return Nearest_Neighbors_Average(Gr, P, Weighted, From_Links, Ls);
  end Nearest_Neighbors_Average_From;

  ----------------------------------
  -- Nearest_Neighbors_Average_To --
  ----------------------------------

  function Nearest_Neighbors_Average_To(Gr: in Graph; P: in PNums; Weighted: in Boolean := True; Ls: in Links_Subset := All_Links) return PNums is
  begin
    return Nearest_Neighbors_Average(Gr, P, Weighted, To_Links, Ls);
  end Nearest_Neighbors_Average_To;

end Graphs.Properties;
