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


-- @filename Modularities_Fast.adb
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2007
-- @revision 31/08/2020
-- @brief Newman's Fast Algorithm implementation

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularities_Fast is

  ---------------------------------
  -- Create_Adjacent_Lists_Graph --
  ---------------------------------

  procedure Create_Adjacent_Lists_Graph(Gr_Neigh: in Graph; Lol: in List_Of_Lists; Mi: in Modularity_Info;
    Mt: in Modularity_Type; Gr_Adj: out Graph; Lists_Map: out PLists_Vector)
  is
    package List_Id_Sets is new Ada.Containers.Ordered_Sets(Integer);
    use List_Id_Sets;

    Directed: Boolean;
    N, N_Adj: Natural;
    L: List;
    El: Edges_List;
    E: Edge;
    Vi, Vj, Vi_Adj, Vj_Adj: Vertex;
    Vertex_To_List_Index: Pintegers;
    I, J, I_Adj, J_Adj: Positive;
    S: Set;
    Delta_Q: Double;
  begin
    pragma Warnings(Off, L);
    pragma Warnings(Off, El);
    Directed := Is_Directed(Gr_Neigh);
    N := Number_Of_Vertices(Gr_Neigh);
    N_Adj:= Number_Of_Lists(Lol);
    Initialize(Gr_Adj, N_Adj, Directed => False);

    -- Initialize mappings
    Vertex_To_List_Index := Alloc(1, N);
    Lists_Map := new Lists_Vector(1..N_Adj);
    I_Adj := 1;
    Remove_Empty(Lol);
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      Lists_Map(I_Adj) := L;
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        I := Index_Of(Next_Element(L));
        Vertex_To_List_Index(I) := I_Adj;
      end loop;
      Restore(L);
      I_Adj := I_Adj + 1;
    end loop;
    Restore(Lol);

    -- Create Edges of Adjacent Lists Graph
    I_Adj := 1;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Clear(S);
      L := Next_List(Lol);
      Save(L);
      Reset(L);
      while Has_Next_Element(L) and Natural(Length(S)) < N_Adj - I_Adj loop
        I := Index_Of(Next_Element(L));
        Vi := Get_Vertex(Gr_Neigh, I);
        El := Edges_From(Vi);
        Save(El);
        Reset(El);
        while Has_Next(El) and Natural(Length(S)) < N_Adj - I_Adj loop
          E := Next(El);
          Vj := To(E);
          J := Index_Of(Vj);
          J_Adj := Vertex_To_List_Index(J);
          if I_Adj < J_Adj and then not Contains(S, J_Adj) then
            Vi_Adj := Get_Vertex(Gr_Adj, I_Adj);
            Vj_Adj := Get_Vertex(Gr_Adj, J_Adj);
            Delta_Q := Modularity_Merging_Variation(Mi, Mt, Lists_Map(I_Adj), Lists_Map(J_Adj));
            Add_Edge(Vi_Adj, Vj_Adj, Delta_Q);
            Insert(S, J_Adj);
          end if;
        end loop;
        Restore(El);
      end loop;
      Restore(L);
      I_Adj := I_Adj + 1;
    end loop;
    Restore(Lol);

    Free(Vertex_To_List_Index);
  end Create_Adjacent_Lists_Graph;

  ----------------------------------
  -- Modularity_Merging_Variation --
  ----------------------------------

  function Modularity_Merging_Variation(Mi: in Modularity_Info; Mt: in Modularity_Type; Li, Lj: in List) return Double is
    Lf, Lt: List;
    Q_Ini, Q_End: Double;
  begin
    pragma Warnings(Off, Lf);
    if Number_Of_Elements(Li) < Number_Of_Elements(Lj) then
      Lf := Li;
      Lt := Lj;
    else
      Lf := Lj;
      Lt := Li;
    end if;
    declare
      Nf: constant Natural := Number_Of_Elements(Lf);
      Moved: Integers(1..Nf);
      E: Finite_Disjoint_Lists.Element;
      Idx: Natural;
    begin
      Save_Modularity(Mi, Li);
      Save_Modularity(Mi, Lj);
      -- Merge
      Idx := 0;
      Save(Lf);
      Reset(Lf);
      while Has_Next_Element(Lf) loop
        E := Next_Element(Lf);
        Idx := Idx + 1;
        Moved(Idx) := Index_Of(E);
        Update_Modularity_Move_Element(Mi, E, Lt, Mt);
      end loop;
      Restore(Lf);
      Q_End := Partial_Modularity(Mi, Lt);
      -- Undo merging
      for K in 1..Nf loop
        E := Get_Element(List_Of_Lists_Of(Lf), Moved(K));
        Move(E, Lf);
      end loop;
      Restore_Modularity(Mi, Li);
      Restore_Modularity(Mi, Lj);
      Q_Ini := Partial_Modularity(Mi, Lf) + Partial_Modularity(Mi, Lt);
    end;
    return Q_End - Q_Ini;
  end Modularity_Merging_Variation;

  --------------------------------
  -- Maximum_Modularity_Merging --
  --------------------------------

  procedure Maximum_Modularity_Merging(Gr_Adj: in Graph; I_Max, J_Max: out Positive; Max_Delta_Q: out Double) is
    N_Adj: Natural;
    El: Edges_List;
    E: Edge;
    Vi, Vj: Vertex;
    J: Positive;
    Delta_Q: Double;
  begin
    pragma Warnings(Off, El);
    Max_Delta_Q := Double'First;
    I_Max := Positive'Last;
    J_Max := Positive'Last;
    N_Adj := Number_Of_Vertices(Gr_Adj);
    for I in 1..N_Adj loop
      Vi := Get_Vertex(Gr_Adj, I);
      El := Edges_From(Vi);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vj := To(E);
        J := Index_Of(Vj);
        if I < J then
          Delta_Q := Value(E);
          if Delta_Q > Max_Delta_Q then
            Max_Delta_Q := Delta_Q;
            I_Max := I;
            J_Max := J;
          end if;
        end if;
      end loop;
      Restore(El);
    end loop;
  end Maximum_Modularity_Merging;

  --------------------------
  -- Merge_Adjacent_Lists --
  --------------------------

  procedure Merge_Adjacent_Lists(Gr_Adj: in Graph; Lists_Map: in PLists_Vector; I, J: in Positive;
    Mi: in Modularity_Info; Mt: in Modularity_Type; Delta_Q_Merge: in Double)
  is
    package Dq_Maps is new Ada.Containers.Ordered_Maps(Integer, Double);
    use Dq_Maps;

    Li, Lj, Lk, Ls, Lr: List;
    El: Edges_List;
    E: Edge;
    Vi, Vj, Vk, Vs, Vr: Vertex;
    Mps, Mprs: Map;
    C: Cursor;
    K: Positive;
    Delta_Q: Double;
  begin
    pragma Warnings(Off, El);
    Vi := Get_Vertex(Gr_Adj, I);
    Vj := Get_Vertex(Gr_Adj, J);
    Li := Lists_Map(I);
    Lj := Lists_Map(J);
    -- Choose Lists
    if Degree_From(Vi) > Degree_From(Vj) then
      Vs := Vi;
      Vr := Vj;
      Ls := Li;
      Lr := Lj;
    else
      Vs := Vj;
      Vr := Vi;
      Ls := Lj;
      Lr := Li;
    end if;
    -- Edges from Save node
    El := Edges_From(Vs);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Vk := To(E);
      K := Index_Of(Vk);
      if Vk /= Vr then
        Insert(Mps, K, Value(E));
      end if;
    end loop;
    Restore(El);
    -- Edges from Remove node
    El := Edges_From(Vr);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Vk := To(E);
      K := Index_Of(Vk);
      if Vk /= Vs then
        Delta_Q := Value(E);
        if Contains(Mps, K) then
          Delta_Q := Delta_Q + Dq_Maps.Element(Mps, K);
          Insert(Mprs, K, Delta_Q);
        else
          Lk := Lists_Map(K);
          Delta_Q := Delta_Q + Modularity_Merging_Variation(Mi, Mt, Ls, Lk);
          Add_Edge(Vs, Vk, Delta_Q);
        end if;
      end if;
    end loop;
    Restore(El);
    -- Update Edges from Save node
    C := First(Mps);
    while Has_Element(C) loop
      K := Key(C);
      Vk := Get_Vertex(Gr_Adj, K);
      E := Get_Edge_Or_No_Edge(Vs, Vk);
      if Contains(Mprs, K) then
        Delta_Q := Dq_Maps.Element(Mprs, K);
      else
        Delta_Q := Dq_Maps.Element(C);
        Lk := Lists_Map(K);
        Delta_Q := Delta_Q  + Modularity_Merging_Variation(Mi, Mt, Lr, Lk);
      end if;
      Set_Value(E, Delta_Q);
      Next(C);
    end loop;
    -- Merge Lists
    Move(Lr, Ls);
    Remove(Lr);
    -- Remove Edges
    El := Edges_From(Vr);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Remove(E);
    end loop;
    Restore(El);
  end Merge_Adjacent_Lists;

  --------------------------
  -- Optimization Process --
  --------------------------

  procedure Optimization_Process(Mi: in Modularity_Info; Mt: in Modularity_Type; Gr_Adj: in Graph;
    Lists_Map: in PLists_Vector; Steps: in Positive; Finished: out Boolean)
  is
    I_Max, J_Max: Positive;
    Max_Delta_Q: Double;
  begin
    for Iters in 1..Steps loop
      Maximum_Modularity_Merging(Gr_Adj, I_Max, J_Max, Max_Delta_Q);
      Finished := Max_Delta_Q < 0.0;
      if Finished then
        return;
      else
        Merge_Adjacent_Lists(Gr_Adj, Lists_Map, I_Max, J_Max, Mi, Mt, Max_Delta_Q);
      end if;
    end loop;
  end Optimization_Process;

  ---------------
  -- Get_Steps --
  ---------------

  function Get_Steps(Nv, Nl: in Natural) return Positive is
    Steps: Natural := 10;
  begin
    if Nv <= 1_000 then
      Steps := 50;
    elsif Nv <= 10_000 then
      if Nl <= 100 then
        Steps := 20;
      elsif Nl <= 1000 then
        Steps := 50;
      else
        Steps := 100;
      end if;
    else
      if Nl <= 100 then
        Steps := 10;
      elsif Nl <= 1000 then
        Steps := 30;
      else
        Steps := 100;
      end if;
    end if;
    return Steps;
  end Get_Steps;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph;
    Log_Name: in Ustring; Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Mi: Modularity_Info;
    Q_Total: Modularity_Rec;
    Gr_Neigh: Graph;
    Gr_Adj: Graph;
    Lists_Map: PLists_Vector;
    Steps: Positive;
    Finished: Boolean;
  begin
    Initialize(Mi, Gr, Mt, R, Pc);
    Gr_Neigh := Neighbors_Graph(Mi);
    Lol_Best := Clone(Lol_Ini);
    Update_Modularity(Mi, Lol_Best, Mt);
    Q_Total := Total_Modularity(Mi);
    Create_Adjacent_Lists_Graph(Gr_Neigh, Lol_Best, Mi, Mt, Gr_Adj, Lists_Map);
    Steps := Get_Steps(Number_Of_Vertices(Gr), Number_Of_Vertices(Gr_Adj));
    Finished := False;
    while not Finished loop
      Optimization_Process(Mi, Mt, Gr_Adj, Lists_Map, Steps, Finished);
      Update_Modularity(Mi, Lol_Best, Mt);
      Q_Total := Total_Modularity(Mi);
      Improvement_Action(Log_Name, Lol_Best, Q_Total);
    end loop;
    Remove_Empty(Lol_Best);
    Sort_Lists(Lol_Best);
    Q_Best := Modularity(Mi, Lol_Best, Mt);
    Free(Gr_Adj);
    Free(Lists_Map);
    Free(Mi);
  end Execute_Repetition;

  --------------------
  -- Fast_Algorithm --
  --------------------

  procedure Newman_Fast_Algorithm(Mt: in Modularity_Type; Gr: in Graph;
    Log_Name: in Ustring; Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Lol_Rep: List_Of_Lists;
    Q_Rep : Modularity_Rec;
  begin
    Initialize(Lol_Best, Number_Of_Vertices(Gr), Together_Initialization);
    for NR in 1 .. Number_Of_Repetitions loop
      Execute_Repetition(MT, Gr, Log_Name, Lol_Ini, Lol_Rep, Q_Rep, R, Pc);
      if NR = 1 or else Q_Rep.Total > Q_Best.Total then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Newman_Fast_Algorithm;

end Modularities_Fast;
