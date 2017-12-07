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


-- @filename Modularities_Fast.adb
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2007
-- @revision 26/10/2014
-- @brief Newman's Fast Algorithm implementation

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularities_Fast is

  ---------------------------------
  -- Create_Adjacent_Lists_Graph --
  ---------------------------------

  procedure Create_Adjacent_Lists_Graph(Gr: in Graph; Lol: in List_Of_Lists; Gr_Adj: out Graph; Lists_Map: out PLists_Vector) is
    Directed: Boolean;
    N, N_Adj: Natural;
    L: List;
    El: Edges_List;
    E, E_Adj: Edge;
    Vi, Vj, Vi_Adj, Vj_Adj: Vertex;
    Vertex_To_List_Index: Pintegers;
    J, I_Adj, J_Adj: Positive;
  begin
    pragma Warnings(Off, L);
    pragma Warnings(Off, El);
    Directed := Is_Directed(Gr);
    N := Number_Of_Vertices(Gr);
    N_Adj:= Number_Of_Lists(Lol);
    Initialize(Gr_Adj, N_Adj, Directed => False);

    -- Initialize mappings
    Vertex_To_List_Index := Alloc(1, N);
    Lists_Map := new Lists_Vector(1..N_Adj);
    J_Adj := 1;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      Lists_Map(J_Adj) := L;
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        J := Index_Of(Next_Element(L));
        Vertex_To_List_Index(J) := J_Adj;
      end loop;
      Restore(L);
      J_Adj := J_Adj + 1;
    end loop;
    Restore(Lol);

    -- Create Edges of Adjacent Lists Graph
    for I in 1..N loop
      Vi := Get_Vertex(Gr, I);
      I_Adj := Vertex_To_List_Index(I);
      Vi_Adj := Get_Vertex(Gr_Adj, I_Adj);
      El := Edges_From(Vi);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vj := To(E);
        J := Index_Of(Vj);
        J_Adj := Vertex_To_List_Index(J);
        Vj_Adj := Get_Vertex(Gr_Adj, J_Adj);
        if (Directed and I_Adj /= J_Adj) or (not Directed and I_Adj < J_Adj) then
          E_Adj := Get_Edge_Or_No_Edge(Vi_Adj, Vj_Adj);
          if E_Adj = No_Edge then
            Add_Edge(Vi_Adj, Vj_Adj, 0.0);
          end if;
        end if;
      end loop;
      Restore(El);
    end loop;

    Free(Vertex_To_List_Index);
  end Create_Adjacent_Lists_Graph;

  -------------------------------------
  -- Initialize_Adjacent_Lists_Graph --
  -------------------------------------

  procedure Initialize_Adjacent_Lists_Graph(Gr_Adj: in Graph; Lists_Map: in PLists_Vector;
    Mi: in Modularity_Info; Mt: in Modularity_Type)
  is
    N_Adj: Natural;
    El: Edges_List;
    E: Edge;
    Vi, Vj: Vertex;
    J: Positive;
    Delta_Q: Double;
  begin
    pragma Warnings(Off, El);
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
          Delta_Q := Modularity_Merging_Variation(Mi, Mt, Lists_Map(I), Lists_Map(J));
          Set_Value(E, Delta_Q);
        end if;
      end loop;
      Restore(El);
    end loop;
  end Initialize_Adjacent_Lists_Graph;

  ----------------------------------
  -- Modularity_Merging_Variation --
  ----------------------------------

  function Modularity_Merging_Variation(Mi: in Modularity_Info; Mt: in Modularity_Type; Li, Lj: in List) return Double is
    Nj: constant Natural := Number_Of_Elements(Lj);
    Moved: Integers(1..Nj);
    E: Finite_Disjoint_Lists.Element;
    Idx: Natural;
    Delta_Q: Double;
  begin
    -- Merge
    Idx := 0;
    Save(Lj);
    Reset(Lj);
    while Has_Next_Element(Lj) loop
      E := Next_Element(Lj);
      Idx := Idx + 1;
      Moved(Idx) := Index_Of(E);
      Move(E, Li);
    end loop;
    Restore(Lj);
    Update_Modularity(Mi, Li, Mt);
    Delta_Q := Total_Modularity(Mi);
    -- Undo merging
    for K in 1..Nj loop
      E := Get_Element(List_Of_Lists_Of(Li), Moved(K));
      Move(E, Lj);
    end loop;
    Update_Modularity(Mi, Li, Mt);
    Update_Modularity(Mi, Lj, Mt);
    Delta_Q := Delta_Q - Total_Modularity(Mi);
    return Delta_Q;
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
    Mi: in Modularity_Info; Mt: in Modularity_Type)
  is
    Li, Lj, Lk: List;
    El: Edges_List;
    E: Edge;
    Vi, Vj, Vk: Vertex;
    K: Positive;
    Delta_Q: Double;
  begin
    pragma Warnings(Off, El);
    Vi := Get_Vertex(Gr_Adj, I);
    Vj := Get_Vertex(Gr_Adj, J);
    -- Merge Lists
    Li := Lists_Map(I);
    Lj := Lists_Map(J);
    Move(Lj, Li);
    Remove(Lj);
    -- Merge Edges
    El := Edges_From(Vj);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Vk := To(E);
      if not Edge_Exists(Vi, Vk) then
        K := Index_Of(Vk);
        Add_Edge(Vi, Vk);
      end if;
    end loop;
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Remove(E);
    end loop;
    Restore(El);
    -- Recalculate weights
    El := Edges_From(Vi);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Vk := To(E);
      K := Index_Of(Vk);
      Lk := Lists_Map(K);
      Delta_Q := Modularity_Merging_Variation(Mi, Mt, Li, Lk);
      Set_Value(E, Delta_Q);
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
        Merge_Adjacent_Lists(Gr_Adj, Lists_Map, I_Max, J_Max, Mi, Mt);
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
    Log_Name: in Unbounded_String; Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Mi: Modularity_Info;
    Q_Total: Modularity_Rec;
    Gr_Adj: Graph;
    Lists_Map: PLists_Vector;
    Steps: Positive;
    Finished: Boolean;
  begin
    Initialize(Mi, Gr, Mt, R, Pc);
    Lol_Best := Clone(Lol_Ini);
    Update_Modularity(Mi, Lol_Best, Mt);
    Q_Total := Total_Modularity(Mi);
    Create_Adjacent_Lists_Graph(Gr, Lol_Best, Gr_Adj, Lists_Map);
    Initialize_Adjacent_Lists_Graph(Gr_Adj, Lists_Map, Mi, Mt);
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
    Log_Name: in Unbounded_String; Lol_Ini: in List_Of_Lists;
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
