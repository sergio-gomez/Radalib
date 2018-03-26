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


-- @filename Modularities_Extremal.adb
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2007
-- @revision 26/03/2018
-- @brief Extremal Modularity Optimization implementation (after J. Duch and A. Arenas)

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Minheaps;
with Linked_Lists;

package body Modularities_Extremal is

  ---------------
  -- Constants --
  ---------------

  Improvement_Tolerance: constant := 1.0e-10;

  --------------------------
  -- Randomly_Divide_List --
  --------------------------

  procedure Randomly_Divide_List(Gen: in out Generator; Module: in List; L1, L2: out List) is
    Lol: List_Of_Lists;
    E: Finite_Disjoint_Lists.Element;
  begin
    Lol := List_Of_Lists_Of(Module);
    L1 := New_List(Lol);
    L2 := New_List(Lol);
    Save(Module);
    Reset(Module);
    for I in 1..Number_Of_Elements(Module) loop
      E := Next_Element(Module);
      if Random(Gen) < 0.5 then
        Move(E, L1);
      else
        Move(E, L2);
      end if;
    end loop;
    Restore(Module);
  end Randomly_Divide_List;

  ----------------------------------
  -- Lowest_Fitness_Node_Movement --
  ----------------------------------

  procedure Lowest_Fitness_Node_Movement(Gr: in Graph; Mi: in Modularity_Info; Mt: in Modularity_Type;
    Sub_L1, Sub_L2: in List; Gen: in out Generator; Tau: in Float:= 1.6)
  is

    H_Size: constant Positive := Number_Of_Elements(Sub_L1) + Number_Of_Elements(Sub_L2);

    type Minheap_Element is record
      Index: Positive;
      Fitness: Float;
    end record;

    function "<"(Left, Right: in Minheap_Element) return Boolean is
    begin
      return Left.Fitness < Right.Fitness;
    end "<";

    package Fitness_Minheap is new Minheaps(Minheap_Element, "<"); use Fitness_Minheap;

    H: Minheap;
    He: Minheap_Element;
    Rand, Aux: Float;
    Node_Canvi, Num: Natural;
    Degree: Float;
    Qi: Double;
    V: Vertex;
    E: Element;
  begin
    Initialize(H, H_Size);

    Save(Sub_L1);
    Reset(Sub_L1);
    while Has_Next_Element(Sub_L1) loop
      E := Next_Element(Sub_L1);
      He.Index := Index_Of(E);
      V := Get_Vertex(Gr, He.Index);
      Degree := Float(Strength_From(Mi, V));
      Qi := Element_Modularity(Mi, E);
      if Degree /= 0.0 then
        He.Fitness := Float(Qi) / Degree;
        Add(He, H);
      end if;
    end loop;
    Restore(Sub_L1);

    Save(Sub_L2);
    Reset(Sub_L2);
    while Has_Next_Element(Sub_L2) loop
      E := Next_Element(Sub_L2);
      He.Index := Index_Of(E);
      V := Get_Vertex(Gr, He.Index);
      Degree := Float(Strength_From(Mi, V));
      Qi := Element_Modularity(Mi, E);
      if Degree /= 0.0 then
        He.Fitness := Float(Qi) / Degree;
        Add(He, H);
      end if;
    end loop;
    Restore(Sub_L2);

    Num := Number_Of_Elements(H);

    if Num > 0 then
      Rand := Random(Gen);
      Aux := Float(Num) ** (1.0 - Tau);
      Rand := Rand * (1.0 - Aux);
      Rand := 1.0 - Rand;
      Node_Canvi := Min(Num, Natural(Rand ** (1.0 / (1.0 - Tau))));
    else
      Node_Canvi := 0;
    end if;

    for I in 1..Node_Canvi-1 loop
      Delete_Min(H);
    end loop;

    if Node_Canvi > 0 then
      He := Head(H);
      E := Get_Element(List_Of_Lists_Of(Sub_L1), He.Index);
      if Belongs_To(E, Sub_L1) then
        Update_Modularity_Move_Element(Mi, E, Sub_L2, Mt);
      else
        Update_Modularity_Move_Element(Mi, E, Sub_L1, Mt);
      end if;
    end if;
    Free(H);
  end Lowest_Fitness_Node_Movement;

  ---------------
  -- Copy_List --
  ---------------

  procedure Copy_List(L: in List; Lol: in List_Of_Lists; L_Copy: in List) is
    I: Positive;
    E_Lol: Element;
  begin
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      E_Lol := Get_Element(Lol, I);
      Move(E_Lol, L_Copy);
    end loop;
    Restore(L);
  end Copy_List;

  -----------------------
  -- Break_And_Enqueue --
  -----------------------

  procedure Break_And_Enqueue(Gr: in Graph; Mi: in Modularity_Info; Mt: in Modularity_Type;
    L: in List; Lol: in List_Of_Lists; Q: in Queue; Q_Act: in out Double)
  is
    use Linked_Lists_Of_Lists;

    Ls: Linked_List;
    Lc, L_Copy: List;
    Q_Aux: Double;
  begin
    Save_Modularity(Mi, L);
    Update_List_Connected_Components(Gr, L, Ls);

    if Size(Ls) = 1 then
      L_Copy := New_List(Lol);
      Copy_List(L, Lol, L_Copy);
      if Number_Of_Elements(L) > 1 then
        Enqueue(L_Copy, Q);
      end if;
    else
      Reset(Ls);
      while Has_Next(Ls) loop
        Lc := Next(Ls);
        Update_Modularity(Mi, Lc, Mt);
      end loop;
      Q_Aux := Total_Modularity(Mi);
      if Q_Aux > Q_Act + Improvement_Tolerance then
        Q_Act := Q_Aux;
        Reset(Ls);
        while Has_Next(Ls) loop
          Lc := Next(Ls);
          L_Copy := New_List(Lol);
          Copy_List(Lc, Lol, L_Copy);
          if Number_Of_Elements(Lc) > 1 then
            Enqueue(L_Copy, Q);
          end if;
        end loop;
      else
        Reset(Ls);
        while Has_Next(Ls) loop
          Lc := Next(Ls);
          if Lc /= L then
            Move(Lc, L);
            Remove(Lc);
          end if;
        end loop;
        L_Copy := New_List(Lol);
        Copy_List(L, Lol, L_Copy);
        Enqueue(L_Copy, Q);
        Restore_Modularity(Mi, L);
      end if;
    end if;

    Free(Ls);
  end Break_And_Enqueue;

  --------------------------
  -- Optimization_Process --
  --------------------------

  procedure Optimization_Process(Gr: in Graph; Mi: in Modularity_Info; Mt: in Modularity_Type;
    Module: in out List; Lol_Best: in List_Of_Lists; Q: in Queue; Q_Ini: in Double;
    Gen: in out Generator)
  is
    Lol: List_Of_Lists;
    Module_Best, L1, L2, L1_Aux, L2_Aux: List;
    E: Element;
    Nc, I: Positive;
    Steps: Natural;
    Q_Act, Q_Max: Double;
    Max_Nonimpr: Natural;
  begin
    -- Initializations
    Nc := Number_Of_Elements(Module);
    if Nc <= 100 then
      Max_Nonimpr := 10 * Nc;                         --       (0, 0) - (100, 1000)
    elsif Nc <= 400 then
      Max_Nonimpr := 1000 + 5 * (Nc - 100);           --  (100, 1000) - (400, 2500)
    elsif Nc <= 1000 then
      Max_Nonimpr := 2500 + 2 * (Nc - 400);           --  (400, 2500) - (1000, 3700)
    elsif Nc <= 2000 then
      Max_Nonimpr := 3700 + 4 * (Nc - 1000) / 5;      -- (1000, 3700) - (2000, 4500)
    elsif Nc <= 5000 then
      Max_Nonimpr := 4500 + 1 * (Nc - 2000) / 5;      -- (2000, 4500) - (5000, 5100)
    else
      Max_Nonimpr := 5100;
    end if;

    Lol := List_Of_Lists_Of(Module);
    Reset(Module);
    E := Get_Element(Module);
    I := Index_Of(E);
    Module_Best := List_Of(Get_Element(Lol_Best, I));
    Q_Max := Q_Ini;

    -- Random split
    Save_Modularity(Mi, Module);
    Randomly_Divide_List(Gen, Module, L1_Aux, L2_Aux);
    Update_Modularity(Mi, L1_Aux, Mt);
    Update_Modularity(Mi, L2_Aux, Mt);
    Q_Act := Total_Modularity(Mi);

    -- Warm-up
    Steps := 0;
    while Q_Act <= Q_Ini + Improvement_Tolerance and Steps <= Max_Nonimpr loop
      Lowest_Fitness_Node_Movement(Gr, Mi, Mt, L1_Aux, L2_Aux, Gen);
      Q_Act := Total_Modularity(Mi);
      Steps := Steps + 1;
    end loop;

    -- Optimization process
    if Q_Act > Q_Ini + Improvement_Tolerance then
      Steps := 0;
      Q_Max := Q_Act;
      L1 := New_List(Lol_Best);
      L2 := New_List(Lol_Best);
      Copy_List(L1_Aux, Lol_Best, L1);
      Copy_List(L2_Aux, Lol_Best, L2);
      while Steps <= Max_Nonimpr loop
        Lowest_Fitness_Node_Movement(Gr, Mi, Mt, L1_Aux, L2_Aux, Gen);
        Q_Act := Total_Modularity(Mi);
        if Q_Act > Q_Max + Improvement_Tolerance then
          Copy_List(L1_Aux, Lol_Best, L1);
          Copy_List(L2_Aux, Lol_Best, L2);
          Q_Max := Q_Act;
          Steps := 0;
        else
          Steps := Steps + 1;
        end if;
      end loop;
      Update_Modularity(Mi, L1, Mt);
      Update_Modularity(Mi, L2, Mt);
      Break_And_Enqueue(Gr, Mi, Mt, L1, Lol, Q, Q_Max);
      Break_And_Enqueue(Gr, Mi, Mt, L2, Lol, Q, Q_Max);
      Remove(Module);
      Remove(Module_Best);
    else
      Move(L1_Aux, Module);
      Move(L2_Aux, Module);
      Restore_Modularity(Mi, Module);
    end if;
    Remove(L1_Aux);
    Remove(L2_Aux);
  end Optimization_Process;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Ustring;
    Gen: in out Generator; Lol_Ini: in List_Of_Lists; Lol_Best: in List_Of_Lists;
    Q_Best: out Modularity_Rec; R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Lol_Rep: List_Of_Lists;
    Mi: Modularity_Info;
    Q: Queue;
    Q_Prev: Double;
    L: List;
    Nc_Prev, Nc_Best: Natural;
  begin
    Initialize(Mi, Gr, Mt, R, Pc);
    Initialize(Q);

    Lol_Rep := Clone(Lol_Ini);
    Save(Lol_Rep);
    Reset(Lol_Rep);
    while Has_Next_List(Lol_Rep) loop
      L := Next_List(Lol_Rep);
      if Number_Of_Elements(L) > 1 then
        Enqueue(L, Q);
      end if;
    end loop;
    Restore(Lol_Rep);
    Update_Modularity(Mi, Lol_Rep, Mt);

    while not Is_Empty(Q) loop
      L := Dequeue(Q);
      Q_Prev := Total_Modularity(Mi);
      Nc_Prev := Number_Of_Lists(Lol_Best);
      Optimization_Process(Gr, Mi, Mt, L, Lol_Best, Q, Q_Prev, Gen);
      Q_Best := Total_Modularity(Mi);
      Nc_Best := Number_Of_Lists(Lol_Best);
      if Nc_Best > Nc_Prev then
        Improvement_Action(Log_Name, Lol_Best, Q_Best);
      end if;
    end loop;

    Remove_Empty(Lol_Best);
    Sort_Lists(Lol_Best);

    Free(Lol_Rep);
    Free(Mi);
    Free(Q);
  end Execute_Repetition;

  -------------------------
  -- Extremal_Modularity --
  -------------------------

  procedure Extremal_Modularity(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists; Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Gen: Generator;
    Lol_Rep: List_Of_Lists;
    Q_Rep: Modularity_Rec;
  begin
    Reset(Gen, 1234);
    Initialize(Lol_Best, Number_Of_Vertices(Gr), Together_Initialization);
    for NR in 1 .. Number_Of_Repetitions loop
      Lol_Rep := Clone(Lol_Ini);
      Execute_Repetition(Mt, Gr, Log_Name, Gen, Lol_Ini, Lol_Rep, Q_Rep, R, Pc);
      if NR = 1 or else Q_Rep.Total > Q_Best.Total then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Extremal_Modularity;

end Modularities_Extremal;
