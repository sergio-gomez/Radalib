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


-- @filename Modularities_Extremal.adb
-- @author Javier Borge
-- @version 1.0
-- @date 20/11/2007
-- @revision 29/07/2017
-- @brief Extremal Modularity Optimization implementation (after J. Duch and A. Arenas)

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Minheaps;

package body Modularities_Extremal is

  -------------------------------------------------------------------------
  -- Constants --
  ---------------

  Maximum_Of_Nonimprovements: constant Positive := 100;
  Improvement_Tolerance: constant := 1.0e-10;

  -------------------------------------------------------------------------
  -- Randomly_Divide_List --
  --------------------------

  procedure Randomly_Divide_List(Lol: in out List_Of_Lists; Gen: in out Generator;
    Modul: in out List; L1, L2: out List)
  is
    E: Finite_Disjoint_Lists.Element;
  begin
    L1 := New_List(Lol);
    L2 := New_List(Lol);
    Reset(Modul);
    for I in 1..Number_Of_Elements(Modul) loop
      E := Next_Element(Modul);
      if Random(Gen) < 0.5 then
        Move(E, L1);
      else
        Move(E, L2);
      end if;
    end loop;
    Remove(Modul);
  end Randomly_Divide_List;

  -------------------------------------------------------------------------
  -- Lowest Fitness Node Movement --
  ----------------------------------

  procedure Lowest_Fitness_Node_Movement(Gr: in Graph; Mi: in Modularity_Info;
    Sub_L1, Sub_L2: in List; Gen: in out Generator; Tau: in Float:= 1.6)
  is

    M_Size: constant Positive:= Number_Of_Elements(Sub_L1) + Number_Of_Elements(Sub_L2);

    type Minheap_Element is record
      Index: Positive;
      Fitness: Float;
    end record;

    function "<"(Left, Right: in Minheap_Element) return Boolean is
    begin
      if Left.Fitness < Right.Fitness then return True;
      else return False; end if;
    end "<";

    package Fitness_Minheap is new Minheaps(Minheap_Element, "<"); use Fitness_Minheap;

    M: Minheap;
    E: Minheap_Element;
    Rand, Aux: Float;
    Node_Canvi: Natural;
    Degree: Float;
    Q_i: Long_Float;
    V: Vertex;
    Lol_Element: Finite_Disjoint_Lists.Element;
  begin
    Initialize(M, M_Size);

    Save(Sub_L1);
    Reset(Sub_L1);
    while Has_Next_Element(Sub_L1) loop
      Lol_Element := Next_Element(Sub_L1);
      E.Index := Index_Of(Lol_Element);
      V := Get_Vertex(Gr, E.Index);
      Degree := Float(Strength_From(Mi, V));
      Q_i := Element_Modularity(Mi, Lol_Element);
      if Degree /= 0.0 then
        E.Fitness := Float(Q_I) / Degree;
        Add(E, M);
      end if;
    end loop;
    Restore(Sub_L1);

    Save(Sub_L2);
    Reset(Sub_L2);
    while Has_Next_Element(Sub_L2) loop
      Lol_Element := Next_Element(Sub_L2);
      E.Index := Index_Of(Lol_Element);
      V := Get_Vertex(Gr, E.Index);
      Degree := Float(Strength_From(Mi, V));
      Q_i := Element_Modularity(Mi, Lol_Element);
      if Degree /= 0.0 then
        E.Fitness := Float(Q_I) / Degree;
        Add(E, M);
      end if;
    end loop;
    Restore(Sub_L2);
    if Number_Of_Elements (M) > 0 then
      Rand := Random(Gen);
      Aux := Float(Number_Of_Elements(M)) ** (1.0-Tau);
      Rand := Rand * (1.0 - Aux);
      Rand := 1.0 - Rand;
      Node_Canvi := Natural(Rand ** (1.0 / (1.0 - Tau)));
    else
      Node_Canvi := 0;
    end if;

    for I in 1..Node_Canvi-1 loop
      Delete_Min(M);
    end loop;

    if Node_Canvi > 0 then
      E := Head(M);
      Lol_Element := Get_Element(List_Of_Lists_Of(Sub_L1), E.Index);
      if Belongs_To(Lol_Element, Sub_L1) then
        Move(Lol_Element, Sub_L2);
      else
        Move (Lol_Element, Sub_L1);
      end if;
    end if;
    Free(M);
  end Lowest_Fitness_Node_Movement;

  -------------------------------------------------------------------------
  -- Copy Lists from Different Lols     --
  -- (copy L1 from a Lol to L2 in Lol2) --
  ----------------------------------------

  procedure Copy_Lols_Lists(Source_L, Destination_L: in List; Destination_Lol: in List_Of_Lists) is
    Valor: Positive;
  begin
    Save(Source_L);
    Reset(Source_L);
    while Has_Next_Element(Source_L) loop
      Valor := Index_Of(Next_Element(Source_L));
      Move(Get_Element(Destination_Lol, Valor), Destination_L);
    end loop;
    Restore(Source_L);
  end Copy_Lols_Lists;

  -------------------------------------------------------------------------
  -- Break and Enqueue --
  -----------------------

   procedure Break_And_Enqueue(Gr: in Graph; Lol_Best: in List_Of_Lists;
     Sub_L1, Sub_L2: in List; Q: in Queue)
   is

     type Visited is array(1..Number_Of_Vertices(Gr)) of Boolean;

     Lol_Aux: List_Of_Lists;
     Aux_List, Added_List: List;
     E: Finite_Disjoint_Lists.Element;
     Value: Positive;
     Revised: Visited := (others => False);
   begin
     pragma Warnings(Off, Aux_List);
     -- "Simulate" edge breaking (result on Lol_Aux)
     Connected_Components(Gr, Lol_Best, Lol_Aux);

     -- Detect and Enqueue new lists
     if Number_Of_Lists(Lol_Best) < Number_Of_Lists (Lol_Aux) then
       Save(Sub_L1);
       Reset(Sub_L1);
       while Has_Next_Element(Sub_L1) loop
         E := Next_Element(Sub_L1);
         Value:= Index_Of(E);
         if not Revised(Value) then
           Added_List := New_List(Lol_Best);
           Aux_List := List_Of(Get_Element(Lol_Aux, Value));
           Save(Aux_List);
           Reset(Aux_List);
           while Has_Next_Element(Aux_List) loop
             Value := Index_Of(Next_Element(Aux_List));
             Revised(Value):= True;
             Move(Get_Element(Lol_Best, Value), Added_List);
           end loop;
           Restore(Aux_List);
           if Number_Of_Elements(Added_List) > 0 then
             Enqueue(Added_List, Q);
           else
             Remove(Added_List);
           end if;
         end if;
       end loop;
       Restore(Sub_L1);
       Remove(Sub_L1);

       Save(Sub_L2);
       Reset(Sub_L2);
       while Has_Next_Element(Sub_L2) loop
         E := Next_Element(Sub_L2);
         Value := Index_Of(E);
         if not Revised(Value) then
           Added_List := New_List(Lol_Best);
           Aux_List := List_Of(Get_Element(Lol_Aux, Value));
           Save(Aux_List);
           Reset(Aux_List);
           while Has_Next_Element(Aux_List) loop
             Value := Index_Of(Next_Element(Aux_List));
             Revised(Value) := True;
             Move(Get_Element(Lol_Best, Value), Added_List);
           end loop;
           Restore(Aux_List);
           if Number_Of_Elements(Added_List) > 0 then
             Enqueue(Added_List, Q);
           else
             Remove(Added_List);
           end if;
         end if;
       end loop;
       Restore(Sub_L2);
       Remove(Sub_L2);
     else
       if Number_Of_Elements(Sub_L1) > 0 then
         Enqueue(Sub_L1, Q);
       else
         Remove(Sub_L1);
       end if;
       if Number_Of_Elements(Sub_L2) > 0 then
         Enqueue(Sub_L2, Q);
       else
         Remove(Sub_L2);
       end if;
     end if;
     Free(Lol_Aux);
   end Break_And_Enqueue;

  -------------------------------------------------------------------------
  -- Optimization Process --
  --------------------------

  procedure Optimization_Process(Gr: in Graph; Mi: in Modularity_Info; Mt: in Modularity_Type;
    Modul: in List; Lol_Best: in List_Of_Lists; Q: in Queue; Q_Ini: in Long_Float;
    Gen: in out Generator)
  is
    Lol_Actual: List_Of_Lists;
    L1, L2, Modul_Aux, L1_Aux, L2_Aux: List;
    Steps: Natural;
    Pos: Positive;
    Q_Act, Q_Max: Long_Float;
    Max_Nonimpr: Natural;
  begin
    Lol_Actual:= Clone(Lol_Best);
    Max_Nonimpr := Min(2 * Number_Of_Elements(Modul), Maximum_Of_Nonimprovements);

    -- Random Initialization
    Save(Modul);
    Reset(Modul);
    Pos := Index_Of(Next_Element(Modul));
    Restore(Modul);

    Modul_Aux:= List_Of(Get_Element(Lol_Actual, Pos));
    Randomly_Divide_List(Lol_Actual, Gen, Modul_Aux, L1_Aux, L2_Aux);

    -- Warm-up
    Steps := 0;
    Update_Modularity(Mi, L1_Aux, Mt);
    Update_Modularity(Mi, L2_Aux, Mt);
    Q_Act := Total_Modularity(Mi);
    while Q_Act <= Q_Ini + Improvement_Tolerance and Steps <= Max_Nonimpr loop
      Lowest_Fitness_Node_Movement(Gr, Mi, L1_Aux, L2_Aux, Gen);
      Update_Modularity(Mi, L1_Aux, Mt);
      Update_Modularity(Mi, L2_Aux, Mt);
      Q_Act := Total_Modularity(Mi);
      Steps := Steps + 1;
    end loop;

    -- Optimization process
    if Q_Act > Q_Ini + Improvement_Tolerance then
      Steps := 0;
      Q_Max := Q_Act;
      L1 := New_List(Lol_Best);
      L2 := New_List(Lol_Best);
      -- Update Lol_Best
      Copy_Lols_Lists(L1_Aux, L1, Lol_Best);
      Copy_Lols_Lists(L2_Aux, L2, Lol_Best);
      Remove(Modul);
      while Steps <= Max_Nonimpr loop
        Lowest_Fitness_Node_Movement(Gr, Mi, L1_Aux, L2_Aux, Gen);
        Update_Modularity(Mi, L1_Aux, Mt);
        Update_Modularity(Mi, L2_Aux, Mt);
        Q_Act := Total_Modularity(Mi);
        if Q_Act > Q_Max + Improvement_Tolerance then
          Q_Max := Q_Act;
          Steps := 0;
          -- Update Lol_Best
          Copy_Lols_Lists(L1_Aux, L1, Lol_Best);
          Copy_Lols_Lists(L2_Aux, L2, Lol_Best);
        else
          Steps := Steps + 1;
        end if;
      end loop;
      Break_And_Enqueue(Gr, Lol_Best, L1, L2, Q);
    end if;
    Free(Lol_Actual);
  end Optimization_Process;

  -------------------------------------------------------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph;
    Log_Name: in Unbounded_String; Gen: in out Generator;
    Lol_Best: in List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Mi: Modularity_Info;
    Q: Queue;
    Q_Ini: Long_Float;
    Modul: List;
  begin
    Initialize(Mi, Gr, Mt, R, Pc);
    Initialize(Q);

    Save(Lol_Best);
    Reset(Lol_Best);
    while Has_Next_List(Lol_Best) loop
      Modul := Next_List(Lol_Best);
      if Number_Of_Elements(Modul) > 1 then
        Enqueue(Modul, Q);
      end if;
    end loop;
    Restore(Lol_Best);
    Update_Modularity(Mi, Lol_Best, Mt);

    while not Is_Empty(Q) loop
      Modul := Dequeue(Q);
      Q_Ini := Total_Modularity(Mi);
      Optimization_Process(Gr, Mi, Mt, Modul, Lol_Best, Q, Q_Ini, Gen);
      Update_Modularity(Mi, Lol_Best, Mt);
      Q_Best := Total_Modularity(Mi);
      Improvement_Action(Log_Name, Lol_Best, Q_Best);
    end loop;

    Sort_Lists(Lol_Best);

    Free(Mi);
    Free(Q);
  end Execute_Repetition;

  -------------------------------------------------------------------------
  -- Extremal_Modularity --
  -------------------------

  procedure Extremal_Modularity(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Unbounded_String;
    Lol_Ini: in List_Of_Lists; Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Gen: Generator;
    Lol_Rep: List_Of_Lists;
    Q_Rep: Modularity_Rec;
  begin
    Reset(Gen);
    Initialize(Lol_Best, Number_Of_Vertices(Gr), Together_Initialization);
    for NR in 1 .. Number_Of_Repetitions loop
      Lol_Rep := Clone (Lol_Ini);
      Execute_Repetition(MT, Gr, Log_Name, Gen, Lol_Rep, Q_Rep, R, Pc);
      if NR = 1 or else Q_Rep.Total > Q_Best.Total then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Extremal_Modularity;
  -------------------------------------------------------------------------

end Modularities_Extremal;
