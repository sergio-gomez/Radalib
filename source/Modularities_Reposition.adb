-- Radalib, Copyright (c) 2021 by
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


-- @filename Modularities_Reposition.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/04/2008
-- @revision 31/08/2020
-- @brief Reposition Algorithm implementation

with Ada.Containers.Ordered_Sets;

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;

package body Modularities_Reposition is

  ---------------
  -- Constants --
  ---------------

  Improvement_Tolerance: constant := 1.0e-10;

  ----------------
  -- Check_Move --
  ----------------

  procedure Check_Move(Mt: in Modularity_Type; Mi: in Modularity_Info; L_Ori, L: in List;
    L_Best: in out List; E_Ori: in Finite_Disjoint_Lists.Element; Dq_Best: in out Double)
  is
    Q_Ini, Q_End, Dq: Double;
  begin
    if L /= L_Ori then
      Save_Modularity(Mi, L_Ori);
      Save_Modularity(Mi, L);
      Update_Modularity_Move_Element(Mi, E_Ori, L, Mt);
      Q_End := Partial_Modularity(Mi, L_Ori) + Partial_Modularity(Mi, L);
      Restore_Modularity(Mi, L_Ori);
      Restore_Modularity(Mi, L);
      Move(E_Ori, L_Ori);
      Q_Ini := Partial_Modularity(Mi, L_Ori) + Partial_Modularity(Mi, L);
      Dq := Q_End - Q_Ini;
      if Dq > Dq_Best + Improvement_Tolerance then
        L_Best := L;
        Dq_Best := Dq;
      end if;
    end if;
  end Check_Move;

  --------------------------
  -- Optimization Process --
  --------------------------

  procedure Optimization_Process(Mt: in Modularity_Type; Gr: in Graph; Mi: in Modularity_Info;
    Lol_Actual, Lol_Best: in out List_Of_Lists; Q_Actual, Q_Best: in out Modularity_Rec;
    Changed: out Boolean)
  is
    package List_Id_Sets is new Ada.Containers.Ordered_Sets(Integer);
    use List_Id_Sets;

    N: constant Positive := Number_Of_Vertices(Gr);
    Gr_Neigh: Graph;
    Processed: array(1..N) of Boolean;
    Nc: Natural;
    L_Ori, L, L_Best: List;
    E_Ori, E: Finite_Disjoint_Lists.Element;
    V_Ori, V: Vertex;
    Elf, Elt: Edges_List;
    Fit, Fit_Worst, Dq_Best: Double;
    Lol_Aux: List_Of_Lists;
    Q_Aux: Modularity_Rec;
    J_Sel: Positive;
    S: Set;
    Id: Natural;
  begin
    pragma Warnings(Off, Elf);
    pragma Warnings(Off, Elt);
    Gr_Neigh := Neighbors_Graph(Mi);
    Processed := (others => False);
    Changed := False;
    Nc := 0;
    -- Try to improve once each Vertex
    for I in 1..N loop
      -- Select a Vertex
      Fit_Worst := Double'Last;
      J_Sel := Positive'Last;
      for J in 1..N loop
        if not Processed(J) then
          E := Get_Element(Lol_Actual, J);
          Fit := Element_Modularity(Mi, E);
          if Fit <= Fit_Worst then
            Fit_Worst := Fit;
            J_Sel := J;
          end if;
        end if;
      end loop;
      -- Look for best move
      Dq_Best := Double'First;
      if J_Sel in 1..N and then not Processed(J_Sel) then
        Clear(S);
        -- Check adjacent Lists
        E_Ori := Get_Element(Lol_Actual, J_Sel);
        L_Ori := List_Of(E_Ori);
        V_Ori := Get_Vertex(Gr_Neigh, J_Sel);
        Elf := Edges_From(V_Ori);
        Save(Elf);
        Reset(Elf);
        while Has_Next(Elf) loop
          V := Next(Elf);
          E := Get_Element(Lol_Actual, Index_Of(V));
          L := List_Of(E);
          Id := Get_Id(L);
          if L /= L_Ori and then not Contains(S, Id) then
            Check_Move(Mt, Mi, L_Ori, L, L_Best, E_Ori, Dq_Best);
            Insert(S, Id);
          end if;
        end loop;
        Restore(Elf);
        if Is_Directed(Gr) then
          Elt := Edges_To(V_Ori);
          Save(Elt);
          Reset(Elt);
          while Has_Next(Elt) loop
            V := Next(Elt);
            E := Get_Element(Lol_Actual, Index_Of(V));
            L := List_Of(E);
            Id := Get_Id(L);
            if L /= L_Ori and then not Contains(S, Id) then
              Check_Move(Mt, Mi, L_Ori, L, L_Best, E_Ori, Dq_Best);
              Insert(S, Id);
            end if;
          end loop;
          Restore(Elt);
        end if;
        -- Check isolated
        if Number_Of_Elements(L_Ori) > 1 then
          L := New_List(Lol_Actual);
          Check_Move(Mt, Mi, L_Ori, L, L_Best, E_Ori, Dq_Best);
          if L_Best /= L or else Dq_Best <= 0.0 then
            Remove(L);
          end if;
        end if;
        -- Apply best move
        if Dq_Best > Improvement_Tolerance then
          Update_Modularity_Move_Element(Mi, E_Ori, L_Best, Mt);
          if Number_Of_Elements(L_Ori) = 0 then
            Remove(L_Ori);
          end if;
          Changed := True;
          Nc := Nc + 1;
        end if;
        Processed(J_Sel) := True;
      end if;
    end loop;
    if Changed then
      -- Ensure connected communities if modularity improved
      Q_Actual := Total_Modularity(Mi);
      Connected_Components(Gr_Neigh, Lol_Actual, Lol_Aux);
      if Number_Of_Lists(Lol_Aux) > Number_Of_Lists(Lol_Actual) then
        Update_Modularity(Mi, Lol_Aux, Mt);
        Q_Aux := Total_Modularity(Mi);
        if Q_Aux.Total > Q_Actual.Total + Improvement_Tolerance then
          Free(Lol_Actual);
          Lol_Actual := Lol_Aux;
          Q_Actual := Q_Aux;
        else
          Free(Lol_Aux);
          Update_Modularity(Mi, Lol_Actual, Mt);
          Q_Actual := Total_Modularity(Mi);
        end if;
      else
        Free(Lol_Aux);
      end if;
      -- Save if best partition
      if Q_Actual.Total > Q_Best.Total + Improvement_Tolerance then
        Q_Best := Q_Actual;
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Actual);
      end if;
    end if;
  end Optimization_Process;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists; Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Mi: Modularity_Info;
    Lol_Actual: List_Of_Lists;
    Q_Actual, Q_Prev: Modularity_Rec;
    Changed: Boolean;
  begin
    Initialize(Mi, Gr, Mt, R, Pc);

    Lol_Best   := Clone(Lol_Ini);
    Lol_Actual := Clone(Lol_Ini);

    Q_Best   := Modularity(Mi, Lol_Best, Mt);
    Q_Actual := Q_Best;
    Q_Prev   := Q_Best;

    Changed := True;
    while Changed loop
      Optimization_Process(Mt, Gr, Mi, Lol_Actual, Lol_Best, Q_Actual, Q_Best, Changed);
      if Changed and then Q_Best.Total > Q_Prev.Total + Improvement_Tolerance then
        Q_Prev := Q_Best;
        Improvement_Action(Log_Name, Lol_Best, Q_Best);
      end if;
    end loop;

    Free(Lol_Actual);

    Sort_Lists(Lol_Best);

    Free(Mi);
  end Execute_Repetition;

  ---------------------------
  -- Reposition_Modularity --
  ---------------------------

  procedure Reposition_Modularity(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists; Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Lol_Rep: List_Of_Lists;
    Q_Rep: Modularity_Rec;
  begin
    Lol_Best := Clone(Lol_Ini);
    for Nr in 1 .. Number_Of_Repetitions loop
      Execute_Repetition(Mt, Gr, Log_Name, Lol_Ini, Lol_Rep, Q_Rep, R, Pc);
      if Nr = 1 or else Q_Rep.Total > Q_Best.Total + Improvement_Tolerance then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Reposition_Modularity;

end Modularities_Reposition;
