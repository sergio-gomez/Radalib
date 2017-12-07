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


-- @filename Modularities_Reposition.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/04/2008
-- @revision 26/10/2014
-- @brief Reposition Algorithm implementation

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
    Dq: Double;
  begin
    if L /= L_Ori then
      Move(E_Ori, L);
      Update_Modularity(Mi, L_Ori, Mt);
      Update_Modularity(Mi, L, Mt);
      Dq := Partial_Modularity(Mi, L_Ori) + Partial_Modularity(Mi, L);
      Move(E_Ori, L_Ori);
      Update_Modularity(Mi, L_Ori, Mt);
      Update_Modularity(Mi, L, Mt);
      Dq := Dq - Partial_Modularity(Mi, L_Ori) - Partial_Modularity(Mi, L);
      if Dq > Dq_Best + Improvement_Tolerance then
        L_Best := L;
        Dq_Best := Dq;
      end if;
    end if;
  end Check_Move;

  ---------------
  -- Make_Move --
  ---------------

  procedure Make_Move(Mt: in Modularity_Type; Mi: in Modularity_Info; I: in Positive;
    L_Best: in List)
  is
    Lol: List_Of_Lists;
    E_Ori: Finite_Disjoint_Lists.Element;
    L_Ori: List;
  begin
    Lol := List_Of_Lists_Of(L_Best);
    E_Ori := Get_Element(Lol, I);
    L_Ori := List_Of(E_Ori);
    Move(E_Ori, L_Best);
    Update_Modularity(Mi, L_Ori, Mt);
    Update_Modularity(Mi, L_Best, Mt);
    if Number_Of_Elements(L_Ori) = 0 then
      Remove(L_Ori);
    end if;
    Remove_Empty(Lol);
  end Make_Move;

  --------------------------
  -- Optimization Process --
  --------------------------

  procedure Optimization_Process(Mt: in Modularity_Type; Gr: in Graph; Mi: in Modularity_Info;
    Lol_Actual, Lol_Best: in out List_Of_Lists; Q_Actual, Q_Best: in out Modularity_Rec;
    Changed: out Boolean)
  is
    N: constant Positive := Number_Of_Vertices(Gr);
    Processed: array(1..N) of Boolean;
    Ne, Nc, Iters: Natural;
    L_Ori, L, L_Best: List;
    E_Ori, E: Finite_Disjoint_Lists.Element;
    V_Ori, V: Vertex;
    Elf, Elt: Edges_List;
    Fit, Fit_Worst, Dq_Best: Double;
    Lol_Aux: List_Of_Lists;
    J_Sel: Positive;
  begin
    pragma Warnings(Off, Elf);
    pragma Warnings(Off, Elt);
    Processed := (others => False);
    Changed := False;
    loop
      Iters := 1;
      -- Try to improve once each Vertex
      Nc := 0;
      for I in 1..N loop
        -- Select a Vertex
        Fit_Worst := Double'Last;
        J_Sel := Positive'Last;
        for J in 1..N loop
          if not Processed(J) then
            E := Get_Element(Lol_Actual, J);
            Fit := Element_Modularity(Mi, E) ;
            if Fit <= Fit_Worst then
              Fit_Worst := Fit;
              J_Sel := J;
            end if;
          end if;
        end loop;
        -- Look for best move
        Dq_Best := Double'First;
        if J_Sel in 1..N and then not Processed(J_Sel) then
          E_Ori := Get_Element(Lol_Actual, J_Sel);
          L_Ori := List_Of(E_Ori);
          V_Ori := Get_Vertex(Gr, J_Sel);
          Elf := Edges_From(V_Ori);
          Ne := Number_Of_Edges(Elf);
          if Is_Directed(Gr) then
            Elt := Edges_To(V_Ori);
            Ne := Ne + Number_Of_Edges(Elt);
          end if;
          if Number_Of_Lists(Lol_Actual) <= Ne then
            -- Check all Lists
            Save(Lol_Actual);
            Reset(Lol_Actual);
            while Has_Next_List(Lol_Actual) loop
              L := Next_List(Lol_Actual);
              Check_Move(Mt, Mi, L_Ori, L, L_Best, E_Ori, Dq_Best);
            end loop;
            Restore(Lol_Actual);
          else
            -- Check only adjacent Lists
            Save(Elf);
            Reset(Elf);
            while Has_Next(Elf) loop
              V := Next(Elf);
              E := Get_Element(Lol_Actual, Index_Of(V));
              L := List_Of(E);
              Check_Move(Mt, Mi, L_Ori, L, L_Best, E_Ori, Dq_Best);
            end loop;
            Restore(Elf);
            if Is_Directed(Gr) then
              Save(Elt);
              Reset(Elt);
              while Has_Next(Elt) loop
                V := Next(Elt);
                E := Get_Element(Lol_Actual, Index_Of(V));
                L := List_Of(E);
                Check_Move(Mt, Mi, L_Ori, L, L_Best, E_Ori, Dq_Best);
              end loop;
              Restore(Elt);
            end if;
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
            Make_Move(Mt, Mi, J_Sel, L_Best);
            Changed := True;
            Nc := Nc + 1;
          end if;
          Processed(J_Sel) := True;
        end if;
      end loop;
      if Changed then
        -- Ensure connected communities if modularity improved
        Connected_Components(Gr, Lol_Actual, Lol_Aux);
        if Number_Of_Lists(Lol_Aux) > Number_Of_Lists(Lol_Actual) and then Modularity(Gr, Lol_Aux, Mt) > Total_Modularity(Mi) + Improvement_Tolerance then
          Free(Lol_Actual);
          Lol_Actual := Lol_Aux;
          Update_Modularity(Mi, Lol_Actual, Mt);
        else
          Free(Lol_Aux);
        end if;
        -- Save if best partition
        Q_Actual := Total_Modularity(Mi);
        if Q_Actual.Total > Q_Best.Total + Improvement_Tolerance then
          Q_Best := Q_Actual;
          Free(Lol_Best);
          Lol_Best := Clone(Lol_Actual);
        end if;
      end if;
      -- Exit if none or all vertices improved, or number of iterations equals number of vertices
      exit when (Nc = 0 or Nc = N) or else Iters = N;
      Iters := Iters + 1;
    end loop;
  end Optimization_Process;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Unbounded_String;
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

  procedure Reposition_Modularity(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Unbounded_String;
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
