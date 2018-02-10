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


-- @filename Modularities_Tabu.ads
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 17/01/2018
-- @brief Tabu Modularity Optimization

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Random_Numbers; use Random_Numbers;

package body Modularities_Tabu is

  ---------------
  -- Constants --
  ---------------

  Improvement_Tolerance: constant := 1.0e-10;

  -------------------------
  -- Decrease_Tabu_Moves --
  -------------------------

  procedure Decrease_Tabu_Moves(Tabu_Moves: in PIntegers) is
  begin
    for I in Tabu_Moves'Range loop
      if Tabu_Moves(I) > 0 then
        Tabu_Moves(I) := Tabu_Moves(I) - 1;
      end if;
    end loop;
  end Decrease_Tabu_Moves;

  ---------------
  -- Get_Steps --
  ---------------

  function Get_Steps(N: in Natural) return Positive is
    Steps: Natural;
  begin
    if N <= 100 then
      Steps := 20;
    elsif N <= 1_000 then
      Steps := 50;
    elsif N <= 10_000 then
      Steps := 100;
    else
      Steps := 100;
    end if;
    return Steps;
  end Get_Steps;

  ---------------------
  -- Get_Subset_Size --
  ---------------------

  function Get_Subset_Size(N: in Natural) return Positive is
    Size: Natural;
  begin
    if N <= 300 then
      Size := N;
    elsif N <= 500 then
      Size := 50;
    elsif N <= 1_000 then
      Size := 100;
    elsif N <= 5_000 then
      Size := 150;
    elsif N <= 10_000 then
      Size := 200;
    else
      Size := 250;
    end if;
    return Size;
  end Get_Subset_Size;

  -----------------
  -- Select_Move --
  -----------------

  procedure Select_Move(Gen: in out Generator; Vi: in Vertex; Ei: in Element; Li, Lj: out List;
    Isolate: out Boolean)
  is
    Lol: List_Of_Lists;
    Vj: Vertex;
    El, Elf, Elt: Edges_List;
    Ej: Element;
    Ne, Nef, Net, N_Li, D: Natural;
  begin
    Lol := List_of_Lists_Of(Ei);
    Li := List_Of(Ei);
    Elf := Edges_From(Vi);
    Elt := Edges_To(Vi);
    Nef := Number_Of_Edges(Elf);
    Net := Number_Of_Edges(Elt);
    Ne := Nef + Net;
    N_Li := Number_Of_Elements(Li);
    if Ne = 0 then
      D := 0;
    else
      if N_Li = 1 then
        D := Random_Uniform(Gen, 1, Ne);
      else
        D := Random_Uniform(Gen, 0, Ne);
      end if;
    end if;
    if D = 0 then
      Lj := Li;
    else
      if D <= Nef then
        El := Elf;
      else
        El := Elt;
        D := D - Nef;
      end if;
      Save(El);
      Reset(El);
      for K in 1..D loop
        Vj := Next(El);
      end loop;
      Ej := Get_Element(Lol, Index_Of(Vj));
      while Belongs_To(Ej, Li) and Has_Next(El) loop
        Vj := Next(El);
        Ej := Get_Element(Lol, Index_Of(Vj));
      end loop;
      Restore(El);
      Lj := List_Of(Ej);
    end if;
    Isolate := False;
    if Lj = Li and N_Li > 1 then
      Isolate := True;
    end if;
  end Select_Move;

  -----------------------
  -- Modularity_Change --
  -----------------------

  procedure Modularity_Change(Mt: in Modularity_Type; Mi: in Modularity_Info; Fast: in Boolean;
    Ei: in Element; Li, Lj: in List; Isolate: in Boolean; Delta_Q: out Double; New_Lol: out Boolean;
    Mi_New: out Modularity_Info; Lol_New: out List_Of_Lists)
  is
    use Linked_Lists_Of_Lists;

    Q_Ini, Q_End: Double;
    Gr: Graph;
    Lol: List_Of_Lists;
    Lk, Lr: List;
    Ls: Linked_List;
  begin
    if Li = Lj and not Isolate then
      Delta_Q := 0.0;
      New_Lol := False;
      return;
    end if;

    Gr := Graph_Of(Mi);
    Lol := List_of_Lists_Of(Li);

    if Isolate then
      Lk := New_List(Lol);
    else
      Lk := Lj;
    end if;

    if not Fast then
      Move(Ei, Lk);
      Update_List_Connected_Components(Gr, Li, Ls);

      New_Lol := (Size(Ls) > 1);

      if New_Lol then
        Lol_New := Clone(Lol);
      else
        Move(Ei, Li);
        Free(Ls);
      end if;
    else
      New_Lol := False;
    end if;

    if New_Lol then
      Q_Ini := Total_Modularity(Mi);
      Mi_New := Clone(Mi);
      Update_Modularity(Mi_New, Lk, Mt);
      Save(Ls);
      Reset(Ls);
      while Has_Next(Ls) loop
        Update_Modularity(Mi_New, Next(Ls), Mt);
      end loop;
      Reset(Ls);
      while Has_Next(Ls) loop
        Lr := Next(Ls);
        if Lr /= Li then
          Move(Lr, Li);
          Remove(Lr);
        end if;
      end loop;
      Restore(Ls);
      Free(Ls);
      Move(Ei, Li);
      Q_End := Total_Modularity(Mi_New);
      Delta_Q := Q_End - Q_Ini;
    else
      Save_Modularity(Mi, Li);
      Save_Modularity(Mi, Lk);
      Update_Modularity_Move_Element(Mi, Ei, Lk, Mt);
      Q_End := Partial_Modularity(Mi, Li) + Partial_Modularity(Mi, Lk);
      Restore_Modularity(Mi, Li);
      Restore_Modularity(Mi, Lk);
      Move(Ei, Li);
      Q_Ini := Partial_Modularity(Mi, Li) + Partial_Modularity(Mi, Lk);
      Delta_Q := Q_End - Q_Ini;
    end if;

    if Isolate then
      Remove(Lk);
    end if;
  end Modularity_Change;

  ---------------------------
  -- Examine_Neighbourhood --
  ---------------------------

  procedure Examine_Neighbourhood(Mt: in Modularity_Type; Gr: in Graph; Mi: in Modularity_Info;
    Gen: in out Generator; Fast, Subset: in Boolean; Q_Best: in Double; Lol: in out List_Of_Lists;
    Tabu_Moves: in PIntegers; Best_Move: out Movement)
  is
    N: constant Positive := Number_Of_Vertices(Gr);
    Num_Nodes, I: Positive;
    Vi: Vertex;
    Ei: Element;
    Li, Lj: List;
    Q_Actual, Dq, Dq_Best: Double;
    Isolate, New_Lol, Used: Boolean;
    Mi_New: Modularity_Info;
    Lol_New: List_Of_Lists;
  begin
    Best_Move.Idx := 0;
    Best_Move.New_Lol := False;
    Q_Actual := Total_Modularity(Mi);
    Dq_Best := Double'First;
    if Subset then
      Num_Nodes := Get_Subset_Size(N);
    else
      Num_Nodes := N;
    end if;

    for Nod in 1..Num_Nodes loop
      if Num_Nodes = N then
        I := Nod;
      else
        I := Random_Uniform(Gen, 1, N);
      end if;
      Vi := Get_Vertex(Gr, I);
      Ei := Get_Element(Lol, I);
      -- Select move
      Select_Move(Gen, Vi, Ei, Li, Lj, Isolate);
      if Lj /= Li or Isolate then
        -- Modularity change
        Modularity_Change(Mt, Mi, Fast, Ei, Li, Lj, Isolate, Dq, New_Lol, Mi_New, Lol_New);
        -- Remove from tabu list
        if Q_Actual + Dq > Q_Best + Improvement_Tolerance then
          Tabu_Moves(I) := 0;
        end if;
        -- Save best move
        Used := False;
        if Tabu_Moves(I) = 0 then
          if Best_Move.Idx = 0 or else Dq > Dq_Best + Improvement_Tolerance then
            if Best_Move.New_Lol then
              Free(Best_Move.Mi_New);
              Free(Best_Move.Lol_New);
            end if;
            if New_Lol then
              Best_Move := (Idx => I, E => Ei, L_From => Li, L_To => Lj, Isolate => Isolate,
                            New_Lol => New_Lol, Delta_Q => Dq, Mi_New => Mi_New, Lol_New => Lol_New);
              Used := True;
            else
              Best_Move := (Idx => I, E => Ei, L_From => Li, L_To => Lj, Isolate => Isolate,
                            New_Lol => False, Delta_Q => Dq, Mi_New => Mi, Lol_New => Lol);
            end if;
            Dq_Best := Dq;
          end if;
        end if;
        if New_Lol and not Used then
          Free(Mi_New);
          Free(Lol_New);
        end if;
      end if;
    end loop;
  end Examine_Neighbourhood;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Ustring;
    Gen: in out Generator; Lol_Ini: in List_Of_Lists; Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec; R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Fast_Subset_Min_Size: constant Natural := 500;
    Slow_Max_Size       : constant Natural := 1_500;

    Fast, Subset: Boolean;
    Mi: Modularity_Info;
    Lol_Actual: List_Of_Lists;
    Q_Actual: Double;
    N, Iter, Max_Iters, Step, Num_Steps: Natural;
    Tabu_Moves: PIntegers;
    Tabu_Tenure: Natural;
    Best_Move: Movement;
    Lk: List;
  begin
    N := Number_Of_Vertices(Gr);

    Initialize(Mi, Gr, Mt, R, Pc);

    Tabu_Moves := Alloc(1, N);
    Tabu_Moves.all := (others => 0);
    Tabu_Tenure := Min(5, N - 1);

    Lol_Best   := Clone(Lol_Ini);
    Lol_Actual := Clone(Lol_Ini);

    Q_Best   := Modularity(Mi, Lol_Best, Mt);
    Q_Actual := Q_Best.Total;

    Max_Iters := Maximum_Of_Nonimprovements(N);
    Num_Steps := Get_Steps(N);

    if N <= Fast_Subset_Min_Size then
      Fast := False;
      Subset := False;
    else
      Fast := True;
      Subset := True;
    end if;
    Iter := 0;
    Step := 0;
    while Iter <= Max_Iters loop
      -- Find best move
      Examine_Neighbourhood(Mt, Gr, Mi, Gen, Fast, Subset, Q_Best.Total, Lol_Actual, Tabu_Moves, Best_Move);
      Decrease_Tabu_Moves(Tabu_Moves);
      Iter := Iter + 1;
      -- Proceed with best move
      if Best_Move.Idx > 0 then
        Tabu_Moves(Best_Move.Idx) := Tabu_Tenure;
        -- Update partition using best move
        if Best_Move.New_Lol then
          Free(Mi);
          Free(Lol_Actual);
          Mi := Best_Move.Mi_New;
          Lol_Actual := Best_Move.Lol_New;
        else
          if Best_Move.Isolate then
            Lk := New_List(Lol_Actual);
          else
            Lk := Best_Move.L_To;
          end if;
          Update_Modularity_Move_Element(Mi, Best_Move.E, Lk, Mt);
          if Number_Of_Elements(Best_Move.L_From) = 0 then
            Remove(Best_Move.L_From);
          end if;
        end if;
        Q_Actual := Q_Actual + Best_Move.Delta_Q;
        -- Save partition if improves current best
        if Q_Actual > Q_Best.Total + Improvement_Tolerance then
          Free(Lol_Best);
          Connected_Components(Gr, Lol_Actual, Lol_Best);
          Q_Best := Modularity(Mi, Lol_Best, Mt);
          if Number_Of_Lists(Lol_Actual) /= Number_Of_Lists(Lol_Best) then
            Free(Lol_Actual);
            Lol_Actual := Clone(Lol_Best);
            Q_Actual := Q_Best.Total;
          end if;
          -- Log
          Step := Step + 1;
          if Step = Num_Steps then
            Improvement_Action(Log_Name, Lol_Best, Q_Best);
            Step := 0;
          end if;
          Iter := 0;
        end if;
      end if;
      if Iter > Max_Iters then
        if Subset then
          Subset := False;
          Iter := 0;
          Improvement_Action(Log_Name, Lol_Best, Q_Best);
        elsif Fast and N <= Slow_Max_Size then
          Fast := False;
          Iter := 0;
          Improvement_Action(Log_Name, Lol_Best, Q_Best);
        end if;
      end if;
    end loop;
    Improvement_Action(Log_Name, Lol_Best, Q_Best);

    Free(Lol_Actual);

    Sort_Lists(Lol_Best);

    Free(Tabu_Moves);
    Free(Mi);
  end Execute_Repetition;

  ---------------------
  -- Tabu_Modularity --
  ---------------------

  procedure Tabu_Modularity(Mt: in Modularity_Type; Gr: in Graph; Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists; Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Lol_Rep: List_Of_Lists;
    Q_Rep: Modularity_Rec;
    Gen: Generator;
  begin
    Reset(Gen);
    Lol_Best := Clone(Lol_Ini);
    for Nr in 1 .. Number_Of_Repetitions loop
      Execute_Repetition(Mt, Gr, Log_Name, Gen, Lol_Ini, Lol_Rep, Q_Rep, R, Pc);
      if Nr = 1 or else Q_Rep.Total > Q_Best.Total + Improvement_Tolerance then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Tabu_Modularity;

end Modularities_Tabu;
