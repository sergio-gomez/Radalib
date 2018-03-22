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


-- @filename Modularities_Louvain.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/12/2017
-- @revision 15/03/2018
-- @brief Louvain Algorithm implementation

with Ada.Containers.Ordered_Sets;

with Utils; use Utils;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularities_Louvain is

  ---------------
  -- Constants --
  ---------------

  Improvement_Tolerance: constant := 1.0e-10;

  --------------------------
  -- Modularity_Variation --
  --------------------------

  function Modularity_Variation(Mi: in Modularity_Info; Mt: in Modularity_Type;
    Ei: in Finite_Disjoint_Lists.Element; Li, Lj: in List) return Double
  is
    Q_Ini, Q_End: Double;
  begin
    Save_Modularity(Mi, Li);
    Save_Modularity(Mi, Lj);
    Update_Modularity_Move_Element(Mi, Ei, Lj, Mt);
    Q_End := Partial_Modularity(Mi, Li) + Partial_Modularity(Mi, Lj);
    Restore_Modularity(Mi, Li);
    Restore_Modularity(Mi, Lj);
    Move(Ei, Li);
    Q_Ini := Partial_Modularity(Mi, Li) + Partial_Modularity(Mi, Lj);
    return Q_End - Q_Ini;
  end Modularity_Variation;

  --------------------------
  -- Optimization_Process --
  --------------------------

  procedure Optimization_Process(Log_Name: in Ustring; Mt: in Modularity_Type; Gr: in Graph;
    Lol: in out List_Of_Lists; Q: out Modularity_Rec; R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    package List_Id_Sets is new Ada.Containers.Ordered_Sets(Integer);
    use List_Id_Sets;

    Increased: Boolean;
    N, J: Positive;
    Directed: Boolean;
    Mi: Modularity_Info;
    Delta_Q, Delta_Q_Max: Double;
    Li, Lj, Lj_Max: List;
    Ei: Finite_Disjoint_Lists.Element;
    Lol_Aux: List_Of_Lists;
    Q_Aux: Modularity_Rec;
    El: Edges_List;
    E: Edge;
    Vi: Vertex;
    S: Set;
    Id: Natural;
    Us: Ustring;
  begin
    N := Number_Of_Vertices(Gr);
    Directed := Is_Directed(Gr);
    Initialize(Mi, Gr, Mt, R, Pc);
    Update_Modularity(Mi, Lol, Mt);
    Q := Total_Modularity(Mi);
    Increased := True;
    while Increased loop
      Increased := False;
      for I in 1..N loop
        -- Find best neighbor
        Clear(S);
        Delta_Q_Max := Double'First;
        Vi := Get_Vertex(Gr, I);
        Ei := Get_Element(Lol, I);
        Li := List_Of(Ei);
        Insert(S, Get_Id(Li));
        El := Edges_From(Vi);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          J := Index_Of(To(E));
          Lj := List_Of(Get_Element(Lol, J));
          Id := Get_Id(Lj);
          if not Contains(S, Id) then
            Delta_Q := Modularity_Variation(Mi, Mt, Ei, Li, Lj);
            if Delta_Q > Delta_Q_Max + Improvement_Tolerance then
              Delta_Q_Max := Delta_Q;
              Lj_Max := Lj;
            end if;
            Insert(S, Id);
          end if;
        end loop;
        Restore(El);
        if Directed then
          El := Edges_To(Vi);
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            E := Next(El);
            J := Index_Of(From(E));
            Lj := List_Of(Get_Element(Lol, J));
            Id := Get_Id(Lj);
            if not Contains(S, Id) then
              Delta_Q := Modularity_Variation(Mi, Mt, Ei, Li, Lj);
              if Delta_Q > Delta_Q_Max + Improvement_Tolerance then
                Delta_Q_Max := Delta_Q;
                Lj_Max := Lj;
              end if;
              Insert(S, Id);
            end if;
          end loop;
          Restore(El);
        end if;
        -- Move node if modularity increases
        if Delta_Q_Max > Improvement_Tolerance then
          Update_Modularity_Move_Element(Mi, Ei, Lj_Max, Mt);
          if Number_Of_Elements(Li) = 0 then
            Remove(Li);
          end if;
          Increased := True;
        end if;
      end loop;
      -- Ensure connected communities if modularity improved
      if Increased then
        Q := Total_Modularity(Mi);
        Connected_Components(Gr, Lol, Lol_Aux);
        if Number_Of_Lists(Lol_Aux) > Number_Of_Lists(Lol) then
          Update_Modularity(Mi, Lol_Aux, Mt);
          Q_Aux := Total_Modularity(Mi);
          if Q_Aux.Total > Q.Total + Improvement_Tolerance then
            Free(Lol);
            Lol := Lol_Aux;
            Q := Q_Aux;
          else
            Free(Lol_Aux);
            Update_Modularity(Mi, Lol, Mt);
            Q := Total_Modularity(Mi);
          end if;
        else
          Free(Lol_Aux);
        end if;
        Us := S2U("Q = " & D2Se0(Q.Total, Aft => 6) &  "   comms = " & I2S(Number_Of_Lists(Lol)));
        Improvement_Action(Log_Name, Null_List_Of_Lists, Null_Modularity_Rec, Us);
      end if;
    end loop;
    Sort_Lists(Lol);
    Sort_By_Size(Lol);
    Free(Mi);
  end Optimization_Process;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(Mt: in Modularity_Type; Gr: in Graph;
    Log_Name: in Ustring; Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Q_Ren: Modularity_Rec;
    Gr_Ren, Gr_Aux: Graph;
    Lol, Lol_Ren, Lol_Aux: List_Of_Lists;
    N, N_Ren: Positive;
    R_Ren: Double;
    Finished: Boolean;
  begin
    R_Ren := R;
    Q_Best := Modularity(Gr, Lol_Ini, Mt, R, Pc);
    Gr_Ren := Clone(Gr);
    N := Number_Of_Vertices(Gr);
    Initialize(Lol, N, Isolated_Initialization);
    Finished := False;
    while not Finished loop
      N_Ren := Number_Of_Vertices(Gr_Ren);
      if N_Ren = N then
        Lol_Ren := Clone(Lol_Ini);
      else
        Initialize(Lol_Ren, N_Ren, Isolated_Initialization);
      end if;
      Optimization_Process(Log_Name, Mt, Gr_Ren, Lol_Ren, Q_Ren, R_Ren, Pc);
      if N_Ren = Number_Of_Lists(Lol_Ren) then
        Free(Gr_Ren);
        Finished := True;
      else
        Renormalize_Graph(Gr_Ren, Lol_Ren, Gr_Aux, R_Ren);
        R_Ren := 0.0;
        Free(Gr_Ren);
        Gr_Ren := Gr_Aux;
        Unrenormalize_List_Of_Lists(Lol_Ren, Lol, Lol_Aux);
        Free(Lol);
        Lol := Lol_Aux;
        if Mt /= Weighted_Newman then
          Q_Ren := Modularity(Gr, Lol, Mt, R, Pc);
        end if;
        Improvement_Action(Log_Name, Lol, Q_Ren, S2U("resizing"));
      end if;
      Free(Lol_Ren);
    end loop;
    if Q_Ren.Total > Q_Best.Total then
      Lol_Best := Lol;
      Q_Best := Q_Ren;
    else
      Lol_Best := Clone(Lol_Ini);
      Free(Lol);
    end if;
  end Execute_Repetition;

  --------------------
  -- Louvain_Algorithm --
  --------------------

  procedure Louvain_Algorithm(Mt: in Modularity_Type; Gr: in Graph;
    Log_Name: in Ustring; Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists; Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance; Pc: in Double := 1.0)
  is
    Lol_Rep: List_Of_Lists;
    Q_Rep : Modularity_Rec;
  begin
    Initialize(Lol_Best, Number_Of_Vertices(Gr), Together_Initialization);
    for NR in 1 .. Number_Of_Repetitions loop
      Execute_Repetition(Mt, Gr, Log_Name, Lol_Ini, Lol_Rep, Q_Rep, R, Pc);
      if NR = 1 or else Q_Rep.Total > Q_Best.Total then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Louvain_Algorithm;

end Modularities_Louvain;
