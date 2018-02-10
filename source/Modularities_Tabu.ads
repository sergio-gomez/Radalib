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

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Utils; use Utils;


generic

  Number_Of_Repetitions: Positive;

  with function Maximum_Of_Nonimprovements(N: in Positive) return Natural;

  with procedure Improvement_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec;
    Us: in Ustring := Null_Ustring);

  with procedure Repetition_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

package Modularities_Tabu is

  procedure Tabu_Modularity(
    Mt: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);


private

  type Movement is record
    Idx: Natural;
    E: Element;
    L_From: List;
    L_To: List;
    Delta_Q: Double;
    Isolate: Boolean;
    New_Lol: Boolean;
    Mi_New: Modularity_Info;
    Lol_New: List_Of_Lists;
  end record;

  procedure Decrease_Tabu_Moves(Tabu_Moves: in PIntegers);

  function Get_Steps(N: in Natural) return Positive;

  function Get_Subset_Size(N: in Natural) return Positive;

  procedure Select_Move(
    Gen: in out Generator;
    Vi: in Vertex;
    Ei: in Element;
    Li, Lj: out List;
    Isolate: out Boolean);

  procedure Modularity_Change(
    Mt: in Modularity_Type;
    Mi: in Modularity_Info;
    Fast: in Boolean;
    Ei: in Element;
    Li, Lj: in List;
    Isolate: in Boolean;
    Delta_Q: out Double;
    New_Lol: out Boolean;
    Mi_New: out Modularity_Info;
    Lol_New: out List_Of_Lists);

  procedure Examine_Neighbourhood(
    Mt: in Modularity_Type;
    Gr: in Graph;
    Mi: in Modularity_Info;
    Gen: in out Generator;
    Fast, Subset: in Boolean;
    Q_Best: in Double;
    Lol: in out List_Of_Lists;
    Tabu_Moves: in PIntegers;
    Best_Move: out Movement);

  procedure Execute_Repetition (
    Mt: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Gen: in out Generator;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularities_Tabu;
