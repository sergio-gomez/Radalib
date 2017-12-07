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


-- @filename Modularities_Reposition.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/04/2008
-- @revision 26/10/2014
-- @brief Reposition Algorithm implementation

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Utils; use Utils;


generic

  Number_Of_Repetitions: Positive;

  with procedure Improvement_Action(
    Log_Name: in Unbounded_String;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

  with procedure Repetition_Action(
    Log_Name: in Unbounded_String;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

package Modularities_Reposition is

  procedure Reposition_Modularity(
    Mt: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Unbounded_String;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);


private

  procedure Check_Move(
    Mt: in Modularity_Type;
    Mi: in Modularity_Info;
    L_Ori, L: in List;
    L_Best: in out List;
    E_Ori: in Finite_Disjoint_Lists.Element;
    Dq_Best: in out Double);

  procedure Make_Move(
    Mt: in Modularity_Type;
    Mi: in Modularity_Info;
    I: in Positive;
    L_Best: in List);

  procedure Optimization_Process(
    Mt: in Modularity_Type;
    Gr: in Graph;
    Mi: in Modularity_Info;
    Lol_Actual, Lol_Best: in out List_Of_Lists;
    Q_Actual, Q_Best: in out Modularity_Rec;
    Changed: out Boolean);

  procedure Execute_Repetition (
    Mt: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Unbounded_String;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularities_Reposition;
