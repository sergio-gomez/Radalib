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


-- @filename Modularities_Extremal.ads
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2007
-- @revision 31/08/2020
-- @brief Extremal Modularity Optimization implementation (after J. Duch and A. Arenas)

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Utils; use Utils;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Operations; use Graphs_Double_Operations;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Queues;

generic

  Number_Of_Repetitions: Positive;

  with procedure Improvement_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec;
    Us: in Ustring := Null_Ustring);

  with procedure Repetition_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

package Modularities_Extremal is

  procedure Extremal_Modularity(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name : in Ustring;
    Lol_Ini : in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

private

  -------------------------------------------------------------------------

  package List_Queues is new Queues(List); use List_Queues;

  -------------------------------------------------------------------------

  procedure Randomly_Divide_List(
    Gen: in out Generator;
    Module: in List;
    L1, L2: out List);

  procedure Lowest_Fitness_Node_Movement(
    Gr: in Graph;
    Mi: in Modularity_Info;
    Mt: in Modularity_Type;
    Sub_L1, Sub_L2: in List;
    Gen: in out Generator;
    Tau: in Float := 1.6);

  procedure Copy_List(
    L: in List;
    Lol: in List_Of_Lists;
    L_Copy: in List);

  procedure Break_And_Enqueue(
    Gr: in Graph;
    Mi: in Modularity_Info;
    Mt: in Modularity_Type;
    L: in List;
    Lol: in List_Of_Lists;
    Q: in Queue;
    Q_Act: in out Double);

  procedure Optimization_Process(
    Gr: in Graph;
    Mi: in Modularity_Info;
    Mt: in Modularity_Type;
    Module: in out List;
    Lol_Best: in List_Of_Lists;
    Q: in Queue;
    Q_Ini: in Double;
    Gen: in out Generator);

  procedure Execute_Repetition(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Gen: in out Generator;
    Lol_Ini : in List_Of_Lists;
    Lol_Best: in List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -------------------------------------------------------------------------

end Modularities_Extremal;
