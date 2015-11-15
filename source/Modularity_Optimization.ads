-- Radalib, Copyright (c) 2015 by
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


-- @filename Modularity_Optimization.ads
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Main access to Modularity Optimization Algorithms

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils; use Utils;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;


package Modularity_Optimization is

  -------------------------------------------------------------------------

  procedure Exhaustive_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  procedure Exhaustive_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    MT: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -------------------------------------------------------------------------

  procedure Default_Improvement_Action(
    Log_Name: in Unbounded_String;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

  procedure Default_Repetition_Action(
    Log_Name: in Unbounded_String;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

  function Default_Maximum_Of_Nonimprovements(
    Num_Vertices: Natural) return Natural;

  -------------------------------------------------------------------------

  generic
    with function Maximum_Of_Nonimprovements(
      Num_Vertices: Natural) return Natural is Default_Maximum_Of_Nonimprovements;
    with procedure Improvement_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Improvement_Action;
    with procedure Repetition_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Tabu_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Tabu_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  -------------------------------------------------------------------------

  generic
    with procedure Improvement_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Improvement_Action;
    with procedure Repetition_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Tabu_Modularity_Bootstrapping(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    Num_Nonimprovements: in Natural := 0;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Tabu_Modularity_Bootstrapping(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    Num_Nonimprovements: in Natural := 0;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  -------------------------------------------------------------------------

  generic
    with procedure Improvement_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Improvement_Action;
    with procedure Repetition_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Spectral_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Spectral_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  -------------------------------------------------------------------------

  generic
    with procedure Improvement_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Improvement_Action;
    with procedure Repetition_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Extremal_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Extremal_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Extremal_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  -------------------------------------------------------------------------

  generic
    with procedure Improvement_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Improvement_Action;
    with procedure Repetition_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Fast_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Fast_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Fast_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  -------------------------------------------------------------------------

  generic
    with procedure Improvement_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Improvement_Action;
    with procedure Repetition_Action(
      Log_Name: in Unbounded_String;
      Lol: in List_Of_Lists;
      Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Reposition_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

  procedure Reposition_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Unbounded_String := Null_Unbounded_String);

end Modularity_Optimization;
