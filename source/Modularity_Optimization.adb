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


-- @filename Modularity_Optimization.adb
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 31/08/2020
-- @brief Main access to Modularity Optimization Algorithms

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Modularities_Exhaustive; use Modularities_Exhaustive;
with Modularities_Spectral;
with Modularities_Tabu;
with Modularities_Extremal;
with Modularities_Fast;
with Modularities_Reposition;
with Modularities_Louvain;

package body Modularity_Optimization is

  --------------------------------
  -- Default_Improvement_Action --
  --------------------------------

  procedure Default_Improvement_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec;
    Us: in Ustring := Null_Ustring) is
  begin
    null;
  end Default_Improvement_Action;

  -------------------------------
  -- Default_Repetition_Action --
  -------------------------------

  procedure Default_Repetition_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec) is
  begin
    null;
  end Default_Repetition_Action;

  ----------------------------------------
  -- Default_Maximum_Of_Nonimprovements --
  ----------------------------------------

  function Default_Maximum_Of_Nonimprovements(Num_Vertices: in Natural) return Natural is
    F: Float;
  begin
    F := Log(Float(Num_Vertices), Base=>10.0);
    return Natural(Float'Floor(10.0 + 10.0 * F));
  end Default_Maximum_Of_Nonimprovements;

  ----------------------------------------
  -- Exhaustive_Modularity_Optimization --
  ----------------------------------------

  procedure Exhaustive_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0) renames Exhaustive_Modularity;

  ----------------------------------------
  -- Exhaustive_Modularity_Optimization --
  ----------------------------------------

  procedure Exhaustive_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    MT: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0) renames Exhaustive_Modularity;

  ------------------------------------------
  -- Generic_Tabu_Modularity_Optimization --
  ------------------------------------------

  procedure Generic_Tabu_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    package Tabu_Mod is new Modularities_Tabu(
      Number_Of_Repetitions      => Num_Repetitions,
      Maximum_Of_Nonimprovements => Maximum_Of_Nonimprovements,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
    use Tabu_Mod;
  begin
    Tabu_Modularity(MT, Gr, Log_Name, Lol_Ini, Lol_Best, Q_Best, R, Pc);
    Sort_Lists(Lol_Best);
  end Generic_Tabu_Modularity_Optimization;

  ----------------------------------
  -- Tabu_Modularity_Optimization --
  ----------------------------------

  procedure Tabu_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Default_Tabu_Mod_Opt is
      new Generic_Tabu_Modularity_Optimization(
      Maximum_Of_Nonimprovements => Default_Maximum_Of_Nonimprovements,
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Default_Tabu_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Tabu_Modularity_Optimization;

  -------------------------------------------
  -- Generic_Tabu_Modularity_Bootstrapping --
  -------------------------------------------

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
    Log_Name: in Ustring := Null_Ustring)
  is
    function Fixed_Nonimprovements(Num_Vertices: Natural) return Natural is
    begin
      return Num_Nonimprovements;
    end Fixed_Nonimprovements;

    procedure Tabu_Mod_Boot is new Generic_Tabu_Modularity_Optimization(
      Maximum_Of_Nonimprovements => Fixed_Nonimprovements,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
  begin
    Tabu_Mod_Boot(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Generic_Tabu_Modularity_Bootstrapping;

  -----------------------------------
  -- Tabu_Modularity_Bootstrapping --
  -----------------------------------

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
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Tabu_Mod_Boot is new Generic_Tabu_Modularity_Bootstrapping(
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Tabu_Mod_Boot(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, Num_Nonimprovements, R, Pc, Log_Name);
  end Tabu_Modularity_Bootstrapping;

  ----------------------------------------------
  -- Generic_Spectral_Modularity_Optimization --
  ----------------------------------------------

  procedure Generic_Spectral_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    package Spectral_Mod is new Modularities_Spectral(
      Number_Of_Repetitions      => Num_Repetitions,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
    use Spectral_Mod;
  begin
    Spectral_Modularity(MT, Gr, Log_Name, Lol_Ini, Lol_Best, Q_Best, R, Pc);
    Sort_Lists(Lol_Best);
  end Generic_Spectral_Modularity_Optimization;

  --------------------------------------
  -- Spectral_Modularity_Optimization --
  --------------------------------------

  procedure Spectral_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Default_Spectral_Mod_Opt is new Generic_Spectral_Modularity_Optimization(
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Default_Spectral_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Spectral_Modularity_Optimization;

  ----------------------------------------------
  -- Generic_Extremal_Modularity_Optimization --
  ----------------------------------------------

  procedure Generic_Extremal_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    package Extremal_Mod is new Modularities_Extremal(
      Number_Of_Repetitions      => Num_Repetitions,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
    use Extremal_Mod;
  begin
    Extremal_Modularity (MT, Gr, Log_Name, Lol_Ini, Lol_Best, Q_Best, R, Pc);
    Sort_Lists(Lol_Best);
  end Generic_Extremal_Modularity_Optimization;

  --------------------------------------
  -- Extremal_Modularity_Optimization --
  --------------------------------------

  procedure Extremal_Modularity_Optimization(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    Lol_Ini: List_Of_Lists;
  begin
    Initialize(Lol_Ini, Number_Of_Vertices(Gr), Together_Initialization);
    Extremal_Modularity_Optimization(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
    Free(Lol_Ini);
  end Extremal_Modularity_Optimization;

  --------------------------------------
  -- Extremal_Modularity_Optimization --
  --------------------------------------

  procedure Extremal_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Default_Extremal_Mod_Opt is new Generic_Extremal_Modularity_Optimization(
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Default_Extremal_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Extremal_Modularity_Optimization;

  ----------------------------------------------------
  -- Generic_Fast_Algorithm_Modularity_Optimization --
  ----------------------------------------------------

  procedure Generic_Fast_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    package Fast_Alg is new Modularities_Fast(
      Number_Of_Repetitions      => Num_Repetitions,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
    use Fast_Alg;
  begin
    Newman_Fast_Algorithm(MT, Gr, Log_Name, Lol_Ini, Lol_Best, Q_Best, R, Pc);
    Sort_Lists(Lol_Best);
  end Generic_Fast_Algorithm_Modularity_Optimization;

  --------------------------------------------
  -- Fast_Algorithm_Modularity_Optimization --
  --------------------------------------------

  procedure Fast_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Default_Fast_Algorithm is new Generic_Fast_Algorithm_Modularity_Optimization(
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Default_Fast_Algorithm(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Fast_Algorithm_Modularity_Optimization;

  --------------------------------------------
  -- Fast_Algorithm_Modularity_Optimization --
  --------------------------------------------

  procedure Fast_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    Lol_Ini: List_Of_Lists;
  begin
    Initialize(Lol_Ini, Number_Of_Vertices(Gr), Isolated_Initialization);
    Fast_Algorithm_Modularity_Optimization(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
    Free(Lol_Ini);
  end Fast_Algorithm_Modularity_Optimization;

  ------------------------------------------------
  -- Generic_Reposition_Modularity_Optimization --
  ------------------------------------------------

  procedure Generic_Reposition_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    package Reposition_Mod is new Modularities_Reposition(
      Number_Of_Repetitions      => Num_Repetitions,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
    use Reposition_Mod;
  begin
    Reposition_Modularity(MT, Gr, Log_Name, Lol_Ini, Lol_Best, Q_Best, R, Pc);
    Sort_Lists(Lol_Best);
  end Generic_Reposition_Modularity_Optimization;

  --------------------------------------
  -- Reposition_Modularity_Optimization --
  --------------------------------------

  procedure Reposition_Modularity_Optimization(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Default_Reposition_Mod_Opt is new Generic_Reposition_Modularity_Optimization(
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Default_Reposition_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Reposition_Modularity_Optimization;

  ----------------------------------------------------
  -- Generic_Louvain_Algorithm_Modularity_Optimization --
  ----------------------------------------------------

  procedure Generic_Louvain_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    package Louvain_Alg is new Modularities_Louvain(
      Number_Of_Repetitions      => Num_Repetitions,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action);
    use Louvain_Alg;
  begin
    Louvain_Algorithm(MT, Gr, Log_Name, Lol_Ini, Lol_Best, Q_Best, R, Pc);
    Sort_Lists(Lol_Best);
  end Generic_Louvain_Algorithm_Modularity_Optimization;

  --------------------------------------------
  -- Louvain_Algorithm_Modularity_Optimization --
  --------------------------------------------

  procedure Louvain_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    procedure Default_Louvain_Algorithm is new Generic_Louvain_Algorithm_Modularity_Optimization(
      Improvement_Action         => Default_Improvement_Action,
      Repetition_Action          => Default_Repetition_Action);
  begin
    Default_Louvain_Algorithm(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
  end Louvain_Algorithm_Modularity_Optimization;

  --------------------------------------------
  -- Louvain_Algorithm_Modularity_Optimization --
  --------------------------------------------

  procedure Louvain_Algorithm_Modularity_Optimization(
    Gr : in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    Num_Repetitions: in Positive := 1;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0;
    Log_Name: in Ustring := Null_Ustring)
  is
    Lol_Ini: List_Of_Lists;
  begin
    Initialize(Lol_Ini, Number_Of_Vertices(Gr), Isolated_Initialization);
    Louvain_Algorithm_Modularity_Optimization(Gr, Lol_Ini, Lol_Best, Q_Best, MT, Num_Repetitions, R, Pc, Log_Name);
    Free(Lol_Ini);
  end Louvain_Algorithm_Modularity_Optimization;

end Modularity_Optimization;
