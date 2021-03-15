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


-- @filename Modularity_Optimization-Combined.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 07/04/2008
-- @revision 25/08/2020
-- @brief Main access to Combined Modularity Optimization Algorithms

package Modularity_Optimization.Combined is

  -- Type to choose between the different Heuristics
  type Heuristic_Type is (Exhaustive, Tabu, Spectral, Extremal, Fast, Louvain, Reposition, Bootstrapping);

  -- Type to choose between the different Initialization modes
  type Initialization_Type is (Ini_Best, Ini_Prev, Ini_Isolated, Ini_Together, Ini_Default);

  -- Type to choose between the different Logging Levels
  type Logging_Level is (None, Summary, Progress, Verbose);

  -- Sufix for Logging List Of Lists File
  Logging_Lol_Current_Sufix  : String := ".curr.tmp.txt";
  Logging_Lol_Heuristic_Sufix: String := ".heur.tmp.txt";
  Logging_Lol_Best_Sufix     : String := ".best.tmp.txt";

  Unknown_Heuristic_Error: exception;
  Unknown_Initialization_Error: exception;
  Unknown_Logging_Error: exception;


  -- Purpose : Obtain a Heuristic Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    h | Exhaustive
  -- Note    :    t | Tabu
  -- Note    :    s | Spectral
  -- Note    :    e | Extremal
  -- Note    :    f | Fast
  -- Note    :    l | Louvain
  -- Note    :    r | Reposition
  -- Note    :    b | Bootstrapping
  --
  -- Ht_Name : The Heuristic Type Name
  -- return  : The Heuristic Type
  -- raises  : Unknown_Heuristic_Error
  function To_Heuristic_Type(Ht_Name: in String) return Heuristic_Type;

  -- Purpose : Obtain an Initialization mode from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    ! | Ini_Best
  -- Note    :    : | Ini_Prev
  -- Note    :    . | Ini_Isolated
  -- Note    :    + | Ini_Together
  -- Note    :    - | Ini_Default
  --
  -- It_Name : The Initialization mode Name
  -- return  : The Initialization mode
  -- raises  : Unknown_Initialization_Error
  function To_Initialization_Type(It_Name: in String) return Initialization_Type;

  -- Purpose : Obtain a Logging Level from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    N | None
  -- Note    :    S | Summary
  -- Note    :    P | Progress
  -- Note    :    V | Verbose
  --
  -- Ll_Name : The Logging Level Name
  -- return  : The Logging Level
  -- raises  : Unknown_Logging_Error
  function To_Logging_Level(Ll_Name: in String) return Logging_Level;

  -- Purpose : Check if a String represents a Heuristic type
  --
  -- S       : The String
  -- return  : True if is Heuristic type
  function Is_Heuristic_Type(S: in String) return Boolean;

  -- Purpose : Check if a String represents an Initialization mode
  --
  -- S       : The String
  -- return  : True if is Initialization mode
  function Is_Initialization_Type(S: in String) return Boolean;

  -- Purpose : Update Best Partition if provided one is better
  -- Note    : If better, a clone of the provided partition is created
  --
  -- Lol     : A Partition
  -- Q       : The Modularity of the Partition
  -- Lol_Best: The Best Partition
  -- Q_Best  : The Modularity of Best Partition
  procedure Update_Best_Partition(
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec;
    Lol_Best: in out List_Of_Lists;
    Q_Best: in out Modularity_Rec);

  -- Purpose : Keep Best Partition of the two provided ones
  -- Note    : The other partition is removed
  --
  -- Lol     : The First Partition
  -- Q       : The Modularity of First Partition
  -- Lol_Best: The Second and Best Partition
  -- Q_Best  : The Modularity of Second and Best Partition
  procedure Keep_Best_Partition(
    Lol: in out List_Of_Lists;
    Q: in Modularity_Rec;
    Lol_Best: in out List_Of_Lists;
    Q_Best: in out Modularity_Rec);

  -- Purpose : Set Initial Partition for next Heuristics according to Initialization mode
  --
  -- Lol_Prev: Input as Previous Partition, Output as Initial Partition
  -- Lol_Best: The Best Partition
  -- It      : The Initialization mode
  procedure Set_Initial_Partition(
    Lol_Prev: in out List_Of_Lists;
    Lol_Best: in List_Of_Lists;
    It: in Initialization_Type);

  -- Purpose : Select the Best among trivial Partitions
  -- Note    : Trivial partitions: Initial, Together, Connected_Components, Isolated
  procedure Best_Trivial_Partition(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Ini: out Modularity_Rec;
    Q_Best: out Modularity_Rec;
    Mt: in Modularity_Type := Weighted_Signed;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -- Purpose : Modularity Optimization using a single given Heuristic
  -- Note    : Repetitions ignored by: h, f, r, l
  -- Note    : Lol_Ini     ignored by: h
  -- Note    : Degeneration  only for: h
  -- Note    : Use Unassigned Lol_Ini for automatic initialization
  generic
    with procedure Improvement_Action(Log_Name: in Ustring; Lol: in List_Of_Lists; Q: in Modularity_Rec; Us: in Ustring := Null_Ustring) is Default_Improvement_Action;
    with procedure Repetition_Action(Log_Name: in Ustring; Lol: in List_Of_Lists; Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Optimization_Single_Heuristic(
    Gr: in Graph;
    Ht: in Heuristic_Type;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Signed;
    Log_Level: in Logging_Level := None;
    Log_Name: in Ustring := Null_Ustring;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -- Purpose : Modularity Optimization using a single given Heuristic
  -- Note    : Repetitions ignored by: h, f, r, l
  -- Note    : Lol_Ini     ignored by: h
  -- Note    : Degeneration  only for: h
  -- Note    : Use Unassigned Lol_Ini for automatic initialization
  procedure Optimization_Single_Heuristic(
    Gr: in Graph;
    Ht: in Heuristic_Type;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Signed;
    Log_Level: in Logging_Level := None;
    Log_Name: in Ustring := Null_Ustring;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -- Purpose : Modularity Optimization using a single given Heuristic
  -- Note    : Repetitions ignored by: h, f, r, l
  -- Note    : Degeneration  only for: h
  procedure Optimization_Single_Heuristic(
    Gr: in Graph;
    Ht: in Heuristic_Type;
    Repetitions: in Positive;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Signed;
    Log_Level: in Logging_Level := None;
    Log_Name: in Ustring := Null_Ustring;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -- Purpose : Modularity Optimization using a combination of Heuristics and Initializations
  -- Note    : Heuristics and Initializations string:
  -- Note    :    [htseflrb!:.+-]+
  -- Note    :    also uppercase symbols
  -- Note    :    also single case-insensitive full names (Exhaustive, ...)
  -- Note    : Repetitions ignored by: h, f, r, l
  -- Note    : Lol_Ini     ignored by: h
  -- Note    : Degeneration  only for: h
  -- Note    : Use Unassigned Lol_Ini for automatic initialization
  generic
    with procedure Improvement_Action(Log_Name: in Ustring; Lol: in List_Of_Lists; Q: in Modularity_Rec; Us: in Ustring := Null_Ustring) is Default_Improvement_Action;
    with procedure Repetition_Action(Log_Name: in Ustring; Lol: in List_Of_Lists; Q: in Modularity_Rec) is Default_Repetition_Action;
  procedure Generic_Optimization_Combined_Heuristic(
    Gr: in Graph;
    Hs: in String;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Signed;
    Log_Level: in Logging_Level := None;
    Log_Name: in Ustring := Null_Ustring;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -- Purpose : Modularity Optimization using a combination of Heuristics and Initializations
  -- Note    : Heuristics and Initializations string:
  -- Note    :    [htseflrb!:.+-]+
  -- Note    :    also uppercase symbols
  -- Note    :    also single case-insensitive full names (Exhaustive, ...)
  -- Note    : Repetitions ignored by: h, f, r, l
  -- Note    : Lol_Ini     ignored by: h
  -- Note    : Degeneration  only for: h
  -- Note    : Use Unassigned Lol_Ini for automatic initialization
  procedure Optimization_Combined_Heuristic(
    Gr: in Graph;
    Hs: in String;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Signed;
    Log_Level: in Logging_Level := None;
    Log_Name: in Ustring := Null_Ustring;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -- Purpose : Modularity Optimization using a combination of Heuristics and Initializations
  -- Note    : Heuristics and Initializations string:
  -- Note    :    [htseflrb!:.+-]+
  -- Note    :    also uppercase symbols
  -- Note    :    also single case-insensitive full names (Exhaustive, ...)
  -- Note    : Repetitions ignored by: h, f, r, l
  -- Note    : Degeneration  only for: h
  procedure Optimization_Combined_Heuristic(
    Gr: in Graph;
    Hs: in String;
    Repetitions: in Positive;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Signed;
    Log_Level: in Logging_Level := None;
    Log_Name: in Ustring := Null_Ustring;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularity_Optimization.Combined;
