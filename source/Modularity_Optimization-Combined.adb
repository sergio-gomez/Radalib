-- Radalib, Copyright (c) 2022 by
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


-- @filename Modularity_Optimization-Combined.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 07/04/2008
-- @revision 19/01/2018
-- @brief Main access to Combined Modularity Optimization Algorithms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Chrono_Utils; use Chrono_Utils;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularity_Optimization.Combined is

  -----------------------
  -- To_Heuristic_Type --
  -----------------------

  function To_Heuristic_Type(Ht_Name: in String) return Heuristic_Type is
  begin
    if    To_Lowercase(Ht_Name) = "h" or To_Lowercase(Ht_Name) = "exhaustive"    then
      return Exhaustive;
    elsif To_Lowercase(Ht_Name) = "t" or To_Lowercase(Ht_Name) = "tabu"          then
      return Tabu;
    elsif To_Lowercase(Ht_Name) = "s" or To_Lowercase(Ht_Name) = "spectral"      then
      return Spectral;
    elsif To_Lowercase(Ht_Name) = "e" or To_Lowercase(Ht_Name) = "extremal"      then
      return Extremal;
    elsif To_Lowercase(Ht_Name) = "f" or To_Lowercase(Ht_Name) = "fast"          then
      return Fast;
    elsif To_Lowercase(Ht_Name) = "l" or To_Lowercase(Ht_Name) = "louvain"       then
      return Louvain;
    elsif To_Lowercase(Ht_Name) = "r" or To_Lowercase(Ht_Name) = "reposition"    then
      return Reposition;
    elsif To_Lowercase(Ht_Name) = "b" or To_Lowercase(Ht_Name) = "bootstrapping" then
      return Bootstrapping;
    else
      raise Unknown_Heuristic_Error;
    end if;
  end To_Heuristic_Type;

  ----------------------------
  -- To_Initialization_Type --
  ----------------------------

  function To_Initialization_Type(It_Name: in String) return Initialization_Type is
  begin
    if    To_Lowercase(It_Name) = "ini_best"     or It_Name = "!" then
      return Ini_Best;
    elsif To_Lowercase(It_Name) = "ini_prev"     or It_Name = ":" then
      return Ini_Prev;
    elsif To_Lowercase(It_Name) = "ini_isolated" or It_Name = "." then
      return Ini_Isolated;
    elsif To_Lowercase(It_Name) = "ini_together" or It_Name = "+" then
      return Ini_Together;
    elsif To_Lowercase(It_Name) = "ini_default"  or It_Name = "-" then
      return Ini_Default;
    else
      raise Unknown_Initialization_Error;
    end if;
  end To_Initialization_Type;

  ----------------------
  -- To_Logging_Level --
  ----------------------

  function To_Logging_Level(Ll_Name: in String) return Logging_Level is
  begin
    if    To_Uppercase(Ll_Name) = "N" or To_Lowercase(Ll_Name) = "none"     then
      return None;
    elsif To_Uppercase(Ll_Name) = "S" or To_Lowercase(Ll_Name) = "summary"  then
      return Summary;
    elsif To_Uppercase(Ll_Name) = "P" or To_Lowercase(Ll_Name) = "progress" then
      return Progress;
    elsif To_Uppercase(Ll_Name) = "V" or To_Lowercase(Ll_Name) = "verbose"  then
      return Verbose;
    else
      raise Unknown_Logging_Error;
    end if;
  end To_Logging_Level;

  -----------------------
  -- Is_Heuristic_Type --
  -----------------------

  function Is_Heuristic_Type(S: in String) return Boolean is
    Ht: Heuristic_Type;
  begin
    Ht := To_Heuristic_Type(S);
    return True;
  exception
    when Unknown_Heuristic_Error =>
      return False;
  end Is_Heuristic_Type;

  ----------------------------
  -- Is_Initialization_Type --
  ----------------------------

  function Is_Initialization_Type(S: in String) return Boolean is
    It: Initialization_Type;
  begin
    It := To_Initialization_Type(S);
    return True;
  exception
    when Unknown_Initialization_Error =>
      return False;
  end Is_Initialization_Type;

  ---------------------------
  -- Update_Best_Partition --
  ---------------------------

  procedure Update_Best_Partition(Lol: in List_Of_Lists; Q: in Modularity_Rec;
    Lol_Best: in out List_Of_Lists; Q_Best: in out Modularity_Rec) is
  begin
    if Q.Total > Q_Best.Total then
      Free(Lol_Best);
      Lol_Best := Clone(Lol);
      Q_Best := Q;
    end if;
  end Update_Best_Partition;

  -------------------------
  -- Keep_Best_Partition --
  -------------------------

  procedure Keep_Best_Partition(Lol: in out List_Of_Lists; Q: in Modularity_Rec;
    Lol_Best: in out List_Of_Lists; Q_Best: in out Modularity_Rec) is
  begin
    if Q.Total > Q_Best.Total then
      Free(Lol_Best);
      Lol_Best := Lol;
      Q_Best := Q;
    else
      Free(Lol);
    end if;
  end Keep_Best_Partition;

  ---------------------------
  -- Set_Initial_Partition --
  ---------------------------

  procedure Set_Initial_Partition(Lol_Prev: in out List_Of_Lists; Lol_Best: in List_Of_Lists; It: in Initialization_Type) is
  begin
    case It is
      when Ini_Best =>
        Free(Lol_Prev);
        Lol_Prev := Clone(Lol_Best);
      when Ini_Prev =>
        null;
      when Ini_Isolated =>
        Reinitialize(Lol_Prev, Isolated_Initialization);
      when Ini_Together =>
        Reinitialize(Lol_Prev, Together_Initialization);
      when Ini_Default =>
        Reinitialize(Lol_Prev, Unassigned_Initialization);
    end case;
  end Set_Initial_Partition;

  ----------------------------
  -- Best_Trivial_Partition --
  ----------------------------

  procedure Best_Trivial_Partition(
    Gr: in Graph;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Ini: out Modularity_Rec;
    Q_Best: out Modularity_Rec;
    Mt: in Modularity_Type := Weighted_Signed;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    N: Natural;
    Lol, Lol_C: List_Of_Lists;
    Mi: Modularity_Info;
    Q, Q_C: Modularity_Rec;
  begin
    N := Number_Of_Vertices(Gr);
    Initialize(Mi, Gr, Mt, R, Pc);
    Q_Ini := (Reward => 0.0, Penalty => 0.0, Total => -1.0);

    if Has_Unassigned(Lol_Ini) then
      Initialize(Lol_Best, N, Isolated_Initialization);
      Update_Modularity(Mi, Lol_Best, Mt);
      Q_Best := Total_Modularity(Mi);
      Lol := Clone(Lol_Best);
      Q := Q_Best;
    else
      Lol_Best := Clone(Lol_Ini);
      Update_Modularity(Mi, Lol_Best, Mt);
      Q_Best := Total_Modularity(Mi);
      Q_Ini := Q_Best;

      Connected_Components(Gr, Lol_Best, Lol);
      if Number_Of_Lists(Lol) > Number_Of_Lists(Lol_Best) then
        Update_Modularity(Mi, Lol, Mt);
        Q := Total_Modularity(Mi);
        Update_Best_Partition(Lol, Q, Lol_Best, Q_Best);
      end if;

      Reinitialize(Lol, Isolated_Initialization);
      Update_Modularity(Mi, Lol, Mt);
      Q := Total_Modularity(Mi);
      Update_Best_Partition(Lol, Q, Lol_Best, Q_Best);
    end if;

    Reinitialize(Lol, Together_Initialization);
    Update_Modularity(Mi, Lol, Mt);
    Q := Total_Modularity(Mi);
    Update_Best_Partition(Lol, Q, Lol_Best, Q_Best);

    Connected_Components(Gr, Lol_C);
    if Number_Of_Lists(Lol_C) > 1 then
      Update_Modularity(Mi, Lol_C, Mt);
      Q_C := Total_Modularity(Mi);
      Update_Best_Partition(Lol_C, Q_C, Lol_Best, Q_Best);
    end if;

    Free(Lol);
    Free(Lol_C);
    Free(Mi);
  end Best_Trivial_Partition;

  -------------------------------------------
  -- Generic_Optimization_Single_Heuristic --
  -------------------------------------------

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
    Pc: in Double := 1.0)
  is

    Q_Ini, Q_0: Modularity_Rec;
    Lol, Lol_0: List_Of_Lists;
    Bootstrapping_Nonimprovements: constant Natural := 0;
    N: Natural;
    Hts: Ustring := S2U(Capitalize(Heuristic_Type'Image(Ht)));

    procedure Put_Lol(Fn, Description: in String; Lol: in List_Of_Lists; Q: in Modularity_Rec) is
      Ft: File_Type;
    begin
      if Lol /= Null_List_Of_Lists then
        Create(Ft, Out_File, Fn);
        Put_Line(Ft, "---------");
        Put_Line(Ft, Description);
        Put_Line(Ft, "Q = " & D2Se0(Q.Total, Aft => 6));
        Put_Line(Ft, "---");
        Put(Ft, Lol);
        Close(Ft);
      end if;
    end Put_Lol;

    procedure The_Improvement_Action(Log_Name: in Ustring; Lol: in List_Of_Lists; Q: in Modularity_Rec; Us: in Ustring := Null_Ustring) is
    begin
      if Log_Level = Verbose then
        if Us /= Null_Ustring then
          if Length(Log_Name) > 0 then
            Put_String_Line(U2S(Log_Name), "  " & U2S(Us));
          else
            Put_Line("  " & U2S(Us));
          end if;
        end if;
        if Q /= Null_Modularity_Rec then
          if Length(Log_Name) > 0 then
            Put_String_Line(U2S(Log_Name), "  Q = " & D2Se0(Q.Total, Aft => 6) & "   comms = " & I2S(Number_Of_Lists(Lol)));
          else
            Put_Line("  Q = " & D2Se0(Q.Total, Aft => 6) & "   comms = " & I2S(Number_Of_Lists(Lol)));
          end if;
        end if;
      end if;
      if Log_Level in Progress..Verbose then
        Put_Lol(U2S(Log_Name) & Logging_Lol_Current_Sufix, U2S(Hts) & " - Improvement action", Lol, Q);
      end if;
      Improvement_Action(Log_Name, Lol, Q);
    end The_Improvement_Action;

    procedure The_Repetition_Action(Log_Name: in Ustring; Lol: in List_Of_Lists; Q: in Modularity_Rec) is
    begin
      if Log_Level = Verbose then
        if Length(Log_Name) > 0 then
          Put_String_Line(U2S(Log_Name), "  End rep");
        else
          Put_Line("  End rep");
        end if;
      end if;
      if Log_Level in Progress..Verbose then
        Put_Lol(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix, U2S(Hts) & " - Repetition action", Lol, Q);
        if Q.Total > Q_Ini.Total then
          Put_Lol(U2S(Log_Name) & Logging_Lol_Best_Sufix, U2S(Hts) & " - Repetition action", Lol, Q);
        else
          Put_Lol(U2S(Log_Name) & Logging_Lol_Best_Sufix, U2S(Hts) & " - Repetition action", Lol_Ini, Q_Ini);
        end if;
      end if;
      Repetition_Action(Log_Name, Lol, Q);
    end The_Repetition_Action;

    procedure Tabu_Mod_Opt is new Generic_Tabu_Modularity_Optimization(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

    procedure Spec_Mod_Opt is new Generic_Spectral_Modularity_Optimization(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

    procedure Extr_Mod_Opt is new Generic_Extremal_Modularity_Optimization(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

    procedure Fast_Mod_Opt is new Generic_Fast_Algorithm_Modularity_Optimization(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

    procedure Louvain_Mod_Opt is new Generic_Louvain_Algorithm_Modularity_Optimization(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

    procedure Repos_Mod_Opt is new Generic_Reposition_Modularity_Optimization(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

    procedure Tabu_Mod_Boot is new Generic_Tabu_Modularity_Bootstrapping(
      Improvement_Action => The_Improvement_Action,
      Repetition_Action  => The_Repetition_Action);

  begin
    if Log_Level = Verbose then
      if Length(Log_Name) > 0 then
        Put_String_Line(To_String(Log_Name), "=====================================");
        Put_String_Line(To_String(Log_Name), "Starting " & I2S(Repetitions) & " " & U2S(Hts));
      else
        Put_Line("=====================================");
        Put_Line("Starting " & I2S(Repetitions) & " " & U2S(Hts));
      end if;
    end if;

    N := Number_Of_Vertices(Gr);
    Degeneration := 1;

    Best_Trivial_Partition(Gr, Lol_Ini, Lol_0, Q_Ini, Q_0, Mt, R, Pc);

    case Ht is
      when Exhaustive =>
        Exhaustive_Modularity_Optimization(Gr, Lol_Best, Q_Best, Degeneration, Mt, R, Pc);
      when Tabu =>
        if Has_Unassigned(Lol_Ini) then
          Initialize(Lol, N, Isolated_Initialization);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Tabu_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
        Free(Lol);
      when Spectral =>
        if Has_Unassigned(Lol_Ini) then
          Connected_Components(Gr, Lol, Weak_Components);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Spec_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
        Free(Lol);
      when Extremal =>
        if Has_Unassigned(Lol_Ini) then
          Connected_Components(Gr, Lol, Weak_Components);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Extr_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
        Free(Lol);
      when Fast =>
        if Has_Unassigned(Lol_Ini) then
          Initialize(Lol, N, Isolated_Initialization);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Fast_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
        Free(Lol);
      when Louvain =>
        if Has_Unassigned(Lol_Ini) then
          Initialize(Lol, N, Isolated_Initialization);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Louvain_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
        Free(Lol);
      when Reposition =>
        if Has_Unassigned(Lol_Ini) then
          Initialize(Lol, N, Isolated_Initialization);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Repos_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
        Free(Lol);
      when Bootstrapping =>
        if Has_Unassigned(Lol_Ini) then
          Initialize(Lol, N, Isolated_Initialization);
        else
          Connected_Components(Gr, Lol_Ini, Lol);
        end if;
        Tabu_Mod_Boot(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, Bootstrapping_Nonimprovements, R, Pc, Log_Name);
        Free(Lol);
    end case;

    Keep_Best_Partition(Lol_0, Q_0, Lol_Best, Q_Best);

    Sort_Lists(Lol_Best);
    Sort_By_Size(Lol_Best);
    if Log_Level /= None then
      Delete_File(U2S(Log_Name) & Logging_Lol_Current_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix);
      Put_Lol(U2S(Log_Name) & Logging_Lol_Best_Sufix, "End Single Heuristic", Lol_Best, Q_Best);
    end if;
    if Log_Level = Verbose then
      if Length(Log_Name) > 0 then
        Put_String_Line(To_String(Log_Name), "End of " & I2S(Repetitions) & " " & U2S(Hts));
      else
        Put_Line("End of " & I2S(Repetitions) & " " & U2S(Hts));
      end if;
    end if;
  end Generic_Optimization_Single_Heuristic;

  -----------------------------------
  -- Optimization_Single_Heuristic --
  -----------------------------------

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
    Pc: in Double := 1.0)
  is
    procedure Opt_Single_Heur is new Generic_Optimization_Single_Heuristic(
      Improvement_Action => Default_Improvement_Action,
      Repetition_Action  => Default_Repetition_Action);
  begin
    Opt_Single_Heur(Gr, Ht, Repetitions, Lol_Ini, Lol_Best, Q_Best, Degeneration, Mt, Log_Level, Log_Name, R, Pc);
  end Optimization_Single_Heuristic;

  -----------------------------------
  -- Optimization_Single_Heuristic --
  -----------------------------------

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
    Pc: in Double := 1.0)
  is
    Lol_Ini: List_Of_Lists;
  begin
    Initialize(Lol_Ini, Number_Of_Vertices(Gr), Unassigned_Initialization);
    Optimization_Single_Heuristic(Gr, Ht, Repetitions, Lol_Ini, Lol_Best, Q_Best, Degeneration, Mt, Log_Level, Log_Name, R, Pc);
    Free(Lol_Ini);
  end Optimization_Single_Heuristic;

  ---------------------------------------------
  -- Generic_Optimization_Combined_Heuristic --
  ---------------------------------------------

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
    Pc: in Double := 1.0)
  is
    procedure Opt_Single_Heur is new Generic_Optimization_Single_Heuristic(
      Improvement_Action => Improvement_Action,
      Repetition_Action  => Repetition_Action);

    function Results_Str(Q: in Modularity_Rec; D: in Duration; N: in Natural) return Ustring is
      U: Ustring;
    begin
      U := S2U("Q_Best = " & D2Se0(Q.Total, Aft => 6) & "   (" & D2S(D, 3) & ")" & "   communities: " & I2S(N));
      if Degeneration > 1 then
        U := U & S2U("   degeneration: " & I2S(Degeneration));
      end if;
      U := U & S2U("   parameters: " & To_Name(Mt, True) & " " & Hs & " " & I2S(Repetitions));
      if R /= No_Resistance or Pc /= 1.0 then
        if R = No_Resistance then
          U := U & S2U(" 0.0");
        else
          if abs R > 1.0e-4 then
            U := U & S2U(" " & D2Ss(R, Aft => 6));
          else
            U := U & S2U(" " & D2S(R, Aft => 4, Exp => 3));
          end if;
        end if;
        if Pc /= 1.0 then
          if abs Pc > 1.0e-4 then
            U := U & S2U(" " & D2Ss(Pc, Aft => 6));
          else
            U := U & S2U(" " & D2S(Pc, Aft => 4, Exp => 3));
          end if;
        end if;
      end if;
      return U;
    end Results_Str;

    Ht: Heuristic_Type;
    It: Initialization_Type;
    Is_Init, Is_Heur, Is_Single_Init, Is_Single_Heur: Boolean;
    Lol_Prev, Lol_Best_Heur: List_Of_Lists;
    Q_Ini, Q_Prev, Q_Best_Heur: Modularity_Rec;
    H_Or_I: String(1..1);
    I: Positive;
    Chrono: Chronometer;
    Degen: Positive;
    Ft: File_Type;
    Us: Ustring;
  begin
    if Log_Level /= None then
      if Length(Log_Name) > 0 then
        Delete_File(U2S(Log_Name));
      end if;
      Delete_File(U2S(Log_Name) & Logging_Lol_Current_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Best_Sufix);
    end if;

    Is_Single_Init := Is_Initialization_Type(Hs);
    Is_Single_Heur := Is_Heuristic_Type(Hs);
    Degeneration := 1;

    if Is_Single_Init then
      -- Just initialization without heuristic
      It := To_Initialization_Type(Hs);
      Best_Trivial_Partition(Gr, Lol_Ini, Lol_Best, Q_Ini, Q_Best, Mt, R, Pc);
      Lol_Prev := Clone(Lol_Ini);
      Set_Initial_Partition(Lol_Prev, Lol_Best, It);
      Free(Lol_Best);
      Lol_Best := Lol_Prev;
      Q_Best := Modularity(Gr, Lol_Best, Mt, R, Pc);
    elsif Is_Single_Heur then
      -- Just heuristic without initialization
      Ht := To_Heuristic_Type(Hs);
      Start(Chrono);
      Opt_Single_Heur(Gr, Ht, Repetitions, Lol_Ini, Lol_Best, Q_Best, Degeneration, Mt, Log_Level, Log_Name, R, Pc);
      Stop(Chrono);
    else
      -- Compound set of heuristics and/or initializations
      Lol_Prev := Clone(Lol_Ini);
      Best_Trivial_Partition(Gr, Lol_Prev, Lol_Best, Q_Prev, Q_Best, Mt, R, Pc);
      I := 1;
      while I <= Hs'Last loop
        H_Or_I := Hs(I..I);
        Is_Init := Is_Initialization_Type(H_Or_I);
        Is_Heur := Is_Heuristic_Type(H_Or_I);
        if Is_Init then
          -- Initialization
          It := To_Initialization_Type(H_Or_I);
          Set_Initial_Partition(Lol_Prev, Lol_Best, It);
        elsif Is_Heur then
          -- Heuristic
          Ht := To_Heuristic_Type(H_Or_I);
          Start(Chrono);
          Opt_Single_Heur(Gr, Ht, Repetitions, Lol_Prev, Lol_Best_Heur, Q_Best_Heur, Degen, Mt, Log_Level, Log_Name, R, Pc);
          Stop(Chrono);
          if Degen > Degeneration then
            Degeneration := Degen;
          end if;
          Update_Best_Partition(Lol_Best_Heur, Q_Best_Heur, Lol_Best, Q_Best);
          Free(Lol_Prev);
          Lol_Prev := Lol_Best_Heur;
          Q_Prev := Q_Best_Heur;
        else
          Put_Line("Unknown Heuristic or Initialization '" & H_Or_I & "'");
          raise Unknown_Heuristic_Error;
        end if;

        if Log_Level /= None then
          if Is_Init then
            Us := S2U(Capitalize(Initialization_Type'Image(It)));
          else
            Us := S2U("Q(" & H_Or_I & ") = " & D2Se0(Q_Best_Heur.Total, Aft => 6) & "   (" & D2S(Elapsed(Chrono), 3) & ")");
          end if;
          if Length(Log_Name) > 0 then
            Open_Or_Create(Ft, U2S(Log_Name));
            if Log_Level in Summary..Progress then
              Put(Ft, "  ");
            elsif Log_Level = Verbose and Is_Init then
              Put_Line(Ft, "=====================================");
            end if;
            Put_Line(Ft, U2S(Us));
            Close(Ft);
          end if;
          if Is_Init then
            Put_Line("        --- " & U2S(Us) & " ---");
          else
            Put_Line("    " & U2S(Us));
          end if;
        end if;

        I := I + 1;
      end loop;
      Free(Lol_Prev);
    end if;

    if Log_Level /= None then
      Delete_File(U2S(Log_Name) & Logging_Lol_Current_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Best_Sufix);
      Us := Results_Str(Q_Best, Accumulated(Chrono), Number_Of_Lists(Lol_Best));
      if Length(Log_Name) > 0 then
        Open_Or_Create(Ft, To_String(Log_Name));
        if Log_Level = Verbose then
          Put_Line(Ft, "=====================================");
        end if;
        Put_Line(Ft, U2S(Us));
        Close(Ft);
      elsif Log_Level = Verbose then
        Put_Line("=====================================");
      end if;
      Put_Line("  " & U2S(Us));
    end if;
  end Generic_Optimization_Combined_Heuristic;

  -------------------------------------
  -- Optimization_Combined_Heuristic --
  -------------------------------------

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
    Pc: in Double := 1.0)
  is
    procedure Opt_Comb_Heur is new Generic_Optimization_Combined_Heuristic(
      Improvement_Action => Default_Improvement_Action,
      Repetition_Action  => Default_Repetition_Action);
  begin
    Opt_Comb_Heur(Gr, Hs, Repetitions, Lol_Ini, Lol_Best, Q_Best, Degeneration, Mt, Log_Level, Log_Name, R, Pc);
  end Optimization_Combined_Heuristic;

  -------------------------------------
  -- Optimization_Combined_Heuristic --
  -------------------------------------

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
    Pc: in Double := 1.0)
  is
    Lol_Ini: List_Of_Lists;
  begin
    Initialize(Lol_Ini, Number_Of_Vertices(Gr), Unassigned_Initialization);
    Optimization_Combined_Heuristic(Gr, Hs, Repetitions, Lol_Ini, Lol_Best, Q_Best, Degeneration, Mt, Log_Level, Log_Name, R, Pc);
    Free(Lol_Ini);
  end Optimization_Combined_Heuristic;

end Modularity_Optimization.Combined;
