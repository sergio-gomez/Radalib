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


-- @filename Modularity_Optimization-Combined.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 07/04/2008
-- @revision 26/10/2014
-- @brief Main access to Combined Modularity Optimization Algorithms

with Ada.Text_IO; use Ada.Text_IO;

with Utils.IO; use Utils.IO;
with Chrono_Utils; use Chrono_Utils;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularity_Optimization.Combined is

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
    elsif To_Lowercase(Ht_Name) = "r" or To_Lowercase(Ht_Name) = "reposition"    then
      return Reposition;
    elsif To_Lowercase(Ht_Name) = "b" or To_Lowercase(Ht_Name) = "bootstrapping" then
      return Bootstrapping;
    else
      raise Unknown_Heuristic_Error;
    end if;
  end To_Heuristic_Type;

  -------------------------------------------
  -- Generic_Optimization_Single_Heuristic --
  -------------------------------------------

  procedure Generic_Optimization_Single_Heuristic
   (Gr: in Graph;
    Ht: in Heuristic_Type;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    Log_Level: in Logging_Level := None;
    Log_Name: in Unbounded_String := Null_Unbounded_String;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is

    Q_Ini, Q_0, Q_Best2: Modularity_Rec;
    Lol, Lol_0, Lol_Best2: List_Of_Lists;
    Num_Nonimprovements: constant Natural := 0;
    N: Natural;

    procedure Put_Lol(Fn, Description: in String; Lol: in List_Of_Lists; Q: in Modularity_Rec) is
      Ft: File_Type;
    begin
      Sort_Lists(Lol);
      Sort_By_Size(Lol);
      Create(Ft, Out_File, Fn);
      Put_Line(Ft, "---------");
      Put_Line(Ft, Description);
      Put_Line(Ft, "Q = " & D2S(Q.Total, Aft => 6, Exp => 0));
      Put_Line(Ft, "---");
      Put(Ft, Lol);
      Close(Ft);
    end Put_Lol;

    procedure The_Improvement_Action(Log_Name: in Unbounded_String; Lol: in List_Of_Lists; Q: in Modularity_Rec) is
    begin
      if Log_Level = Verbose then
        if Length(Log_Name) > 0 then
          Put_String_Line(U2S(Log_Name), "  Q = " & D2S(Modularity(Gr, Lol, Mt, R, Pc), Aft => 6, Exp => 0));
        else
          Put_Line("  Q = " & D2S(Modularity(Gr, Lol, Mt, R, Pc), Aft => 6, Exp => 0));
        end if;
      end if;
      if Log_Level in Progress..Verbose then
        Put_Lol(U2S(Log_Name) & Logging_Lol_Current_Sufix, "Improvement action - Current Lol", Lol, Q);
      end if;
      Improvement_Action(Log_Name, Lol, Q);
    end The_Improvement_Action;

    procedure The_Repetition_Action(Log_Name: in Unbounded_String; Lol: in List_Of_Lists; Q: in Modularity_Rec) is
    begin
      if Log_Level = Verbose then
        if Length(Log_Name) > 0 then
          Put_String_Line(U2S(Log_Name), "  End rep");
        else
          Put_Line("  End rep");
        end if;
      end if;
      if Log_Level in Progress..Verbose then
        Put_Lol(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix, "Repetition action - Best Lol of Current Heuristic", Lol, Q);
        if Q.Total > Q_Ini.Total then
          Put_Lol(U2S(Log_Name) & Logging_Lol_Best_Sufix, "Repetition action - Best Lol", Lol, Q);
        else
          Put_Lol(U2S(Log_Name) & Logging_Lol_Best_Sufix, "Repetition action - Best Lol", Lol_Ini, Q_Ini);
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
        Put_String_Line(To_String(Log_Name), "Starting " & I2S(Repetitions) & " " & To_Lowercase(Heuristic_Type'Image(Ht)));
      else
        Put_Line("=====================================");
        Put_Line("Starting " & I2S(Repetitions) & " " & To_Lowercase(Heuristic_Type'Image(Ht)));
      end if;
    end if;

    N := Number_Of_Vertices(Gr);

    Initialize(Lol_0, N, Together_Initialization);
    Q_0 := Modularity(Gr, Lol_0, Mt, R, Pc);
    if not Has_Unassigned(Lol_Ini) then
      Q_Ini := Modularity(Gr, Lol_Ini, Mt, R, Pc);
      if Q_Ini.Total > Q_0.Total then
        Q_0 := Q_Ini;
        Free(Lol_0);
        Lol_0 := Clone(Lol_Ini);
      end if;
    end if;
    Degeneration := 1;

    case HT is
      when Exhaustive =>
        Exhaustive_Modularity_Optimization(Gr, Lol_Best, Q_Best, Degeneration, Mt, R, Pc);
      when Tabu =>
        if Has_Unassigned(Lol_Ini) then
          Connected_Components(Gr, Lol, Weak_Components);
          Tabu_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
          Reinitialize(Lol, Isolated_Initialization);
          Tabu_Mod_Opt(Gr, Lol, Lol_Best2, Q_Best2, Mt, Repetitions, R, Pc, Log_Name);
          Free(Lol);
          if Q_Best.Total >= Q_Best2.Total then
            Free(Lol_Best2);
          else
            Q_Best := Q_Best2;
            Free(Lol_Best);
            Lol_Best := Lol_Best2;
          end if;
        else
          Tabu_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
        end if;
      when Spectral =>
        Spec_Mod_Opt(Gr, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
      when Extremal =>
        Connected_Components(Gr, Lol, Weak_Components);
        Extr_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, R, Pc, Log_Name);
        Free(Lol);
      when Fast =>
        if Has_Unassigned(Lol_Ini) then
          Initialize(Lol, N, Isolated_Initialization);
          Fast_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
          Free(Lol);
        else
          Fast_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
        end if;
      when Reposition =>
        if Has_Unassigned(Lol_Ini) then
          Connected_Components(Gr, Lol, Weak_Components);
          Repos_Mod_Opt(Gr, Lol, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
          Reinitialize(Lol, Isolated_Initialization);
          Repos_Mod_Opt(Gr, Lol, Lol_Best2, Q_Best2, Mt, 1, R, Pc, Log_Name);
          Free(Lol);
          if Q_Best.Total >= Q_Best2.Total then
            Free(Lol_Best2);
          else
            Q_Best := Q_Best2;
            Free(Lol_Best);
            Lol_Best := Lol_Best2;
          end if;
        else
          Repos_Mod_Opt(Gr, Lol_Ini, Lol_Best, Q_Best, Mt, 1, R, Pc, Log_Name);
        end if;
      when Bootstrapping =>
        if Has_Unassigned(Lol_Ini) then
          Connected_Components(Gr, Lol, Weak_Components);
          Tabu_Mod_Boot(Gr, Lol, Lol_Best, Q_Best, Mt, Repetitions, Num_Nonimprovements, R, Pc, Log_Name);
          Reinitialize(Lol, Isolated_Initialization);
          Tabu_Mod_Boot(Gr, Lol, Lol_Best2, Q_Best2, Mt, Repetitions, Num_Nonimprovements, R, Pc, Log_Name);
          Free(Lol);
          if Q_Best.Total >= Q_Best2.Total then
            Free(Lol_Best2);
          else
            Q_Best := Q_Best2;
            Free(Lol_Best);
            Lol_Best := Lol_Best2;
          end if;
        else
          Tabu_Mod_Boot(Gr, Lol_Ini, Lol_Best, Q_Best, Mt, Repetitions, Num_Nonimprovements, R, Pc, Log_Name);
        end if;
    end case;
    if Q_Best.Total < Q_0.Total then
      Q_Best := Q_0;
      Free(Lol_Best);
      Lol_Best := Lol_0;
    else
      Free(Lol_0);
    end if;

    Sort_Lists(Lol_Best);
    Sort_By_Size(Lol_Best);
    if Log_Level /= None then
      Delete_File(U2S(Log_Name) & Logging_Lol_Current_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix);
      Put_Lol(U2S(Log_Name) & Logging_Lol_Best_Sufix, "End Single Heuristic", Lol_Best, Q_Best);
    end if;
    if Log_Level = Verbose then
      if Length(Log_Name) > 0 then
        Put_String_Line(To_String(Log_Name), "End of " & I2S(Repetitions) & " " & To_Lowercase(Heuristic_Type'Image(HT)));
      else
        Put_Line("End of " & I2S(Repetitions) & " " & To_Lowercase(Heuristic_Type'Image(HT)));
      end if;
    end if;
  end Generic_Optimization_Single_Heuristic;

  -----------------------------------
  -- Optimization_Single_Heuristic --
  -----------------------------------

  procedure Optimization_Single_Heuristic
   (Gr: in Graph;
    Ht: in Heuristic_Type;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    Log_Level: in Logging_Level := None;
    Log_Name: in Unbounded_String := Null_Unbounded_String;
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

  procedure Optimization_Single_Heuristic
   (Gr: in Graph;
    Ht: in Heuristic_Type;
    Repetitions: in Positive;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    Log_Level: in Logging_Level := None;
    Log_Name: in Unbounded_String := Null_Unbounded_String;
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

  procedure Generic_Optimization_Combined_Heuristic
   (Gr: in Graph;
    Hs: in String;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    Log_Level: in Logging_Level := None;
    Log_Name: in Unbounded_String := Null_Unbounded_String;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    procedure Opt_Single_Heur is new Generic_Optimization_Single_Heuristic(
      Improvement_Action => Improvement_Action,
      Repetition_Action  => Repetition_Action);

    function Results_Str(Q: in Modularity_Rec; D: in Duration; N: in Natural) return String is
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
      return U2S(U);
    end Results_Str;

    Ht: Heuristic_Type;
    Is_Single_Heur: Boolean;
    Lol_Prev: List_Of_Lists;
    Heur: String(1..1);
    Chrono: Chronometer;
    Degen: Positive;
    Ft: File_Type;
  begin
    if Log_Level /= None then
      if Length(Log_Name) > 0 then
        Delete_File(U2S(Log_Name));
      end if;
      Delete_File(U2S(Log_Name) & Logging_Lol_Current_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Best_Sufix);
    end if;

    Is_Single_Heur := True;
    begin
      Ht := To_Heuristic_Type(Hs);
    exception
      when Unknown_Heuristic_Error => Is_Single_Heur := False;
    end;

    if Is_Single_Heur then
      Ht := To_Heuristic_Type(Hs);
      Start(Chrono);
      Opt_Single_Heur(Gr, Ht, Repetitions, Lol_Ini, Lol_Best, Q_Best, Degeneration, Mt, Log_Level, Log_Name, R, Pc);
      Stop(Chrono);
    else
      Degeneration := 1;
      Lol_Prev := Clone(Lol_Ini);
      for I in Hs'Range loop
        Heur := Hs(I..I);
        Ht := To_Heuristic_Type(Heur);
        Start(Chrono);
        Opt_Single_Heur(Gr, Ht, Repetitions, Lol_Prev, Lol_Best, Q_Best, Degen, Mt, Log_Level, Log_Name, R, Pc);
        Stop(Chrono);
        if Degen > Degeneration then
          Degeneration := Degen;
        end if;
        Free(Lol_Prev);
        Lol_Prev := Lol_Best;
        if Log_Level /= None then
          if Length(Log_Name) > 0 then
            Open_Or_Create(Ft, U2S(Log_Name));
            if Log_Level in Summary..Progress then
              Put(Ft, "  ");
            end if;
            Put(Ft, "Q(" & Heur & ") = " & D2S(Q_Best.Total, Aft => 6, Exp => 0) & "   (");
            Put_Duration(Ft, Elapsed(Chrono), 3);
            Put_Line(Ft, ")");
            Close(Ft);
          end if;
          Put("    Q(" & Heur & ") = " & D2S(Q_Best.Total, Aft => 6, Exp => 0) & "   (");
          Put_Duration(Elapsed(Chrono), 3);
          Put_Line(")");
        end if;
      end loop;
    end if;

    if Log_Level /= None then
      Delete_File(U2S(Log_Name) & Logging_Lol_Current_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Heuristic_Sufix);
      Delete_File(U2S(Log_Name) & Logging_Lol_Best_Sufix);
      if Length(Log_Name) > 0 then
        Open_Or_Create(Ft, To_String(Log_Name));
        if Log_Level = Verbose then
          Put_Line(Ft, "=====================================");
        end if;
        Put_Line(Ft, Results_Str(Q_Best, Accumulated(Chrono), Number_Of_Lists(Lol_Best)));
        Close(Ft);
      elsif Log_Level = Verbose then
        Put_Line("=====================================");
      end if;
      Put_Line("  " & Results_Str(Q_Best, Accumulated(Chrono), Number_Of_Lists(Lol_Best)));
    end if;
  end Generic_Optimization_Combined_Heuristic;

  -------------------------------------
  -- Optimization_Combined_Heuristic --
  -------------------------------------

  procedure Optimization_Combined_Heuristic
   (Gr: in Graph;
    Hs: in String;
    Repetitions: in Positive;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    Log_Level: in Logging_Level := None;
    Log_Name: in Unbounded_String := Null_Unbounded_String;
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

  procedure Optimization_Combined_Heuristic
   (Gr: in Graph;
    Hs: in String;
    Repetitions: in Positive;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    Log_Level: in Logging_Level := None;
    Log_Name: in Unbounded_String := Null_Unbounded_String;
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
