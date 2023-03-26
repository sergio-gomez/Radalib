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


-- @filename Communities_Detection.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/05/2008
-- @revision 14/09/2020
-- @brief Modularity optimization by combination of different heuristics

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Pajek_IO; use Pajek_IO;
with Modularity_Optimization.Combined; use Modularity_Optimization.Combined;


procedure Communities_Detection is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2023 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Community detection by modularity optimization:               ==");
    Put_Line("==   - unweighted (UN)          - uniform nullcase (UUN, WUN)    ==");
    Put_Line("==   - weighted (WN)            - local average (WLA, WULA)      ==");
    Put_Line("==   - weighted signed (WS)     - wh links unwh nullcase (WLUN)  ==");
    Put_Line("==   - link rank (WLR)          - bipartite path motif (WBPM)    ==");
    Put_Line("==   - no nullcase (WNN)        - bipartite path signed (WBPS)   ==");
    Put_Line("== Algorithms, which can be combined:                            ==");
    Put_Line("==   - exhaustive search (h)    - louvain (l)                    ==");
    Put_Line("==   - tabu search (t)          - fast algorithm (f)             ==");
    Put_Line("==   - extremal (e)             - reposition (r)                 ==");
    Put_Line("==   - spectral (s)             - bootstrapping (b)              ==");
    Put_Line("== Initialization modes:                                         ==");
    Put_Line("==   - isolated (.)             - best (!)     - default (-)     ==");
    Put_Line("==   - together (+)             - prev (:)                       ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Heuristics: Ustring;
  Gr_Name: Ustring;
  Log_Name: Ustring;
  Mod_Name: Ustring;
  Lol_Name: Ustring;
  Log_Level: Logging_Level;
  Repetitions: Positive;
  Resistance_Str: Ustring := Null_Ustring;
  Penalty_Str: Ustring := Null_Ustring;
  Mod_Type: Modularity_Type;
  Resistance: Double := No_Resistance;
  Penalty_Coeff: Double := 1.0;

  Log_Filename: Ustring := Null_Ustring;
  Log_Ext: constant String := ".log";
  Gr: Graph;
  Lol_Ini: List_Of_Lists;
  Lol_Best: List_Of_Lists;
  Q_Best: Modularity_Rec;
  Degeneration: Positive;
  Ft: File_Type;
begin
  Put_Info;

  if Argument_Count not in 6..8 then
    Put_Line("Usage:  " & Command_Name & "  log_level  modularity_type  heuristics  repetitions  [ resistance  [ penalty_coeff ] ]  net_name  lol_best_name");
    New_Line;
    Put_Line("   Logging Levels      :  N | S | P | V");
    Put_Line("                            also lowercase symbols");
    Put_Line("                            also case-insensitive full names (None, ...)");
    Put_Line("                              n = None              p = Progress");
    Put_Line("                              s = Summary           v = Verbose");
    New_Line;
    Put_Line("   Modularity Types    :  UN | UUN | WN | WS | WUN | WLA | WULA | WLUN | WNN | WLR | WBPM | WBPS");
    Put_Line("                            also lowercase symbols");
    Put_Line("                            also case-insensitive full names (Unweighted_Newman, ...)");
    Put_Line("                              UN   = Unweighted_Newman");
    Put_Line("                              UUN  = Unweighted_Uniform_Nullcase");
    Put_Line("                              WN   = Weighted_Newman");
    Put_Line("                              WS   = Weighted_Signed");
    Put_Line("                              WUN  = Weighted_Uniform_Nullcase");
    Put_Line("                              WLA  = Weighted_Local_Average");
    Put_Line("                              WULA = Weighted_Uniform_Local_Average");
    Put_Line("                              WLUN = Weighted_Links_Unweighted_Nullcase");
    Put_Line("                              WNN  = Weighted_No_Nullcase");
    Put_Line("                              WLR  = Weighted_Link_Rank");
    Put_Line("                              WBPM = Weighted_Bipartite_Path_Motif");
    Put_Line("                              WBPS = Weighted_Bipartite_Path_Signed");
    New_Line;
    Put_Line("   Heuristics String   :  [htseflrb!:.+-]+");
    Put_Line("                            also uppercase symbols");
    Put_Line("                            also single case-insensitive full names (Exhaustive, ...)");
    Put_Line("                            heuristics");
    Put_Line("                              h = Exhaustive        l = Louvain");
    Put_Line("                              t = Tabu              f = Fast");
    Put_Line("                              s = Spectral          r = Reposition");
    Put_Line("                              e = Extremal          b = Bootstrapping");
    Put_Line("                            initializations");
    Put_Line("                              ! = Ini_Best          . = Ini_Isolated");
    Put_Line("                              : = Ini_Prev          + = Ini_Together");
    Put_Line("                              - = Ini_Default ");
    New_Line;
    Put_Line("   Repetitions         :  positive integer");
    Put_Line("                            does not apply to [hlfr] algorithms");
    New_Line;
    Put_Line("   Resistance          :  resistance of nodes to join communities, as a common self-loop");
    Put_Line("                            positive or negative real number");
    Put_Line("                            0 | 0.0 | default => no resistance, do not add self-loops");
    New_Line;
    Put_Line("   Penalty Coefficient :  relative importance of null-case term");
    Put_Line("                            non-negative real number");
    Put_Line("                            default => 1.0");
    New_Line;
    Put_Line("   Network name        :  name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   Lol Best Filename   :  file for the best partition found in Lol format");
    Put_Line("                            if file exists, the partition becomes the initial partition");
    return;
  elsif Argument_Count = 6 then
    Log_Name       := S2U(Argument(1));
    Mod_Name       := S2U(Argument(2));
    Heuristics     := S2U(Argument(3));
    Repetitions    := S2I(Argument(4));
    Gr_Name        := S2U(Argument(5));
    Lol_Name       := S2U(Argument(6));
  elsif Argument_Count = 7 then
    Log_Name       := S2U(Argument(1));
    Mod_Name       := S2U(Argument(2));
    Heuristics     := S2U(Argument(3));
    Repetitions    := S2I(Argument(4));
    Resistance_Str := S2U(Argument(5));
    Gr_Name        := S2U(Argument(6));
    Lol_Name       := S2U(Argument(7));
  elsif Argument_Count = 8 then
    Log_Name       := S2U(Argument(1));
    Mod_Name       := S2U(Argument(2));
    Heuristics     := S2U(Argument(3));
    Repetitions    := S2I(Argument(4));
    Resistance_Str := S2U(Argument(5));
    Penalty_Str    := S2U(Argument(6));
    Gr_Name        := S2U(Argument(7));
    Lol_Name       := S2U(Argument(8));
  end if;

  Log_Level := To_Logging_Level(Argument(1));
  Mod_Type  := To_Modularity_Type(Argument(2));

  if Log_Level /= None then
    Log_Filename := Lol_Name & Log_Ext;
  end if;

  if Resistance_Str = Null_Ustring or else (U2S(Resistance_Str) = "0" or U2S(Resistance_Str) = "0.0") then
    Resistance := No_Resistance;
  else
    Resistance := U2D(Resistance_Str);
  end if;
  if Penalty_Str = Null_Ustring then
    Penalty_Coeff := 1.0;
  else
    Penalty_Coeff := U2D(Penalty_Str);
  end if;

  Get_Graph(U2S(Gr_Name), Gr);
  Put_Line(U2S(Gr_Name) & " :");
  if Resistance_Str = Null_Ustring then
    Put_Line("  Parameters: " & U2S(Mod_Name) & " " & U2S(Heuristics) & " " & I2S(Repetitions));
  elsif Penalty_Str = Null_Ustring then
    Put_Line("  Parameters: " & U2S(Mod_Name) & " " & U2S(Heuristics) & " " & I2S(Repetitions) & " " & U2S(Resistance_Str));
  else
    Put_Line("  Parameters: " & U2S(Mod_Name) & " " & U2S(Heuristics) & " " & I2S(Repetitions) & " " & U2S(Resistance_Str) & " " & U2S(Penalty_Str));
  end if;

  if File_Exists(U2S(Lol_Name)) then
    Open(Ft, In_File, U2S(Lol_Name));
    Skip_Line(Ft, 4);
    Get(Ft, Lol_Ini);
    Close(Ft);
    Optimization_Combined_Heuristic(Gr, U2S(Heuristics), Repetitions, Lol_Ini, Lol_Best, Q_Best,
                                    Degeneration, Mod_Type, Log_Level, Log_Filename, Resistance, Penalty_Coeff);
    Free(Lol_Ini);
  else
    Optimization_Combined_Heuristic(Gr, U2S(Heuristics), Repetitions, Lol_Best, Q_Best,
                                    Degeneration, Mod_Type, Log_Level, Log_Filename, Resistance, Penalty_Coeff);
  end if;

  Create(Ft, Out_File, U2S(Lol_Name));
  Put_Line(Ft, "---------");
  if Resistance_Str = Null_Ustring then
    Put_Line(Ft, "Parameters: " & U2S(Mod_Name) & " " & U2S(Heuristics) & " " & I2S(Repetitions));
  elsif Penalty_Str = Null_Ustring then
    Put_Line(Ft, "Parameters: " & U2S(Mod_Name) & " " & U2S(Heuristics) & " " & I2S(Repetitions) & " " & U2S(Resistance_Str));
  else
    Put_Line(Ft, "Parameters: " & U2S(Mod_Name) & " " & U2S(Heuristics) & " " & I2S(Repetitions) & " " & U2S(Resistance_Str) & " " & U2S(Penalty_Str));
  end if;
  Put_Line(Ft, "Q = " & D2S(Q_Best.Total, Aft => 6, Exp => 0));
  Put_Line(Ft, "---");
  Put(Ft, Lol_Best);
  Close(Ft);

  Free(Lol_Best);
  Free(Gr);
exception
  when Unknown_Logging_Error    => Put_Line("Error: unknown Logging Level '" & U2S(Log_Name) & "'");
  when Unknown_Modularity_Error => Put_Line("Error: unknown Modularity Type '" & U2S(Mod_Name) & "'");
  when Unknown_Heuristic_Error  => Put_Line("Error: unknown Heuristic description '" & U2S(Heuristics) & "'");
end Communities_Detection;

