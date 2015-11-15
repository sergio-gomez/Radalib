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


-- @filename Mesoscales_Detection.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 19/11/2008
-- @revision 22/09/2015
-- @brief Search Mesoscales using Self-Loops

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Long_Elementary_Functions; use Ada.Numerics.Long_Elementary_Functions;
with Ada.Text_Io; use Ada.Text_Io;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Utils.IO_Double; use Utils.IO_Double;
with Utils.IO_Integer; use Utils.IO_Integer;
with Chrono_Utils; use Chrono_Utils;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_Io; use Pajek_Io;
with Contingency_Tables; use Contingency_Tables;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Properties_D; use Graphs_Double_Properties_D;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Modularity_Optimization.Combined; use Modularity_Optimization.Combined;


procedure Mesoscales_Detection is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2015 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Mesoscales search in complex networks by optimization of      ==");
    Put_Line("== modularity using common self-loops                            ==");
    Put_Line("== Implements several algorithms for modularity optimization:    ==");
    Put_Line("==   - exhaustive search (h)                                     ==");
    Put_Line("==   - tabu search (t)                                           ==");
    Put_Line("==   - extremal optimization (e)                                 ==");
    Put_Line("==   - spectral optimization (s)                                 ==");
    Put_Line("==   - fast algorithm (f)                                        ==");
    Put_Line("==   - fine-tuning by reposition (r) or bootstrapping (b)        ==");
    Put_Line("== See README.txt                                                ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Default_Num_Steps            : constant Natural := 100;
  Default_Max_Delta_Loop_Ratio : constant Double  := 1.0;
  Default_Min_Self_Loop        : constant Double  := -1.0;
  Default_Max_Self_Loop        : constant Double  := 1.0;
  Num_Bisection_Steps  : constant Natural := 10;
  Loop_Epsilon         : constant Double  := 1.0E-4;
  Degeneration_Epsilon : constant Double  := 1.0E-6;
  Out_Lols_Sufix  : constant String := "-lols.txt";
  Out_Table_Sufix : constant String := "-table.txt";

  Fn_Net: Ustring;
  Mod_Type: Weighted_Modularity_Type;
  Heuristics: Ustring;
  Num_Repetitions: Positive;
  Num_Steps: Natural;
  Max_Delta_Loop_Ratio: Double;
  Min_Self_Loop: Double;
  Max_Self_Loop: Double;
  Auto_Self_Loop_Limits: Boolean;

  Gr: Graph;
  Q_Worst: constant Modularity_Rec := (Double'First, 0.0, Double'First);
  Q_Best: Modularity_Rec := Q_Worst;
  Lol_Best: List_Of_Lists;
  Degeneration: Positive;

  Chrono: Chronometer;
  Fn_Base, Fn_Path: Ustring;
  Fn_Out_Lols: Ustring;
  Fn_Out_Table: Ustring;
  F_Out: File_Type;
  Q_Best_Prev: Modularity_Rec;
  Lol_Best_Prev: List_Of_Lists;
  Two_W, Two_W_Pos, Two_W_Neg: Double;
  W_Out, W_In, W_Pos_Out, W_Neg_Out, W_Pos_In, W_Neg_In: PDoubles;
  Max_Root, Max_Root_Pos, Max_Root_Neg, Min_Root_Pos, Min_Root_Neg: Double;
  Loop_Min, Loop_Max, Delta_Loop_Ratio, The_Loop, The_Loop_Prev, Loop_Left, Loop_Right: Double;
  Wh, Factor, Tau, B, C, X: Double;
  N, Num_Comms, Num_Count, Cnt: Natural;
  Vi, Vj: Vertex;
  E: Edge;
  Ct: Contingency_Table;
  Exhaustive_Search, Different, Prev_Together: Boolean;

begin
  Put_Info;

  -- Initializations
  Start(Chrono, 0.0);

  Num_Steps             := Default_Num_Steps;
  Max_Delta_Loop_Ratio  := Default_Max_Delta_Loop_Ratio;
  Min_Self_Loop         := Default_Min_Self_Loop;
  Max_Self_Loop         := Default_Max_Self_Loop;
  Auto_Self_Loop_Limits := True;

  if Argument_Count = 4 then
    Fn_Net          := S2U(Argument(1));
    Mod_Type        := To_Modularity_Type(Argument(2));
    Heuristics      := S2U(Argument(3));
    Num_Repetitions := S2I(Argument(4));
  elsif Argument_Count = 6 then
    Fn_Net          := S2U(Argument(1));
    Mod_Type        := To_Modularity_Type(Argument(2));
    Heuristics      := S2U(Argument(3));
    Num_Repetitions := S2I(Argument(4));
    if Is_Integer(Argument(5)) then
      Num_Steps             := S2I(Argument(5));
      Max_Delta_Loop_Ratio  := S2D(Argument(6));
    else
      Min_Self_Loop         := S2D(Argument(5));
      Max_Self_Loop         := S2D(Argument(6));
      Auto_Self_Loop_Limits := False;
    end if;
  elsif Argument_Count = 8 then
    Fn_Net          := S2U(Argument(1));
    Mod_Type        := To_Modularity_Type(Argument(2));
    Heuristics      := S2U(Argument(3));
    Num_Repetitions := S2I(Argument(4));
    Num_Steps             := S2I(Argument(5));
    Max_Delta_Loop_Ratio  := S2D(Argument(6));
    Min_Self_Loop         := S2D(Argument(7));
    Max_Self_Loop         := S2D(Argument(8));
    Auto_Self_Loop_Limits := False;
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  weighted_modularity_type  heuristics  repetitions  [ num_steps  max_delta_loop_ratio ]  [ min_self_loop  max_self_loop ]");
    New_Line;
    Put_Line("   Network Name              :  Name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   Weighted Modularity Types :  WN | WS | WUN | WLA | WULA | WLUN | WNN | WLR");
    Put_Line("                                  also lowercase symbols");
    Put_Line("                                  also case-insensitive full names (Weighted_Newman, ...)");
    Put_Line("                                  WN   = Weighted_Newman");
    Put_Line("                                  WS   = Weighted_Signed");
    Put_Line("                                  WUN  = Weighted_Uniform_Nullcase");
    Put_Line("                                  WLA  = Weighted_Local_Average");
    Put_Line("                                  WULA = Weighted_Uniform_Local_Average");
    Put_Line("                                  WLUN = Weighted_Links_Unweighted_Nullcase");
    Put_Line("                                  WNN  = Weighted_No_Nullcase");
    Put_Line("                                  WLR  = Weighted_Link_Rank");
    New_Line;
    Put_Line("   Heuristics String         :  [htsefrb]+");
    Put_Line("                                  also uppercase symbols");
    Put_Line("                                  also single case-insensitive full names (Exhaustive, ...)");
    Put_Line("                                  h = Exhaustive");
    Put_Line("                                  t = Tabu");
    Put_Line("                                  s = Spectral");
    Put_Line("                                  e = Extremal");
    Put_Line("                                  f = Fast");
    Put_Line("                                  r = Reposition");
    Put_Line("                                  b = Bootstrapping");
    New_Line;
    Put_Line("   Repetitions               :  positive integer");
    Put_Line("                                  does not apply to [hfr] algorithms");
    New_Line;
    Put_Line("   Number of Steps           :  default => " & I2S(Default_Num_Steps));
    New_Line;
    Put_Line("   Max Delta Loop Ratio      :  default => " & D2S(Default_Max_Delta_Loop_Ratio, Aft => 4, Exp => 0));
    Put_Line("                                  ratio between the last and the first increments of the self-loop");
    Put_Line("                                  use 1 for a linear scale of the self-loop");
    New_Line;
    Put_Line("   Min Self-loop             :  default => " & D2S(Default_Min_Self_Loop, Aft => 4, Exp => 0));
    Put_Line("                                  for WN and WS the default is calculated from the network");
    New_Line;
    Put_Line("   Max Self-loop             :  default => " & D2S(Default_Max_Self_Loop, Aft => 4, Exp => 0));
    Put_Line("                                  for WN and WS the default is calculated from the network");
    return;
  end if;

  Fn_Base      := S2U(File_Base_Name(U2S(Fn_Net)));
  Fn_Path      := S2U(File_Path(U2S(Fn_Net)));
  Fn_Out_Lols  := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Lols_Sufix));
  Fn_Out_Table := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Table_Sufix));
  Put_Line(U2S(Fn_Base) & ":");

  Delete_File(U2S(Fn_Out_Lols));
  Delete_File(U2S(Fn_Out_Table));

  Exhaustive_Search := False;
  begin
    if To_Heuristic_Type(U2S(Heuristics)) = Exhaustive then
      Exhaustive_Search := True;
    end if;
  exception
    when Unknown_Heuristic_Error => null;
  end;

  Get_Graph(U2S(Fn_Net), Gr);
  N := Number_Of_Vertices(Gr);

  Delta_Loop_Ratio := Max_Delta_Loop_Ratio ** (1.0 / Double(Num_Steps - 1));

  -- Prepare max and min self-loops
  if Auto_Self_Loop_Limits then
    case Mod_Type is
      when Weighted_Newman =>
        Two_W := Total_Strength(Gr);
        W_Out := Strength_From(Gr);
        W_In  := Strength_To(Gr);

        Min_Self_Loop := -Total_Strength(Gr) / Double(N);
        Max_Root := Min_Self_Loop;
        for I in 1..N loop
          Vi := Get_Vertex(Gr, I);
          for J in 1..N loop
            if I < J or (Is_Directed(Gr) and I /= J) then
              Vj := Get_Vertex(Gr, J);
              E := Get_Edge_Or_No_Edge(Vi, Vj);
              Wh := 0.0;
              if E /= No_Edge then
                Wh := Value(E);
              end if;
              B := W_Out(I) + W_In(J) - Double(N) * Wh;
              C := W_Out(I) * W_In(J) - Two_W * Wh;
              if Is_Directed(Gr) then
                E := Get_Edge_Or_No_Edge(Vj, Vi);
                Wh := 0.0;
                if E /= No_Edge then
                  Wh := Value(E);
                end if;
                B := (B + W_Out(J) + W_In(I) - Double(N) * Wh) / 2.0;
                C := (C + W_Out(J) * W_In(I) - Two_W * Wh) / 2.0;
              end if;
              X := B * B - 4.0 * C;
              if X >= 0.0 then
                X := (- B + Sqrt(X)) / 2.0;
              end if;
              if X > Max_Root then
                Max_Root := X;
              end if;
            end if;
          end loop;
        end loop;
        Max_Self_Loop := Max_Root;

        Free(W_Out);
        Free(W_In);

      when Weighted_Signed =>
        Two_W_Pos   := Total_Strength(Gr, Positive_Links);
        Two_W_Neg   := Total_Strength(Gr, Negative_Links);
        W_Pos_Out   := Strength(Gr, From_Links, Positive_Links);
        W_Neg_Out   := Strength(Gr, From_Links, Negative_Links);
        W_Pos_In    := Strength(Gr, To_Links, Positive_Links);
        W_Neg_In    := Strength(Gr, To_Links, Negative_Links);

        Max_Root_Pos := Double'First;
        Max_Root_Neg := Double'First;
        Min_Root_Pos := Double'Last;
        Min_Root_Neg := Double'Last;
        for I in 1..N loop
          Vi := Get_Vertex(Gr, I);
          for J in 1..N loop
            if I < J or (Is_Directed(Gr) and I /= J) then
              Vj := Get_Vertex(Gr, J);
              E := Get_Edge_Or_No_Edge(Vi, Vj);
              Wh := 0.0;
              if E /= No_Edge then
                Wh := Value(E);
              end if;
              -- Positive self-loops
              if Two_W_Neg > 0.0 then
                B := W_Pos_Out(I) + W_Pos_In(J) - Double(N) * (Wh + W_Neg_Out(I) * W_Neg_In(J) / Two_W_Neg);
                C := W_Pos_Out(I) * W_Pos_In(J) - Two_W_Pos * (Wh + W_Neg_Out(I) * W_Neg_In(J) / Two_W_Neg);
              else
                B := W_Pos_Out(I) + W_Pos_In(J) - Double(N) * Wh;
                C := W_Pos_Out(I) * W_Pos_In(J) - Two_W_Pos * Wh;
              end if;
              if Is_Directed(Gr) then
                E := Get_Edge_Or_No_Edge(Vj, Vi);
                Wh := 0.0;
                if E /= No_Edge then
                  Wh := Value(E);
                end if;
                if Two_W_Neg > 0.0 then
                  B := (B + W_Pos_Out(J) + W_Pos_In(I) - Double(N) * (Wh + W_Neg_Out(J) * W_Neg_In(I) / Two_W_Neg)) / 2.0;
                  C := (C + W_Pos_Out(J) * W_Pos_In(I) - Two_W_Pos * (Wh + W_Neg_Out(J) * W_Neg_In(I) / Two_W_Neg)) / 2.0;
                else
                  B := (B + W_Pos_Out(I) + W_Pos_In(J) - Double(N) * Wh) / 2.0;
                  C := (C + W_Pos_Out(I) * W_Pos_In(J) - Two_W_Pos * Wh) / 2.0;
                end if;
              end if;
              X := B * B - 4.0 * C;
              if X >= 0.0 then
                X := (- B + Sqrt(X)) / 2.0;
              end if;
              if X > Max_Root_Pos then
                Max_Root_Pos := X;
              end if;
              if X < Min_Root_Pos then
                Min_Root_Pos := X;
              end if;
              -- Negative self-loops
              if Two_W_Pos > 0.0 then
                B := W_Neg_Out(I) + W_Neg_In(J) + Double(N) * (Wh - W_Pos_Out(I) * W_Pos_In(J) / Two_W_Pos);
                C := W_Neg_Out(I) * W_Neg_In(J) + Two_W_Neg * (Wh - W_Pos_Out(I) * W_Pos_In(J) / Two_W_Pos);
              else
                B := W_Neg_Out(I) + W_Neg_In(J) + Double(N) * Wh;
                C := W_Neg_Out(I) * W_Neg_In(J) + Two_W_Neg * Wh;
              end if;
              if Is_Directed(Gr) then
                Wh := 0.0;
                if Edge_Exists(Vj, Vi) then
                  Wh := Value(Get_Edge(Vj, Vi));
                end if;
                if Two_W_Neg > 0.0 then
                  B := (B + W_Neg_Out(J) + W_Neg_In(I) + Double(N) * (Wh - W_Pos_Out(J) * W_Pos_In(I) / Two_W_Pos)) / 2.0;
                  C := (C + W_Neg_Out(J) * W_Neg_In(I) + Two_W_Neg * (Wh - W_Pos_Out(J) * W_Pos_In(I) / Two_W_Pos)) / 2.0;
                else
                  B := (B + W_Neg_Out(J) + W_Neg_In(I) + Double(N) * Wh) / 2.0;
                  C := (C + W_Neg_Out(J) * W_Neg_In(I) + Two_W_Neg * Wh) / 2.0;
                end if;
              end if;
              X := B * B - 4.0 * C;
              if X >= 0.0 then
                X := (- B + Sqrt(X)) / 2.0;
              end if;
              if X > Max_Root_Neg then
                Max_Root_Neg := X;
              end if;
              if X < Min_Root_Neg then
                Min_Root_Neg := X;
              end if;
            end if;
          end loop;
        end loop;
        if Max_Root_Pos > 0.0 then
          Max_Self_Loop := Max_Root_Pos;
        else
          Max_Self_Loop := -Min_Root_Neg;
        end if;
        if Max_Root_Neg > 0.0 then
          Min_Self_Loop := -Max_Root_Neg;
        else
          Min_Self_Loop := Min_Root_Pos;
        end if;

        Free(W_Pos_Out);
        Free(W_Neg_Out);
        Free(W_Pos_In);
        Free(W_Neg_In);

        -- Improve Min_Self_Loop
        if Min_Self_Loop < 0.0 then
          Put_Line("Self-loop_Low = " & D2S(Min_Self_Loop, Aft => 6, Exp => 0));
          Loop_Left  := Min_Self_Loop;
          Loop_Right := 0.0;
          The_Loop   := Loop_Left;
          Num_Comms := 1;
          -- Bisection to improve Min_Self_Loop
          for Count in 1..Num_Bisection_Steps loop
            if Num_Comms = 1 then
              Loop_Left  := The_Loop;
              The_Loop   := (The_Loop + Loop_Right) / 2.0;
            else
              Loop_Right := The_Loop;
              The_Loop   := (Loop_Left + The_Loop) / 2.0;
            end if;
            Optimization_Combined_Heuristic(Gr, "r", 1, Lol_Best, Q_Best, Degeneration, Mod_Type, None, Null_Ustring, The_Loop);
            Num_Comms := Number_Of_Lists(Lol_Best);
            Free(Lol_Best_Prev);
            Free(Lol_Best);
            if Num_Comms = 1 then
              Optimization_Combined_Heuristic(Gr, "e", 1, Lol_Best, Q_Best, Degeneration, Mod_Type, None, Null_Ustring, The_Loop);
              Num_Comms := Number_Of_Lists(Lol_Best);
              Free(Lol_Best);
            end if;
            Put(".");
--            Put_Line(
--              "bisect:"
--              & "  " & D2S(Loop_Left, Aft => 6, Exp => 0)
--              & "  " & D2S(The_Loop, Aft => 6, Exp => 0)
--              & "  " & D2S(Loop_Right, Aft => 6, Exp => 0)
--              & "  " & I2S(Num_Comms)
--              & "  " & D2S(Q_Best.Total, Aft => 6)
--            );
          end loop;
          if Num_Comms > 1 then
            The_Loop := Loop_Left;
            Loop_Left := Loop_Left - (Loop_Right - Loop_Left);
          end if;
          -- Ensure Min_Self_Loop is a lower bound
          loop
            Optimization_Combined_Heuristic(Gr, U2S(Heuristics), Num_Repetitions, Lol_Best, Q_Best, Degeneration, Mod_Type, None, Null_Ustring, The_Loop);
            Num_Comms := Number_Of_Lists(Lol_Best);
            Free(Lol_Best);
            Put(".");
--            Put_Line(
--              "ensure:"
--              & "  " & D2S(Loop_Left, Aft => 6, Exp => 0)
--              & "  " & D2S(The_Loop, Aft => 6, Exp => 0)
--              & "  " & D2S(Loop_Right, Aft => 6, Exp => 0)
--              & "  " & I2S(Num_Comms)
--              & "  " & D2S(Q_Best.Total, Aft => 6)
--            );
            exit when Num_Comms = 1;
            The_Loop := Loop_Left;
            Loop_Left := Loop_Left - (Loop_Right - Loop_Left);
          end loop;
          Min_Self_Loop := The_Loop;
          New_Line;
        end if;

      when others =>
        Min_Self_Loop := Default_Min_Self_Loop;
        Max_Self_Loop := Default_Max_Self_Loop;
    end case;
  end if;

  Loop_Min := Min_Self_Loop;
  Loop_Max := Max_Self_Loop;

  -- Print min and max self-loops and prepare other parameters
  Put_Line("Self-loop_Min = " & D2S(Loop_Min, Aft => 6, Exp => 0));
  Put_Line("Self-loop_Max = " & D2S(Loop_Max, Aft => 6, Exp => 0));

  if Auto_Self_Loop_Limits then
    case Mod_Type is
      when Weighted_Newman =>
        Loop_Min := Loop_Min + Loop_Epsilon;
        Loop_Max := Loop_Max + Loop_Epsilon;
      when Weighted_Signed =>
        Loop_Min := Loop_Min - Loop_Epsilon;
        Loop_Max := Loop_Max + Loop_Epsilon;
      when others =>
        null;
    end case;
  end if;
  if Loop_Min < 0.0 and Loop_Max >= 0.0 then
    Num_Count := Num_Steps + 1;
  else
    Num_Count := Num_Steps;
  end if;

  -- Search best partition for running parameter
  The_Loop_Prev := Loop_Min;
  Cnt := 0;
  for Count in 0..Num_Count loop
    Put(".");
    -- Calculate self-loops
    if Cnt = 0 then
      The_Loop := Loop_Min;
    elsif Cnt = Num_Steps then
      The_Loop := Loop_Max;
    else
      if Max_Delta_Loop_Ratio = 1.0 then
        Factor := Double(Cnt) / Double(Num_Steps);
      else
        Factor := (Delta_Loop_Ratio ** Double(Cnt) - 1.0) / (Delta_Loop_Ratio ** Double(Num_Steps) - 1.0);
      end if;
      The_Loop := Loop_Min + Factor * (Loop_Max - Loop_Min);
    end if;
    if The_Loop_Prev < 0.0 and The_Loop >= 0.0 then
      The_Loop := 0.0;
    else
      Cnt := Cnt + 1;
    end if;
    The_Loop_Prev := The_Loop;
    Tau := The_Loop - Loop_Min;
    -- Search best partition
    Prev_Together := False;
    Free(Lol_Best);
    if Count = 0 then
      Initialize(Lol_Best_Prev, N, Unassigned_Initialization);
    elsif Number_Of_Lists(Lol_Best_Prev) <= 1 then
      Prev_Together := True;
      Free(Lol_Best_Prev);
      Initialize(Lol_Best_Prev, N, Unassigned_Initialization);
    elsif Count = Num_Count then
      Free(Lol_Best_Prev);
      Initialize(Lol_Best_Prev, N, Isolated_Initialization);
    end if;
    Optimization_Combined_Heuristic(Gr, U2S(Heuristics), Num_Repetitions, Lol_Best_Prev, Lol_Best, Q_Best,
                                    Degeneration, Mod_Type, None, Null_Ustring, The_Loop);
    -- Check if partition is different from previous one
    Different := not (Prev_Together and Number_Of_Lists(Lol_Best) = 1);
    if Different then
      Initialize(Ct, Lol_Best, Lol_Best_Prev);
      Different := Number_Of_Disagreements(Ct) > 0;
      Free(Ct);
    end if;
    if Exhaustive_Search and Different and (Count > 0 and Count < Num_Count) then
      Q_Best_Prev := Modularity(Gr, Lol_Best_Prev, Mod_Type, The_Loop);
      if Q_Best.Total - Q_Best_Prev.Total < Degeneration_Epsilon then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Best_Prev);
        Different := False;
      end if;
    end if;
    -- Print values to table
    Open_Or_Create(F_Out, U2S(Fn_Out_Table));
    Put(F_Out, The_Loop, Fore => 0, Aft => 6, Exp => 0); Put(F_Out, HTab);
    Put(F_Out, Tau, Fore => 0, Aft => 6, Exp => 0); Put(F_Out, HTab);
    Put(F_Out, Q_Best.Total, Fore => 0, Aft => 6, Exp => 0); Put(F_Out, HTab);
    Put(F_Out, Number_Of_Lists(Lol_Best), Width => 0);
    Close(F_Out);
    -- Print partition if different from previous one
    if (Count = 0 or Count = Num_Count) or Different then
      Open_Or_Create(F_Out, U2S(Fn_Out_Lols));
      Put_Line(F_Out, "--------");
      Put(F_Out, "Self-loop(" & I2S(Count) & ") = ");
      Put(F_Out, The_Loop, Fore => 0, Aft => 4, Exp => 0);
      if Exhaustive_Search and Degeneration > 1 then
        Put(F_Out, "    (Degeneration found = "); Put(F_Out, Degeneration, Width => 0); Put(F_Out, ")");
      end if;
      New_Line(F_Out);
      Put(F_Out, "Q_Best = "); Put(F_Out, Q_Best.Total, Fore => 0, Aft => 6, Exp => 0); New_Line(F_Out);
      Put_Line(F_Out, "---");
      Put(F_Out, Lol_Best);
      Close(F_Out);
      Free(Lol_Best_Prev);
      Lol_Best_Prev := Clone(Lol_Best);
    end if;
--    if Number_Of_Lists(Lol_Best) = N then
--      exit;
--    end if;
  end loop;

  Free(Lol_Best);
  Free(Lol_Best_Prev);
  Free(Gr);

  Stop(Chrono);
  New_Line; Put_Elapsed(Chrono, 2); New_Line;
end Mesoscales_Detection;
