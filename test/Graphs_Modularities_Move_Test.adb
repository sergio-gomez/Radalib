-- Radalib, Copyright (c) 2019 by
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


-- @filename Graphs_Modularities_Move_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/12/2017
-- @revision 28/12/2017
-- @brief Test of Graphs Modularities Updates

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Utils; use Utils;
with Utils.IO_Integer; use Utils.IO_Integer;
with Utils.IO_Double; use Utils.IO_Double;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities; use Graphs_Double_Modularities;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_IO; use Pajek_IO;

procedure Graphs_Modularities_Move_Test is

  procedure Put_Modularity(Mr: in Modularity_Rec) is
  begin
    Put(Mr.Total, Aft => 8, Exp => 0);
    Put(" = "); Put(Mr.Reward, Aft => 8, Exp => 0);
    if Mr.Penalty >= 0.0 then
      Put(" - "); Put(Mr.Penalty, Aft => 8, Exp => 0);
    else
      Put(" + "); Put(abs Mr.Penalty, Aft => 8, Exp => 0);
    end if;
  end Put_Modularity;

  Default_Num_Skip_Lines: constant := 0;

  Resistance_Str: Ustring := Null_Ustring;
  Penalty_Str   : Ustring := Null_Ustring;
  Mt: Modularity_Type;
  Num_Skip_Lines: Natural := Default_Num_Skip_Lines;
  Gr: Graph;
  Fn_Net: Ustring;
  Fn_Clu: Ustring;
  Resistance: Double := No_Resistance;
  Penalty_Coeff: Double := 1.0;
  Lol: List_Of_Lists;

  Mi: Modularity_Info;
  Mr_Ini, Mr, Mre_Ini1, Mre_Ini2, Mre1, Mre2: Modularity_Rec;
  E1, E2, E34: Finite_Disjoint_Lists.Element;
  L: List;
  I1, I2: Positive;


begin
  if Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Fn_Clu := S2U(Argument(2));
    Resistance_Str := S2U(Argument(3));
    Resistance := S2D(Argument(3));
  elsif Argument_Count = 4 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    if Is_Real(Argument(3)) then
      Resistance_Str := S2U(Argument(3));
      Resistance := S2D(Argument(3));
      Mt := To_Modularity_Type(Argument(4));
    else
      Mt := To_Modularity_Type(Argument(3));
      Num_Skip_Lines := S2I(Argument(4));
    end if;
  elsif Argument_Count = 5 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Resistance_Str := S2U(Argument(3));
    Resistance := S2D(Argument(3));
    if Is_Real(Argument(4)) then
      Penalty_Str := S2U(Argument(4));
      Penalty_Coeff := S2D(Argument(4));
      Mt := To_Modularity_Type(Argument(5));
    else
      Mt := To_Modularity_Type(Argument(4));
      Num_Skip_Lines := S2I(Argument(5));
    end if;
  elsif Argument_Count = 6 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Resistance_Str := S2U(Argument(3));
    Resistance := S2D(Argument(3));
    Penalty_Str := S2U(Argument(4));
    Penalty_Coeff := S2D(Argument(4));
    Mt := To_Modularity_Type(Argument(5));
    Num_Skip_Lines := S2I(Argument(6));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  clu_or_lol_name  [ resistance  [ penalty_coeff ] ]  modularity_type  [ number_of_lines_to_skip ]");
    New_Line;
    Put_Line("   net_name                :  name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   clu_or_lol_name         :  name of the file with the partition in Pajek or Lol format");
    New_Line;
    Put_Line("   resistance              :  resistance of nodes to join communities in the form of a common self-loop");
    Put_Line("                                positive or negative real number");
    Put_Line("                                0 | 0.0 | default => no resistance, i.e. do not add self-loops");
    New_Line;
    Put_Line("   penalty_coeff           :  relative importance of null-case term");
    Put_Line("                                non-negative real number");
    Put_Line("                                default => 1.0");
    New_Line;
    Put_Line("   modularity_type         :  UN | UUN | WN | WS | WUN | WLA | WULA | WLUN | WNN | WLR");
    Put_Line("                                also lowercase symbols");
    Put_Line("                                also case-insensitive full names (Unweighted_Newman, ...)");
    Put_Line("                                UN   = Unweighted_Newman");
    Put_Line("                                UUN  = Unweighted_Uniform_Nullcase");
    Put_Line("                                WN   = Weighted_Newman");
    Put_Line("                                WS   = Weighted_Signed");
    Put_Line("                                WUN  = Weighted_Uniform_Nullcase");
    Put_Line("                                WLA  = Weighted_Local_Average");
    Put_Line("                                WULA = Weighted_Uniform_Local_Average");
    Put_Line("                                WLUN = Weighted_Links_Unweighted_Nullcase");
    Put_Line("                                WNN  = Weighted_No_Nullcase");
    Put_Line("                                WLR  = Weighted_Link_Rank");
    New_Line;
    Put_Line("   number_of_lines_to_skip :  number of lines to skip at the beginning of the Lol files");
    Put_Line("                                ignored for partitions in Pajek format");
    Put_Line("                                non-negative integer");
    Put_Line("                                default => " & I2S(Default_Num_Skip_Lines));
    return;
  end if;

  Get_Graph(U2S(Fn_Net), Gr);

  if Tail(Fn_Clu, 4) = ".clu" then
    Get_Partition(U2S(Fn_Clu), Lol);
  else
    Get(U2S(Fn_Clu), Lol, Num_Skip_Lines);
  end if;

  Put(U2S(Fn_Net) & " + " & U2S(Fn_Clu) & "  (" & To_Name(Mt, True));
  if Resistance /= No_Resistance or Penalty_Coeff /= 1.0 then
    Put(", resistance: " & U2S(Resistance_Str));
    if Penalty_Coeff /= 1.0 then
      Put(", penalty coefficient: " & U2S(Penalty_Str));
    end if;
  end if;
  Put_Line("):");

  E1 := Get_Element(Lol, 1);
  E2 := Get_Element(Lol, 2);
  E34 := Get_Element(Lol, 34);

  I1 := Index_Of(E1);
  I2 := Index_Of(E2);

  Initialize(Mi, Gr, Mt, Resistance, Penalty_Coeff);
  Mr_Ini := Modularity(Mi, Lol, Mt);
  Mre_Ini1 := Element_Modularity(Mi, E1);
  Mre_Ini2 := Element_Modularity(Mi, E2);

  L := List_Of(E34);
  Update_Modularity_Move_Element(Mi, E1, L, Mt);
  Mr := Total_Modularity(Mi);
  Put("  Q_Moved  = "); Put_Modularity(Mr); New_Line;
  Mr := Modularity(Mi, Lol, Mt);
  Put("  Q_Check  = "); Put_Modularity(Mr); New_Line;

  L := List_Of(E2);
  Update_Modularity_Move_Element(Mi, E1, L, Mt);
  Mr := Total_Modularity(Mi);
  Put("  Q_Ini  = "); Put_Modularity(Mr_Ini); New_Line;
  Put("  Q_Back = "); Put_Modularity(Mr); New_Line;
  Mre1 := Element_Modularity(Mi, E1);
  Put("    q(" & I2S(I1) & ")_Ini  = "); Put_Modularity(Mre_Ini1); New_Line;
  Put("    q(" & I2S(I1) & ")_Back = "); Put_Modularity(Mre1); New_Line;
  Mre2 := Element_Modularity(Mi, E2);
  Put("    q(" & I2S(I2) & ")_Ini  = "); Put_Modularity(Mre_Ini2); New_Line;
  Put("    q(" & I2S(I2) & ")_Back = "); Put_Modularity(Mre2); New_Line;

  Free(Lol);
  Free(Gr);

exception
  when E: others =>
    Put_Line(Exception_Information(E));
end Graphs_Modularities_Move_Test;
