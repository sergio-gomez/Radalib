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


-- @filename Modularity_Calculation.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/02/2007
-- @revision 19/09/2015
-- @brief Calculate Modularity

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

procedure Modularity_Calculation is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2015 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Calculate the total modularity, decomposed in node            ==");
    Put_Line("== and community contributions                                   ==");
    Put_Line("== See README.txt                                                ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  type Modularity_Details_Type is (Total, Total_Communities, Total_Nodes, Total_Communities_Nodes);

  Unknown_Modularity_Details: exception;

  function To_Modularity_Details(S: in String) return Modularity_Details_Type is
  begin
    if    To_Uppercase(S) = "T"   or To_Lowercase(S) = "total"                   then
      return Total;
    elsif To_Uppercase(S) = "TC"  or To_Lowercase(S) = "total_communities"       then
      return Total_Communities;
    elsif To_Uppercase(S) = "TN"  or To_Lowercase(S) = "total_nodes"             then
      return Total_Nodes;
    elsif To_Uppercase(S) = "TCN" or To_Lowercase(S) = "total_communities_nodes" then
      return Total_Communities_Nodes;
    else
      raise Unknown_Modularity_Details;
    end if;
  end To_Modularity_Details;

  procedure Modularity_Details(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type; R, Pc: in Double; Md: in Modularity_Details_Type) is

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

    Mi: Modularity_Info;
    Mr: Modularity_Rec;
    L: List;
    C: Natural;
    E: Finite_Disjoint_Lists.Element;
  begin
    pragma Warnings(Off, L);
    Initialize(Mi, Gr, Mt, R, Pc);
    Mr := Modularity(Mi, Lol, Mt);
    Put("  Q = "); Put_Modularity(Mr);
    New_Line;
    if Md = Total_Communities or Md = Total_Communities_Nodes then
      Save(Lol);
      Reset(Lol);
      C := 0;
      while Has_Next_List(Lol) loop
        C := C + 1;
        L := Next_List(Lol);
        Mr := Partial_Modularity(Mi, L);
        Put("    Q("); Put(C, Width => 0);
        Put(") = "); Put_Modularity(Mr);
        Put("     ("); Put(Number_Of_Elements(L), Width => 0); Put(" nodes)");
        New_Line;
        if Md = Total_Communities_Nodes then
          Save(L);
          Reset(L);
          while Has_Next_Element(L) loop
            E := Next_Element(L);
            Mr := Element_Modularity(Mi, E);
            Put("      q("); Put(Index_Of(E), Width => 0);
            Put(") = "); Put_Modularity(Mr);
            New_Line;
          end loop;
          Restore(L);
        end if;
      end loop;
      Restore(Lol);
    elsif Md = Total_Nodes then
      for I in 1..Number_Of_Vertices(Gr) loop
        Mr := Element_Modularity(Mi, Get_Element(Lol, I));
        Put("    q("); Put(I, Width => 0);
        Put(") = "); Put_Modularity(Mr);
        New_Line;
      end loop;
    end if;
    New_Line;
    Free(Mi);
  end Modularity_Details;


  Default_Modularity_Details: constant Modularity_Details_Type := Total_Communities_Nodes;
  Default_Num_Skip_Lines: constant := 0;

  Resistance_Str: Ustring := Null_Ustring;
  Penalty_Str   : Ustring := Null_Ustring;
  Mt: Modularity_Type;
  Md: Modularity_Details_Type := Default_Modularity_Details;
  Num_Skip_Lines: Natural     := Default_Num_Skip_Lines;
  Gr: Graph;
  Fn_Net: Ustring;
  Fn_Clu: Ustring;
  Resistance: Double := No_Resistance;
  Penalty_Coeff: Double := 1.0;
  Lol: List_Of_Lists;


begin
  Put_Info;

  if Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Mt := To_Modularity_Type(Argument(3));
  elsif Argument_Count = 4 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    if Is_Real(Argument(3)) then
      Resistance_Str := S2U(Argument(3));
      Resistance := S2D(Argument(3));
      Mt := To_Modularity_Type(Argument(4));
    elsif Is_Integer(Argument(4)) then
      Mt := To_Modularity_Type(Argument(3));
      Num_Skip_Lines := S2I(Argument(4));
    else
      Mt := To_Modularity_Type(Argument(3));
      Md := To_Modularity_Details(Argument(4));
    end if;
  elsif Argument_Count = 5 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    if Is_Real(Argument(3)) then
      Resistance_Str := S2U(Argument(3));
      Resistance := S2D(Argument(3));
      if Is_Real(Argument(4)) then
        Penalty_Str := S2U(Argument(4));
        Penalty_Coeff := S2D(Argument(4));
        Mt := To_Modularity_Type(Argument(5));
      elsif Is_Integer(Argument(5)) then
        Mt := To_Modularity_Type(Argument(4));
        Num_Skip_Lines := S2I(Argument(5));
      else
        Mt := To_Modularity_Type(Argument(4));
        Md := To_Modularity_Details(Argument(5));
      end if;
    else
      Mt := To_Modularity_Type(Argument(3));
      Md := To_Modularity_Details(Argument(4));
      Num_Skip_Lines := S2I(Argument(5));
    end if;
  elsif Argument_Count = 6 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Resistance_Str := S2U(Argument(3));
    Resistance := S2D(Argument(3));
    if Is_Real(Argument(4)) then
      Penalty_Str := S2U(Argument(4));
      Penalty_Coeff := S2D(Argument(4));
      Mt := To_Modularity_Type(Argument(5));
      if Is_Integer(Argument(6)) then
        Num_Skip_Lines := S2I(Argument(6));
      else
        Md := To_Modularity_Details(Argument(6));
      end if;
    else
      Mt := To_Modularity_Type(Argument(4));
      Md := To_Modularity_Details(Argument(5));
      Num_Skip_Lines := S2I(Argument(6));
    end if;
  elsif Argument_Count = 7 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Resistance_Str := S2U(Argument(3));
    Resistance := S2D(Argument(3));
    Penalty_Str := S2U(Argument(4));
    Penalty_Coeff := S2D(Argument(4));
    Mt := To_Modularity_Type(Argument(5));
    Md := To_Modularity_Details(Argument(6));
    Num_Skip_Lines := S2I(Argument(7));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  clu_or_lol_name  [ resistance  [ penalty_coeff ] ]  modularity_type  [ modularity_details ]  [ number_of_lines_to_skip ]");
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
    Put_Line("   modularity_details      :  T | TC | TN | TCN");
    Put_Line("                                also lowercase symbols");
    Put_Line("                                also case-insensitive full names (Total, Total_Communities, ...)");
    Put_Line("                                T   = Total");
    Put_Line("                                TC  = Total_Communities");
    Put_Line("                                TN  = Total_Nodes");
    Put_Line("                                TCN = Total_Communities_Nodes");
    Put_Line("                                default => " & Capitalize(Modularity_Details_Type'Image(Default_Modularity_Details)));
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

  Modularity_Details(Gr, Lol, Mt, Resistance, Penalty_Coeff, Md);

  Free(Lol);
  Free(Gr);

exception
  when E: others =>
    Put_Line(Exception_Information(E));
end Modularity_Calculation;
