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


-- @filename Extract_Subgraphs.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/06/2011
-- @revision 19/09/2015
-- @brief Extract Subgraphs from a Graph

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphs_String; use Graphs_String;
with Graphs_String_Algorithms; use Graphs_String_Algorithms;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Extract_Subgraphs is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2015 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Extract subgraphs from a graph                                ==");
    Put_Line("== See README.txt                                                ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Default_Num_Skip_Lines: constant := 0;
  Net_Sufix: constant String := ".net";
  Sg_Infix : constant String := "-sg";
  Fn_Net: Ustring;
  Fn_Lol: Ustring;
  Fn_Out: Ustring;
  Fn_Sub_Net: Ustring;
  Num_Skip_Lines: Natural := Default_Num_Skip_Lines;
  Gr, Sub_Gr: Graph;
  Lol: List_Of_Lists;
  L: List;
  Num_Sg, Sg, Digs: Natural;

begin
  Put_Info;

  if Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    Fn_Lol := S2U(Argument(2));
    Fn_Out := S2U(Argument(3));
    Num_Skip_Lines := Default_Num_Skip_Lines;
  elsif Argument_Count = 4 then
    Fn_Net := S2U(Argument(1));
    Fn_Lol := S2U(Argument(2));
    Fn_Out := S2U(Argument(3));
    Num_Skip_Lines := S2I(Argument(4));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  clu_or_lol_name  out_name_prefix  [ number_of_lines_to_skip ]");
    New_Line;
    Put_Line("   net_name                :  name of the network file in Pajek format");
    New_Line;
    Put_Line("   clu_or_lol_name         :  name of the file with the lists of nodes in Pajek or Lol format");
    New_Line;
    Put_Line("   out_name_prefix         :  prefix of the name of the output subgraph files");
    New_Line;
    Put_Line("   number_of_lines_to_skip :  number of lines to skip at the beginning of the Lol file");
    Put_Line("                                ignored for partitions in Pajek format");
    Put_Line("                                non-negative integer");
    Put_Line("                                default => " & I2S(Default_Num_Skip_Lines));
    return;
  end if;

  Get_Graph(U2S(Fn_Net), Gr);

  if Tail(Fn_Lol, 4) = ".clu" then
    Get_Partition(U2S(Fn_Lol), Lol);
  else
    Get(U2S(Fn_Lol), Lol, Num_Skip_Lines);
  end if;
  Num_Sg := Number_Of_Lists(Lol);
  if Num_Sg < 99 then
    Digs := 2;
  else
    Digs := 3;
  end if;

  Put(U2S(Fn_Net) & " + " & U2S(Fn_Lol) & " :   " & I2S(Num_Sg) & " subgraphs");

  Sg := 0;
  Save(Lol);
  Reset(Lol);
  while Has_Next_List(Lol) loop
    L := Next_List(Lol);
    Create_Subgraph(Gr, L, Sub_Gr);
    Sg := Sg + 1;
    if Num_Sg = 1 then
      Fn_Sub_Net := Fn_Out & S2U(Sg_Infix & Net_Sufix);
    else
      Fn_Sub_Net := Fn_Out & S2U(Sg_Infix & "-" & Right_Justify(I2S(Sg), Digs, '0') & Net_Sufix);
    end if;
    Put_Graph(U2S(Fn_Sub_Net), Sub_Gr);
    Free(Sub_Gr);
  end loop;
  Restore(Lol);

  Free(Lol);
  Free(Gr);
end Extract_Subgraphs;
