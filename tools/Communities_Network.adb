-- Radalib, Copyright (c) 2018 by
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


-- @filename Communities_Network.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 07/12/2008
-- @revision 26/02/2016
-- @brief Find the Communities Network of a given Network

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Integer_Algorithms; use Graphs_Integer_Algorithms;
with Graphs_Float; use Graphs_Float;
with Graphs_Float_Algorithms; use Graphs_Float_Algorithms;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Communities_Network is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2018 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Find the Network of Communities of a given Network            ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Default_Num_Skip_Lines: constant := 0;
  Num_Skip_Lines: Natural := Default_Num_Skip_Lines;
  Aft: Field := Default_Float_Aft;
  Net_Name, Lol_Name, Comms_Net_Name, Wh_Id_Name: Ustring;
  Gr_I, Gr_I_Red: Graphs_Integer.Graph;
  Gr_F, Gr_F_Red: Graphs_Float.Graph;
  Gr_D, Gr_D_Red: Graphs_Double.Graph;
  Wh_Id: Numeric_Type_Id;
  Lol: List_Of_Lists;

begin
  Put_Info;

  if Argument_Count = 4 then
    Net_Name := S2U(Argument(1));
    Lol_Name := S2U(Argument(2));
    Num_Skip_Lines := Default_Num_Skip_Lines;
    Comms_Net_Name := S2U(Argument(3));
    Wh_Id_Name := S2U(Argument(4));
  elsif Argument_Count = 5 then
    Net_Name := S2U(Argument(1));
    Lol_Name := S2U(Argument(2));
    if Is_Integer(Argument(3)) then
      Num_Skip_Lines := S2I(Argument(3));
      Comms_Net_Name := S2U(Argument(4));
      Wh_Id_Name := S2U(Argument(5));
    else
      Num_Skip_Lines := Default_Num_Skip_Lines;
      Comms_Net_Name := S2U(Argument(3));
      Wh_Id_Name := S2U(Argument(4));
      Aft := S2I(Argument(5));
    end if;
  elsif Argument_Count = 6 then
    Net_Name := S2U(Argument(1));
    Lol_Name := S2U(Argument(2));
    Num_Skip_Lines := S2I(Argument(3));
    Comms_Net_Name := S2U(Argument(4));
    Wh_Id_Name := S2U(Argument(5));
    Aft := S2I(Argument(6));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  clu_or_lol_name  [ number_of_lines_to_skip ]  comms_net_name  weights_type  [ decimal_digits ]");
    New_Line;
    Put_Line("   net_name                :  name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   clu_or_lol_name         :  name of the file with the partition in Pajek or Lol format");
    New_Line;
    Put_Line("   number_of_lines_to_skip :  non-negative integer");
    Put_Line("                                default => " & I2S(Default_Num_Skip_Lines));
    Put_Line("                                ignored for partitions in Pajek format");
    New_Line;
    Put_Line("   comms_net_name          :  name of the output network of communities file in Pajek format");
    New_Line;
    Put_Line("   weights_type            :  I | F | D");
    Put_Line("                                also lowercase symbols");
    Put_Line("                                also case-insensitive full names");
    Put_Line("                                I = Integer = Int");
    Put_Line("                                F = Float");
    Put_Line("                                D = Double");
    New_Line;
    Put_Line("   decimal_digits          :  number of decimal digits for Float or Double output weights");
    Put_Line("                                ignored for Integer weights");
    Put_Line("                                default => " & I2S(Aft));
    return;
  end if;

  Put_Line(U2S(Net_Name) & "  +  " & U2S(Lol_Name) & "  ->  " & U2S(Comms_Net_Name));

  Wh_Id := To_Type_Id(U2S(Wh_Id_Name));

  if Tail(Lol_Name, 4) = ".clu" then
    Get_Partition(U2S(Lol_Name), Lol);
  else
    Get(U2S(Lol_Name), Lol, Num_Skip_Lines);
  end if;

  case Wh_Id is
    when Integer_Id..Longint_Id =>
      Get_Graph(U2S(Net_Name), Gr_I);
      Renormalize_Graph(Gr_I, Lol, Gr_I_Red);
      Put_Graph(U2S(Comms_Net_Name), Gr_I_Red);
      Free(Gr_I);
      Free(Gr_I_Red);
    when Float_Id =>
      Get_Graph(U2S(Net_Name), Gr_F);
      Renormalize_Graph(Gr_F, Lol, Gr_F_Red);
      Put_Graph(U2S(Comms_Net_Name), Gr_F_Red, Aft);
      Free(Gr_F);
      Free(Gr_F_Red);
    when Double_Id =>
      Get_Graph(U2S(Net_Name), Gr_D);
      Renormalize_Graph(Gr_D, Lol, Gr_D_Red);
      Put_Graph(U2S(Comms_Net_Name), Gr_D_Red, Aft);
      Free(Gr_D);
      Free(Gr_D_Red);
  end case;

  Free(Lol);
end Communities_Network;
