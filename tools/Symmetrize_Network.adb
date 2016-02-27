-- Radalib, Copyright (c) 2016 by
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


-- @filename Symmetrize_Network.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/12/2008
-- @revision 26/02/2016
-- @brief Network Symmetrization

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Graphs_Integer; use Graphs_Integer;
with Graphs_Integer_Algorithms; use Graphs_Integer_Algorithms;
with Graphs_Float; use Graphs_Float;
with Graphs_Float_Algorithms; use Graphs_Float_Algorithms;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Symmetrize_Network is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2016 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Symmetrization of a directed graph                            ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Aft: Field := Default_Float_Aft;
  Net_Name, Sym_Net_Name, Wh_Id_Name: Ustring;
  Gr_I: Graphs_Integer.Graph;
  Gr_F: Graphs_Float.Graph;
  Gr_D: Graphs_Double.Graph;
  Wh_Id: Numeric_Type_Id;

begin
  Put_Info;

  if Argument_Count = 3 then
    Net_Name     := S2U(Argument(1));
    Sym_Net_Name := S2U(Argument(2));
    Wh_Id_Name   := S2U(Argument(3));
  elsif Argument_Count = 4 then
    Net_Name     := S2U(Argument(1));
    Sym_Net_Name := S2U(Argument(2));
    Wh_Id_Name   := S2U(Argument(3));
    Aft          := S2I(Argument(4));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  sym_net_name  weights_type  [ decimal_digits ]");
    New_Line;
    Put_Line("   net_name       : name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   sym_net_name   : name of the output symmetrized network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   weights_type   :  I | F | D");
    Put_Line("                       also lowercase symbols");
    Put_Line("                       also case-insensitive full names");
    Put_Line("                       I = Integer = Int");
    Put_Line("                       F = Float");
    Put_Line("                       D = Double");
    New_Line;
    Put_Line("   decimal_digits :  number of decimal digits for Float or Double output weights");
    Put_Line("                       ignored for Integer weights");
    Put_Line("                       default => " & I2S(Aft));
    return;
  end if;

  Wh_Id := To_Type_Id(U2S(Wh_Id_Name));

  case Wh_Id is
    when Integer_Id..Longint_Id =>
      Get_Graph(U2S(Net_Name), Gr_I);
      if Is_Directed(Gr_I) then
        Put_Line(U2S(Net_Name) & "  ->  " & U2S(Sym_Net_Name));
        Symmetrize(Gr_I);
        Put_Graph(U2S(Sym_Net_Name), Gr_I);
      else
        Put_Line(U2S(Net_Name) & " is already an undirected network");
      end if;
      Free(Gr_I);
    when Float_Id =>
      Get_Graph(U2S(Net_Name), Gr_F);
      if Is_Directed(Gr_F) then
        Put_Line(U2S(Net_Name) & "  ->  " & U2S(Sym_Net_Name));
        Symmetrize(Gr_F);
        Put_Graph(U2S(Sym_Net_Name), Gr_F, Aft);
      else
        Put_Line(U2S(Net_Name) & " is already an undirected network");
      end if;
      Free(Gr_F);
    when Double_Id =>
      Get_Graph(U2S(Net_Name), Gr_D);
      if Is_Directed(Gr_D) then
        Put_Line(U2S(Net_Name) & "  ->  " & U2S(Sym_Net_Name));
        Symmetrize(Gr_D);
        Put_Graph(U2S(Sym_Net_Name), Gr_D, Aft);
      else
        Put_Line(U2S(Net_Name) & " is already an undirected network");
      end if;
      Free(Gr_D);
  end case;

end Symmetrize_Network;
