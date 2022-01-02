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


-- @filename Spanning_Tree.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 25/02/2011
-- @revision 06/03/2018
-- @brief Minimum or Maximum Spanning Tree

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

procedure Spanning_Tree is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2022 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Find the minimum or maximum spanning tree of a                ==");
    Put_Line("== weighted network                                              ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Aft: Field := Default_Float_Aft;
  Net_Name, Mst_Net_Name, Optim_Name, Wh_Id_Name: Ustring;
  Gr_I, Gr_I_Mst: Graphs_Integer.Graph;
  Gr_F, Gr_F_Mst: Graphs_Float.Graph;
  Gr_D, Gr_D_Mst: Graphs_Double.Graph;
  Wh_Id: Numeric_Type_Id;

begin
  Put_Info;

  if Argument_Count = 4 then
    Net_Name     := S2U(Argument(1));
    Mst_Net_Name := S2U(Argument(2));
    Optim_Name   := S2U(Argument(3));
    Wh_Id_Name   := S2U(Argument(4));
  elsif Argument_Count = 5 then
    Net_Name     := S2U(Argument(1));
    Mst_Net_Name := S2U(Argument(2));
    Optim_Name   := S2U(Argument(3));
    Wh_Id_Name   := S2U(Argument(4));
    Aft          := S2I(Argument(5));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  mst_net_name  optimization_type  weights_type  [ decimal_digits ]");
    New_Line;
    Put_Line("   net_name          : name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   mst_net_name      : name of the output spanning tree file in Pajek format (*.net)");
    New_Line;
    Put_Line("   optimization_type :  MIN | MAX");
    Put_Line("                          also lowercase symbols");
    Put_Line("                          also case-insensitive full names");
    Put_Line("                          MIN = Minimum");
    Put_Line("                          MAX = Maximum");
    New_Line;
    Put_Line("   weights_type      :  I | F | D");
    Put_Line("                          also lowercase symbols");
    Put_Line("                          also case-insensitive full names");
    Put_Line("                          I = Integer = Int");
    Put_Line("                          F = Float");
    Put_Line("                          D = Double");
    New_Line;
    Put_Line("   decimal_digits    :  number of decimal digits for Float or Double output weights");
    Put_Line("                          ignored for Integer weights");
    Put_Line("                          default => " & I2S(Aft));
    return;
  end if;

  Put_Line(U2S(Net_Name) & "  ->  " & U2S(Mst_Net_Name));
  Put_Line(Capitalize(Graphs_Integer_Algorithms.Optimum_Type'Image(To_Optimum_Type(U2S(Optim_Name)))) & " spanning tree");

  Wh_Id := To_Type_Id(U2S(Wh_Id_Name));

  case Wh_Id is
    when Integer_Id..Longint_Id =>
      Get_Graph(U2S(Net_Name), Gr_I);
      Spanning_Tree(Gr_I, Gr_I_Mst, Optim => To_Optimum_Type(U2S(Optim_Name)));
      Put_Graph(U2S(Mst_Net_Name), Gr_I_Mst);
      Free(Gr_I);
      Free(Gr_I_Mst);
    when Float_Id =>
      Get_Graph(U2S(Net_Name), Gr_F);
      Spanning_Tree(Gr_F, Gr_F_Mst, Optim => To_Optimum_Type(U2S(Optim_Name)));
      Put_Graph(U2S(Mst_Net_Name), Gr_F_Mst, Aft => Aft);
      Free(Gr_F);
      Free(Gr_F_Mst);
    when Double_Id =>
      Get_Graph(U2S(Net_Name), Gr_D);
      Spanning_Tree(Gr_D, Gr_D_Mst, Optim => To_Optimum_Type(U2S(Optim_Name)));
      Put_Graph(U2S(Mst_Net_Name), Gr_D_Mst, Aft => Aft);
      Free(Gr_D);
      Free(Gr_D_Mst);
  end case;

end Spanning_Tree;
