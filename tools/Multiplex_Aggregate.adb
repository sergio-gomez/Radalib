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


-- @filename Multiplex_Aggregate.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/10/2014
-- @revision 26/02/2016
-- @brief Aggregate the Layers of a Multiplex

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Integer_Multilayer; use Graphs_Integer_Multilayer;
with Graphs_Float; use Graphs_Float;
with Graphs_Float_Multilayer; use Graphs_Float_Multilayer;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Multilayer; use Graphs_Double_Multilayer;
with Multilayer_IO; use Multilayer_IO;
with Pajek_IO; use Pajek_IO;

procedure Multiplex_Aggregate is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2016 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Aggregate the Layers of a Multiplex                           ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Unknown_Network_Type_Error: exception;
  Unknown_Aggregation_Type_Error: exception;

  function Is_Directed(S: in String) return Boolean is
  begin
    if    To_Uppercase(S) = "D" or To_Lowercase(S) = "directed"   then
      return True;
    elsif To_Uppercase(S) = "U" or To_Lowercase(S) = "undirected" then
      return False;
    else
      raise Unknown_Network_Type_Error;
    end if;
  end Is_Directed;

  function To_Aggregation_Type(S: in String) return Boolean is
  begin
    if    To_Uppercase(S) = "W" or To_Lowercase(S) = "weighted"   then
      return True;
    elsif To_Uppercase(S) = "U" or To_Lowercase(S) = "unweighted" then
      return False;
    else
      raise Unknown_Aggregation_Type_Error;
    end if;
  end To_Aggregation_Type;

  Fn_In: Ustring;
  Fn_Out: Ustring;
  Directed: Boolean;
  Weighted: Boolean;
  Wh_Id: Numeric_Type_Id;
  Aft: Field := Default_Float_Aft;

  Mpx_I: Graphs_Integer_Multilayer.Multiplex;
  Mpx_F: Graphs_Float_Multilayer.Multiplex;
  Mpx_D: Graphs_Double_Multilayer.Multiplex;
  Gr_I: Graphs_Integer.Graph;
  Gr_F: Graphs_Float.Graph;
  Gr_D: Graphs_Double.Graph;


begin
  Put_Info;

  if Argument_Count = 5 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Directed := Is_Directed(Argument(3));
    Weighted := To_Aggregation_Type(Argument(4));
    Wh_Id := To_Type_Id(Argument(5));
    Aft := Default_Float_Aft;
  elsif Argument_Count = 6 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Directed := Is_Directed(Argument(3));
    Weighted := To_Aggregation_Type(Argument(4));
    Wh_Id := To_Type_Id(Argument(5));
    Aft := S2I(Argument(6));
  else
    Put_Line("Usage:  " & Command_Name & "  list_input_file  output_file  network_type  aggregation_type  weights_type  [ decimal_digits ]");
    New_Line;
    Put_Line("   list_input_file  :  text file containing the list of links of a multiplex");
    New_Line;
    Put_Line("   output_file      :  prefix of output layer networks");
    New_Line;
    Put_Line("   network_type     :  D | U");
    Put_Line("                         also lowercase symbols");
    Put_Line("                         also case-insensitive full names (Directed, Undirected)");
    Put_Line("                         D = Directed");
    Put_Line("                         U = Undirected");
    Put_Line("                         for repeated Edges, only the last one is stored");
    New_Line;
    Put_Line("   aggregation_type :  W | U");
    Put_Line("                         also lowercase symbols");
    Put_Line("                         also case-insensitive full names (Weighted, Unweighted)");
    Put_Line("                         W = Weighted");
    Put_Line("                         U = Unweighted");
    New_Line;
    Put_Line("   weights_type     :  I | F | D");
    Put_Line("                         also lowercase symbols");
    Put_Line("                         also case-insensitive full names (Integer, ...)");
    Put_Line("                         I = Integer = Int");
    Put_Line("                         F = Float");
    Put_Line("                         D = Double");
    New_Line;
    Put_Line("   decimal_digits   :  number of decimal digits for float and double weights");
    Put_Line("                         ignored for Integer weights");
    Put_Line("                         default => " & I2S(Aft));
    return;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  case Wh_Id is
    when Integer_Id..Longint_Id =>
      Get_Multiplex(U2S(Fn_In), Mpx_I, Directed);
      Gr_I := Aggregate_Layers(Mpx_I, Weighted => Weighted);
      Put_Graph(U2S(Fn_Out), Gr_I);
      Free(Gr_I);
      Free(Mpx_I);
    when Float_Id =>
      Get_Multiplex(U2S(Fn_In), Mpx_F, Directed);
      Gr_F := Aggregate_Layers(Mpx_F, Weighted => Weighted);
      Put_Graph(U2S(Fn_Out), Gr_F, Aft);
      Free(Gr_F);
      Free(Mpx_F);
    when Double_Id =>
      Get_Multiplex(U2S(Fn_In), Mpx_D, Directed);
      Gr_D := Aggregate_Layers(Mpx_D, Weighted => Weighted);
      Put_Graph(U2S(Fn_Out), Gr_D, Aft);
      Free(Gr_D);
      Free(Mpx_D);
  end case;

end Multiplex_Aggregate;
