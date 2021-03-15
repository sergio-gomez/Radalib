-- Radalib, Copyright (c) 2021 by
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


-- @filename Multiplex_Extract_Layers.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/10/2014
-- @revision 26/02/2016
-- @brief Extract the Layers of a Multiplex as Networks in Pajek format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Graphs_String_Multilayer; use Graphs_String_Multilayer;
with Multilayer_IO; use Multilayer_IO;
with Pajek_IO; use Pajek_IO;

procedure Multiplex_Extract_Layers is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2021 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Extract the Layers of a Multiplex as Networks in Pajek format ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Unknown_Network_Type_Error: exception;

  Net_Sufix: constant String := ".net";

  Fn_In: Ustring;
  Fn_Out: Ustring;
  Fn_Net: Ustring;
  Directed: Boolean;

  Mpx: Multiplex;
  Grs: PGraphs;
  Layer_Name: Ustring;

begin
  Put_Info;

  if Argument_Count /= 3 then
    Put_Line("Usage:  " & Command_Name & "  list_input_file  net_output_prefix  network_type");
    New_Line;
    Put_Line("   list_input_file   : text file containing the list of links of a multiplex");
    New_Line;
    Put_Line("   net_output_prefix : prefix of output layer networks");
    New_Line;
    Put_Line("   network_type      : D | U");
    Put_Line("                         also lowercase symbols");
    Put_Line("                         also case-insensitive full names (Directed, Undirected)");
    Put_Line("                         D = Directed");
    Put_Line("                         U = Undirected");
    Put_Line("                         for repeated Edges, only the last one is stored");
    return;
  elsif Argument_Count = 3 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    if    To_Uppercase(Argument(3)) = "D" or To_Lowercase(Argument(3)) = "directed"   then
      Directed := True;
    elsif To_Uppercase(Argument(3)) = "U" or To_Lowercase(Argument(3)) = "undirected" then
      Directed := False;
    else
      raise Unknown_Network_Type_Error;
    end if;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  Get_Multiplex(U2S(Fn_In), Mpx, Directed);

  Put_Line("  " & I2S(Number_Of_Layers(Mpx)) & " layers");
  Put_Line("  " & I2S(Number_Of_Vertices(Mpx)) & " nodes");
  if Directed then
    Put_Line("  Directed layers");
  else
    Put_Line("  Undirected layers");
  end if;

  Grs := Get_Layers(Mpx);
  for L in Grs'Range loop
    Layer_Name := Get_Layer_Name(Mpx, L);
    if Is_Integer(Layer_Name) and then S2I(U2S(Layer_Name)) = L then
      Fn_Net := Fn_Out & ("-lay" & Right_Justify(I2S(L), 2, '0') & Net_Sufix);
    else
      Fn_Net := Fn_Out & S2U("-lay" & Right_Justify(I2S(L), 2, '0') & "-") & Layer_Name & S2U(Net_Sufix);
    end if;
    Put_Graph(U2S(Fn_Net), Grs(L));
  end loop;
  Free(Mpx);

end Multiplex_Extract_Layers;
