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


-- @filename Net_To_Matrix.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 23/04/2011
-- @revision 26/02/2016
-- @brief Convert a network in Pajek format to Matrix format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Graphs_String; use Graphs_String;
with Pajek_IO; use Pajek_IO;

procedure Net_To_Matrix is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2016 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Convert a network file in Pajek format (*.net) into           ==");
    Put_Line("== a file with a graph in matrix form                            ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Default_No_Link: constant Ustring := S2U("0");
  Fn_In: Ustring;
  Fn_Out: Ustring;
  No_Link: Ustring;
  F_Out: File_Type;
  Gr: Graph;
  N: Natural;
  V: Vertex;
  El: Edges_List;
  E: Edge;
  J_Next: Positive;
  Wh_Next: Ustring;

begin
  Put_Info;

  if Argument_Count = 2 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    No_Link := Default_No_Link;
  elsif Argument_Count = 3 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    No_Link := S2U(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & " net_input_file  matrix_output_file  [ no_link_string ]");
    New_Line;
    Put_Line("   net_input_file     : name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   matrix_output_file : output text file containing the weights matrix of the network");
    Put_Line("                          the first line contains the names of the nodes");
    New_Line;
    Put_Line("   no_link_string     : string used to identify unexistent links within the matrix file");
    Put_Line("                          default => " & U2S(Default_No_Link));
    return;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  Get_Graph(U2S(Fn_In), Gr);
  N := Number_Of_Vertices(Gr);

  Create(F_Out, Out_File, U2S(Fn_Out));
  -- Names of the nodes
  for I in 1..N loop
    V := Get_Vertex(Gr, I);
    if I > 1 then
      Put(F_Out, HTab);
    end if;
    Put(F_Out, Get_Name(V));
  end loop;
  New_Line(F_Out);
  -- Weights matrix
  for I in 1..N loop
    V := Get_Vertex(Gr, I);
    El := Edges_From(V);
    Save(El);
    Reset(El);
    J_Next := N + 1;
    Wh_Next := No_Link;
    if Has_Next(El) then
      E := Next(El);
      J_Next := Index_Of(To(E));
      Wh_Next := Value(E);
    end if;
    for J in 1..N loop
      if J > 1 then
        Put(F_Out, HTab);
      end if;
      if J = J_Next then
        Put(F_Out, U2S(Wh_Next));
        if Has_Next(El) then
          E := Next(El);
          J_Next := Index_Of(To(E));
          Wh_Next := Value(E);
        end if;
      else
        Put(F_Out, U2S(No_Link));
      end if;
    end loop;
    New_Line(F_Out);
    Restore(El);
  end loop;
  Close(F_Out);

  Free(Gr);

end Net_To_Matrix;
