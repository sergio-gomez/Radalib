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


-- @filename Net_To_List.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/11/2012
-- @revision 26/02/2016
-- @brief Convert a network in Pajek format to List format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Graphs_String; use Graphs_String;
with Pajek_IO; use Pajek_IO;

procedure Net_To_List is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2019 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Convert a network file in Pajek format (*.net) into           ==");
    Put_Line("== a file with the list of links                                 ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Fn_In: Ustring;
  Fn_Out: Ustring;
  F_Out: File_Type;
  Gr: Graph;
  N: Natural;
  Is_Dir: Boolean;
  Vi, Vj: Vertex;
  El: Edges_List;
  E: Edge;
  J: Positive;
  Wh: Ustring;

begin
  Put_Info;

  if Argument_Count = 2 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
  else
    Put_Line("Usage:  " & Command_Name & " net_input_file  list_output_file");
    New_Line;
    Put_Line("   net_input_file   : name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   list_output_file : name of the output network file in list format");
    return;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  Get_Graph(U2S(Fn_In), Gr);
  N := Number_Of_Vertices(Gr);
  Is_Dir := Is_Directed(Gr);

  Create(F_Out, Out_File, U2S(Fn_Out));
  for I in 1..N loop
    Vi := Get_Vertex(Gr, I);
    El := Edges_From(Vi);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      Vj := To(E);
      J := Index_Of(Vj);
      Wh := Value(E);
      if Is_Dir or else I <= J then
        Put_Line(F_Out, Get_Name(Vi) & HTab & Get_Name(Vj) & HTab & U2S(Wh));
      end if;
    end loop;
    Restore(El);
  end loop;
  Close(F_Out);

  Free(Gr);

end Net_To_List;
