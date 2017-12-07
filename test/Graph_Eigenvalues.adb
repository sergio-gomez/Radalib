-- Radalib, Copyright (c) 2017 by
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


-- @filename Graph_Eigenvalues.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 27/07/2009
-- @revision 22/09/2015
-- @brief Eigenvalues of the weights matrix of a Graph

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Arrays_Float; use Arrays_Float;
with Arrays_Utils_Float; use Arrays_Utils_Float;
with Graphs_Float; use Graphs_Float;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Graph_Eigenvalues is

  Fn_Net: UString;
  Fn_Eig: UString;
  Gr: Graph;
  Wh: PFloatss;
  Eig: PFloats;
  N: Natural;
  V: Vertex;
  El: Edges_List;
  E: Edge;
  J: Positive;
  Ft: File_Type;

begin
  pragma Warnings(Off, El);
  if Argument_Count = 1 then
    Fn_Net := S2U(Argument(1));
    Fn_Eig := Null_Ustring;
  elsif Argument_Count = 2 then
    Fn_Net := S2U(Argument(1));
    Fn_Eig := S2U(Argument(2));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  [ eigenvalues_name ]");
    return;
  end if;

  Put_Line(U2S(Fn_Net) & ":");
  Get_Graph(U2S(Fn_Net), Gr);

  N := Number_Of_Vertices(Gr);
  Wh := Alloc(1, N, 0.0);
  Eig := Alloc(1, N);

  for I in 1..N loop
    V := Get_Vertex(Gr, I);
    El := Edges_From(V);
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      J := Index_Of(To(E));
      Wh(I, J) := Value(E);
    end loop;
    Restore(El);
  end loop;

  Eig.all := Eigenvalues(Wh.all);
  Sort(Eig);
  Put_Line("  Maximum eigenvalue: " & F2S(Eig(Eig'Last), Aft => 6));
  Put_Line("  Minimum eigenvalue: " & F2S(Eig(Eig'First), Aft => 6));
  New_Line;

  if Fn_Eig /= Null_Ustring then
    Create(Ft, Out_File, U2S(Fn_Eig));
    for I in Eig'Range loop
      Put_Line(Ft, F2S(Eig(I), Aft => 6));
    end loop;
    Close(Ft);
  end if;

  Free(Eig);
  Free(Wh);
  Free(Gr);
end Graph_Eigenvalues;
