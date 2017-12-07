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


-- @filename Pajek_IO_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/03/2006
-- @revision 28/10/2014
-- @brief Test of Pajek_IO package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Graphs_Double; use Graphs_Double;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.Io; use Finite_Disjoint_Lists.Io;
with Pajek_Io; use Pajek_Io;
with Utils.Io; use Utils.Io;

procedure Pajek_IO_Test is

  procedure Put(Gr: in Graph) is
    Vf, Vt: Vertex;
    E: Edge;
    El: Edges_List;
  begin
    pragma Warnings(Off, El);
    for F in 1..Number_Of_Vertices(Gr) loop
      Put(F, Width => 2); Put(" -> ");
      Vf := Get_Vertex(Gr, F);
      Put("("); Put(Degree_From(Vf), Width => 0); Put(") ");
      El := Edges_From(Vf);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vt := To(E);
        Put(Index_Of(Vt), Width => 0); Put(" ");
      end loop;
      Restore(El);
      New_Line;
    end loop;
    Put_Line("---");
    for T in 1..Number_Of_Vertices(Gr) loop
      Put(T, Width => 2); Put(" <- ");
      Vt := Get_Vertex(Gr, T);
      Put("("); Put(Degree_To(Vt), Width => 0); Put(") ");
      El := Edges_To(Vt);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vf := From(E);
        Put(Index_Of(Vf), Width => 0); Put(" ");
      end loop;
      Restore(El);
      New_Line;
    end loop;
    Put_Line("---");
    for I in 1..Number_Of_Vertices(Gr) loop
      for J in 1..Number_Of_Vertices(Gr) loop
        E := Get_Edge_Or_No_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J));
        if E = No_Edge then
          Put(" .  ");
        else
          Put_Double(Value(E), Aft => 1); Put(" ");
        end if;
      end loop;
      New_Line;
    end loop;
    Put_Line("------");
  end Put;

  Fn_In_Net1: constant String  := "pajek_test_in1.net";
  Fn_In_Net2: constant String  := "pajek_test_in2.net";
  Fn_In_Clu : constant String  := "pajek_test_in.clu";
  Fn_Out_Net1: constant String := "pajek_test_out1.net";
  Fn_Out_Net2: constant String := "pajek_test_out2.net";
  Fn_Out_Clu : constant String := "pajek_test_out.clu";
  Gr: Graph;
  Lol: List_Of_Lists;

begin
  Put_Line("=========");
  Put_Line(Fn_In_Net1 & " -> " & Fn_Out_Net1);
  Put_Line("---");
  Get_Graph(Fn_In_Net1, Gr);
  Put(Gr);
  Put_Graph(Fn_Out_Net1, Gr, Aft => 1, Exp => 0);
  Free(Gr);

  New_Line;

  Put_Line("=========");
  Put_Line(Fn_In_Net2 & " -> " & Fn_Out_Net2);
  Put_Line("---");
  Get_Graph(Fn_In_Net2, Gr);
  Put(Gr);
  Put_Graph(Fn_Out_Net2, Gr, Aft => 1, Exp => 0);
  Free(Gr);

  New_Line;

  Put_Line("=========");
  Put_Line(Fn_In_Clu & " -> " & Fn_Out_Clu);
  Put_Line("---");
  Get_Partition(Fn_In_Clu, Lol);
  Put(Lol);
  Put_Partition(Fn_Out_Clu, Lol);
  Free(Lol);
end Pajek_IO_Test;
