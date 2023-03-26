-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


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
with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Pajek_IO; use Pajek_IO;

procedure Pajek_IO_Test is

  procedure Put(Gr: in Graph) is
    Vf, Vt: Vertex;
    E: Edge;
    El: Edges_List;
  begin
    pragma Warnings(Off, El);
    for I in 1..Number_Of_Vertices(Gr) loop
      Put(I, Width => 2); Put(":  ");
      Put("Name = '" & Get_Name(Get_Vertex(Gr, I)) & "'  ");
      Put("Tag = '" & Get_Tag(Get_Vertex(Gr, I)) & "'  ");
      Put("Marked = '" & To_Lowercase(Boolean'Image(Is_Marked(Get_Vertex(Gr, I)))) & "'");
      New_Line;
    end loop;
    Put_Line("---");
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

  Fn_Net_In_Prefix  : constant String := "test-pajek_in";
  Fn_Net_Out_Prefix : constant String := "test-pajek_out";
  Fn_Net_Sufix      : constant String := ".net";
  Fn_In_Clu  : constant String := "test-pajek_in.clu";
  Fn_Out_Clu : constant String := "test-pajek_out.clu";

  Num_Nets: constant Integer := 4;
  Fn_Net_In : UString;
  Fn_Net_Out: UString;
  Gr: Graph;
  Lol: List_Of_Lists;

begin
  for I in 1..Num_Nets loop
    Put_Line("=========");
    Fn_Net_In  := S2U(Fn_Net_In_Prefix  & I2S(I) & Fn_Net_Sufix);
    Fn_Net_Out := S2U(Fn_Net_Out_Prefix & I2S(I) & Fn_Net_Sufix);
    Put_Line(U2S(Fn_Net_In) & " -> " & U2S(Fn_Net_Out));
    Put_Line("---");
    Get_Graph(U2S(Fn_Net_In), Gr);
    Put(Gr);
    Put_Graph(U2S(Fn_Net_Out), Gr, Aft => 1, Exp => 0);
    Free(Gr);

    New_Line;
  end loop;

  Put_Line("=========");
  Put_Line(Fn_In_Clu & " -> " & Fn_Out_Clu);
  Put_Line("---");
  Get_Partition(Fn_In_Clu, Lol);
  Put(Lol);
  Put_Partition(Fn_Out_Clu, Lol);
  Free(Lol);
end Pajek_IO_Test;
