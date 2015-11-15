-- Radalib, Copyright (c) 2015 by
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


-- @filename Graphs_Structure_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2009
-- @revision 26/10/2014
-- @brief Test of Graphs Structure package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Numerics.Discrete_Random;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Integer_Structure; use Graphs_Integer_Structure;
with Utils; use Utils;

procedure Graphs_Structure_Test is

  subtype Values is Integer range -20..20;
  package Int_Randoms is new Ada.Numerics.Discrete_Random(Values); use Int_Randoms;

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
        Put(" ");
        if E = No_Edge then
          Put("   .");
        else
          Put(Value(E), Width => 4);
        end if;
      end loop;
      New_Line;
    end loop;
  end Put;

  procedure Put(P: in PIntegerss) is
  begin
    for I in P'Range(1) loop
      for J in P'Range(2) loop
        Put(" ");
        if P(I, J) = 0 then
          Put("   .");
        else
          Put(P(I, J), Width => 4);
        end if;
      end loop;
      New_Line;
    end loop;
  end Put;

  procedure Put(Pgr: in Public_Graph) is
  begin
    for F in Pgr'range loop
      Put(F, Width => 2); Put(" -> ");
      if Pgr(F).From = null then
        Put_Line("(0)");
      else
        Put("("); Put(Pgr(F).From'Length, Width => 0); Put(") ");
        for T in Pgr(F).From'Range loop
          Put(Pgr(F).From(T).Index, Width => 0); Put(" ");
        end loop;
        New_Line;
      end if;
    end loop;
    Put_Line("---");
    for T in Pgr'Range loop
      Put(T, Width => 2); Put(" <- ");
      if Pgr(T).To = null then
        Put_Line("(0)");
      else
        Put("("); Put(Pgr(T).To'Length, Width => 0); Put(") ");
        for F in Pgr(T).To'Range loop
          Put(Pgr(T).To(F).Index, Width => 0); Put(" ");
        end loop;
        New_Line;
      end if;
    end loop;
    Put_Line("---");
    for F in Pgr'range loop
      Put(F, Width => 2); Put(" -> ");
      if Pgr(F).From = null then
        Put_Line("(0)");
      else
        Put("("); Put(Pgr(F).From'Length, Width => 0); Put(") ");
        for T in Pgr(F).From'Range loop
          Put(Pgr(F).From(T).Index, Width => 0); Put(":"); Put(Pgr(F).From(T).Value, Width => 0); Put(" ");
        end loop;
        New_Line;
      end if;
    end loop;
    Put_Line("---");
    for T in Pgr'Range loop
      Put(T, Width => 2); Put(" <- ");
      if Pgr(T).To = null then
        Put_Line("(0)");
      else
        Put("("); Put(Pgr(T).To'Length, Width => 0); Put(") ");
        for F in Pgr(T).To'Range loop
          Put(Pgr(T).To(F).Index, Width => 0); Put(":"); Put(Pgr(T).To(F).Value, Width => 0); Put(" ");
        end loop;
        New_Line;
      end if;
    end loop;
  end Put;

  Gr: Graph;
  N: Natural;
  G: Generator;
  Am, Wm: PIntegerss;
  Pgr: Public_Graph;
  V: Values;

begin

  Reset(G);

  for Directed in reverse Boolean loop

    Put_Line("A graph");
    N := 10;
    Initialize(Gr, N, Directed);
    for I in 1..N  loop
      for J in 1..N loop
        V := Random(G);
        if (V in -3..3) and (V /= 0) then
          Add_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J), V);
        end if;
      end loop;
    end loop;
    Put(Gr);
    Put_Line("------");

    Put_Line("Adjacency matrix");
    Am := Adjacency_Matrix(Gr);
    Put(Am);
    Free(Am);
    Put_Line("------");

    Put_Line("Weights matrix");
    Wm := Weights_Matrix(Gr);
    Put(Wm);
    Free(Wm);
    Put_Line("------");

    Put_Line("Public graph");
    Pgr := Graph_Structure(Gr);
    Put(Pgr);
    Put_Line("---------");

    Put_Line("Back to graph");
    Free(Gr);
    Gr := Graph_Structure_To_Graph(Pgr);
    Put(Gr);
    Put_Line("---------");
    New_Line;

    Free(Gr);
    Free(Pgr);

  end loop;

end Graphs_Structure_Test;
