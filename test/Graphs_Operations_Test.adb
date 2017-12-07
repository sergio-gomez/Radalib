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


-- @filename Graphs_Operations_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 29/09/2009
-- @revision 26/10/2014
-- @brief Test of Graphs Operations package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Numerics.Discrete_Random;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Integer_Operations; use Graphs_Integer_Operations;
with Utils; use Utils;

procedure Graphs_Operations_Test is

  subtype Values is Integer range -20..20;
  package Int_Randoms is new Ada.Numerics.Discrete_Random(Values); use Int_Randoms;

  procedure Put(Gr: in Graph) is
    E: Edge;
  begin
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
    Put_Line("---");
  end Put;

  procedure Put(V: in PIntegers) is
  begin
    for I in V'Range loop
      Put(" ");
      Put(V(I), Width => 4);
    end loop;
    New_Line;
    Put_Line("---");
  end Put;

  Gr, Gr1, Gr2: Graph;
  P, P1: PIntegers;
  N: Natural;
  G: Generator;
  V: Values;

begin

  Reset(G);

  for Directed in reverse Boolean loop

    New_Line(2);
    Put_Line("==================");
    if Directed then
      Put_Line("Directed Graphs:");
    else
      Put_Line("Undirected Graphs:");
    end if;
    Put_Line("==================");

    Put_Line("A graph");
    Initialize(Gr, 2, Directed);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 2), -1);
    Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 1), 2);
    Put(Gr);

    Put_Line("Graph powers");
    for Power in 0..5 loop
      Put_Line("  Power = " & I2S(Power));
      Gr1 := Gr ** Power;
      Put(Gr1);
      Free(Gr1);
    end loop;

    Free(Gr);

    Put_Line("Two small graphs");
    Initialize(Gr1, 3, Directed);
    Add_Edge(Get_Vertex(Gr1, 1), Get_Vertex(Gr1, 1), 1);
    Add_Edge(Get_Vertex(Gr1, 1), Get_Vertex(Gr1, 2), 2);
    Add_Edge(Get_Vertex(Gr1, 1), Get_Vertex(Gr1, 3), 3);
    Add_Edge(Get_Vertex(Gr1, 2), Get_Vertex(Gr1, 1), 3);
    Add_Edge(Get_Vertex(Gr1, 2), Get_Vertex(Gr1, 2), 2);
    Add_Edge(Get_Vertex(Gr1, 2), Get_Vertex(Gr1, 3), 1);
    Add_Edge(Get_Vertex(Gr1, 3), Get_Vertex(Gr1, 1), -2);
    Add_Edge(Get_Vertex(Gr1, 3), Get_Vertex(Gr1, 2), 0);
    Add_Edge(Get_Vertex(Gr1, 3), Get_Vertex(Gr1, 3), 2);
    Put(Gr1);
    Initialize(Gr2, 3, Directed);
    Add_Edge(Get_Vertex(Gr2, 1), Get_Vertex(Gr2, 1), 1);
    Add_Edge(Get_Vertex(Gr2, 1), Get_Vertex(Gr2, 2), -1);
    Add_Edge(Get_Vertex(Gr2, 1), Get_Vertex(Gr2, 3), 1);
    Add_Edge(Get_Vertex(Gr2, 2), Get_Vertex(Gr2, 1), 1);
    Add_Edge(Get_Vertex(Gr2, 2), Get_Vertex(Gr2, 2), 0);
    Add_Edge(Get_Vertex(Gr2, 2), Get_Vertex(Gr2, 3), 2);
    Add_Edge(Get_Vertex(Gr2, 3), Get_Vertex(Gr2, 1), 1);
    Add_Edge(Get_Vertex(Gr2, 3), Get_Vertex(Gr2, 2), 1);
    Add_Edge(Get_Vertex(Gr2, 3), Get_Vertex(Gr2, 3), 3);
    Put(Gr2);

    Put_Line("Sum of graphs");
    Gr := Gr1 + Gr2;
    Put(Gr);
    Free(Gr);

    Gr := Gr2 + Gr1;
    Put(Gr);
    Free(Gr);

    Put_Line("Product of graphs");
    Gr := Gr1 * Gr2;
    Put(Gr);
    Free(Gr);

    Gr := Gr2 * Gr1;
    Put(Gr);
    Free(Gr);

    Put_Line("A vector");
    P := Alloc(1, Number_Of_Vertices(Gr1));
    for I in P'Range loop
      P(I) := Random(G) mod 3;
    end loop;
    Put(P);

    Put_Line("Product of graphs by a vector");
    P1 := Gr1 * P;
    Put(P1);
    Free(P1);
    P1 := Gr2 * P;
    Put(P1);
    Free(P1);

    Free(P);
    Free(Gr1);
    Free(Gr2);

    Put_Line("Two random graphs");
    N := 10;
    Initialize(Gr1, N, Directed);
    for I in 1..N  loop
      for J in 1..N loop
        V := Random(G);
        if (V in -1..1) and (V /= 0) then
          Add_Edge(Get_Vertex(Gr1, I), Get_Vertex(Gr1, J), V);
        end if;
      end loop;
    end loop;
    Put(Gr1);
    Initialize(Gr2, N, Directed);
    for I in 1..N  loop
      for J in 1..N loop
        V := Random(G);
        if (V in -3..3) and (V /= 0) then
          Add_Edge(Get_Vertex(Gr2, I), Get_Vertex(Gr2, J), V);
        end if;
      end loop;
    end loop;
    Put(Gr2);

    Put_Line("Sum of graphs");
    Gr := Gr1 + Gr2;
    Put(Gr);
    Free(Gr);

    Gr := Gr2 + Gr1;
    Put(Gr);
    Free(Gr);

    Put_Line("Product of graphs");
    Gr := Gr1 * Gr2;
    Put(Gr);
    Free(Gr);

    Gr := Gr2 * Gr1;
    Put(Gr);
    Free(Gr);

    Put_Line("Graph power = 10");
    Gr := Gr1 ** 10;
    Put(Gr);
    Free(Gr);

    Free(Gr1);
    Free(Gr2);

  end loop;

end Graphs_Operations_Test;
