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


-- @filename Graphs_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2004
-- @revision 30/10/2014
-- @brief Test of Graphs package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Graphs_Integer; use Graphs_Integer;
with Utils; use Utils;

procedure Graphs_Test is

  Num: constant Natural := 4;

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
      if Has_Self_Loop(Vf) then
        Put(" has self-loop");
      end if;
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
      if Has_Self_Loop(Vt) then
        Put(" has self-loop");
      end if;
      New_Line;
    end loop;
    Put_Line("---");
    for I in 1..Number_Of_Vertices(Gr) loop
      for J in 1..Number_Of_Vertices(Gr) loop
        E := Get_Edge_Or_No_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J));
        if E /= No_Edge then
          Put(Value(E), Width => 3);
        else
          Put("  .");
        end if;
      end loop;
      New_Line;
    end loop;
    Put_Line("---");
    Put("Total degree        : "); Put(Total_Degree(Gr), Width => 0); New_Line;
    Put("Number of edges     : "); Put(Number_Of_Edges(Gr), Width => 0); New_Line;
    Put("Number of self-loops: "); Put(Number_Of_Self_Loops(Gr), Width => 0); New_Line;
  end Put;

  Gr, Gr_Clone: Graph;
  El: Edges_List;
  E: Edge;
  Vf, Vt: Vertex;

begin
  pragma Warnings(Off, El);

  for Directed in reverse Boolean loop
    for Weighted in reverse Boolean loop

      Initialize(Gr, Num, Directed);

      New_Line;
      Put_Line("=============================");
      if Weighted then
        Put("Weighted ");
      else
        Put("Unweighted ");
      end if;
      if Is_Directed(Gr) then
        Put_Line("digraphs:");
      else
        Put_Line("undirected graphs:");
      end if;
      Put_Line("=============================");

      Put_Line("Original graph");
      Put_Line("---");
      for I in 1..Num loop
        for J in I..Num loop
          if Weighted then
            Add_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J mod Num + 1), 10 * ((I + J) mod Num));
          else
            Add_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J mod Num + 1));
          end if;
        end loop;
      end loop;
      E := Get_Edge(Get_Vertex(Gr, 3), Get_Vertex(Gr, 4));
      if Weighted then
        Set_Value(E, 25);
      end if;
      Put(Gr);
      Put_Line("---------");

      Put_Line("All edges from second and to last Vertices removed");
      Put_Line("---");
      for I in 1..Num loop
        Remove_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, I));
        Remove_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, Num));
      end loop;
      Put(Gr);
      Put_Line("---------");

      Put_Line("Change Directed <--> Undirected");
      Put_Line("---");
      if Is_Directed(Gr) then
        To_Undirected(Gr);
      else
        To_Directed(Gr);
      end if;
      Put(Gr);
      Put_Line("---------");

      Put_Line("Add edge 2 -> 3");
      Put_Line("---");
      if Weighted then
        Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 3), 5);
      else
        Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 3));
      end if;
      Put(Gr);
      Put_Line("---------");

      Put_Line("Change to Unweighted");
      Put_Line("---");
      if Is_Weighted(Gr) then
        To_Unweighted(Gr);
        Put(Gr);
      else
        Put_Line("No, is unweighted");
      end if;
      Put_Line("---------");

      Put_Line("All edges from first Vertex removed");
      Put_Line("---");
      Vf := Get_Vertex(Gr, 1);
      El := Edges_From(Vf);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vt := To(E);
        Remove_Edge(Vf, Vt);
      end loop;
      Restore(El);
      Put(Gr);
      Put_Line("---------");

      Put_Line("Add names, tags and marks to vertices, and find the vertices from name");
      Put_Line("---");
      for I in 1..Num loop
        Set_Name(Get_Vertex(Gr, I), "Name" & I2S(I));
        Set_Tag(Get_Vertex(Gr, I), "Tag " & I2S(I));
        Set_Mark(Get_Vertex(Gr, I), (I mod 2) = 0);
      end loop;
      Put(Gr);
      Put_Line("---");
      for I in reverse 1..Num loop
        Put_Line("Name" & I2s(I) & " -> Vertex " & I2S(Index_Of(Get_Vertex(Gr, "Name" & I2s(I)))));
      end loop;
      Put_Line("---------");

      Put_Line("Clone of last graph");
      Put_Line("---");
      Gr_Clone := Clone(Gr);
      Free(Gr);
      Put(Gr_Clone);
      Free(Gr_Clone);
      Put_Line("---------");

    end loop;
  end loop;

end Graphs_Test;
