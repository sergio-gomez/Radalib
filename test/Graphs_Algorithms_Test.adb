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


-- @filename Graphs_Algorithms_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2004
-- @revision 14/01/2018
-- @brief Test of Graphs Algorithms package

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Integer_Algorithms; use Graphs_Integer_Algorithms;
with Graphs_Integer_Modularities_D; use Graphs_Integer_Modularities_D;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Utils; use Utils;
with Linked_Lists;

procedure Graphs_Algorithms_Test is

  use Linked_Lists_Of_Lists;

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
          Put("  .");
        else
          Put(Value(E), Width => 3);
        end if;
      end loop;
      New_Line;
    end loop;
    Put_Line("---------");
  end Put;

  procedure Put(L: in List) is
  begin
    Put(" [");
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      Put(I2S(Index_Of(Next_Element(L))));
      if Has_Next_Element(L) then
        Put(",");
      end if;
    end loop;
    Restore(L);
    Put("]");
  end Put;

  procedure Put(Ls: in Linked_List) is
  begin
    Save(Ls);
    Reset(Ls);
    while Has_Next(Ls) loop
      Put(Next(Ls));
    end loop;
    Restore(Ls);
  end Put;


  Gr, Sub_Gr, Gr_Mst, Gr_Ren: Graph;
  Lol, Ren, Lol_Ren: List_Of_Lists;
  L: List;
  Ls: Linked_List;
  R: Integer;

begin

  for Directed in reverse Boolean loop

    Initialize(Gr, 10, Directed);

    New_Line(3);
    Put_Line("==================");
    if Is_Directed(Gr) then
      Put_Line("Directed Graphs:");
    else
      Put_Line("Undirected Graphs:");
    end if;
    Put_Line("==================");

    Put_Line("Initial graph");
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 3));
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 4));
    Add_Edge(Get_Vertex(Gr, 3), Get_Vertex(Gr, 5));
    Add_Edge(Get_Vertex(Gr, 3), Get_Vertex(Gr, 4));
    Add_Edge(Get_Vertex(Gr, 6), Get_Vertex(Gr, 10));
    Add_Edge(Get_Vertex(Gr, 7), Get_Vertex(Gr, 1));
    Add_Edge(Get_Vertex(Gr, 7), Get_Vertex(Gr, 5));
    Add_Edge(Get_Vertex(Gr, 8), Get_Vertex(Gr, 2));
    Add_Edge(Get_Vertex(Gr, 9), Get_Vertex(Gr, 8));
    Add_Edge(Get_Vertex(Gr, 9), Get_Vertex(Gr, 9));
    Add_Edge(Get_Vertex(Gr, 10), Get_Vertex(Gr, 6));
    Put(Gr);

    Put_Line("Weak conected components of previous graph");
    Connected_Components(Gr, Lol);
    Put(Lol);
    Put_Line("------");

    Free(Lol);

    Put_Line("Strong conected components of previous graph");
    Connected_Components(Gr, Lol, Strong_Components);
    Put(Lol);
    Put_Line("------");

    Free(Lol);

    Put_Line("Another lol");
    Initialize(Ren, 10);
    L := New_List(Ren);
    Move(Get_Element(Ren, 1), L);
    Move(Get_Element(Ren, 3), L);
    Move(Get_Element(Ren, 4), L);
    L := New_List(Ren);
    Move(Get_Element(Ren, 5), L);
    Move(Get_Element(Ren, 6), L);
    Move(Get_Element(Ren, 7), L);
    Move(Get_Element(Ren, 8), L);
    L := New_List(Ren);
    Move(Get_Element(Ren, 2), L);
    Move(Get_Element(Ren, 9), L);
    Move(Get_Element(Ren, 10), L);
    Put(Ren);
    Put_Line("------");

    Put_Line("Weak conected components of previous graph within previous lol");
    Connected_Components(Gr, Ren, Lol);
    Put(Lol);
    Put_Line("------");

    Put_Line("Same but one component at a time");
    Clear(Ren);
    L := New_List(Ren);
    Move(Get_Element(Ren, 1), L);
    Move(Get_Element(Ren, 3), L);
    Move(Get_Element(Ren, 4), L);
    Update_List_Connected_Components(Gr, L, Ls);
    Put("1st list divided in " & I2S(Size(Ls)) & " lists:"); Put(Ls); New_Line;
    Free(Ls);
    L := New_List(Ren);
    Move(Get_Element(Ren, 5), L);
    Move(Get_Element(Ren, 6), L);
    Move(Get_Element(Ren, 7), L);
    Move(Get_Element(Ren, 8), L);
    Update_List_Connected_Components(Gr, L, Ls);
    Put("2nd list divided in " & I2S(Size(Ls)) & " lists:"); Put(Ls); New_Line;
    Free(Ls);
    L := New_List(Ren);
    Move(Get_Element(Ren, 2), L);
    Move(Get_Element(Ren, 9), L);
    Move(Get_Element(Ren, 10), L);
    Update_List_Connected_Components(Gr, L, Ls);
    Put("3rd list divided in " & I2S(Size(Ls)) & " lists:"); Put(Ls); New_Line;
    Free(Ls);
    Put(Ren);
    Put_Line("------");

    Free(Ren);

    Put_Line("Subgraph of vertices 1 to 4");
    Clear(Lol);
    L := New_List(Lol);
    Move(Get_Element(Lol, 1), L);
    Move(Get_Element(Lol, 2), L);
    Move(Get_Element(Lol, 3), L);
    Move(Get_Element(Lol, 4), L);
    Create_Subgraph(Gr, L, Sub_Gr);
    Put(Sub_Gr);
    Free(Sub_Gr);

    Put_Line("Isolating set of vertices 1 to 4");
    Isolate_List(Gr, L);
    Put(Gr);

    Free(Lol);
    Free(Gr);

    Put_Line("Another graph");
    Initialize(Gr, 5, Directed);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 2), 2);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 3), 1);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 4), 1);
    Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 1), 2);
    Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 2), 1);
    Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 3), 2);
    Add_Edge(Get_Vertex(Gr, 3), Get_Vertex(Gr, 2), 1);
    Add_Edge(Get_Vertex(Gr, 3), Get_Vertex(Gr, 4), 1);
    Add_Edge(Get_Vertex(Gr, 5), Get_Vertex(Gr, 1), 1);
    Put(Gr);

    Put_Line("Minimum Spanning Tree");
    Spanning_Tree(Gr, Gr_Mst, Optim => Minimum);
    Put(Gr_Mst);
    Free(Gr_Mst);

    Put_Line("Maximum Spanning Tree");
    Spanning_Tree(Gr, Gr_Mst, Optim => Maximum);
    Put(Gr_Mst);
    Free(Gr_Mst);

    Put_Line("Symmetrized");
    Symmetrize(Gr);
    Put(Gr);

    Free(Gr);

    Put_Line("Another graph");
    Initialize(Gr, 8, Directed);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 2), 1);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 3), 1);
    Add_Edge(Get_Vertex(Gr, 2), Get_Vertex(Gr, 3), 1);
    Add_Edge(Get_Vertex(Gr, 3), Get_Vertex(Gr, 4), 1);
    Add_Edge(Get_Vertex(Gr, 4), Get_Vertex(Gr, 5), 1);
    Add_Edge(Get_Vertex(Gr, 5), Get_Vertex(Gr, 6), 1);
    Add_Edge(Get_Vertex(Gr, 6), Get_Vertex(Gr, 7), 1);
    Add_Edge(Get_Vertex(Gr, 6), Get_Vertex(Gr, 8), 1);
    Add_Edge(Get_Vertex(Gr, 7), Get_Vertex(Gr, 8), 1);
    Add_Edge(Get_Vertex(Gr, 1), Get_Vertex(Gr, 1), 1);
    Add_Edge(Get_Vertex(Gr, 4), Get_Vertex(Gr, 4), 1);
    Add_Edge(Get_Vertex(Gr, 5), Get_Vertex(Gr, 5), 1);
    Put(Gr);

    Put_Line("Renormalizing lol");
    Initialize(Ren, 8);
    L := New_List(Ren);
    Move(Get_Element(Ren, 1), L);
    Move(Get_Element(Ren, 2), L);
    Move(Get_Element(Ren, 3), L);
    L := New_List(Ren);
    Move(Get_Element(Ren, 4), L);
    Move(Get_Element(Ren, 5), L);
    L := New_List(Ren);
    Move(Get_Element(Ren, 6), L);
    Move(Get_Element(Ren, 7), L);
    Move(Get_Element(Ren, 8), L);
    Put(Ren);
    Put_Line("------");

    Put_Line("Renormalized graph");
    Renormalize_Graph(Gr, Ren, Gr_Ren);
    Put(Gr_Ren);

    Put_Line("Lol of renormalized graph");
    Initialize(Lol_Ren, 3);
    L := New_List(Lol_Ren);
    Move(Get_Element(Lol_Ren, 3), L);
    Move(Get_Element(Lol_Ren, 2), L);
    L := New_List(Lol_Ren);
    Move(Get_Element(Lol_Ren, 1), L);
    Put(Lol_Ren);
    Put_Line("------");

    Put_Line("Unrenormalized lol");
    Unrenormalize_List_Of_Lists(Lol_Ren, Ren, Lol);
    Put(Lol);
    Put_Line("------");

    for Mt in Modularity_Type loop
      Put("Modularity(Gr    , Lol    , " & Capitalize(Modularity_Type'Image(Mt)) & ") := ");
      Put_Line(D2S(Modularity(Gr, Lol, Mt), Aft => 6, Exp => 0));
      Put("Modularity(Gr_Ren, Lol_Ren, " & Capitalize(Modularity_Type'Image(Mt)) & ") := ");
      Put_Line(D2S(Modularity(Gr_Ren, Lol_Ren, Mt), Aft => 6, Exp => 0));
      Put_Line("------");
    end loop;

    Free(Gr_Ren);

    Put_Line("The graph again");
    Put(Gr);

    R := 10;
    Put_Line("Renormalized graph with resistance " & I2S(R));
    Renormalize_Graph(Gr, Ren, Gr_Ren, R);
    Put(Gr_Ren);

    for Mt in Modularity_Type loop
      Put("Modularity(Gr    , Lol    , " & Capitalize(Modularity_Type'Image(Mt)) & ") := ");
      Put_Line(D2S(Modularity(Gr, Lol, Mt, Double(R)), Aft => 6, Exp => 0));
      Put("Modularity(Gr_Ren, Lol_Ren, " & Capitalize(Modularity_Type'Image(Mt)) & ") := ");
      Put_Line(D2S(Modularity(Gr_Ren, Lol_Ren, Mt, No_Resistance), Aft => 6, Exp => 0));
      Put_Line("------");
    end loop;



    Free(Lol);
    Free(Lol_Ren);
    Free(Ren);
    Free(Gr);

  end loop;

end Graphs_Algorithms_Test;
