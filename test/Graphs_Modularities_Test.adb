-- Radalib, Copyright (c) 2022 by
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


-- @filename Graphs_Modularities_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/02/2007
-- @revision 25/09/2020
-- @brief Test of Graphs Modularities

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Utils.IO_Integer; use Utils.IO_Integer;
with Utils.IO_Double; use Utils.IO_Double;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities; use Graphs_Double_Modularities;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_IO; use Pajek_IO;

procedure Graphs_Modularities_Test is

  procedure Graph_Info(Gr: in Graph; Mt: in Modularity_Type) is
    Vf, Vt, V: Vertex;
    E: Edge;
    El: Edges_List;
    Mi: Modularity_Info;
    Eigenvec: PDoubles;
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
    for I in 1..Number_Of_Vertices(Gr) loop
      for J in 1..Number_Of_Vertices(Gr) loop
        E := Get_Edge_Or_No_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J));
        if E = No_Edge then
          Put("   . ");
        else
          Put(" "); Put(Value(E), Fore => 2, Aft => 1, Exp => 0);
        end if;
      end loop;
      New_Line;
    end loop;
    Put_Line("---");
    Initialize(Mi, Gr, Mt);
    Put("Total Degree              = "); Put(Total_Degree(Mi), Width => 0); New_Line;
    Put("Number of Edges           = "); Put(Number_Of_Edges(Mi), Width => 0); New_Line;
    Put("Number of Self-Loops      = "); Put(Number_Of_Self_Loops(Mi), Width => 0); New_Line;
    Put("Total Strength            = "); Put(Total_Strength(Mi), Fore => 2, Aft => 1, Exp => 0); New_Line;
    Put("Total Self-Loops Strength = "); Put(Total_Self_Loops_Strength(Mi), Fore => 2, Aft => 1, Exp => 0); New_Line;
    Put_Line("---");
    for I in 1..Number_Of_Vertices(Gr) loop
      Put(I, Width => 2); Put(": ");
      V := Get_Vertex(Gr, I);
      Put("Degree(From, To) = ("); Put(Degree_From(Mi, V), Width => 0); Put(", ");
      Put(Degree_To(Mi, V), Width => 0); Put(")");
      Put("   Strength(From, To) = ("); Put(Strength_From(Mi, V), Fore => 2, Aft => 1, Exp => 0); Put(", ");
      Put(Strength_To(Mi, V), Fore => 2, Aft => 1, Exp => 0); Put(")");
      if Has_Self_Loop(Mi, V) then
        Put("   Self-loop = "); Put(Self_Loop(Mi, V), Fore => 0, Aft => 1, Exp => 0);
      end if;
      New_Line;
    end loop;
    if Mt = Weighted_Link_Rank then
      Eigenvec := Link_Rank_Eigenvector(Mi);
      if Eigenvec /= null then
        Put_Line("---");
        Put("Left leading eigenvector:"); New_Line;
        for I in 1..Number_Of_Vertices(Gr) loop
          Put(I, Width => 2); Put(": ");
          Put(Eigenvec(I), Fore => 2, Aft => 8, Exp => 0);
          New_Line;
        end loop;
      end if;
    end if;
    Free(Mi);
--    Put_Line("---");
--    Put("Total Degree              = "); Put(Total_Degree(Gr), Width => 0); New_Line;
--    Put("Number of Edges           = "); Put(Number_Of_Edges(Gr), Width => 0); New_Line;
--    Put("Number of Self-Loops      = "); Put(Number_Of_Self_Loops(Gr), Width => 0); New_Line;
--    Put("Total Strength            = "); Put(Total_Strength(Gr), Fore => 2, Aft => 1, Exp => 0); New_Line;
--    Put("Total Self-Loops Strength = "); Put(Total_Self_Loops_Strength(Gr), Fore => 2, Aft => 1, Exp => 0); New_Line;
--    Put_Line("---");
--    for I in 1..Number_Of_Vertices(Gr) loop
--      Put(I, Width => 2); Put(": ");
--      V := Get_Vertex(Gr, I);
--      Put("Degree(From, To) = ("); Put(Degree_From(V), Width => 0); Put(", ");
--      Put(Degree_To(V), Width => 0); Put(")");
--      Put("   Strength(From, To) = ("); Put(Strength_From(V), Fore => 2, Aft => 1, Exp => 0); Put(", ");
--      Put(Strength_To(V), Fore => 2, Aft => 1, Exp => 0); Put(")");
--      if Has_Self_Loop(V) then
--        Put("   Self-loop = "); Put(Self_Loop(V), Fore => 0, Aft => 1, Exp => 0);
--      end if;
--      New_Line;
--    end loop;
  end Graph_Info;

  procedure Modularity_Details(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type) is
    Mi: Modularity_Info;
    Mr: Modularity_Rec;
    L: List;
    C: Natural;
    E: Finite_Disjoint_Lists.Element;
  begin
    pragma Warnings(Off, L);
    Initialize(Mi, Gr, Mt);
    Mr := Modularity(Mi, Lol, Mt);
    Put_Line(Modularity_Type'Image(Mt));
    Put("  Q = "); Put(Mr.Total, Aft => 8, Exp => 0);
    Put(" = "); Put(Mr.Reward, Aft => 8, Exp => 0);
    Put(" - "); Put(Mr.Penalty, Aft => 8, Exp => 0);
    New_Line;
    Save(Lol);
    Reset(Lol);
    C := 0;
    while Has_Next_List(Lol) loop
      C := C + 1;
      L := Next_List(Lol);
      Mr := Partial_Modularity(Mi, L);
      Put("    Q("); Put(C, Width => 0);
      Put(") = "); Put(Mr.Total, Aft => 8, Exp => 0);
      Put(" = "); Put(Mr.Reward, Aft => 8, Exp => 0);
      Put(" - "); Put(Mr.Penalty, Aft => 8, Exp => 0);
      New_Line;
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        E := Next_Element(L);
        Mr := Element_Modularity(Mi, E);
        Put("      q("); Put(Index_Of(E), Width => 0);
        Put(") = "); Put(Mr.Total, Aft => 8, Exp => 0);
        Put(" = "); Put(Mr.Reward, Aft => 8, Exp => 0);
        Put(" - "); Put(Mr.Penalty, Aft => 8, Exp => 0);
        New_Line;
      end loop;
      Restore(L);
    end loop;
    Restore(Lol);
--    for I in 1..Number_Of_Vertices(Gr) loop
--      Mr := Element_Modularity(Mi, Get_Element(Lol, I));
--      Put("    q("); Put(I, Width => 0);
--      Put(") = "); Put(Mr.Total, Aft => 8, Exp => 0);
--      Put(" = "); Put(Mr.Reward, Aft => 8, Exp => 0);
--      Put(" - "); Put(Mr.Penalty, Aft => 8, Exp => 0);
--      New_Line;
--    end loop;
    Free(Mi);
  end Modularity_Details;


  Mt: Modularity_Type;
  Gr: Graph;
  Fn_Net: Ustring;
  Fn_Clu: Ustring;
  Lol: List_Of_Lists;

begin
  if Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Mt := To_Modularity_Type(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & "  net_file_name  clu_or_lol_file_name  modularity_type");
    return;
  end if;

  Get_Graph(U2S(Fn_Net), Gr);

  if Tail(Fn_Clu, 4) = ".clu" then
    Get_Partition(U2S(Fn_Clu), Lol);
  else
    Get(U2S(Fn_Clu), Lol, 0);
  end if;

  Put_Line("------");
  Put_Line(U2S(Fn_Net) & " + " & U2S(Fn_Clu));

  Put_Line("------");
  Graph_Info(Gr, Mt);

  Put_Line("------");
  Put(Lol);

  Put_Line("------");
  Modularity_Details(Gr, Lol, Mt);

  Free(Lol);
  Free(Gr);
end Graphs_Modularities_Test;
