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


-- @filename Graphs_Modularities_Test_D.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/03/2006
-- @revision 02/09/2020
-- @brief Test of Graphs Modularities package

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Utils.IO_Integer; use Utils.IO_Integer;
with Utils.IO_Double; use Utils.IO_Double;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities; use Graphs_Double_Modularities;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Pajek_IO; use Pajek_IO;

procedure Graphs_Modularities_Test_D is

  Gr: Graph;
  Fn_Net: Ustring;
  Fn_Clu: Ustring;

  Q_Best: array(Modularity_Type) of Modularity_Rec := (others => Null_Modularity_Rec);
  Lol_Best: array(Modularity_Type) of List_Of_Lists;
  Lol_Clu: List_Of_Lists;

  procedure Handler(Lol: in List_Of_Lists) is
    Q: Modularity_Rec;
  begin
    for Mt in Modularity_Type loop
      Q := Modularity(Gr, Lol, Mt);
      if Q.Total > Q_Best(Mt).Total then
        Q_Best(Mt) := Q;
        Free(Lol_Best(Mt));
        Lol_Best(Mt) := Clone(Lol);
      end if;
    end loop;
  end Handler;

  procedure Traversal is new Generic_All_Partitions_Traversal(Handler);

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
    Free(Mi);
  end Graph_Info;

  procedure Modularity_Details(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type) is
    Mi: Modularity_Info;
    Mr: Modularity_Rec;
  begin
    Initialize(Mi, Gr, Mt);
    Mr := Modularity(Mi, Lol, Mt);
    Put_Line(Modularity_Type'Image(Mt));
    Put("  Q = "); Put(Mr.Total, Aft => 6, Exp => 0);
    Put(" = "); Put(Mr.Reward, Aft => 6, Exp => 0);
    Put(" - "); Put(Mr.Penalty, Aft => 6, Exp => 0);
    New_Line;
    for I in 1..Number_Of_Vertices(Gr) loop
      Mr := Element_Modularity(Mi, Get_Element(Lol, I));
      Put("    q("); Put(I, Width => 2);
      Put(") = "); Put(Mr.Total, Aft => 6, Exp => 0);
      Put(" = "); Put(Mr.Reward, Aft => 6, Exp => 0);
      Put(" - "); Put(Mr.Penalty, Aft => 6, Exp => 0);
      New_Line;
    end loop;
    Free(Mi);
  end Modularity_Details;

begin
  if Argument_Count = 2 then
    Fn_Net := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
  else
    Put_Line("Usage:  " & Command_Name & "  net_file_name  clu_or_lol_file_name");
    return;
  end if;

  Get_Graph(U2S(Fn_Net), Gr);
  Graph_Info(Gr, Mt => Weighted_Link_Rank);
  Put_Line("------");

  if Tail(Fn_Clu, 4) = ".clu" then
    Get_Partition(U2S(Fn_Clu), Lol_Clu);
  else
    Get(U2S(Fn_Clu), Lol_Clu, 0);
  end if;

  Put(Lol_Clu);
  for Mt in Modularity_Type loop
    Put_Line("---");
    Modularity_Details(Gr, Lol_Clu, Mt);
  end loop;
  Put_Line("------");

  for Mt in Modularity_Type loop
    Initialize(Lol_Best(Mt), Number_Of_Vertices(Gr));
  end loop;

  Traversal(Number_Of_Vertices(Gr));

  for Mt in Modularity_Type loop
    Put_Line(Modularity_Type'Image(Mt));
    Put("  Q_Best = "); Put_Double(Q_Best(Mt).Total, Aft => 6);
    Put(" = "); Put_Double(Q_Best(Mt).Reward, Aft => 6);
    Put(" - "); Put_Double(Q_Best(Mt).Penalty, Aft => 6);
    New_Line;
    Put_Line("---");
    Put(Lol_Best(Mt));
    Put_Line("------");
  end loop;

  for Mt in Modularity_Type loop
    Free(Lol_Best(Mt));
  end loop;

  Free(Gr);
end Graphs_Modularities_Test_D;
