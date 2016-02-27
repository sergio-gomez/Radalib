-- Radalib, Copyright (c) 2016 by
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


-- @filename Graphs_Properties_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 30/11/2007
-- @revision 08/03/2013
-- @brief Test of Graph Properties

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Utils.IO_Double; use Utils.IO_Double;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Properties_D; use Graphs_Double_Properties_D;
with Pajek_IO; use Pajek_IO;
with Statistics_Double; use Statistics_Double;

procedure Graphs_Properties_Test is

  procedure Put_Doubles(P: in PDoubles; Aft: in Field := Default_Float_Aft) is
    One_Line: constant Boolean := False;
    Show_Index: constant Boolean := not One_Line;
  begin
    if One_Line then
      for I in P'Range loop
        if Show_Index then
          Put("   " & Right_Justify(I2S(I), 2) & ": ");
        else
          Put("   ");
        end if;
        Put(D2S(P(I), Aft => Aft, Exp => 0));
      end loop;
      New_Line;
    else
      for I in P'Range loop
        if Show_Index then
          Put("  " & Right_Justify(I2S(I), 2) & ": ");
        else
          Put("  ");
        end if;
        Put(P(I), Fore => 0, Aft => Aft, Exp => 0);
        New_Line;
      end loop;
    end if;
  end Put_Doubles;

  procedure Put_Shortest_Path(Gr: in Graph; Dists: in PDoubless; Preds: in PIntegerss; F, T: in Positive) is

    procedure Put_Path(Preds: in PIntegerss; F, T: in Positive) is
    begin
      if F = T then
        Put("  " & I2S(F));
      else
        Put_Path(Preds, F, Preds(F, T));
        Put(" -> " & I2S(T));
      end if;
    end Put_Path;

    use Linked_Edges;
    Vf, Vt: Vertex;
    Path: Graph_Path;
    E: Edge;
    I, J: Natural;
    D: Double;
  begin
    Put_Line("Shortest Path " & I2S(F) & " -> " & I2S(T) & ": ");
    if Dists(F, T) = Plus_Infinity then
      Put_Line("  no path available");
    else
      I := Preds(F, T);
      Put("  " & I2S(T));
      while I /= F loop
        Put(" <- " & I2S(I));
        I := Preds(F, I);
      end loop;
      if F /= T then
        Put(" <- " & I2S(F));
      end if;
      New_Line;
      Put_Path(Preds, F, T);
      New_Line;
      Put_Line("  length " & D2Se0(Dists(F, T), 2));
      Vf := Get_Vertex(Gr, F);
      Vt := Get_Vertex(Gr, T);
      Path := Shortest_Path(Vf, Vt, Dists, Preds);
      Save(Path);
      Reset(Path);
      while Has_Next(Path) loop
        E := Next(Path);
        I := Index_Of(From(E));
        J := Index_Of(To(E));
        D := Value(E);
        Put_Line("  (" & I2S(I) & ", " & I2S(J) & ") : " & D2Se0(D, 2));
      end loop;
      Free(Path);
    end if;
  end Put_Shortest_Path;

  Gr: Graph;
  V: Vertex;
  P, P2: PDoubles;
  Dists: PDoubless;
  Preds: PIntegerss;

begin

  if Argument_Count /= 1 then
    Put_Line("Usage:  " & Command_Name & "  net_file_name");
    return;
  end if;

  Get_Graph(Argument(1), Gr);

  New_Line;
  Put_Line("-" ** (Argument(1)'Length + 6));
  Put_Line("-- " & Argument(1) & " --");
  Put_Line("-" ** (Argument(1)'Length + 6));
  New_Line;

  Put_Line("Number Of Edges : " & I2S(Number_Of_Edges(Gr)));
  Put_Line("Total Degree    : " & I2S(Total_Degree(Gr)));
  Put_Line("Average Degree  : " & D2S(Average_Degree(Gr), Aft => 4, Exp => 0));
  Put_Line("Degree From :");
  P := Degree_From(Gr); Put_Doubles(P, Aft => 1); Free(P);
  Put_Line("Degree To :");
  P := Degree_To(Gr); Put_Doubles(P, Aft => 1); Free(P);
  New_Line;

  Put_Line("Total Strength   : " & D2S(Total_Strength(Gr), Aft => 1, Exp => 0));
  Put_Line("Average Strength : " & D2S(Average_Strength(Gr), Aft => 4, Exp => 0));
  Put_Line("Strength From :");
  P := Strength_From(Gr); Put_Doubles(P, Aft => 1); Free(P);
  Put_Line("Strength To :");
  P := Strength_To(Gr); Put_Doubles(P, Aft => 1); Free(P);
  New_Line;

  Put_Line("Total Self-loops Strength : " & D2S(Total_Self_Loops_Strength(Gr), Aft => 4, Exp => 0));
  Put_Line("Self-loop :");
  P := Self_Loop(Gr); Put_Doubles(P, Aft => 1); Free(P);
  New_Line;

  Put_Line("Average Path Length (weighted)   : " & D2S(Average_Path_Length(Gr), Aft => 4, Exp => 0));
  Put_Line("Average Path Length (unweighted) : " & D2S(Average_Path_Length(Gr, Weighted => False), Aft => 4, Exp => 0));
  Put_Line("Average Path Length From (weighted)   :");
  P := Average_Path_Length(Gr, Ld => From_Links); Put_Doubles(P, Aft => 4); Free(P);
  Put_Line("Average Path Length To   (weighted)   :");
  P := Average_Path_Length(Gr, Ld => To_Links); Put_Doubles(P, Aft => 4); Free(P);
  Put_Line("Average Path Length From (unweighted) :");
  P := Average_Path_Length(Gr, Weighted => False, Ld => From_Links); Put_Doubles(P, Aft => 4); Free(P);
  Put_Line("Average Path Length To   (unweighted) :");
  P := Average_Path_Length(Gr, Weighted => False, Ld => To_Links); Put_Doubles(P, Aft => 4); Free(P);
  New_Line;

  Put_Line("Diameter (weighted)   : " & D2S(Diameter(Gr), Aft => 1, Exp => 0));
  Put_Line("Diameter (unweighted) : " & D2S(Diameter(Gr, Weighted => False), Aft => 1, Exp => 0));
  Put_Line("Maximum Path Length (weighted)   :");
  P := Maximum_Path_Length(Gr); Put_Doubles(P, Aft => 1); Free(P);
  Put_Line("Maximum Path Length (unweighted) :");
  P := Maximum_Path_Length(Gr, Weighted => False); Put_Doubles(P, Aft => 1); Free(P);
  New_Line;

  Put_Line("Efficiency (weighted)   : " & D2S(Efficiency(Gr), Aft => 6, Exp => 0));
  Put_Line("Efficiency (unweighted) : " & D2S(Efficiency(Gr, Weighted => False), Aft => 6, Exp => 0));
  Put_Line("Efficiency (weighted)   :");
  P := Efficiency(Gr); Put_Doubles(P, Aft => 6); Free(P);
  Put_Line("Efficiency (unweighted) :");
  P := Efficiency(Gr, Weighted => False); Put_Doubles(P, Aft => 6); Free(P);
  New_Line;

  Put_Line("Average Clustering Coefficient : " & D2S(Average_Clustering_Coefficient(Gr, Weighted => True), Aft => 6, Exp => 0));
  Put_Line("Clustering Coefficient :");
  P := Clustering_Coefficient(Gr, Weighted => True); Put_Doubles(P, Aft => 6); Free(P);
  New_Line;

  Put_Line("Vertex Betweenness :");
  P := Vertex_Betweenness(Gr); Put_Doubles(P, Aft => 6); Free(P);
  New_Line;

  Put_Line("Assortativity                        : " & D2S(Assortativity(Gr, Weighted => False), Aft => 6, Exp => 0));
  Put_Line("Assortativity Error                  : " & D2S(Assortativity_Error(Gr, Weighted => False), Aft => 6, Exp => 0));
  Put_Line("Weighted Assortativity               : " & D2S(Assortativity(Gr, Weighted => True), Aft => 6, Exp => 0));
  Put_Line("Weighted Assortativity Error         : " & D2S(Assortativity_Error(Gr, Weighted => True), Aft => 6, Exp => 0));
  P := Degree_From(Gr); P2 := Degree_To(Gr);
  Put_Line("Degree-Degree Correlation            : " & D2S(Linked_Nodes_Correlation(Gr, P, P2, Weighted => False), Aft => 6, Exp => 0));
  Put_Line("Degree-Degree Correlation Error      : " & D2S(Linked_Nodes_Correlation_Error(Gr, P, P2, Weighted => False), Aft => 6, Exp => 0));
  Free(P); Free(P2);
  P := Strength_From(Gr); P2 := Strength_To(Gr);
  Put_Line("Strength-Strength Correlation        : " & D2S(Linked_Nodes_Correlation(Gr, P, P2, Weighted => True), Aft => 6, Exp => 0));
  Put_Line("Strength-Strength Correlation Error  : " & D2S(Linked_Nodes_Correlation_Error(Gr, P, P2, Weighted => True), Aft => 6, Exp => 0));
  Free(P); Free(P2);
  P := Degree_From(Gr); P2 := Clustering_Coefficient(Gr, Weighted => True);
  Put_Line("Degree-Clustering Correlation        : " & D2S(Pearson_Correlation(P, P2), Aft => 6, Exp => 0));
  Put_Line("Degree-Clustering Correlation Error  : " & D2S(Pearson_Correlation_Error(P, P2), Aft => 6, Exp => 0));
  Free(P); Free(P2);
  P := Degree_From(Gr); P2 := Vertex_Betweenness(Gr);
  Put_Line("Degree-Betweenness Correlation       : " & D2S(Pearson_Correlation(P, P2), Aft => 6, Exp => 0));
  Put_Line("Degree-Betweenness Correlation Error : " & D2S(Pearson_Correlation_Error(P, P2), Aft => 6, Exp => 0));
  Free(P); Free(P2);
  New_Line;

  P := Degree_To(Gr); P2 := Nearest_Neighbors_Average_From(Gr, P, Weighted => True);
  Put_Line("Weighted Average Nearest Neighbors From of Degree To : " & D2S(Average(P2), Aft => 6, Exp => 0));
  Put_Doubles(P2, Aft => 6);
  Free(P); Free(P2);
  New_Line;

  V := Get_Vertex(Gr, 1);
  Put_Line("Shortest Path Length From Node 1 (weighted)   :");
  P := Shortest_Path_Length(V, Ld => From_Links); Put_Doubles(P, Aft => 4); Free(P);
  Put_Line("Shortest Path Length To   Node 1 (weighted)   :");
  P := Shortest_Path_Length(V, Ld => To_Links); Put_Doubles(P, Aft => 4); Free(P);
  Put_Line("Shortest Path Length From Node 1 (unweighted) :");
  P := Shortest_Path_Length(V, Weighted => False, Ld => From_Links); Put_Doubles(P, Aft => 4); Free(P);
  Put_Line("Shortest Path Length To   Node 1 (unweighted) :");
  P := Shortest_Path_Length(V, Weighted => False, Ld => To_Links); Put_Doubles(P, Aft => 4); Free(P);
  New_Line;


  Shortest_Paths(Gr, Dists, Preds);
  for I in 1..Number_Of_Vertices(Gr) loop
    Put_Shortest_Path(Gr, Dists, Preds, 1, I);
  end loop;
  Free(Dists);
  Free(Preds);

  Free(Gr);

end Graphs_Properties_Test;
