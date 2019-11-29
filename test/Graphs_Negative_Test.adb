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


-- @filename Graphs_Negative_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 07/07/2008
-- @revision 26/10/2014
-- @brief Test of Graphs with Negative Weights

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Utils.IO_Double; use Utils.IO_Double;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Properties_D; use Graphs_Double_Properties_D;
with Pajek_IO; use Pajek_IO;
with Statistics_Double; use Statistics_Double;

procedure Graphs_Negative_Test is

  function To_S(Tot, Pos, Neg: in Integer) return String is
  begin
    return "(a) " & I2S(Tot) & "   (+) " & I2S(Pos) & "   (-) " & I2S(Neg);
  end To_S;

  function To_S(Tot, Pos, Neg: in Double; Aft: in Field := Default_Float_Aft) return String is
  begin
    return "(a) " & D2S(Tot, Aft => Aft, Exp => 0) & "   (+) " & D2S(Pos, Aft => Aft, Exp => 0) & "   (-) " & D2S(Neg, Aft => Aft, Exp => 0);
  end To_S;

  procedure Put_Doubles(Ppos: in PDoubles; Aft: in Field := Default_Float_Aft; One_Line: in Boolean := True) is
    Show_Index: constant Boolean := not One_Line;
  begin
    if One_Line then
      Put("   (+)");
      for I in Ppos'Range loop
        if Show_Index then
          Put("   " & Right_Justify(I2S(I), 2) & ": ");
        else
          Put("   ");
        end if;
        Put(D2S(Ppos(I), Aft => Aft, Exp => 0));
      end loop;
      New_Line;
    else
      for I in Ppos'Range loop
        if Show_Index then
          Put("  " & Right_Justify(I2S(I), 2) & ": ");
        else
          Put("  ");
        end if;
        Put("  (+) ");
        Put(Ppos(I), Fore => 0, Aft => Aft, Exp => 0);
        New_Line;
      end loop;
    end if;
  end Put_Doubles;

  procedure Put_Doubles(Ptot, Ppos, Pneg: in PDoubles; Aft: in Field := Default_Float_Aft; Three_Lines: in Boolean := True) is
    Show_Index: constant Boolean := not Three_Lines;
    Pd: constant array(1..3) of PDoubles := (Ptot, Ppos, Pneg);
    S: constant array(1..3) of String(1..3) := ("(a)", "(+)", "(-)");
  begin
    if Three_Lines then
      for T in Pd'Range loop
        Put("   " & S(T));
        for I in Ptot'Range loop
          if Show_Index then
            Put("   " & Right_Justify(I2S(I), 2) & ": ");
          else
            Put("   ");
          end if;
          Put(D2S(Pd(T)(I), Aft => Aft, Exp => 0));
        end loop;
        New_Line;
      end loop;
    else
      for I in Ptot'Range loop
        if Show_Index then
          Put("  " & Right_Justify(I2S(I), 2) & ": ");
        else
          Put("  ");
        end if;
        for T in Pd'Range loop
          Put(S(T) & " " & D2S(Pd(T)(I), Aft => Aft, Exp => 0) & "   ");
        end loop;
        New_Line;
      end loop;
    end if;
  end Put_Doubles;

  Gr: Graph;
  Toti, Posi, Negi: Integer;
  Tot, Pos, Neg: Double;
  Ptot, Ppos, Pneg, Ptot2, Ppos2, Pneg2: PDoubles;

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

  Put_Line("Number Of Edges: " & I2S(Number_Of_Edges(Gr)));

  Toti := D2I(Total_Degree(Gr, All_Links));
  Posi := D2I(Total_Degree(Gr, Positive_Links));
  Negi := D2I(Total_Degree(Gr, Negative_Links));
  Put_Line("Total Degree   : " & To_S(Toti, Posi, Negi));

  Tot := Average_Degree(Gr, All_Links);
  Pos := Average_Degree(Gr, Positive_Links);
  Neg := Average_Degree(Gr, Negative_Links);
  Put_Line("Average Degree : " & To_S(Tot, Pos, Neg, Aft => 4));

  Ptot := Degree_From(Gr, All_Links);
  Ppos := Degree_From(Gr, Positive_Links);
  Pneg := Degree_From(Gr, Negative_Links);
  Put_Line("Degree From:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 1, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  Ptot := Degree_To(Gr, All_Links);
  Ppos := Degree_To(Gr, Positive_Links);
  Pneg := Degree_To(Gr, Negative_Links);
  Put_Line("Degree To:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 1, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  New_Line;

  Tot := Total_Strength(Gr, All_Links);
  Pos := Total_Strength(Gr, Positive_Links);
  Neg := Total_Strength(Gr, Negative_Links);
  Put_Line("Total Strength  : " & To_S(Tot, Pos, Neg, Aft => 1));

  Tot := Average_Strength(Gr, All_Links);
  Pos := Average_Strength(Gr, Positive_Links);
  Neg := Average_Strength(Gr, Negative_Links);
  Put_Line("Average Strength: " & To_S(Tot, Pos, Neg, Aft => 1));

  Ptot := Strength_From(Gr, All_Links);
  Ppos := Strength_From(Gr, Positive_Links);
  Pneg := Strength_From(Gr, Negative_Links);
  Put_Line("Strength From:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 1, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  Ptot := Strength_To(Gr, All_Links);
  Ppos := Strength_To(Gr, Positive_Links);
  Pneg := Strength_To(Gr, Negative_Links);
  Put_Line("Strength To:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 1, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  New_Line;

  Tot := Total_Self_Loops_Strength(Gr, All_Links);
  Pos := Total_Self_Loops_Strength(Gr, Positive_Links);
  Neg := Total_Self_Loops_Strength(Gr, Negative_Links);
  Put_Line("Total Self-loops Strength: " & To_S(Tot, Pos, Neg, Aft => 1));

  Ptot := Self_Loop(Gr, All_Links);
  Ppos := Self_Loop(Gr, Positive_Links);
  Pneg := Self_Loop(Gr, Negative_Links);
  Put_Line("Self-loop:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 1, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  New_Line;

  Tot := Average_Clustering_Coefficient(Gr, False, All_Links);
  Pos := Average_Clustering_Coefficient(Gr, False, Positive_Links);
  Neg := Average_Clustering_Coefficient(Gr, False, Negative_Links);
  Put_Line("Average Clustering Coefficient: " & To_S(Tot, Pos, Neg, Aft => 4));

  Ptot := Clustering_Coefficient(Gr, False, All_Links);
  Ppos := Clustering_Coefficient(Gr, False, Positive_Links);
  Pneg := Clustering_Coefficient(Gr, False, Negative_Links);
  Put_Line("Clustering Coefficient:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 4, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  New_Line;

  Tot := Average_Clustering_Coefficient(Gr, True, All_Links);
  Pos := Average_Clustering_Coefficient(Gr, True, Positive_Links);
  Neg := Average_Clustering_Coefficient(Gr, True, Negative_Links);
  Put_Line("Average Weighted Clustering Coefficient: " & To_S(Tot, Pos, Neg, Aft => 4));

  Ptot := Clustering_Coefficient(Gr, True, All_Links);
  Ppos := Clustering_Coefficient(Gr, True, Positive_Links);
  Pneg := Clustering_Coefficient(Gr, True, Negative_Links);
  Put_Line("Weighted Clustering Coefficient:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 4, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg);

  New_Line;

  Tot := Assortativity(Gr, False, All_Links);
  Pos := Assortativity(Gr, False, Positive_Links);
  Neg := Assortativity(Gr, False, Negative_Links);
  Put_Line("Assortativity                 : " & To_S(Tot, Pos, Neg, Aft => 6));

  Tot := Assortativity(Gr, True, All_Links);
  Pos := Assortativity(Gr, True, Positive_Links);
  Neg := Assortativity(Gr, True, Negative_Links);
  Put_Line("Weighted Assortativity        : " & To_S(Tot, Pos, Neg, Aft => 6));

  Ptot := Degree_From(Gr, All_Links);      Ptot2 := Degree_To(Gr, All_Links);
  Ppos := Degree_From(Gr, Positive_Links); Ppos2 := Degree_To(Gr, Positive_Links);
  Pneg := Degree_From(Gr, Negative_Links); Pneg2 := Degree_To(Gr, Negative_Links);
  Tot := Linked_Nodes_Correlation(Gr, Ptot, Ptot2, False, All_Links);
  Pos := Linked_Nodes_Correlation(Gr, Ppos, Ppos2, False, Positive_Links);
  Neg := Linked_Nodes_Correlation(Gr, Pneg, Pneg2, False, Negative_Links);
  Put_Line("Degree-Degree Correlation     : " & To_S(Tot, Pos, Neg, Aft => 6));
  Free(Ptot); Free(Ppos); Free(Pneg); Free(Ptot2); Free(Ppos2); Free(Pneg2);

  Ptot := Strength_From(Gr, All_Links);      Ptot2 := Strength_To(Gr, All_Links);
  Ppos := Strength_From(Gr, Positive_Links); Ppos2 := Strength_To(Gr, Positive_Links);
  Pneg := Strength_From(Gr, Negative_Links); Pneg2 := Strength_To(Gr, Negative_Links);
  Tot := Linked_Nodes_Correlation(Gr, Ptot, Ptot2, True, All_Links);
  Pos := Linked_Nodes_Correlation(Gr, Ppos, Ppos2, True, Positive_Links);
  Neg := Linked_Nodes_Correlation(Gr, Pneg, Pneg2, True, Negative_Links);
  Put_Line("Strength-Strength Correlation : " & To_S(Tot, Pos, Neg, Aft => 6));
  Free(Ptot); Free(Ppos); Free(Pneg); Free(Ptot2); Free(Ppos2); Free(Pneg2);

  Ptot := Degree_From(Gr, All_Links);      Ptot2 := Clustering_Coefficient(Gr, False, All_Links);
  Ppos := Degree_From(Gr, Positive_Links); Ppos2 := Clustering_Coefficient(Gr, False, Positive_Links);
  Pneg := Degree_From(Gr, Negative_Links); Pneg2 := Clustering_Coefficient(Gr, False, Negative_Links);
  Tot := Correlation(Ptot, Ptot2);
  Pos := Correlation(Ppos, Ppos2);
  Neg := Correlation(Pneg, Pneg2);
  Put_Line("Degree-Clustering Correlation : " & To_S(Tot, Pos, Neg, Aft => 6));
  Free(Ptot); Free(Ppos); Free(Pneg); Free(Ptot2); Free(Ppos2); Free(Pneg2);

  New_Line;

  Ptot2 := Degree_From(Gr, All_Links);      Ptot := Nearest_Neighbors_Average(Gr, Ptot2, False, From_Links, All_Links);
  Ppos2 := Degree_From(Gr, Positive_Links); Ppos := Nearest_Neighbors_Average(Gr, Ppos2, False, From_Links, Positive_Links);
  Pneg2 := Degree_From(Gr, Negative_Links); Pneg := Nearest_Neighbors_Average(Gr, Pneg2, False, From_Links, Negative_Links);
  Put_Line("Nearest Neighbors Average From of Degree To:");
  Put_Doubles(Ptot, Ppos, Pneg, Aft => 4, Three_Lines => True);
  Free(Ptot); Free(Ppos); Free(Pneg); Free(Ptot2); Free(Ppos2); Free(Pneg2);

  New_Line;

  if not Has_Links(Gr, Negative_Links) then
    Pos := Average_Path_Length(Gr);
    Put_Line("Average Path Length: " & D2S(Pos, Aft => 4, Exp => 0));

    Ppos := Average_Path_Length(Gr);
    Put_Line("Average Path Length:");
    Put_Doubles(Ppos, Aft => 4, One_Line => True);
    Free(Ppos);

    New_Line;

    Pos := Diameter(Gr);
    Put_Line("Diameter  : " & D2S(Pos, Aft => 1, Exp => 0));

    Ppos := Maximum_Path_Length(Gr);
    Put_Line("Maximum Path Length:");
    Put_Doubles(Ppos, Aft => 1, One_Line => True);
    Free(Ppos);

    New_Line;

    Pos := Efficiency(Gr);
    Put_Line("Efficiency: " & D2S(Pos, Aft => 6, Exp => 0));

    Put_Line("Efficiency:");
    Ppos := Efficiency(Gr);
    Put_Doubles(Ppos, Aft => 4, One_Line => True);
    Free(Ppos);

    New_Line;

    Ppos := Vertex_Betweenness(Gr);
    Put_Line("Vertex Betweenness:");
    Put_Doubles(Ppos, Aft => 4, One_Line => True);
    Free(Ppos);

    New_Line;
  end if;

  Free(Gr);

exception
  when E: others =>
    Put_Line(Exception_Information(E));
end Graphs_Negative_Test;
