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


-- @filename Multilayer_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/10/2014
-- @revision 14/01/2015
-- @brief Test of Graphs.Multilayer and Multilayer_IO packages

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Multilayer; use Graphs_Double_Multilayer;
with Multilayer_IO; use Multilayer_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Multilayer_Test is

  procedure Put(Gr: in Graph; Verbose: in Boolean := True) is
    Vf, Vt: Vertex;
    E: Edge;
    El: Edges_List;
  begin
    pragma Warnings(Off, El);
    if Verbose then
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
    end if;
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
  end Put;

  procedure Put(Mpx: in Multiplex; Verbose: in Boolean := True) is
    Grs: PGraphs;
  begin
    Grs := Get_Layers(Mpx);
    Put_Line("Nodes: " & I2S(Number_Of_Vertices(Mpx)));
    if Verbose then
      for I in 1..Number_Of_Vertices(Mpx) loop
        Put_Line("  " & I2S(I) & ": " & Get_Name(Get_Vertex(Grs(1), I)));
      end loop;
    end if;
    for L in Grs'Range loop
      Put_Line("---");
      Put_Line("Layer " & I2S(L) & ": " & U2S(Get_Layer_Name(Mpx, L)));
      Put_Line("---");
      Put(Grs(L), Verbose);
    end loop;
    Put_Line("------");
  end Put;

  Fn_In1 : constant String := "multiplex_in1.txt";
  Fn_In2 : constant String := "multiplex_in2.txt";
  Fn_Out1: constant String := "multiplex_out1d.txt";
  Fn_Out2: constant String := "multiplex_out1u.txt";
  Mpx: Multiplex;
  Gr: Graph;
  Index: PIntegers;

begin
  Put_Line("=========");
  Put_Line(Fn_In1 & " + directed -> " & Fn_Out1);
  Put_Line("---");
  Get_Multiplex(Fn_In1, Mpx, Directed => True);
  Put(Mpx, Verbose => True);
  Put_Multiplex(Fn_Out1, Mpx, Aft => 1, Exp => 0);
  Free(Mpx);

  New_Line;

  Put_Line("=========");
  Put_Line(Fn_In1 & " + undirected -> " & Fn_Out2);
  Put_Line("---");
  Get_Multiplex(Fn_In1, Mpx, Directed => False);
  Put(Mpx, Verbose => False);
  Put_Multiplex(Fn_Out2, Mpx, Aft => 1, Exp => 0);
  Free(Mpx);

  New_Line;

  Put_Line("=========");
  Put_Line(Fn_In2 & " + undirected -> " & Fn_Out2);
  Put_Line("---");
  Get_Multiplex(Fn_In2, Mpx, Directed => False);
  Put(Mpx, Verbose => False);

  Put_Line("Aggregated: all layers weighted");
  Gr := Aggregate_Layers(Mpx);
  Put_Line("---");
  Put(Gr, Verbose => False);
  Put_Line("------");
  Free(Gr);

  Put_Line("Aggregated: all layers unweighted");
  Gr := Aggregate_Layers(Mpx, Weighted => False);
  Put_Line("---");
  Put(Gr, Verbose => False);
  Put_Line("------");
  Free(Gr);

  Put_Line("Aggregated: layers L1+L2+L4 weighted");
  Index := Alloc(1, 3);
  Index.all := (1, 2, 4);
  Gr := Aggregate_Layers(Mpx, Index);
  Put_Line("---");
  Put(Gr, Verbose => False);
  Put_Line("------");
  Free(Gr);

  Put_Line("Aggregated: layers L3+L5 weighted");
  Index := Alloc(1, 2);
  Index.all := (3, 5);
  Gr := Aggregate_Layers(Mpx, Index);
  Put_Line("---");
  Put(Gr, Verbose => False);
  Put_Line("------");
  Free(Gr);

  Free(Mpx);
end Multilayer_Test;
