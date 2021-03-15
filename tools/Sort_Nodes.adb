-- Radalib, Copyright (c) 2021 by
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


-- @filename Sort_Nodes.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/04/2013
-- @revision 06/03/2018
-- @brief Sort nodes randomly or according to degree

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with Arrays_Utils_Float; use Arrays_Utils_Float;
with Graphs_String; use Graphs_String;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Sort_Nodes is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2021 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Sort nodes randomly or according to degree                    ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  type Sort_Direction_Type is (Ascending, Descending, Random);

  Unknown_Sort_Direction_Error: exception;

  function Get_Sort_Direction_Type(S: in String) return Sort_Direction_Type is
  begin
    if    To_Uppercase(S) = "A" or To_Lowercase(S) = "asc"  or To_Lowercase(S) = "ascending"  then
      return Ascending;
    elsif To_Uppercase(S) = "D" or To_Lowercase(S) = "desc" or To_Lowercase(S) = "descending" then
      return Descending;
    elsif To_Uppercase(S) = "R" or To_Lowercase(S) = "rand" or To_Lowercase(S) = "random"     then
      return Random;
    else
      raise Unknown_Sort_Direction_Error;
    end if;
  end Get_Sort_Direction_Type;

  function To_String(Sd: in Sort_Direction_Type) return String is
  begin
    return Capitalize(Sort_Direction_Type'Image(Sd));
  end To_String;

  Default_Sort_Direction: constant Sort_Direction_Type := Ascending;

  Fn_Net: UString;
  Fn_Out: UString;
  Sort_Direction: Sort_Direction_Type := Default_Sort_Direction;
  Gr, Gr_Sorted: Graph;
  G: Generator;
  N: Natural;
  Key: PFloats;
  Pos_Orig, Pos_Sorted: PIntegers;
  V, Vfs, Vts: Vertex;
  El: Edges_List;
  E: Edge;
  J: Positive;

begin
  Put_Info;

  pragma Warnings(Off, El);

  if Argument_Count = 2 then
    Fn_Net := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Sort_Direction := Default_Sort_Direction;
  elsif Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Sort_Direction := Get_Sort_Direction_Type(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  sorted_net_name  [ sort_direction ]");
    New_Line;
    Put_Line("   sort_direction :  A | D | R");
    Put_Line("                       also lowercase symbols");
    Put_Line("                       also case-insensitive full names (Ascending, ...)");
    Put_Line("                       A = Asc  = Ascending");
    Put_Line("                       D = Desc = Descending");
    Put_Line("                       R = Rand = Random");
    Put_Line("                       default => " & To_String(Default_Sort_Direction));
    return;
  end if;

  Reset(G);

  Put_Line(U2S(Fn_Net) & " -> " & U2S(Fn_Out));
  Put_Line(Capitalize(Sort_Direction_Type'Image(Sort_Direction)) & " sort");

  Get_Graph(U2S(Fn_Net), Gr);

  N := Number_Of_Vertices(Gr);
  Key := Alloc(1, N);

  for I in 1..N loop
    V := Get_Vertex(Gr, I);
    case Sort_Direction is
      when Ascending =>
        Key(I) := Float(Degree_From(V));
      when Descending =>
        Key(I) := -Float(Degree_From(V));
      when Random =>
        Key(I) := Random(G);
    end case;
  end loop;
  Sort(Key, Pos_Orig);

  Initialize(Gr_Sorted, N, Is_Directed(Gr));

  Pos_Sorted := Alloc(1, N);
  for I in 1..N loop
    Vfs := Get_Vertex(Gr_Sorted, I);
    V := Get_Vertex(Gr, Pos_Orig(I));
    Set_Name(Vfs, Get_Name(V));
    Set_Tag(Vfs, Get_Tag(V));
    Pos_Sorted(Pos_Orig(I)) := I;
  end loop;

  for I in 1..N loop
    El := Edges_From(Get_Vertex(Gr, I));
    Vfs := Get_Vertex(Gr_Sorted, Pos_Sorted(I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      J := Index_Of(To(E));
      Vts := Get_Vertex(Gr_Sorted, Pos_Sorted(J));
      Add_Edge(Vfs, Vts, Value(E));
    end loop;
    Restore(El);
  end loop;

  Put_Graph(U2S(Fn_Out), Gr_Sorted);

  Free(Key);
  Free(Pos_Orig);
  Free(Pos_Sorted);
  Free(Gr);
  Free(Gr_Sorted);
end Sort_Nodes;
