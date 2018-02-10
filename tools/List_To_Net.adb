-- Radalib, Copyright (c) 2018 by
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


-- @filename List_To_Net.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/10/2008
-- @revision 26/02/2016
-- @brief Convert a network in List format to Pajek format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Graphs_String; use Graphs_String;
with Pajek_IO; use Pajek_IO;
with Linked_Lists;

procedure List_To_Net is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2018 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Convert a file with the list of links of a graph into         ==");
    Put_Line("== a network file in Pajek format (*.net)                        ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  -- Hash Map to store names
  package Unbounded_Strings_Maps is new Ada.Containers.Hashed_Maps(
    Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
    Element_Type    => Integer,
    Hash            => Ada.Strings.Unbounded.Hash,
    Equivalent_Keys => Ada.Strings.Unbounded."=");
  use Unbounded_Strings_Maps;

  Names_Map: Map;

  -- Linked List to sort names
  package Us_Linked_Lists is new Linked_Lists(Ustring); use Us_Linked_Lists;

  Names_List: Linked_List;
  Numeric_Names: Boolean := False;

  function Lower(Left, Right: in Ustring) return Boolean is
  begin
    if Numeric_Names then
      return U2I(Left) < U2I(Right);
    else
      return Left < Right;
    end if;
  end Lower;

  -- Network types
  type Network_Type is (Auto, Directed, Undirected);
  Unknown_Network_Type_Error: exception;

  Net_Type: Network_Type := Auto;

  -- Local variables
  Fn_In: Ustring;
  Fn_Out: Ustring;
  F_In: File_Type;
  Us: Ustring;
  N: Natural;
  Gr: Graph;
  Dir: Boolean;
  F, T: Positive;

begin
  Put_Info;

  if Argument_Count /= 2 and Argument_Count /= 3 then
    Put_Line("Usage:  " & Command_Name & "  list_input_file  net_output_file  [ network_type ]");
    New_Line;
    Put_Line("   list_input_file : text file containing a list of links");
    New_Line;
    Put_Line("   net_output_file : name of the output network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   network_type    :  A | D | U");
    Put_Line("                        also lowercase symbols");
    Put_Line("                        also case-insensitive full names (Auto, Directed, Undirected)");
    Put_Line("                        A = Auto");
    Put_Line("                        D = Directed");
    Put_Line("                        U = Undirected");
    Put_Line("                        default => Auto");
    Put_Line("                        in Auto, if the Graph is Symmetric, the output is Undirected");
    Put_Line("                        exception raised if inconsistent values exist");
    return;
  elsif Argument_Count = 2 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    Net_Type := Auto;
  elsif Argument_Count = 3 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    if    To_Uppercase(Argument(3)) = "A" or To_Lowercase(Argument(3)) = "auto"       then
      Net_Type := Auto;
    elsif To_Uppercase(Argument(3)) = "D" or To_Lowercase(Argument(3)) = "directed"   then
      Net_Type := Directed;
    elsif To_Uppercase(Argument(3)) = "U" or To_Lowercase(Argument(3)) = "undirected" then
      Net_Type := Undirected;
    else
      raise Unknown_Network_Type_Error;
    end if;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  Open(F_In, In_File, U2S(Fn_In));

  Initialize(Names_List);

  -- Read and save names, and determine if they are numeric
  begin
    N := 0;
    Numeric_Names := True;
    while not End_Of_File(F_In) loop
      Comments_Skip(F_In);
      Line_Spaces_Skip(F_In);
      Line_Comment_Skip(F_In);
      if not End_Of_Line(F_In) then
        -- Vertex From
        Get_Word(F_In, Us);
        if not Contains(Names_Map, Us) then
          N := N + 1;
          Insert(Names_Map, Us, N);
          Add_Last(Us, Names_List);
          if Numeric_Names then
            Numeric_Names := Is_Integer(Us);
          end if;
        end if;
        -- Separator
        Separator_Skip(F_In);
        -- Vertex To
        Get_Word(F_In, Us);
        if not Contains(Names_Map, Us) then
          N := N + 1;
          Insert(Names_Map, Us, N);
          Add_Last(Us, Names_List);
          if Numeric_Names then
            Numeric_Names := Is_Integer(Us);
          end if;
        end if;
      end if;
      if not End_Of_File(F_In) then
        Skip_Line(F_In);
      end if;
    end loop;
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Current position at " & U2S(Fn_In) & ": " & I2S(Integer(Line(F_In))) & "," & I2S(Integer(Col(F_In))));
  end;

  -- Initialize network
  if Net_Type = Undirected then
    Dir := False;
  else
    Dir := True;
  end if;
  Initialize(Gr, N, Directed => Dir);

  -- Set vertices names
  Sort(Names_List, Lower'Access);
  Clear(Names_Map);
  N := 0;
  Save(Names_List);
  Reset(Names_List);
  while Has_Next(Names_List) loop
    N := N + 1;
    Us := Next(Names_List);
    Insert(Names_Map, Us, N);
    Set_Name(Get_Vertex(Gr, N), U2S(Us));
  end loop;
  Restore(Names_List);
  Free(Names_List);

  -- Read network
  Reset(F_In);
  begin
    while not End_Of_File(F_In) loop
      Comments_Skip(F_In);
      Line_Spaces_Skip(F_In);
      Line_Comment_Skip(F_In);
      if not End_Of_Line(F_In) then
        -- Vertex From
        Get_Word(F_In, Us);
        F := Element(Names_Map, Us);
        -- Separator
        Separator_Skip(F_In);
        -- Vertex To
        Get_Word(F_In, Us);
        T := Element(Names_Map, Us);
        -- Separator and weight
        Us := S2U("1");
        if Separator_Skip(F_In) then
          Line_Spaces_Skip(F_In);
          Line_Comment_Skip(F_In);
          if not End_Of_Line(F_In) then
            Get_Word(F_In, Us);
          end if;
        end if;
        -- Add edge
        Add_Edge_Unchecked(Get_Vertex(Gr, F), Get_Vertex(Gr, T), Us);
      end if;
      if not End_Of_File(F_In) then
        Skip_Line(F_In);
      end if;
    end loop;
    Restore_Consistency(Gr);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Current position at " & U2S(Fn_In) & ": " & I2S(Integer(Line(F_In))) & "," & I2S(Integer(Col(F_In))));
      return;
  end;
  Close(F_In);

  -- Set final network Type
  if Net_Type = Auto then
    if Is_Symmetric(Gr) then
      To_Undirected(Gr);
    end if;
  end if;

  -- Write network
  Put_Line("  " & I2S(N) & " vertices");
  if Numeric_Names then
    Put_Line("  Numeric names");
  else
    Put_Line("  Non-numeric names");
  end if;
  if Net_Type = Auto then
    Put("  Auto  =>");
  end if;
  if Is_Directed(Gr) then
    Put_Line("  Directed network");
  else
    Put_Line("  Undirected network");
  end if;

  Put_Graph(U2S(Fn_Out), Gr);
  Free(Gr);

end List_To_Net;
