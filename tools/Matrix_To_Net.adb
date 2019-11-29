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


-- @filename Matrix_To_Net.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/09/2006
-- @revision 26/02/2016
-- @brief Convert a network in Matrix format to Pajek format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Graphs_String; use Graphs_String;
with Pajek_IO; use Pajek_IO;

procedure Matrix_To_Net is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2019 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Convert a file with a graph in matrix form into               ==");
    Put_Line("== a network file in Pajek format (*.net)                        ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Default_No_Link: constant Ustring := S2U("0");
  Fn_In: Ustring;
  Fn_Out: Ustring;
  No_Link: Ustring;
  F_In: File_Type;
  Us: Ustring;
  N_Rows, N_Cols, N, I, J: Natural;
  Names_Col1, Names_Row1: Boolean := False;
  Gr: Graph;

begin
  Put_Info;

  if Argument_Count = 2 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    No_Link := Default_No_Link;
  elsif Argument_Count = 3 then
    Fn_In := S2U(Argument(1));
    Fn_Out := S2U(Argument(2));
    No_Link := S2U(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & " matrix_input_file  net_output_file  [ no_link_string ]");
    New_Line;
    Put_Line("   matrix_input_file : text file containing an adjacency or weights matrix");
    New_Line;
    Put_Line("   net_output_file   : name of the output network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   no_link_string    : string used to identify unexistent links within the matrix file");
    Put_Line("                         default => " & U2S(Default_No_Link));
    return;
  end if;

  Put_Line(U2S(Fn_In) & "  ->  " & U2S(Fn_Out));
  if not File_Exists(U2S(Fn_In)) then
    Put_Line("Error: Input file not found!");
    return;
  end if;

  Open(F_In, In_File, U2S(Fn_In));

  -- Determine network size and names location (none, columns or rows)
  N_Cols := 0;
  Comments_Skip(F_In);
  while not End_Of_Line(F_In) loop
    Line_Spaces_Skip(F_In);
    Line_Comment_Skip(F_In);
    if not End_Of_Line(F_In) then
      Get_Word(F_In, Us);
      N_Cols := N_Cols + 1;
      Separator_Skip(F_In);
    end if;
  end loop;
  if not End_Of_File(F_In) then
    Skip_Line(F_In);
  end if;
  N_Rows := 1;
  while not End_Of_File(F_In) loop
    Comments_Skip(F_In);
    Line_Spaces_Skip(F_In);
    Line_Comment_Skip(F_In);
    if not End_Of_Line(F_In) then
      N_Rows := N_Rows + 1;
    end if;
    if not End_Of_File(F_In) then
      Skip_Line(F_In);
    end if;
  end loop;
  if N_Cols = N_Rows then
    Names_Col1 := False;
    Names_Row1 := False;
    N := N_Cols;
  elsif N_Cols = N_Rows + 1 then
    Names_Col1 := True;
    Names_Row1 := False;
    N := N_Rows;
  elsif N_Cols + 1 = N_Rows then
    Names_Col1 := False;
    Names_Row1 := True;
    N := N_Cols;
  else
    Put_Line("Error: incompatible matrix size: " & I2S(N_Rows) & " x " & I2S(N_Cols));
    return;
  end if;

  Initialize(Gr, N, Directed => True);
  Put_Line("  N: " & I2S(N));
  if Names_Col1 then
    Put_Line("  Names in first column");
  end if;
  if Names_Row1 then
    Put_Line("  Names in first row");
  end if;

  -- Read network
  Reset(F_In);
  begin
    -- Read names in first row
    if Names_Row1 then
      Comments_Skip(F_In);
      J := 1;
      while not End_Of_Line(F_In) loop
        Line_Spaces_Skip(F_In);
        Line_Comment_Skip(F_In);
        if not End_Of_Line(F_In) then
          Get_Word(F_In, Us);
          Set_Name(Get_Vertex(Gr, J), U2S(Us));
          Separator_Skip(F_In);
          J := J + 1;
        end if;
      end loop;
      Skip_Line(F_In);
    end if;
    -- Read links
    I := 1;
    while not End_Of_File(F_In) loop
      Comments_Skip(F_In);
      -- Read names in first column
      if Names_Col1 then
        Line_Spaces_Skip(F_In);
        Line_Comment_Skip(F_In);
        if not End_Of_Line(F_In) then
          Get_Word(F_In, Us);
          Set_Name(Get_Vertex(Gr, I), U2S(Us));
          Separator_Skip(F_In);
        end if;
      end if;
      J := 1;
      while not End_Of_Line(F_In) loop
        Line_Spaces_Skip(F_In);
        Line_Comment_Skip(F_In);
        if not End_Of_Line(F_In) then
          Get_Word(F_In, Us);
          if Us /= No_Link then
            Add_Edge(Get_Vertex(Gr, I), Get_Vertex(Gr, J), Us);
          end if;
          Separator_Skip(F_In);
          J := J + 1;
        end if;
      end loop;
      if not End_Of_File(F_In) then
        Skip_Line(F_In);
      end if;
      I := I + 1;
    end loop;
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Current position at " & U2S(Fn_In) & ": " & I2S(Integer(Line(F_In))) & "," & I2S(Integer(Col(F_In))));
      Put_Line("Current link: " & I2S(I) & " -> " & I2S(J));
      return;
  end;
  Close(F_In);

  if Is_Symmetric(Gr) then
    To_Undirected(Gr);
    Put_Line("  Undirected network");
  else
    Put_Line("  Directed network");
  end if;

  -- Write network
  Put_Graph(U2S(Fn_Out), Gr);
  Free(Gr);

end Matrix_To_Net;
