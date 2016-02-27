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


-- @filename Convert_Clu_To_Lol.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/03/2007
-- @revision 26/02/2016
-- @brief Convert a List of Lists in a file from Clu to Lol format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Pajek_IO; use Pajek_IO;

procedure Convert_Clu_To_Lol is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2016 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Convert a file with a partition in Pajek format (*.clu)       ==");
    Put_Line("== into a file with a partition in Lol format                    ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Fn_Lol: Unbounded_String;
  Fn_Clu: Unbounded_String;
  Lol: List_Of_Lists;
  Sorted: Boolean;

begin
  Put_Info;

  if Argument_Count = 2 then
    Fn_Clu := To_Unbounded_String(Argument(1));
    Fn_Lol := To_Unbounded_String(Argument(2));
    Sorted := False;
  elsif Argument_Count = 3 then
    Fn_Clu := To_Unbounded_String(Argument(1));
    Fn_Lol := To_Unbounded_String(Argument(2));
    Sorted := True;
  else
    Put_Line("Usage:  " & Command_Name & "  clu_file_name  lol_file_name  [ sorted ]");
    New_Line;
    Put_Line("   clu_file_name :  name of the input partition file in Pajek format (*.clu)");
    New_Line;
    Put_Line("   lol_file_name :  name of the output partition file in Lol format");
    New_Line;
    Put_Line("   sorted        :  any string as 3rd parameter produces a sorted List of Lists");
    Put_Line("                      communities sorted by decreasing size");
    Put_Line("                      elements of each community sorted by index");
    return;
  end if;

  Put_Line(To_String(Fn_Clu) & "  ->  " & To_String(Fn_Lol));
  Get_Partition(To_String(Fn_Clu), Lol);
  if Sorted then
    Sort_Lists(Lol);
    Sort_By_Size(Lol);
  end if;
  Put(To_String(Fn_Lol), Lol);

  Free(Lol);
end Convert_Clu_To_Lol;

