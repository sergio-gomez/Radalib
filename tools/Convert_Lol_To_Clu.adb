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


-- @filename Convert_Lol_To_Clu.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 01/03/2007
-- @revision 14/01/2018
-- @brief Convert a List of Lists in a file from Lol to Clu format

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Convert_Lol_To_Clu is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2023 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Convert a file with a partition in Lol format into            ==");
    Put_Line("== a file with a partition in Pajek format (*.clu)               ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Default_Num_Skip_Lines: constant := 0;
  Num_Skip_Lines: Natural := Default_Num_Skip_Lines;
  Fn_Lol: Ustring;
  Fn_Clu: Ustring;
  Lol: List_Of_Lists;

begin
  Put_Info;

  if Argument_Count = 2 then
    Fn_Lol := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Num_Skip_Lines := Default_Num_Skip_Lines;
  elsif Argument_Count = 3 then
    Fn_Lol := S2U(Argument(1));
    Fn_Clu := S2U(Argument(2));
    Num_Skip_Lines := S2I(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & "  lol_file_name  clu_file_name  [ number_of_lines_to_skip ]");
    New_Line;
    Put_Line("   lol_file_name           :  name of the input partition file in Lol format");
    New_Line;
    Put_Line("   clu_file_name           :  name of the output partition file in Pajek format (*.clu)");
    New_Line;
    Put_Line("   number_of_lines_to_skip :  number of lines to skip at the beginning of the Lol file");
    Put_Line("                                non-negative integer");
    Put_Line("                                default => " & I2S(Default_Num_Skip_Lines));
    return;
  end if;

  Put_Line(U2S(Fn_Lol) & "  ->  " & U2S(Fn_Clu));

  Get(U2S(Fn_Lol), Lol, Num_Skip_Lines);
  Put_Partition(U2S(Fn_Clu), Lol);
  Free(Lol);
end Convert_Lol_To_Clu;
