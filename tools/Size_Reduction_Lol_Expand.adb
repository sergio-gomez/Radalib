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


-- @filename Size_Reduction_Lol_Expand.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/11/2008
-- @revision 26/02/2016
-- @brief Converts a partition of a reduced graph into a partition of the original graph

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Utils; use Utils;
with Graphs_Float_Algorithms; use Graphs_Float_Algorithms;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

procedure Size_Reduction_Lol_Expand is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2018 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Expansion of a partition of a size-reduced network into       ==");
    Put_Line("== a partition of the original network                           ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  type Header_Mode_Type is (Copy_Header, No_Header, Separator_Header);

  Unknown_Header_Mode: exception;

  function To_Header_Mode(S: in String) return Header_Mode_Type is
  begin
    if    To_Uppercase(S) = "CH" or To_Lowercase(S) = "copy_header"      then
      return Copy_Header;
    elsif To_Uppercase(S) = "NH" or To_Lowercase(S) = "no_header"        then
      return No_Header;
    elsif To_Uppercase(S) = "SH" or To_Lowercase(S) = "separator_header" then
      return Separator_Header;
    else
      raise Unknown_Header_Mode;
    end if;
  end To_Header_Mode;

  Default_Header_Mode   : constant Header_Mode_Type := No_Header;
  Default_Header_Lines  : constant := 0;
  Separator_Line: constant String := "========================";

  Header_Mode: Header_Mode_Type := Default_Header_Mode;
  Header_Lines: Natural  := Default_Header_Lines;

  Lol_Ren_Name, Ren_Name, Lol_Name: Ustring;
  Lol_Ren, Ren, Lol: List_Of_Lists;
  F_In, F_Out: File_Type;

begin
  Put_Info;

  if Argument_Count = 3 then
    Lol_Ren_Name   := S2U(Argument(1));
    Ren_Name       := S2U(Argument(2));
    Lol_Name       := S2U(Argument(3));
    Header_Lines  := Default_Header_Lines;
    Header_Mode   := Default_Header_Mode;
  elsif Argument_Count = 5 then
    Lol_Ren_Name   := S2U(Argument(1));
    Ren_Name       := S2U(Argument(2));
    Lol_Name       := S2U(Argument(3));
    Header_Lines  := S2I(Argument(4));
    Header_Mode   := To_Header_Mode(Argument(5));
  else
    Put_Line("Usage:  " & Command_Name & "  reduced_lol_name  reducing_lol_name  expanded_lol_name  [ header_lines  header_mode ]");
    New_Line;
    Put_Line("   reduced_lol_name  : name of the input partition file in Lol format of a size-reduced network");
    Put_Line("                         the file may contain many partitions");
    New_Line;
    Put_Line("   reducing_lol_name : name of the input partition file in Lol format which has reduced a network");
    New_Line;
    Put_Line("   expanded_lol_name : name of the output partition file in Lol format");
    Put_Line("                         corresponds to the expansion of the partition of the size-reduced network");
    New_Line;
    Put_Line("   header_lines      : number of lines of the header before a partition in Lol format");
    Put_Line("                         non-negative integer");
    Put_Line("                         default => " & I2S(Default_Header_Lines));
    Put_Line("                         ignored for partitions in Pajek format (*.clu)");
    New_Line;
    Put_Line("   header_mode       : CH | NH | SH");
    Put_Line("                         also lowercase symbols");
    Put_Line("                         also case-insensitive full names (Copy_Header, ...)");
    Put_Line("                         CH = Copy_Header");
    Put_Line("                         NH = No_Header");
    Put_Line("                         SH = Separator_Header");
    Put_Line("                         default => " & Capitalize(Header_Mode_Type'Image(Default_Header_Mode)));
    return;
  end if;

  Put_Line(U2S(Lol_Ren_Name) & " + " & U2S(Ren_Name) & "  ->  " & U2S(Lol_Name));

  Get(U2S(Ren_Name), Ren, Header_Lines);
  Open(F_In, In_File, U2S(Lol_Ren_Name));
  Create(F_Out, Out_File, U2S(Lol_Name));

  loop
    case Header_Mode is
      when Copy_Header =>
        for I in 1..Header_Lines loop
          Put_Line(F_Out, Get_Line(F_In));
        end loop;
        Get(F_In, Lol_Ren, 0);
      when No_Header =>
        Get(F_In, Lol_Ren, Header_Lines);
      when Separator_Header =>
        Get(F_In, Lol_Ren, Header_Lines);
        Put_Line(F_Out, Separator_Line);
    end case;
    while End_Of_Line(F_In) and not End_Of_File(F_In) loop
      Skip_Line(F_In);
    end loop;

    Unrenormalize_List_Of_Lists(Lol_Ren, Ren, Lol);
    Sort_Lists(Lol);
    Sort_By_Size(Lol);
    Put(F_Out, Lol);
    Free(Lol_Ren);
    Free(Lol);

    exit when End_Of_File(F_In);
  end loop;

  Close(F_Out);
  Close(F_In);
  Free(Ren);

end Size_Reduction_Lol_Expand;
