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


-- @filename Reformat_Partitions.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/05/2008
-- @revision 26/02/2016
-- @brief Reformat Partition file changing indices to Vertices Names

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Pajek_IO; use Pajek_IO;
with Graphs_String; use Graphs_String;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

procedure Reformat_Partitions is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2019 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Reformat partitions in Lol or Pajek format changing           ==");
    Put_Line("== nodes' indices by nodes' names, and grouping in columns       ==");
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
  Default_Group_By      : constant := 1;
  Default_Justify_Width : constant := 1;
  Default_Skip_Size     : constant := 0;

  Separator_Line: constant String := "========================";

  Header_Mode   : Header_Mode_Type := Default_Header_Mode;
  Header_Lines  : Natural  := Default_Header_Lines;
  Group_By      : Positive := Default_Group_By;
  Justify_Width : Positive := Default_Justify_Width;
  Skip_Size     : Natural  := Default_Skip_Size;

  Fn_Gr : Ustring;
  Fn_Lol: Ustring;
  Fn_Out: Ustring;
  Is_Clu: Boolean;
  F_In, F_Out: File_Type;
  Count, Num_Small, I: Natural;
  Gr: Graph;
  V: Vertex;
  Lol: List_Of_Lists;
  L: List;

begin
  Put_Info;

  pragma Warnings(Off, L);

  if Argument_Count = 3 then
    Fn_Gr  := S2U(Argument(1));
    Fn_Lol := S2U(Argument(2));
    Fn_Out := S2U(Argument(3));
    Header_Lines  := Default_Header_Lines;
    Header_Mode   := Default_Header_Mode;
    Group_By      := Default_Group_By;
    Justify_Width := Default_Justify_Width;
    Skip_Size     := Default_Skip_Size;
  elsif Argument_Count = 5 then
    Fn_Gr  := S2U(Argument(1));
    Fn_Lol := S2U(Argument(2));
    Fn_Out := S2U(Argument(3));
    Header_Lines  := S2I(Argument(4));
    Header_Mode   := To_Header_Mode(Argument(5));
    Group_By      := Default_Group_By;
    Justify_Width := Default_Justify_Width;
    Skip_Size     := Default_Skip_Size;
  elsif Argument_Count = 6 then
    Fn_Gr  := S2U(Argument(1));
    Fn_Lol := S2U(Argument(2));
    Fn_Out := S2U(Argument(3));
    Header_Lines  := Default_Header_Lines;
    Header_Mode   := Default_Header_Mode;
    Group_By      := S2I(Argument(4));
    Justify_Width := S2I(Argument(5));
    Skip_Size     := S2I(Argument(6));
  elsif Argument_Count = 8 then
    Fn_Gr  := S2U(Argument(1));
    Fn_Lol := S2U(Argument(2));
    Fn_Out := S2U(Argument(3));
    Header_Lines  := S2I(Argument(4));
    Header_Mode   := To_Header_Mode(Argument(5));
    Group_By      := S2I(Argument(6));
    Justify_Width := S2I(Argument(7));
    Skip_Size     := S2I(Argument(8));
  else
    Put_Line("Usage:  " & Command_Name & " net_name  clu_or_lol_name  lol_out_name  [ header_lines  header_mode ]  [ group_by  justify_width  skip_size ]");
    New_Line;
    Put_Line("   net_name        : name of the network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   clu_or_lol_name : name of the partitions file in Pajek format (*.clu) or Lol format");
    Put_Line("                       in Lol format, the file may contain many partitions, e.g. those describing mesoscales");
    New_Line;
    Put_Line("   lol_out_name    : name of the reformatted partition file");
    New_Line;
    Put_Line("   header_lines    : number of lines of the header before a partition in Lol format");
    Put_Line("                       non-negative integer");
    Put_Line("                       default => " & I2S(Default_Header_Lines));
    Put_Line("                       ignored for partitions in Pajek format (*.clu)");
    New_Line;
    Put_Line("   header_mode     : CH | NH | SH");
    Put_Line("                       also lowercase symbols");
    Put_Line("                       also case-insensitive full names (Copy_Header, ...)");
    Put_Line("                       CH = Copy_Header");
    Put_Line("                       NH = No_Header");
    Put_Line("                       SH = Separator_Header");
    Put_Line("                       default => " & Capitalize(Header_Mode_Type'Image(Default_Header_Mode)));
    New_Line;
    Put_Line("   group_by        : number of columns for the nodes' names in the reformatted partition file");
    Put_Line("                       positive integer");
    Put_Line("                       default => " & I2S(Default_Group_By));
    New_Line;
    Put_Line("   justify_width   : width of the columns for the nodes' names");
    Put_Line("                       positive integer");
    Put_Line("                       default => " & I2S(Default_Justify_Width));
    New_Line;
    Put_Line("   skip_size       : modules smaller or equal to this size are skipped");
    Put_Line("                       non-negative integer");
    Put_Line("                       default => " & I2S(Default_Skip_Size));
    return;
  end if;

  Put_Line(U2S(Fn_Lol) & " -> " & U2S(Fn_Out));

  -- Read Graph and open files
  Get_Graph(To_String(Fn_Gr), Gr);

  Is_Clu := (Tail(Fn_Lol, 4) = ".clu");
  if not Is_Clu then
    Open(F_In, In_File, U2S(Fn_Lol));
  end if;
  Create(F_Out, Out_File, U2S(Fn_Out));


  loop
    -- Process Header and Get Partition
    if Is_Clu then
      Get_Partition(U2S(Fn_Lol), Lol);
    else
      case Header_Mode is
        when Copy_Header =>
          for I in 1..Header_Lines loop
            Put_Line(F_Out, Get_Line(F_In));
          end loop;
          Get(F_In, Lol, 0);
        when No_Header =>
          Get(F_In, Lol, Header_Lines);
        when Separator_Header =>
          Get(F_In, Lol, Header_Lines);
          Put_Line(F_Out, Separator_Line);
      end case;
      while End_Of_Line(F_In) and not End_Of_File(F_In) loop
        Skip_Line(F_In);
      end loop;
    end if;
    Sort_Lists(Lol);
    Sort_By_Size(Lol);

    -- Reformat Partition
    Put_Line(F_Out, I2S(Number_Of_Lists(Lol)) & " communities");
--    Put_Line(F_Out, "Number of communities: " & I2S(Number_Of_Lists(Lol)));
    Count := 0;
    Num_Small := 0;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      if Number_Of_Elements(L) > Skip_Size then
        Count := Count + 1;
--        Put_Line(F_Out, "  ---------");
--        Put(F_Out, "  Community " & I2S(Count) & " of " & I2S(Number_Of_Lists(Lol)));
--        Put(F_Out, "  Community " & I2S(Count));
--        Put_Line(F_Out, " (size: " & I2S(Number_Of_Elements(L)) & "):");
        Put_Line(F_Out, I2S(Number_Of_Elements(L)) & " nodes in community " & I2S(Count));
        I := 0;
        Save(L);
        Reset(L);
        while Has_Next_Element(L) loop
--          if I mod Group_By = 0 then
--            Put(F_Out, "    ");
--          end if;
          V := Get_Vertex(Gr, Index_Of(Next_Element(L)));
          Put(F_Out, Left_Justify(Get_Name(V), Justify_Width));
          I := I + 1;
          if I mod Group_By = 0 or not Has_Next_Element(L) then
            New_Line(F_Out);
          end if;
        end loop;
        Restore(L);
      else
        Num_Small := Num_Small + 1;
      end if;
    end loop;
    if Num_Small > 0 then
      Count := Count + 1;
--      Put_Line(F_Out, "  ---------");
      if Num_Small < Number_Of_Lists(Lol) then
        Put(F_Out, "  Rest of ");
      else
        Put(F_Out, "  All ");
      end if;
      Put_Line(F_Out, "nodes in Communities " & I2S(Count) & " to " & I2S(Number_Of_Lists(Lol)) & " of size <= " & I2S(Skip_Size));
    end if;
    New_Line(F_Out, 2);
    Restore(Lol);

    exit when Is_Clu or else End_Of_File(F_In);
  end loop;

  Close(F_Out);
  if not Is_Clu then
    Close(F_In);
  end if;

end Reformat_Partitions;
