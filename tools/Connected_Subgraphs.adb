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


-- @filename Connected_Subgraphs.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 21/03/2008
-- @revision 26/02/2016
-- @brief Split a Graph in its Connected Subgraphs

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphs_String; use Graphs_String;
with Graphs_String_Algorithms; use Graphs_String_Algorithms;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Connected_Subgraphs is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2021 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Split a network into its weak or strong connected components  ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Net_Sufix         : constant String := ".net";
  Cc_Sufix          : constant String := "-cc.txt";
  Default_Ct_Str    : constant String := "Weak";
  Default_Skip_Size : constant := 0;

  Skip_Size: Natural := Default_Skip_Size;
  Fn: Ustring;
  Ct: Components_Type;
  Fn_Net: Ustring;
  Ct_Str: Ustring;
  Fn_Cc: Ustring;
  Fn_Sub_Net: Ustring;
  Gr, Sub_Gr: Graph;
  Cc: List_Of_Lists;
  Num_Cc: Positive;
  L: List;
  I: Natural;

begin
  Put_Info;

  if Argument_Count = 1 then
    Fn := S2U(Argument(1));
    Ct_Str := S2U(Default_Ct_Str);
    Skip_Size := Default_Skip_Size;
  elsif Argument_Count = 2 then
    Fn := S2U(Argument(1));
    if Is_Integer(Argument(2)) then
      Ct_Str := S2U(Default_Ct_Str);
      Skip_Size := S2I(Argument(2));
    else
      Ct_Str := S2U(Argument(2));
      Skip_Size := Default_Skip_Size;
    end if;
  elsif Argument_Count = 3 then
    Fn := S2U(Argument(1));
    Ct_Str := S2U(Argument(2));
    Skip_Size := S2I(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name_without_ext  [ components_type ]   [ skip_size ]");
    New_Line;
    Put_Line("   net_name_without_ext :  name of the network file in Pajek format without the .net extension");
    New_Line;
    Put_Line("   components_type      :  W | S");
    Put_Line("                             also lowercase symbols");
    Put_Line("                             also case-insensitive full names (Weak, Strong)");
    Put_Line("                             W = Weak");
    Put_Line("                             S = Strong");
    Put_Line("                             default => " & Default_Ct_Str);
    New_Line;
    Put_Line("   skip_size            :  components smaller or equal to this size are skipped");
    Put_Line("                             non-negative integer");
    Put_Line("                             default => " & I2S(Default_Skip_Size));
    return;
  end if;

  begin
    Ct := To_Components_Type(U2S(Ct_Str));
  exception
    when Unknown_Components_Type_Error =>
      Put_Line("Error: components type must be 'weak' or 'strong'");
      return;
  end;

  Fn_Net := Fn & Net_Sufix;
  Fn_Cc := Fn & Cc_Sufix;

  Put(U2S(Fn_Net) & ": ");
  Get_Graph(U2S(Fn_Net), Gr);
  Connected_Components(Gr, Cc, Ct);

  Num_Cc := Number_Of_Lists(Cc);
  if Num_Cc = 1 then
    Put_Line(Head(To_Lowercase(Components_Type'Image(Ct)), Index(Components_Type'Image(Ct), "_") - 1) & "ly connected graph");
  else
    Put(I2S(Num_Cc) & " " & Translate(To_Lowercase(Components_Type'Image(Ct)), "_", " ") & " of sizes");
    Put(U2S(Fn_Cc), Cc);
    I := 0;
    Save(Cc);
    Reset(Cc);
    while Has_Next_List(Cc) loop
      L := Next_List(Cc);
      if I > 0 then
        Put(",");
      end if;
      if Number_Of_Elements(L) <= Skip_Size then
        Put_Line(" ...");
        Put("  skipped " & I2S(Num_Cc - I) & " components of size <= " & I2S(Skip_Size));
        exit;
      end if;
      Put(" " & I2S(Number_Of_Elements(L)));
      Create_Subgraph(Gr, L, Sub_Gr);
      I := I + 1;
      Fn_Sub_Net := Fn & ("-cc-" & Right_Justify(I2S(I), 2, '0')  & Net_Sufix);
      Put_Graph(To_String(Fn_Sub_Net), Sub_Gr);
      Free(Sub_Gr);
    end loop;
    Restore(Cc);
    New_Line;
  end if;

  Free(Cc);
  Free(Gr);
end Connected_Subgraphs;
