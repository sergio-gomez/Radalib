-- Radalib, Copyright (c) 2015 by
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


-- @filename Mesoscales_Fine_Tuning.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 03/10/2006
-- @revision 22/09/2015
-- @brief Fine Tuning of Mesoscales obtained with variable self-loops

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Utils.IO_Double; use Utils.IO_Double;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Properties_D; use Graphs_Double_Properties_D;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Pajek_IO; use Pajek_IO;
with Linked_Lists;

procedure Mesoscales_Fine_Tuning is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2015 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Mesoscales fine-tuning after Mesoscales detection             ==");
    Put_Line("== See README.txt                                                ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  package Lols_Lists is new Linked_Lists(List_Of_Lists);
  use Lols_Lists;

  Net_Sufix        : constant String := ".net";
  Lols_Sufix       : constant String := "-lols.txt";
  Lols_Extra_Sufix : constant String := "-lols-extra.txt";
  Table_Sufix      : constant String := "-table.txt";
  Out_Lols_Sufix   : constant String := "-lols-improved.txt";
  Out_Table_Sufix  : constant String := "-table-improved.txt";

  Mod_Type: Weighted_Modularity_Type;
  Fn_Base       : Ustring;
  Fn_Net        : Ustring;
  Fn_Lols       : Ustring;
  Fn_Lols_Extra : Ustring;
  Fn_Table      : Ustring;
  Fn_Out_Lols   : Ustring;
  Fn_Out_Table  : Ustring;
  F_Lols      : File_Type;
  F_Table     : File_Type;
  F_Out_Lols  : File_Type;
  F_Out_Table : File_Type;

  Gr: Graph;
  Lol, Lol_Best, Lol_Best_Prev: List_Of_Lists;
  Lols: Linked_List;
  W: Word_Access;
  Loop_Wh: Double;
  Q, Q_Best: Double;
  Iter: Natural;

begin
  Put_Info;

  if Argument_Count = 2 then
    Fn_Base := S2U(Argument(1));
    Mod_Type  := To_Modularity_Type(Argument(2));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name_without_ext  weighted_modularity_type");
    New_Line;
    Put_Line("   net_name_without_ext      :  name of the network file in Pajek format without the .net extension");
    Put_Line("                                  it is supposed that files with this name and the following endings exist:");
    Put_Line("                                    *-table.txt: table with four columns: r, r-r_min, Q, num_comms");
    Put_Line("                                    *-lols.txt: the partitions found for the mesoscale in Lol format");
    Put_Line("                                    *-lols-extra.txt : optional file with extra partitions");
    New_Line;
    Put_Line("   weighted_modularity_types :  WN | WS | WUN | WLA | WULA | WLUN | WNN | WLR");
    Put_Line("                                  also lowercase symbols");
    Put_Line("                                  also case-insensitive full names (Weighted_Newman, ...)");
    Put_Line("                                  WN   = Weighted_Newman");
    Put_Line("                                  WS   = Weighted_Signed");
    Put_Line("                                  WUN  = Weighted_Uniform_Nullcase");
    Put_Line("                                  WLA  = Weighted_Local_Average");
    Put_Line("                                  WULA = Weighted_Uniform_Local_Average");
    Put_Line("                                  WLUN = Weighted_Links_Unweighted_Nullcase");
    Put_Line("                                  WNN  = Weighted_No_Nullcase");
    Put_Line("                                  WLR  = Weighted_Link_Rank");
    return;
  end if;

  Fn_Net        := Fn_Base & S2U(Net_Sufix);
  Fn_Lols       := Fn_Base & S2U(Lols_Sufix);
  Fn_Lols_Extra := Fn_Base & S2U(Lols_Extra_Sufix);
  Fn_Table      := Fn_Base & S2U(Table_Sufix);
  Fn_Out_Lols   := Fn_Base & S2U(Out_Lols_Sufix);
  Fn_Out_Table  := Fn_Base & S2U(Out_Table_Sufix);
  Put_Line(U2S(Fn_Net) & ":");

  -- Read Graph
  Get_Graph(U2S(Fn_Net), Gr);

  -- Read Lists of Lists
  Initialize(Lols);
  Open(F_Lols, In_File, U2S(Fn_Lols));
  while not End_Of_File(F_Lols) loop
    Skip_Line(F_Lols, 4);
    Get(F_Lols, Lol);
    Add_Last(Clone(Lol), Lols);
    Free(Lol);
    Comments_Skip(F_Lols);
  end loop;
  Close(F_Lols);
  if File_Exists(U2S(Fn_Lols_Extra)) then
    Open(F_Lols, In_File, U2S(Fn_Lols_Extra));
    while not End_Of_File(F_Lols) loop
      Skip_Line(F_Lols, 4);
      Get(F_Lols, Lol);
      Add_Last(Clone(Lol), Lols);
      Free(Lol);
      Comments_Skip(F_Lols);
    end loop;
    Close(F_Lols);
  end if;

  -- Makeup modularities
  Open(F_Table, In_File, U2S(Fn_Table));
  Create(F_Out_Lols, Out_File, U2S(Fn_Out_Lols));
  Create(F_Out_Table, Out_File, U2S(Fn_Out_Table));
  Iter := 0;
  Lol_Best_Prev := Get_First(Lols);
  while not End_Of_File(F_Table) loop
    -- Read loop from table
    Put(".");
    Get_Word(F_Table, W);
    Loop_Wh := S2D(W.all);
    Put(F_Out_Table, W.all & HTab);
    Free_Word(W);
    Get_Word(F_Table, W);
    Put(F_Out_Table, W.all & HTab);
    Free_Word(W);
    Skip_Line(F_Table);
    Comments_Skip(F_Table);

    -- Search best List Of Lists
    Q_Best := Double'First;
    Save(Lols);
    Reset(Lols);
    while Has_Next(Lols) loop
      Lol := Next(Lols);
      Q := Modularity(Gr, Lol, Mod_Type, Loop_Wh);
      if Q > Q_Best then
        Lol_Best := Lol;
        Q_Best := Q;
      end if;
    end loop;
    Restore(Lols);

    -- Write results
    Put(F_Out_Table, Q_Best, Fore => 0, Aft => 6, Exp => 0); Put(F_Out_Table, HTab);
    Put_Line(F_Out_Table, I2S(Number_Of_Lists(Lol_Best)));
    if Iter = 0 or else Lol_Best /= Lol_Best_Prev then
      Put_Line(F_Out_Lols, "--------");
      Put(F_Out_Lols, "Self-loop(" & I2S(Iter) & ") = ");
      Put(F_Out_Lols, Loop_Wh, Fore => 0, Aft => 4, Exp => 0);
      New_Line(F_Out_Lols);
      Put(F_Out_Lols, "Q_Best = "); Put(F_Out_Lols, Q_Best, Fore => 0, Aft => 6, Exp => 0); New_Line(F_Out_Lols);
      Put_Line(F_Out_Lols, "---");
      Put(F_Out_Lols, Lol_Best);
    end if;
    Lol_Best_Prev := Lol_Best;
    Iter := Iter + 1;
  end loop;
  Close(F_Table);
  Close(F_Out_Lols);
  Close(F_Out_Table);

  -- Free all
  Save(Lols);
  Reset(Lols);
  while Has_Next(Lols) loop
    Lol := Get(Lols);
    Remove(Lols);
    Free(Lol);
  end loop;
  Restore(Lols);
  Free(Lols);
  Free(Gr);
end Mesoscales_Fine_Tuning;
