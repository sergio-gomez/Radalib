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


-- @filename Compare_Partitions.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/03/2008
-- @revision 08/04/2018
-- @brief Compare Partitions

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Contingency_Tables; use Contingency_Tables;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Compare_Partitions is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2019 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Compare partitions in Lol or Pajek format                     ==");
    Put_Line("== Many indices and metrics are calculated:                      ==");
    Put_Line("==   - number of pairs, agreements and disagreements             ==");
    Put_Line("==   - Jaccard, Rand, adjusted Rand, Fowlkes Mallows             ==");
    Put_Line("==   - normalized mutual information, asymmetric Wallace         ==");
    Put_Line("==   - Mirkin, van Dongen, variation of information              ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  -- Output Format type
  type Output_Format_Type is (Verbose, Table);

  Unknown_Output_Format_Error: exception;

  function Get_Output_Format(S: in String) return Output_Format_Type is
  begin
    if    To_Uppercase(S) = "V" or To_Lowercase(S) = "verbose" then
      return Verbose;
    elsif To_Uppercase(S) = "T" or To_Lowercase(S) = "table"   then
      return Table;
    else
      raise Unknown_Output_Format_Error;
    end if;
  end Get_Output_Format;

  -- Write Contingency Table
  procedure Put_Contingency_Table(Ct: in Contingency_Table) is
    Lol1, Lol2: List_Of_Lists;
    L1, L2: List;
  begin
    Get_Lists_Of_Lists(Ct, Lol1, Lol2);
    Save(Lol1);
    Reset(Lol1);
    while Has_Next_List(Lol1) loop
      L1 := Next_List(Lol1);
      Save(Lol2);
      Reset(Lol2);
      while Has_Next_List(Lol2) loop
        L2 := Next_List(Lol2);
        Put(I2S(Number_Of_Elements(Ct, L1, L2)));
        if Has_Next_List(Lol2) then
          Put(HTab);
        end if;
      end loop;
      Restore(Lol2);
      New_Line;
    end loop;
    Restore(Lol1);
  end Put_Contingency_Table;

  -- Write Info in Verbose format
  procedure Put_Info_Verbose(Ct: in Contingency_Table; Name1, Name2: in String; Show_Contingency_Table, Only_One: in Boolean; Num1, Num2: in Positive) is
    Lol1, Lol2: List_Of_Lists;
    D1, D2, D: Longint;
    Awi1, Awi2: Float;
  begin
    Get_Lists_Of_Lists(Ct, Lol1, Lol2);
    if not Only_One then
      Put_Line("---------------------------------------------------");
    end if;
    if Only_One then
      Put_Line("Partition1: " & Name1);
      Put_Line("Partition2: " & Name2);
    else
      Put_Line("Partition1 (" & I2S(Num1) & "): " & Name1);
      Put_Line("Partition2 (" & I2S(Num2) & "): " & Name2);
    end if;
    New_Line;
    Put_Line("Number of Lists(1) : " & I2S(Number_Of_Lists(Lol1)));
    Put_Line("Number of Lists(2) : " & I2S(Number_Of_Lists(Lol2)));
    New_Line;
    Put_Line("Same Class Agreements : " & L2S(Number_Of_Same_Class_Agreements(Ct)));
    Put_Line("Agreements            : " & L2S(Number_Of_Agreements(Ct)));
    Number_Of_Disagreements(Ct, D1, D2);
    if D1 = 0 then
      Put_Line("Disagreements(1)      : " & L2S(D1) & "       (Partition1 inside Partition2)");
    else
      Put_Line("Disagreements(1)      : " & L2S(D1));
    end if;
    if D2 = 0 then
      Put_Line("Disagreements(2)      : " & L2S(D2) & "       (Partition2 inside Partition1)");
    else
      Put_Line("Disagreements(2)      : " & L2S(D2));
    end if;
    D := Number_Of_Disagreements(Ct);
    if D = 0 then
      Put_Line("Disagreements         : " & L2S(D)  & "       (Partition1 = Partition2)");
    else
      Put_Line("Disagreements         : " & L2S(D));
    end if;
    Put_Line("Pairs                 : " & L2S(Number_Of_Pairs(Ct)));
    New_Line;
    Put_Line("Jaccard Index         : " & F2S(Jaccard_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Rand Index            : " & F2S(Rand_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Adjusted Rand Index   : " & F2S(Adjusted_Rand_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Fowlkes Mallows Index : " & F2S(Fowlkes_Mallows_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (maximum)    : " & F2S(Normalized_Mutual_Information_Index(Ct, Maximum), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (arithmetic) : " & F2S(Normalized_Mutual_Information_Index(Ct, Arithmetic_Mean), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (geometric)  : " & F2S(Normalized_Mutual_Information_Index(Ct, Geometric_Mean), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (minimum)    : " & F2S(Normalized_Mutual_Information_Index(Ct, Minimum), Aft => 4, Exp => 0));
    Asymmetric_Wallace_Index(Ct, Awi1, Awi2);
    if Awi1 = 1.0 then
      Put_Line("Asymmetric Wallace Index(1) : " & F2S(Awi1, Aft => 4, Exp => 0) & "       (Partition1 inside Partition2)");
    else
      Put_Line("Asymmetric Wallace Index(1) : " & F2S(Awi1, Aft => 4, Exp => 0));
    end if;
    if Awi2 = 1.0 then
      Put_Line("Asymmetric Wallace Index(2) : " & F2S(Awi2, Aft => 4, Exp => 0) & "       (Partition2 inside Partition1)");
    else
      Put_Line("Asymmetric Wallace Index(2) : " & F2S(Awi2, Aft => 4, Exp => 0));
    end if;
    New_Line;
    Put_Line("Mirkin Metric                   : " & F2S(Mirkin_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Van Dongen Metric               : " & F2S(Van_Dongen_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Variation Of Information Metric : " & F2S(Variation_Of_Information_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Mirkin Metric                   : " & F2S(Normalized_Mirkin_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Van Dongen Metric               : " & F2S(Normalized_Van_Dongen_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Variation Of Information Metric : " & F2S(Normalized_Variation_Of_Information_Metric(Ct), Aft => 4, Exp => 0));
    if Show_Contingency_Table then
      New_Line;
      Put_Line("Contingency table (Partition1 x Partition2):");
      Put_Contingency_Table(Ct);
    end if;
  end Put_Info_Verbose;

  -- Write Info Table Header
  procedure Put_Info_Table_Header(Only_One: in Boolean) is
  begin
    if Only_One then
      Put("Partition1" & HTab & "Partition2");
    else
      Put("Partition1" & HTab & "Index1" & HTab & "Partition2" & HTab & "Index2");
    end if;
    Put(Htab & "Number_of_Lists(1)" & Htab & "Number_of_Lists(2)");
    Put(Htab & "Same_Class_Agreements");
    Put(Htab & "Agreements");
    Put(Htab & "Disagreements(1)" & HTab & "Disagreements(2)");
    Put(Htab & "Disagreements");
    Put(Htab & "Pairs");
    Put(Htab & "Jaccard_Index");
    Put(Htab & "Rand_Index");
    Put(Htab & "Adjusted_Rand_Index");
    Put(Htab & "Fowlkes_Mallows_Index");
    Put(Htab & "Normalized_Mutual_Information_Index_(maximum)");
    Put(Htab & "Normalized_Mutual_Information_Index_(arithmetic)");
    Put(Htab & "Normalized_Mutual_Information_Index_(geometric)");
    Put(Htab & "Normalized_Mutual_Information_Index_(minimum)");
    Put(Htab & "Asymmetric_Wallace_Index(1)");
    Put(Htab & "Asymmetric_Wallace_Index(2)");
    Put(Htab & "Mirkin_Metric");
    Put(Htab & "Van_Dongen_Metric");
    Put(Htab & "Variation_Of_Information_Metric");
    Put(Htab & "Normalized_Mirkin_Metric");
    Put(Htab & "Normalized_Van_Dongen_Metric");
    Put(Htab & "Normalized_Variation_Of_Information_Metric");
    Put(Htab & "Partition1_Equals_Partition2");
    Put(Htab & "Partition1_Inside_Partition2");
    Put(Htab & "Partition2_Inside_Partition1");
    New_Line;
  end Put_Info_Table_Header;

  -- Write Info Values in Table format
  procedure Put_Info_Table_Values(Ct: in Contingency_Table; Name1, Name2: in String; Only_One: in Boolean; Num1, Num2: in Positive) is
    Lol1, Lol2: List_Of_Lists;
    D1, D2, D: Longint;
    Awi1, Awi2: Float;
  begin
    Get_Lists_Of_Lists(Ct, Lol1, Lol2);
    if Only_One then
      Put(Name1 & HTab & Name2);
    else
      Put(Name1 & HTab & I2S(Num1) & HTab & Name2 & HTab & I2S(Num2));
    end if;
    Put(HTab & I2S(Number_Of_Lists(Lol1)) & HTab & I2S(Number_Of_Lists(Lol2)));
    Put(HTab & L2S(Number_Of_Same_Class_Agreements(Ct)));
    Put(HTab & L2S(Number_Of_Agreements(Ct)));
    Number_Of_Disagreements(Ct, D1, D2);
    Put(HTab & L2S(D1) & HTab & L2S(D2));
    D := D1 + D2;
    Put(HTab & L2S(D));
    Put(Htab & L2S(Number_Of_Pairs(Ct)));
    Put(Htab & F2Se0(Jaccard_Index(Ct), Aft => 4));
    Put(Htab & F2Se0(Rand_Index(Ct), Aft => 4));
    Put(Htab & F2Se0(Adjusted_Rand_Index(Ct), Aft => 4));
    Put(Htab & F2Se0(Fowlkes_Mallows_Index(Ct), Aft => 4));
    Put(Htab & F2Se0(Normalized_Mutual_Information_Index(Ct, Maximum), Aft => 4));
    Put(Htab & F2Se0(Normalized_Mutual_Information_Index(Ct, Arithmetic_Mean), Aft => 4));
    Put(Htab & F2Se0(Normalized_Mutual_Information_Index(Ct, Geometric_Mean), Aft => 4));
    Put(Htab & F2Se0(Normalized_Mutual_Information_Index(Ct, Minimum), Aft => 4));
    Asymmetric_Wallace_Index(Ct, Awi1, Awi2);
    Put(Htab & F2Se0(Awi1, Aft => 4));
    Put(Htab & F2Se0(Awi2, Aft => 4));
    Put(Htab & F2Se0(Mirkin_Metric(Ct), Aft => 4));
    Put(Htab & F2Se0(Van_Dongen_Metric(Ct), Aft => 4));
    Put(Htab & F2Se0(Variation_Of_Information_Metric(Ct), Aft => 4));
    Put(Htab & F2Se0(Normalized_Mirkin_Metric(Ct), Aft => 4));
    Put(Htab & F2Se0(Normalized_Van_Dongen_Metric(Ct), Aft => 4));
    Put(Htab & F2Se0(Normalized_Variation_Of_Information_Metric(Ct), Aft => 4));
    if D = 0 then
      Put(Htab & "Yes" & Htab & "Yes" & Htab & "Yes");
    elsif D1 = 0 then
      Put(Htab & "No" & Htab & "Yes" & Htab & "No");
    elsif D2 = 0 then
      Put(Htab & "No" & Htab & "No" & Htab & "Yes");
    else
      Put(Htab & "No" & Htab & "No" & Htab & "No");
    end if;
    New_Line;
  end Put_Info_Table_Values;


  Default_Num_Skip_Lines: constant := 0;
  Default_Output_Format: constant Output_Format_Type := Verbose;
  Max_Rows: constant := 30;
  Max_Cols: constant := 10;

  Num_Skip_Lines: Natural := Default_Num_Skip_Lines;
  Output_Format: Output_Format_Type := Default_Output_Format;

  Lol1_Name, Lol2_Name, Ct_Name: Ustring;
  Has_Lol1, Has_Lol2: Boolean;
  Is_Clu1, Is_Clu2: Boolean;
  Only_One: Boolean;
  F_Lol1, F_Lol2, F_Out: File_Type;
  Lol1, Lol2: List_Of_Lists;
  Num1, Num2: Natural;
  Ct: Contingency_Table;
  Show_Contingency_Table: Boolean;

begin
  Put_Info;

  if Argument_Count = 2 then
    Lol1_Name := S2U(Argument(1));
    Lol2_Name := S2U(Argument(2));
    Ct_Name   := Null_Ustring;
    Output_Format  := Default_Output_Format;
    Num_Skip_Lines := Default_Num_Skip_Lines;
  elsif Argument_Count = 3 then
    Lol1_Name := S2U(Argument(1));
    Lol2_Name := S2U(Argument(2));
    if Is_Integer(Argument(3)) then
      Ct_Name        := Null_Ustring;
      Output_Format  := Default_Output_Format;
      Num_Skip_Lines := S2I(Argument(3));
    else
      Ct_Name        := S2U(Argument(3));
      Output_Format  := Default_Output_Format;
      Num_Skip_Lines := Default_Num_Skip_Lines;
    end if;
  elsif Argument_Count = 4 then
    Lol1_Name := S2U(Argument(1));
    Lol2_Name := S2U(Argument(2));
    if Is_Integer(Argument(4)) then
      Ct_Name        := S2U(Argument(3));
      Output_Format  := Default_Output_Format;
      Num_Skip_Lines := S2I(Argument(4));
    else
      Ct_Name        := S2U(Argument(3));
      Output_Format  := Get_Output_Format(Argument(4));
      Num_Skip_Lines := Default_Num_Skip_Lines;
    end if;
  elsif Argument_Count = 5 then
    Lol1_Name := S2U(Argument(1));
    Lol2_Name := S2U(Argument(2));
    Ct_Name   := S2U(Argument(3));
    Output_Format  := Get_Output_Format(Argument(4));
    Num_Skip_Lines := S2I(Argument(5));
  else
    Put_Line("Usage:  " & Command_Name & "  clu_or_lol(s)_1_name  clu_or_lol(s)_2_name  [ out_name [ out_format ] ]  [ number_of_lines_to_skip ]");
    New_Line;
    Put_Line("   clu_or_lol(s)_1_name    :  name of the file with the first partition(s) in Pajek or Lol(s) format");
    Put_Line("                                only one partition per file if in Pajek format");
    New_Line;
    Put_Line("   clu_or_lol(s)_2_name    :  name of the file with the second partition(s) in Pajek or Lol(s) format");
    Put_Line("                                only one partition per file if in Pajek format");
    New_Line;
    Put_Line("   out_name                :  name of the output file");
    Put_Line("                                contingency table not shown in verbose format if output is not file and size > " & I2S(Max_Rows) & "x" & I2S(Max_Cols));
    New_Line;
    Put_Line("   out_format              :  V | T");
    Put_Line("                                also lowercase symbols");
    Put_Line("                                also case-insensitive full names (Verbose, ...)");
    Put_Line("                                V = Verbose");
    Put_Line("                                T = Table");
    Put_Line("                                default => " & Capitalize(Output_Format_Type'Image(Default_Output_Format)));
    New_Line;
    Put_Line("   number_of_lines_to_skip :  number of lines to skip at the beginning of the Lol files");
    Put_Line("                                ignored for partitions in Pajek format");
    Put_Line("                                non-negative integer");
    Put_Line("                                default => " & I2S(Default_Num_Skip_Lines));
    return;
  end if;

  if Ct_Name /= Null_Ustring then
    Put_Line(U2S(Lol1_Name) & " + " & U2S(Lol2_Name) & " -> " & U2S(Ct_Name));
    if File_Exists(U2S(Ct_Name)) then
      Delete_File(U2S(Ct_Name));
    end if;
    Create(F_Out, Out_File, U2S(Ct_Name));
    Set_Output(F_Out);
  end if;

  Is_Clu1 := Tail(Lol1_Name, 4) = ".clu";
  Is_Clu2 := Tail(Lol2_Name, 4) = ".clu";

  if not Is_Clu1 then
    Open(F_Lol1, In_File, U2S(Lol1_Name));
  end if;
  if not Is_Clu2 then
    Open(F_Lol2, In_File, U2S(Lol2_Name));
  end if;

  Only_One := True;
  Has_Lol1 := True;
  Has_Lol2 := True;
  Num1 := 0;
  Num2 := 0;

  while Has_Lol1 loop
    Num1 := Num1 + 1;
    if Is_Clu1 then
      Get_Partition(U2S(Lol1_Name), Lol1);
      Has_Lol1 := False;
    else
      Get(F_Lol1, Lol1, Num_Skip_Lines);
      Comments_Skip(F_Lol1);
      Has_Lol1 := not End_Of_File(F_Lol1);
    end if;

    while Has_Lol2 loop
      Num2 := Num2 + 1;
      if Is_Clu2 then
        Get_Partition(U2S(Lol2_Name), Lol2);
        Has_Lol2 := False;
      else
        Get(F_Lol2, Lol2, Num_Skip_Lines);
        Comments_Skip(F_Lol2);
        Has_Lol2 := not End_Of_File(F_Lol2);
      end if;

      Only_One := Only_One and (not Has_Lol1) and (not Has_Lol2);
      Show_Contingency_Table := Only_One and ((Ct_Name /= Null_Ustring) or ((Number_Of_Lists(Lol1) <= Max_Rows) and (Number_Of_Lists(Lol2) <= Max_Cols)));

      Initialize(Ct, Lol1, Lol2);
      case Output_Format is
        when Verbose =>
          Put_Info_Verbose(Ct, U2S(Lol1_Name), U2S(Lol2_Name), Show_Contingency_Table, Only_One, Num1, Num2);
        when Table =>
          if Num1 = 1 and Num2 = 1 then
            Put_Info_Table_Header(Only_One);
          end if;
          Put_Info_Table_Values(Ct, U2S(Lol1_Name), U2S(Lol2_Name), Only_One, Num1, Num2);
      end case;
      Free(Ct);

      Free(Lol2);
    end loop;

    Free(Lol1);
  end loop;

  if not Is_Clu1 then
    Close(F_Lol1);
  end if;
  if not Is_Clu2 then
    Close(F_Lol2);
  end if;
  if Ct_Name /= Null_Ustring then
    Close(F_Out);
  end if;

end Compare_Partitions;
