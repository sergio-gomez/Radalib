-- Radalib, Copyright (c) 2022 by
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


-- @filename Contingency_Tables_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 1/04/2005
-- @revision 08/04/2018
-- @brief Test of Contingency_Tables package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Contingency_Tables; use Contingency_Tables;
with Utils; use Utils;

procedure Contingency_Tables_Test is

  procedure Put(L: in List) is
    E: Element;
  begin
    Put(">"); Put(Number_Of_Elements(L), Width => 2); Put(": ");
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      E := Next_Element(L);
      Put(Index_Of(E), Width => 0); Put(" ");
    end loop;
    Restore(L);
    New_Line;
  end Put;

  procedure Put(Lol: in List_Of_Lists) is
  begin
    --Put("Number of lists: "); Put(Number_Of_Lists(Lol), Width => 0); New_Line;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Put(Next_List(Lol));
    end loop;
    Restore(Lol);
    if Number_Of_Elements(Unassigned_list(Lol)) > 0 then
      Put_Line("Unassigned list:");
      Put(Unassigned_List(Lol));
    end if;
  end Put;

  procedure Put(Ct: in Contingency_Table) is
    Lol1, Lol2: List_Of_Lists;
    L1, L2: List;
    D1, D2, D: Longint;
    Awi1, Awi2: Float;
  begin
    Get_Lists_Of_Lists(Ct, Lol1, Lol2);
    New_Line;
    Put(Lol1);
    Put_Line("---");
    Put(Lol2);
    Put_Line("---");
    Put_Line("Contingency table:");
    Save(Lol1);
    Reset(Lol1);
    while Has_Next_List(Lol1) loop
      L1 := Next_List(Lol1);
      Save(Lol2);
      Reset(Lol2);
      while Has_Next_List(Lol2) loop
        L2 := Next_List(Lol2);
        Put(Number_Of_Elements(Ct, L1, L2), Width => 0); Put(" ");
      end loop;
      Restore(Lol2);
      New_Line;
    end loop;
    Restore(Lol1);
    Put_Line("---");
    Put_Line("Same Class Agreements : " & L2S(Number_Of_Same_Class_Agreements(Ct)));
    Put_Line("Agreements            : " & L2S(Number_Of_Agreements(Ct)));
    Number_Of_Disagreements(Ct, D1, D2);
    if D1 = 0 then
      Put_Line("Disagreements(1)      : " & L2S(D1) & "       (Lol1 inside Lol2)");
    else
      Put_Line("Disagreements(1)      : " & L2S(D1));
    end if;
    if D2 = 0 then
      Put_Line("Disagreements(2)      : " & L2S(D2) & "       (Lol2 inside Lol1)");
    else
      Put_Line("Disagreements(2)      : " & L2S(D2));
    end if;
    D := Number_Of_Disagreements(Ct);
    if D = 0 then
      Put_Line("Disagreements         : " & L2S(D)  & "       (Lol1 = Lol2)");
    else
      Put_Line("Disagreements         : " & L2S(D));
    end if;
    Put_Line("Pairs                 : " & L2S(Number_Of_Pairs(Ct)));
    Put_Line("---");
    Put_Line("Rand Index            : " & F2S(Rand_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Adjusted Rand Index   : " & F2S(Adjusted_Rand_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Jaccard Index         : " & F2S(Jaccard_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Fowlkes Mallows Index : " & F2S(Fowlkes_Mallows_Index(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (maximum)    : " & F2S(Normalized_Mutual_Information_Index(Ct, Maximum), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (arithmetic) : " & F2S(Normalized_Mutual_Information_Index(Ct, Arithmetic_Mean), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (geometric)  : " & F2S(Normalized_Mutual_Information_Index(Ct, Geometric_Mean), Aft => 4, Exp => 0));
    Put_Line("Normalized Mutual Information Index (minimum)    : " & F2S(Normalized_Mutual_Information_Index(Ct, Minimum), Aft => 4, Exp => 0));
    Asymmetric_Wallace_Index(Ct, Awi1, Awi2);
    if Awi1 = 1.0 then
      Put_Line("Asymmetric Wallace Index(1) : " & F2S(Awi1, Aft => 4, Exp => 0) & "       (Lol1 inside Lol2)");
    else
      Put_Line("Asymmetric Wallace Index(1) : " & F2S(Awi1, Aft => 4, Exp => 0));
    end if;
    if Awi2 = 1.0 then
      Put_Line("Asymmetric Wallace Index(2) : " & F2S(Awi2, Aft => 4, Exp => 0) & "       (Lol2 inside Lol1)");
    else
      Put_Line("Asymmetric Wallace Index(2) : " & F2S(Awi2, Aft => 4, Exp => 0));
    end if;
    Put_Line("---");
    Put_Line("Mirkin Metric                   : " & F2S(Mirkin_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Van Dongen Metric               : " & F2S(Van_Dongen_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Variation Of Information Metric : " & F2S(Variation_Of_Information_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Mirkin Metric                   : " & F2S(Normalized_Mirkin_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Van Dongen Metric               : " & F2S(Normalized_Van_Dongen_Metric(Ct), Aft => 4, Exp => 0));
    Put_Line("Normalized Variation Of Information Metric : " & F2S(Normalized_Variation_Of_Information_Metric(Ct), Aft => 4, Exp => 0));

  end Put;

  Lol1, Lol2: List_Of_Lists;
  L: List;
  Ct: Contingency_Table;

begin

  Initialize(Lol1, 4);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 1), L);
  Move(Get_Element(Lol1, 3), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 2), L);
  Move(Get_Element(Lol1, 4), L);

  Initialize(Lol2, 4);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 1), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 2), L);
  Move(Get_Element(Lol2, 3), L);
  Move(Get_Element(Lol2, 4), L);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);
  Put_Line("=========");

  Transpose(Ct);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

  Initialize(Lol1, 8);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 1), L);
  Move(Get_Element(Lol1, 2), L);
  Move(Get_Element(Lol1, 3), L);
  Move(Get_Element(Lol1, 4), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 5), L);
  Move(Get_Element(Lol1, 6), L);
  Move(Get_Element(Lol1, 7), L);
  Move(Get_Element(Lol1, 8), L);

  Initialize(Lol2, 8);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 5), L);
  Move(Get_Element(Lol2, 6), L);
  Move(Get_Element(Lol2, 7), L);
  Move(Get_Element(Lol2, 8), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 1), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 2), L);
  Move(Get_Element(Lol2, 3), L);
  Move(Get_Element(Lol2, 4), L);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

  Initialize(Lol1, 8);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 1), L);
  Move(Get_Element(Lol1, 2), L);
  Move(Get_Element(Lol1, 3), L);
  Move(Get_Element(Lol1, 4), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 5), L);
  Move(Get_Element(Lol1, 6), L);
  Move(Get_Element(Lol1, 7), L);
  Move(Get_Element(Lol1, 8), L);

  Initialize(Ct, Lol1, Lol1);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Put_Line("=========");

  Initialize(Lol1, 10);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 1), L);
  Move(Get_Element(Lol1, 2), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 3), L);
  Move(Get_Element(Lol1, 4), L);
  Move(Get_Element(Lol1, 5), L);
  Move(Get_Element(Lol1, 6), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 7), L);
  Move(Get_Element(Lol1, 8), L);
  Move(Get_Element(Lol1, 9), L);
  Move(Get_Element(Lol1, 10), L);

  Initialize(Lol2, 10);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 1), L);
  Move(Get_Element(Lol2, 3), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 2), L);
  Move(Get_Element(Lol2, 4), L);
  Move(Get_Element(Lol2, 5), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 6), L);
  Move(Get_Element(Lol2, 7), L);
  Move(Get_Element(Lol2, 8), L);
  Move(Get_Element(Lol2, 9), L);
  Move(Get_Element(Lol2, 10), L);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

  Initialize(Lol1, 10, Together_Initialization);
  Initialize(Lol2, 10, Unassigned_Initialization);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

  Initialize(Lol1, 10, Isolated_Initialization);
  Initialize(Lol2, 10, Together_Initialization);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

  Initialize(Lol1, 10, Unassigned_Initialization);
  Initialize(Lol2, 10, Isolated_Initialization);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

  Initialize(Lol1, 10, Together_Initialization);
  Initialize(Lol2, 10, Together_Initialization);

  Initialize(Ct, Lol1, Lol2);
  Put(Ct);

  Free(Ct);
  Free(Lol1);
  Free(Lol2);
  Put_Line("=========");

end Contingency_Tables_Test;
