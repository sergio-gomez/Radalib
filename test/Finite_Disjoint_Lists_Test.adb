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


-- @filename Finite_Disjoint_Lists_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 19/12/2014
-- @brief Test of Finite_Disjoint_Lists package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

procedure Finite_Disjoint_Lists_Test is

  Fn: constant String := "finite_disjoint_lists_test.txt";
  Total: constant := 50;
  Num: Natural := 5;

  Lol, Lol_Clone, Lol1, Lol2: List_Of_Lists;
  La: array(1..Total) of List;
  L: List;
  E: Element;
  K: Positive;

  procedure Put_Title(S: in String) is
  begin
    Put_Line("----------- " & S & " -----------");
  end Put_Title;

begin

  Put_Title("Initialize");
  Initialize(Lol, Total);
  Put(Lol);

  Put_Title("Create Lists");
  for N in 1..Num loop
    La(N) := New_List(Lol);
  end loop;
  Put(Lol);

  Put_Title("Fill Lists");
  for I in 1..Total loop
    for J in reverse 1..Num loop
      if I mod J = 0 then
        E := Get_Element(Lol, I);
        Move(E, La(J));
        exit;
      end if;
    end loop;
  end loop;
  if not Has_Unassigned(Lol) then
    Put_Line("Yes, there are no unassigned elements");
  end if;
  Put(Lol);
  Put("La(2) before Sort By Size: "); Put(La(2));

  Put_Title("Sort by Size");
  Sort_By_Size(Lol);
  Put("La(2)  after Sort By Size: "); Put(La(2));
  Save(Lol);
  Reset(Lol);
  K := 1;
  while Has_Next_List(Lol) loop
    La(K) := Next_List(Lol);
    K := K + 1;
  end loop;
  Restore(Lol);
  Put(Lol);

  Put_Title("Move 4th List to 5th List");
  Move(La(4), La(5));
  Put(Lol);

  Put_Title("Remove List containing 34");
  Remove(List_Of(Get_Element(Lol, 34)));
  Put(Lol);

  Put_Title("Remove 3rd List");
  Remove(La(3));
  if not Belongs_To(La(3), Lol) then
    Put_Line("Yes, list really removed");
  end if;
  Put(Lol);

  Put_Title("Unassign 1st and 2nd Elements of 1st List");
  Reset(La(1));
  Move(Next_Element(La(1)), Unassigned_List(Lol));
  Unassign(Next_Element(La(1)));
  if Has_Unassigned(Lol) then
    Put_Line("Yes, there are unassigned elements");
  end if;
  Put(Lol);

  Put_Title("Unassign Elements 1, 11 and 50");
  Unassign(Get_Element(Lol, 1));
  Unassign(Get_Element(Lol, 11));
  Unassign(Get_Element(Lol, 48));
  Put(Lol);

  Put_Title("Move 28 to 1st List if belongs to the 2nd one");
  if Belongs_To(Get_Element(Lol, 28), La(2)) then
    Move(Get_Element(Lol, 28), La(1));
  end if;
  Put(Lol);

  Put_Title("Sort Elements");
  Sort_Lists(Lol);
  Put(Lol);

  Put_Title("Clear List of Lists");
  Clear(Lol);
  Put(Lol);

  Put_Title("Fill Lists");
  for N in 1..Num loop
    La(N) := New_List(Lol);
  end loop;
  for I in 1..Total loop
    for J in reverse 1..Num loop
      if I mod J = 0 then
        E := Get_Element(Lol, I);
        Move(E, La(J));
        exit;
      end if;
    end loop;
  end loop;
  Put(Lol);

  Put_Title("Save and Restore of Lists");
  Reset(Lol);
  Save(Lol);
  while Has_Next_List(Lol) loop
    Save(Lol);
    while Has_Next_List(Lol) loop
      L := Get_List(Lol);
      Put("> "); Put(L);
      Next_List(Lol);
    end loop;
    Restore(Lol);
    Put_Line("-");
    Next_List(Lol);
  end loop;
  Restore(Lol);
  Put("> "); Put(Get_List(Lol));
  Put_Line("---");
  Put(Lol);

  Put_Title("Save and Restore of Elements");
  Reset(Lol);
  L := Get_List(Lol);
  while Has_Next_Element(L) loop
    Save(L);
    while Has_Next_Element(L) loop
      E := Get_Element(L);
      Put(Index_Of(E), Width => 0); Put(" ");
      Next_Element(L);
    end loop;
    Restore(L);
    New_Line;
    Next_Element(L);
  end loop;

  Put_Title("Test IO");
  Put(Fn, Lol);
  Put(Fn, Lol, Append_File);
  Put(Lol); Put_Line("---");
  Free(Lol);
  Get(Fn, Lol);
  Put(Lol);

  Put_Title("Move Elements of 1st List to new Lists");
  L := Next_List(Lol);
  Reset(L);
  while Has_Next_Element(L) loop
    Move(Next_Element(L), New_List(Lol));
  end loop;
  Remove(L);
  Put(Lol);

  Put_Title("Clone");
  Num := 0;
  Reset(Lol);
  while Has_Next_List(Lol) loop
    L := Get_List(Lol);
    while Has_Next_Element(L) loop
      E := Get_Element(L);
      Put(Index_Of(E), Width => 0); Put(" ");
      Next_Element(L);
      Num := Num + 1;
      exit when Num = 10;
    end loop;
    exit when Num = 10;
    Next_List(Lol);
    New_Line;
  end loop;
  Put("... ");

  Lol_Clone := Clone(Lol);
  Free(Lol);

  while Has_Next_List(Lol_Clone) loop
    L := Get_List(Lol_Clone);
    while Has_Next_Element(L) loop
      E := Get_Element(L);
      Put(Index_Of(E), Width => 0); Put(" ");
      Next_Element(L);
    end loop;
    Next_List(Lol_Clone);
    New_Line;
  end loop;

  Free(Lol_Clone);

  Put_Title("Maximal refinement");
  Initialize(Lol1, 9, Unassigned_Initialization);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 1), L);
  Move(Get_Element(Lol1, 2), L);
  Move(Get_Element(Lol1, 3), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 4), L);
  Move(Get_Element(Lol1, 5), L);
  Move(Get_Element(Lol1, 6), L);
  L := New_List(Lol1);
  Move(Get_Element(Lol1, 7), L);
  Move(Get_Element(Lol1, 8), L);
  Move(Get_Element(Lol1, 9), L);
  Put(Lol1);

  Initialize(Lol2, 9, Unassigned_Initialization);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 1), L);
  Move(Get_Element(Lol2, 2), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 3), L);
  Move(Get_Element(Lol2, 4), L);
  Move(Get_Element(Lol2, 5), L);
  Move(Get_Element(Lol2, 6), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 7), L);
  Move(Get_Element(Lol2, 9), L);
  L := New_List(Lol2);
  Move(Get_Element(Lol2, 8), L);
  Put(Lol2);

  Lol := Maximal_Refinement(Lol1, Lol2);
  Put(Lol);
  Free(Lol1);
  Free(Lol2);
  Free(Lol);

  Put_Title("All Partitions Traversal");
  All_Partitions_Traversal(4, Put'Access);

  Put_Title("All Subsets Traversal");
  All_Subsets_Traversal(4, Put'Access);

  Put_Title("All Unassigned Subsets Traversal");
  Initialize(Lol, 7);
  L := New_List(Lol);
  Move(Get_Element(Lol, 1), L);
  Move(Get_Element(Lol, 3), L);
  Move(Get_Element(Lol, 7), L);
  All_Unassigned_Subsets_Traversal(Lol, Put'Access);
  Free(Lol);

  Put_Title("Subsets Traversal");
  Subsets_Traversal(4, 3, Put'Access);

  Put_Title("Unassigned Subsets Traversal");
  Initialize(Lol, 7);
  L := New_List(Lol);
  Move(Get_Element(Lol, 1), L);
  Move(Get_Element(Lol, 3), L);
  Move(Get_Element(Lol, 7), L);
  Unassigned_Subsets_Traversal(Lol, 3, Put'Access);
  Free(Lol);

end Finite_Disjoint_Lists_Test;
