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


-- @filename Linked_Lists_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 3/11/2004
-- @revision 25/03/2018
-- @brief Test of Linked_Lists package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;

with Chrono_Utils; use Chrono_Utils;
with Utils.IO; use Utils.IO;
with Linked_Lists;

procedure Linked_Lists_Test is

  Num: Natural := 15;
  T1, T2: Time;
  Chrono: Chronometer;

  -- Random numbers utils
  subtype Small_Range is Integer range 1..Num;
  package Small_Random is new Ada.Numerics.Discrete_Random(Small_Range);
  use Small_Random;

  package Big_Random is new Ada.Numerics.Discrete_Random(Positive);
  use Big_Random;

  package D_Io is new Fixed_Io(Duration);
  use D_Io;

  Gs: Small_Random.Generator;
  Gb: Big_Random.Generator;

  -- Linked Lists
  package Int_Linked_Lists is new Linked_Lists(Integer);
  use Int_Linked_Lists;

  Ll, Cl, Jl: Linked_List;
  E: Integer;

  -- Sorting Linked Lists
  function Lower(Left, Right: in Integer) return Boolean;
  pragma Inline(Lower);

  function Lower(Left, Right: in Integer) return Boolean is
  begin
    return Left <= Right;
  end Lower;

  function Sorted(Ll: in Linked_List) return Boolean is
    A, B: Integer;
  begin
    if Size(Ll) >= 2 then
      Save(Ll);
      Reset(Ll);
      A := Next(Ll);
      while Has_Next(Ll) loop
        B := Next(Ll);
        if not Lower(A, B) then
          Restore(Ll);
          return False;
        end if;
        A := B;
      end loop;
      Restore(Ll);
    end if;
    return True;
  end Sorted;

  -- Put Linked Lists
  procedure Put(Ll: in Linked_List) is
    E: Integer;
  begin
    Put("> "); Put(Size(Ll), Width => 3); Put(": ");
    Save(Ll);
    Reset(Ll);
    while Has_Next(Ll) loop
      E := Next(Ll);
      Put(E, Width => 0); Put(" ");
    end loop;
    Restore(Ll);
    New_Line;
  end Put;


begin

  Initialize(Ll);
  Put(Ll);

  for I in 1..Num loop
    Add(I, 1, Ll);
  end loop;
  Put(Ll);

  for I in 1..Num/2 loop
    Remove_First(Ll);
  end loop;
  Put(Ll);

  Remove(Num / 4, Ll);
  Put(Ll);

  Add(Num, 5, Ll);
  Put(Ll);

  Put_Line("Get all reversed:");
  for I in reverse 1..Size(Ll) loop
    Put(Get(I, Ll), Width => 0); Put(" ");
  end loop;
  New_Line;
  Put(Ll);

  Put_Line("Find all:");
  for I in 1..Num loop
    if Belongs_To(I, Ll) then
      Put(Position(I, Ll), Width => 0); Put(" ");
    else
      Put(". ");
    end if;
  end loop;
  New_Line;
  Put(Ll);

  Put_Line("Remove even:");
  Reset(Ll);
  while Has_Next(Ll) loop
    E := Get(Ll);
    if E mod 2 = 0 then
      Remove(Ll);
    end if;
    Next(Ll);
  end loop;
  Put(Ll);

  Put_Line("Reversed iteration:");
  Set_End(Ll);
  while Has_Prev(Ll) loop
    E := Prev(Ll);
    Put(E, Width => 0); Put(" ");
  end loop;
  New_Line;
  Put(Ll);

  Put_Line("Save and Restore:");
  Reset(Ll);
  while Has_Next(Ll) loop
    Save(Ll);
    while Has_Next(Ll) loop
      Put(Next(Ll), Width => 0); Put(" ");
    end loop;
    New_Line;
    Restore(Ll);
    Next(Ll);
  end loop;
  Put(Ll);

  Put_Line("Remove:");
  Set_End(Ll);
  while Has_Prev(Ll) loop
    Prev(Ll);
    if Get(Ll) mod 3 = 0 then
      Remove(Ll);
    end if;
  end loop;
  Put(Ll);

  Put_Line("Add:");
  Add_First(Num, Ll);
  Add_First(Num, Ll);
  Add_Last(Num, Ll);
  Add_Last(Num, Ll);
  Add(Num, (1 + Size(Ll)) / 2, Ll);
  Put(Ll);

  Put_Line("Replace:");
  Save(Ll);
  Reset(Ll);
  while Has_Next(Ll) loop
    E := Get(Ll);
    Replace(Ll, E * 10);
    Next(Ll);
  end loop;
  Restore(Ll);
  Put(Ll);

  Put_Line("Remove all:");
  Remove_All(Num * 10, Ll);
  Put(Ll);

  Clear(Ll);
  Add_First(Num, Ll);
  Add_Last(Num, Ll);
  Remove_All(Num, Ll);
  Put(Ll);

  Put_Line("Sort:");
  Clear(Ll);
  Reset(Gs);
  for I in 1..3*Num/2 loop
    Add_Last(Random(Gs), Ll);
  end loop;
  Put(Ll);

  Sort(Ll, Lower'Access);
  Put(Ll);

  Put_Line("Removing duplicates:");
  Remove_Adjacent_Duplicates(Ll);
  Put(Ll);

  Put_Line("Clone and join:");
  Cl := Clone(Ll);
  Jl := Join(Ll, Cl);
  Put(Jl);
  Free(Cl);
  Free(Jl);

  Num := 200;
  Put("Sorting a random linked list of size "); Put(Num, Width => 0); New_Line;
  Clear(Ll);
  Reset(Gb);
  Put_Line("  Building...");
  T1 := Clock;
  for I in 1..Num loop
    Add(I, 1 + (Random(Gb) mod I), Ll);
  end loop;
  T2 := Clock;
  Put("    Elapsed time: "); Put(To_Duration(T2-T1), Fore=>0, Aft=>6, Exp=>0); New_Line;
  Put_Line("  Sorting...");
  T1 := Clock;
  Sort(Ll, Lower'access);
  T2 := Clock;
  Put("    Elapsed time: "); Put(To_Duration(T2-T1), Fore=>0, Aft=>6, Exp=>0); New_Line;
  Put_Line("  Checking...");
  if Sorted(Ll) then
    Put_Line("    OK!");
  else
    Put_Line("    Error found!");
  end if;

  Num := 1_000_000;
  Put("Sorting a random linked list of size "); Put(Num, Width => 0); New_Line;
  Clear(Ll);
  Reset(Gb);
  Put_Line("  Building...");
  T1 := Clock;
  for I in 1..Num loop
    Add_First(1 + (Random(Gb) mod (Num / 100)), Ll);
  end loop;
  T2 := Clock;
  Put("    Elapsed time: "); Put(To_Duration(T2-T1), Fore=>0, Aft=>6, Exp=>0); New_Line;
  Put_Line("  Sorting...");
  T1 := Clock;
  Sort(Ll, Lower'access);
  T2 := Clock;
  Put("    Elapsed time: "); Put(To_Duration(T2-T1), Fore=>0, Aft=>6, Exp=>0); New_Line;
  Put_Line("  Removing duplicates...");
  T1 := Clock;
  Remove_Adjacent_Duplicates(Ll);
  T2 := Clock;
  Put("    Elapsed time: "); Put(To_Duration(T2-T1), Fore=>0, Aft=>6, Exp=>0); New_Line;
  Put("    New size: "); Put(Size(Ll), Width=>0); New_Line;
  Put_Line("  Checking...");
  if Sorted(Ll) then
    Put_Line("    OK!");
  else
    Put_Line("    Error found!");
  end if;

  Put_Line("Sorting and checking large random linked lists:");
  Put("  Proceeding: ");
  Reset(Chrono);
  for Factor in 1..20 loop
    Clear(Ll);
    Num := 100_000 * Factor;
    for I in 1..Num loop
      Add_First(1 + (Random(Gb) mod (5 * Num)), Ll);
    end loop;
    Start(Chrono);
    Sort(Ll, Lower'Access);
    Stop(Chrono);
    if Sorted(Ll) then
      Put(".");
    else
      Put("!");
    end if;
  end loop;
  New_Line;
  Put("  Sorting time: "); Put_Duration(Elapsed(Chrono), 6); New_Line;


  Free(Ll);

end Linked_Lists_Test;
