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


-- @filename Minheaps_Test.adb
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 22/10/2007
-- @revision 21/01/2018
-- @brief Test of Minheaps package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Numerics.Discrete_Random;
with Utils; use Utils;
with Minheaps;

procedure Minheaps_Test is

  Num: constant Integer := 15;
  Reps: constant Integer := 1_000_000;

  package Int_Minheaps is new Minheaps(Integer);
  use Int_Minheaps;

  subtype Values is Integer range 1..100;
  package Random_Values is new Ada.Numerics.Discrete_Random(Values);
  use Random_Values;

  St, Ct: Minheap;
  G: Generator;
  Stat: State;
  X, Xprev: Integer;

begin

  Initialize(St, Num);
  Put("Size: "); Put(Size(St), Width => 0); New_Line;

  Add(666, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(444, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(555, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(222, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(333, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(666, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(444, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;
  Add(222, St);
  Put("Head: "); Put(Head(St), Width => 0); New_Line;

  Put(Number_Of_Elements(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Delete_Min(St), Width => 0); Put(" ");
  end loop;
  New_Line;

  Add(10, St);
  Add(12, St);
  Add( 1, St);
  Add(14, St);
  Add( 6, St);
  Add( 5, St);
  Add( 8, St);
  Add(15, St);
  Add( 3, St);
  Add( 9, St);
  Add( 7, St);
  Add( 4, St);
  Add(11, St);
  Add(13, St);
  Add( 2, St);

  Put(Number_Of_Elements(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Delete_Min(St), Width => 0); Put(" ");
  end loop;
  New_Line;

  for I in 1..Num loop
    if (I mod 2) = 0 then
      Add(I, St);
    else
      Add(-I, St);
    end if;
  end loop;

  Ct := Clone(St);

  Put(Number_Of_Elements(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Head(St), Width => 0); Put(" ");
    Delete_Min(St);
  end loop;
  New_Line;

  Put(Number_Of_Elements(Ct), Width => 0); Put(": ");
  while not Is_Empty(Ct) loop
    Put(Delete_Min(Ct), Width => 0); Put(" ");
  end loop;
  New_Line;

  Free(Ct);

  for I in 1..Num loop
    if (I mod 2) = 0 then
      Add(I, St);
    else
      Add(-I, St);
    end if;
  end loop;

  Put("Number of Elements: "); Put(Number_Of_Elements(St), Width => 0); Put(" -> ");
  Clear(St); Put(Number_Of_Elements(St), Width => 0); New_Line;

  Put("Test of " & I2S(Reps) & " random minheaps: ");
  Reset(G, 0);
  for K in 1..Reps loop
    Save(G, Stat);
    for I in 1..Num loop
      Add(Random(G), St);
    end loop;
    Xprev := Delete_Min(St);
    while not Is_Empty(St) loop
      X := Delete_Min(St);
      if X < Xprev then
        Put_Line("Error!!!");
        while not Is_Empty(St) loop
          Delete_Min(St);
        end loop;
        Reset(G, Stat);
        for I in 1..Num loop
          Add(Random(G), St);
        end loop;
        while not Is_Empty(St) loop
          Put(Delete_Min(St), Width => 0); Put(" ");
        end loop;
        New_Line;
        return;
      end if;
      Xprev := X;
    end loop;
  end loop;
  Put_Line("OK"); New_Line;

end Minheaps_Test;
