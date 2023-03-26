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


-- @filename Stacks_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 4/04/2005
-- @revision 26/10/2014
-- @brief Test of Stacks package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Stacks;

procedure Stacks_Test is

  Num: constant Natural := 25;

  package Int_Stacks is new Stacks(Integer);
  use Int_Stacks;

  St, Ct: Stack;

begin

  Initialize(St);

  for I in 1..Num loop
    Push(I, St);
  end loop;
  Ct := Clone(St);

  Push(Num + 1, St);
  Put(Size(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Pop(St), Width => 0); Put(" ");
  end loop;
  New_Line;

  Push(Num + 1, Ct);
  Put(Size(Ct), Width => 0); Put(": ");
  while not Is_Empty(Ct) loop
    Put(Pop(Ct), Width => 0); Put(" ");
  end loop;
  New_Line;

  for I in 1..Num loop
    Push(I, St);
    if I mod 5 = 0 then
      Pop(St);
      Push(-1, St);
    end if;
  end loop;

  Put(Size(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Top(St), Width => 0); Put(" ");
    Pop(St);
  end loop;
  New_Line;

  for I in 1..Num loop
    Push(I, St);
  end loop;

  Put(Size(St), Width => 0); Put(" -> ");
  Clear(St);
  Put(Size(St), Width => 0); Put(" -> ");
  Clear(St);
  Put(Size(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Pop(St), Width => 0); Put(" ");
  end loop;
  New_Line(2);

  Free(St);
  Free(Ct);

end Stacks_Test;
