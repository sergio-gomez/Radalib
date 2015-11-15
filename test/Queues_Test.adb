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


-- @filename Queues_Test.adb
-- @author Javier Borge
-- @version 1.0
-- @date 5/10/2007
-- @revision 26/10/2014
-- @brief Test of Queues package

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Queues;

procedure Queues_Test is

  Num: constant Natural := 25;

  package Int_Queues is new Queues(Integer);
  use Int_Queues;

  St, Ct: Queue;

begin

  Initialize(St);

  for I in 1..Num loop
    Enqueue(I, St);
  end loop;
  Ct := Clone(St);

  Enqueue(Num + 1, St);
  Put(Size(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Dequeue(St), Width => 0); Put(" ");
  end loop;
  New_Line;

  Enqueue(Num + 1, Ct);
  Put(Size(Ct), Width => 0); Put(": ");
  while not Is_Empty(Ct) loop
    Put(Dequeue(Ct), Width => 0); Put(" ");
  end loop;
  New_Line;

  for I in 1..Num loop
    Enqueue(I, St);
    if I mod 5 = 0 then
      Enqueue(Dequeue(St), St);
    end if;
  end loop;

  Put(Size(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Head(St), Width => 0); Put(" ");
    Dequeue(St);
  end loop;
  New_Line;

  for I in 1..Num loop
    Enqueue(I, St);
  end loop;

  Put(Size(St), Width => 0); Put(" -> ");
  Clear(St);
  Put(Size(St), Width => 0); Put(" -> ");
  Clear(St);
  Put(Size(St), Width => 0); Put(": ");
  while not Is_Empty(St) loop
    Put(Dequeue(St), Width => 0); Put(" ");
  end loop;
  New_Line(2);

  Free(St);
  Free(Ct);

end Queues_Test;
