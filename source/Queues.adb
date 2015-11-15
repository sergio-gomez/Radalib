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


-- @filename Queues.adb
-- @author Javier Borge
-- @version 1.0
-- @date 5/10/2007
-- @revision 08/04/2012
-- @brief Treatment of Queues

with Ada.Unchecked_Deallocation;

package body Queues is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Node, Pnode);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Queue_Rec, Queue);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Q: out Queue) is
  begin
    Q := new Queue_Rec;
    Q.First := null;
    Q.Last := null;
    Q.Num := 0;
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Q: in Queue) return Boolean is
  begin
    return Q /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(Q: in out Queue) is
    P, S: Pnode;
  begin
    if Q /= null then
      P := Q.First;
      while P /= null loop
        S := P.Next;
        Dispose(P);
        P := S;
      end loop;
      Dispose(Q);
      Q := null;
    end if;
  end Free;

  -------------
  -- Enqueue --
  -------------

  procedure Enqueue(E: in Item; Q: in Queue) is
    P: Pnode;
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;

    P := new Node'(E, null);
    if Is_Empty(Q) then
      Q.First := P;
      Q.Last := P;
    else
      Q.Last.Next := P;
      Q.Last := P;
    end if;
    Q.Num := Q.Num + 1;
  end Enqueue;

  -------------
  -- Dequeue --
  -------------

  procedure Dequeue(Q: in Queue) is
    P: Pnode;
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;
    if Q.Num = 0 then
      raise Void_Queue_Error;
    end if;

    P := Q.First.Next;
    Dispose(Q.First);
    Q.First := P;
    Q.Num := Q.Num - 1;
  end Dequeue;

  -------------
  -- Dequeue --
  -------------

  function Dequeue(Q: in Queue) return Item is
    E: Item;
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;
    if Q.Num = 0 then
      raise Void_Queue_Error;
    end if;

    E := Q.First.Value;
    Dequeue(Q);
    return E;
  end Dequeue;


  ----------
  -- Head --
  ----------

  function Head(Q: in Queue) return Item is
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;
    if Q.Num = 0 then
      raise Void_Queue_Error;
    end if;

    return Q.First.Value;
  end Head;

  ----------
  -- Size --
  ----------

  function Size(Q: in Queue) return Natural is
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;

    return Q.Num;
  end Size;

  --------------
  -- Is_Empty --
  --------------

  function Is_Empty(Q: in Queue) return Boolean is
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;

    return Q.Num = 0;
  end Is_Empty;

  -----------
  -- Clear --
  -----------

  procedure Clear(Q: in Queue) is
    P, S: Pnode;
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;

    P := Q.First;
    while P /= null loop
      S := P.Next;
      Dispose(P);
      P := S;
    end loop;
    Q.First := null;
    Q.Last := null;
    Q.Num := 0;
  end Clear;

  -----------
  -- Clone --
  -----------

  function Clone(Q: in Queue) return Queue is
    Ct: Queue;
    P, S: Pnode;
  begin
    if Q = null then
      raise Uninitialized_Queue_Error;
    end if;

    Ct := new Queue_Rec;
    Ct.Num := Q.Num;
    if Q.Num = 0 then
      Ct.First := null;
      Ct.Last := null;
    else
      P := Q.First;
      S := new Node'(P.Value, null);
      Ct.First := S;
      while P.Next /= null loop
        P := P.Next;
        S.Next := new Node'(P.Value, null);
        S := S.Next;
      end loop;
      Ct.Last := S;
    end if;
    return Ct;
  end Clone;

end Queues;
