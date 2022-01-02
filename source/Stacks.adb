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


-- @filename Stacks.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 4/04/2005
-- @revision 08/04/2012
-- @brief Treatment of Stacks

with Ada.Unchecked_Deallocation;

package body Stacks is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Node, Pnode);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Stack_Rec, Stack);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(St: out Stack) is
  begin
    St := new Stack_Rec;
    St.Top := null;
    St.Num := 0;
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(St: in Stack) return Boolean is
  begin
    return St /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(St: in out Stack) is
    P, Q: Pnode;
  begin
    if St /= null then
      P := St.Top;
      while P /= null loop
        Q := P.Next;
        Dispose(P);
        P := Q;
      end loop;
      Dispose(St);
      St := null;
    end if;
  end Free;

  ----------
  -- Push --
  ----------

  procedure Push(E: in Item; St: in Stack) is
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;

    St.Top := new Node'(E, St.Top);
    St.Num := St.Num + 1;
  end Push;

  ---------
  -- Pop --
  ---------

  procedure Pop(St: in Stack) is
    P: Pnode;
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;
    if St.Num = 0 then
      raise Void_Stack_Error;
    end if;

    P := St.Top.Next;
    Dispose(St.Top);
    St.Top := P;
    St.Num := St.Num - 1;
  end Pop;

  ---------
  -- Pop --
  ---------

  function Pop(St: in Stack) return Item is
    E: Item;
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;
    if St.Num = 0 then
      raise Void_Stack_Error;
    end if;

    E := St.Top.Value;
    Pop(St);
    return E;
  end Pop;

  ---------
  -- Top --
  ---------

  function Top(St: in Stack) return Item is
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;
    if St.Num = 0 then
      raise Void_Stack_Error;
    end if;

    return St.Top.Value;
  end Top;

  ----------
  -- Size --
  ----------

  function Size(St: in Stack) return Natural is
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;

    return St.Num;
  end Size;

  --------------
  -- Is_Empty --
  --------------

  function Is_Empty(St: in Stack) return Boolean is
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;

    return St.Num = 0;
  end Is_Empty;

  -----------
  -- Clear --
  -----------

  procedure Clear(St: in Stack) is
    P, Q: Pnode;
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;

    P := St.Top;
    while P /= null loop
      Q := P.Next;
      Dispose(P);
      P := Q;
    end loop;
    St.Top := null;
    St.Num := 0;
  end Clear;

  -----------
  -- Clone --
  -----------

  function Clone(St: in Stack) return Stack is
    Ct: Stack;
    P, Q: Pnode;
  begin
    if St = null then
      raise Uninitialized_Stack_Error;
    end if;

    Ct := new Stack_Rec;
    Ct.Num := St.Num;
    if St.Num = 0 then
      Ct.Top := null;
    else
      P := St.Top;
      Q := new Node'(P.Value, null);
      Ct.Top := Q;
      while P.Next /= null loop
        P := P.Next;
        Q.Next := new Node'(P.Value, null);
        Q := Q.Next;
      end loop;
    end if;
    return Ct;
  end Clone;

end Stacks;

