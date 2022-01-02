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


-- @filename Minheaps.adb
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 22/10/2007
-- @revision 21/01/2018
-- @brief Treatment of Minheaps

with Ada.Unchecked_Deallocation;

package body Minheaps is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Minheap_Rec, Minheap);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(M: out Minheap; Size: in Positive) is
  begin
    M := new Minheap_Rec(Size);
    M.Num := 0;
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(M: in Minheap) return Boolean is
  begin
    return M /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(M: in out Minheap) is
  begin
    if M /= null then
      Dispose(M);
      M := null;
    end if;
  end Free;

  ----------
  -- Size --
  ----------

  function Size(M: in Minheap) return Natural is
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;

    return M.Size;
  end Size;

  ---------
  -- Add --
  ---------

  procedure Add(E: in Item; M: in Minheap) is
    Position: Natural;
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;
    if M.Num = M.Size then
      raise Overloaded_Minheap_Error;
    end if;

    M.Num := M.Num + 1;
    Position := M.Num;
    while Position > 1 and then E < M.Elements(Position / 2) loop
      M.Elements(Position) := M.Elements(Position / 2);
      Position := Position / 2;
    end loop;
    M.Elements(Position) := E;
  end Add;

  --------------------
  -- Delete Minimum --
  --------------------

  procedure Delete_Minimum(M: in Minheap) is
    Position, Pos_Son: Natural;
    E: Item;
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;
    if M.Num = 0 then
      raise Void_Minheap_Error;
    end if;

    E := M.Elements(M.Num);
    M.Num := M.Num - 1;
    Position := 1;
    while 2 * Position <= M.Num loop
      Pos_Son := 2 * Position;
      if Pos_Son < M.Num and then M.Elements(Pos_Son + 1) < M.Elements(Pos_Son) then
        Pos_Son := Pos_Son + 1;
      end if;
      exit when not (M.Elements(Pos_Son) < E);
      M.Elements(Position) := M.Elements(Pos_Son);
      Position := Pos_Son;
    end loop;
    M.Elements(Position) := E;
  end Delete_Minimum;

  --------------------
  -- Delete Minimum --
  --------------------

  function Delete_Minimum(M: in Minheap) return Item is
    Aux: Item;
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;
    if M.Num = 0 then
      raise Void_Minheap_Error;
    end if;

    Aux := M.Elements(1);
    Delete_Minimum(M);
    return Aux;
  end Delete_Minimum;

  -------------
  -- Minimum --
  -------------

  function Minimum(M: in Minheap) return Item is
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;
    if M.Num = 0 then
      raise Void_Minheap_Error;
    end if;

    return M.Elements(1);
  end Minimum;

  ------------------------
  -- Number_Of_Elements --
  ------------------------

  function Number_Of_Elements(M: in Minheap) return Natural is
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;

    return M.Num;
  end Number_Of_Elements;

  --------------
  -- Is_Empty --
  --------------

  function Is_Empty(M: in Minheap) return Boolean is
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;

    return M.Num = 0;
  end Is_Empty;

  -----------
  -- Clear --
  -----------

  procedure Clear(M: in Minheap) is
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;

    M.Num := 0;
  end Clear;

  -----------
  -- Clone --
  -----------

  function Clone(M: in Minheap) return Minheap is
    Clon: Minheap;
  begin
    if M = null then
      raise Uninitialized_Minheap_Error;
    end if;

    Clon := new Minheap_Rec(M.Size);
    Clon.Num := M.Num;
    Clon.Elements(1..M.Num) := M.Elements(1..M.Num);

    return Clon;
  end Clone;

end Minheaps;
