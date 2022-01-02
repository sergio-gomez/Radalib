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


-- @filename Linked_Lists.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 3/11/2004
-- @revision 25/03/2018
-- @brief Treatment of Linked Lists

with Ada.Unchecked_Deallocation;
with Minheaps;

package body Linked_Lists is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Node, Pnode);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Linked_List_Rec, Linked_List);

  --------------
  -- Get_Node --
  --------------

  function Get_Node(Pos: in Positive; Ll: in Linked_List) return Pnode is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    if Pos = 1 then
      return Ll.First;
    elsif Pos = Ll.Num then
      return Ll.Last;
    elsif Pos < Ll.Num then
      P := Ll.First;
      for I in 2..Pos loop
        P := P.Next;
      end loop;
      return P;
    else
      return null;
    end if;
  end Get_Node;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Ll: out Linked_List) is
  begin
    Ll := new Linked_List_Rec;
    Ll.First := null;
    Ll.Last := null;
    Ll.Current:= null;
    Initialize(Ll.Saved);
    Ll.Num := 0;
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Ll: in Linked_List) return Boolean is
  begin
    return Ll /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(Ll: in out Linked_List) is
    P, Q: Pnode;
  begin
    if Ll /= null then
      P := Ll.First;
      while P /= null loop
        Q := P.Next;
        Dispose(P);
        P := Q;
      end loop;
      Free(Ll.Saved);
      Dispose(Ll);
      Ll := null;
    end if;
  end Free;

  ---------------
  -- Add_First --
  ---------------

  procedure Add_First(E: in Item; Ll: in Linked_List) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    P := new Node'(E, Ll.First, null);
    Ll.First := P;
    Ll.Num := Ll.Num + 1;
    if Ll.Num = 1 then
      Ll.Last := P;
      Ll.Current := P;
    else
      P.Next.Prev := P;
      if Ll.Current = P.Next then
        Ll.Current := P;
      end if;
    end if;
  end Add_First;

  --------------
  -- Add_Last --
  --------------

  procedure Add_Last(E: in Item; Ll: in Linked_List) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    P := new Node'(E, null, Ll.Last);
    Ll.Last := P;
    Ll.Num := Ll.Num + 1;
    if Ll.Num = 1 then
      Ll.First := P;
      Ll.Current := P;
    else
      P.Prev.Next := P;
    end if;
  end Add_Last;

  ---------
  -- Add --
  ---------

  procedure Add(E: in Item; Pos: in Positive; Ll: in Linked_List) is
    P, Q: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Pos > Ll.Num + 1 then
      raise Index_Out_Of_Bounds_Error;
    end if;

    if Pos = 1 then
      Add_First(E, Ll);
    elsif Pos = Ll.Num + 1 then
      Add_Last(E, Ll);
    else
      Q := Get_Node(Pos, Ll);
      P := new Node'(E, Q, Q.Prev);
      Q.Prev := P;
      P.Prev.Next := P;
      Ll.Num := Ll.Num + 1;
      if Ll.Current = Q then
        Ll.Current := P;
      end if;
    end if;
  end Add;

  ---------
  -- Add --
  ---------

  procedure Add(E: in Item; Ll: in Linked_List) is
    P, Q: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    if Ll.Num = 0 or Ll.Current = Ll.First then
      Add_First(E, Ll);
    elsif Ll.Current = null then
      Add_Last(E, Ll);
    else
      Q := Ll.Current;
      P := new Node'(E, Q, Q.Prev);
      Q.Prev := P;
      P.Prev.Next := P;
      Ll.Num := Ll.Num + 1;
      Ll.Current := P;
    end if;
  end Add;

  ----------
  -- Size --
  ----------

  function Size(Ll: in Linked_List) return Natural is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    return Ll.Num;
  end Size;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(E: in Item; Ll: in Linked_List) return Boolean is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    P := Ll.First;
    while P /= null loop
      if P.Value = E then
        return True;
      end if;
      P := P.Next;
    end loop;
    return False;
  end Belongs_To;

  --------------
  -- Position --
  --------------

  function Position(E: in Item; Ll: in Linked_List) return Natural is
    P: Pnode;
    Pos: Natural;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    Pos := 0;
    P := Ll.First;
    while P /= null loop
      Pos := Pos + 1;
      if P.Value = E then
        return Pos;
      end if;
      P := P.Next;
    end loop;
    return 0;
  end Position;

  ---------------
  -- Get_First --
  ---------------

  function Get_First(Ll: in Linked_List) return Item is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    return Ll.First.Value;
  end Get_First;

  --------------
  -- Get_Last --
  --------------

  function Get_Last(Ll: in Linked_List) return Item is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    return Ll.Last.Value;
  end Get_Last;

  ---------
  -- Get --
  ---------

  function Get(Pos: in Positive; Ll: in Linked_List) return Item is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Pos > Ll.Num + 1 then
      raise Index_Out_Of_Bounds_Error;
    end if;

    P := Get_Node(Pos, Ll);
    return P.Value;
  end Get;

  ---------
  -- Get --
  ---------

  function Get(Ll: in Linked_List) return Item is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Ll.Current = null then
      raise No_More_Items_Error;
    end if;

    return Ll.Current.Value;
  end Get;

  -------------------
  -- Replace_First --
  -------------------

  procedure Replace_First(Ll: in Linked_List; E: in Item) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    Ll.First.Value := E;
  end Replace_First;

  ------------------
  -- Replace_Last --
  ------------------

  procedure Replace_Last(Ll: in Linked_List; E: in Item) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    Ll.Last.Value := E;
  end Replace_Last;

  -------------
  -- Replace --
  -------------

  procedure Replace(Pos: in Positive; Ll: in Linked_List; E: in Item) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Pos > Ll.Num + 1 then
      raise Index_Out_Of_Bounds_Error;
    end if;

    P := Get_Node(Pos, Ll);
    P.Value := E;
  end Replace;

  -------------
  -- Replace --
  -------------

  procedure Replace(Ll: in Linked_List; E: in Item) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Ll.Current = null then
      raise No_More_Items_Error;
    end if;

    Ll.Current.Value := E;
  end Replace;

  ------------------
  -- Remove_First --
  ------------------

  procedure Remove_First(Ll: in Linked_List) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    P := Ll.First;
    Ll.First := P.Next;
    Ll.Num := Ll.Num - 1;
    if Ll.Current = P then
      Ll.Current := P.Next;
    end if;
    if Ll.Num = 0 then
      Ll.Last := null;
    else
      Ll.First.Prev := null;
    end if;
    Dispose(P);
  end Remove_First;

  -----------------
  -- Remove_Last --
  -----------------

  procedure Remove_Last(Ll: in Linked_List) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    P := Ll.Last;
    Ll.Last := P.Prev;
    Ll.Num := Ll.Num - 1;
    if Ll.Current = P then
      Ll.Current := null;
    end if;
    if Ll.Num = 0 then
      Ll.First := null;
    else
      Ll.Last.Next := null;
    end if;
    Dispose(P);
  end Remove_Last;

  ------------
  -- Remove --
  ------------

  procedure Remove(Pos: in Positive; Ll: in Linked_List) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Pos > Ll.Num then
      raise Index_Out_Of_Bounds_Error;
    end if;

    if Pos = 1 then
      Remove_First(Ll);
    elsif Pos = Ll.Num then
      Remove_Last(Ll);
    else
      P := Get_Node(Pos, Ll);
      P.Prev.Next := P.Next;
      P.Next.Prev := P.Prev;
      Ll.Num := Ll.Num - 1;
      if Ll.Current = P then
        Ll.Current := P.Next;
      end if;
      Dispose(P);
    end if;
  end Remove;

  ------------
  -- Remove --
  ------------

  procedure Remove(Ll: in Linked_List) is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Current = null then
      raise No_More_Items_Error;
    end if;

    if Ll.Num = 1 or Ll.Current = Ll.First then
      Remove_First(Ll);
    elsif Ll.Current = Ll.Last then
      Remove_Last(Ll);
    else
      P := Ll.Current;
      P.Prev.Next := P.Next;
      P.Next.Prev := P.Prev;
      Ll.Num := Ll.Num - 1;
      Ll.Current := P.Next;
      Dispose(P);
    end if;
  end Remove;

  ----------------
  -- Remove_All --
  ----------------

  procedure Remove_All(E: in Item; Ll: in Linked_List) is
    P, Q: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    P := Ll.First;
    while P /= null loop
      if P.Value = E then
        if P = Ll.First then
          Remove_First(Ll);
          P := Ll.First;
        elsif P = Ll.Last then
          Remove_Last(Ll);
          P := Ll.Last;
        else
          P.Prev.Next := P.Next;
          P.Next.Prev := P.Prev;
          Ll.Num := Ll.Num - 1;
          if Ll.Current = P then
            Ll.Current := P.Next;
          end if;
          Q := P.Prev;
          Dispose(P);
          P := Q;
        end if;
      else
        P := P.Next;
      end if;
    end loop;
  end Remove_All;

  --------------
  -- Has_Next --
  --------------

  function Has_Next(Ll: in Linked_List) return Boolean is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    return Ll.Current /= null;
  end Has_Next;

  --------------
  -- Has_Prev --
  --------------

  function Has_Prev(Ll: in Linked_List) return Boolean is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    return Ll.Current /= Ll.First;
  end Has_Prev;

  ----------
  -- Next --
  ----------

  function Next(Ll: in Linked_List) return Item is
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Ll.Current = null then
      raise No_More_Items_Error;
    end if;

    P := Ll.Current;
    Ll.Current := P.Next;
    return P.Value;
  end Next;

  ----------
  -- Next --
  ----------

  procedure Next(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Ll.Current = null then
      raise No_More_Items_Error;
    end if;

    Ll.Current := Ll.Current.Next;
  end Next;

  ----------
  -- Prev --
  ----------

  function Prev(Ll: in Linked_List) return Item is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Ll.Current = Ll.First then
      raise No_More_Items_Error;
    end if;

    if Ll.Current = null then
      Ll.Current := Ll.Last;
    else
      Ll.Current := Ll.Current.Prev;
    end if;
    return Ll.Current.Value;
  end Prev;

  ----------
  -- Prev --
  ----------

  procedure Prev(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Ll.Current = Ll.First then
      raise No_More_Items_Error;
    end if;

    if Ll.Current = null then
      Ll.Current := Ll.Last;
    else
      Ll.Current := Ll.Current.Prev;
    end if;
  end Prev;

  -----------
  -- Reset --
  -----------

  procedure Reset(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    Ll.Current := Ll.First;
  end Reset;

  ---------------
  -- Set_First --
  ---------------

  procedure Set_First(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    Ll.Current := Ll.First;
  end Set_First;

  --------------
  -- Set_Last --
  --------------

  procedure Set_Last(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;

    Ll.Current := Ll.Last;
  end Set_Last;

  -------------
  -- Set_End --
  -------------

  procedure Set_End(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    Ll.Current := null;
  end Set_End;

  ---------
  -- Set --
  ---------

  procedure Set(Pos: in Positive; Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Ll.Num = 0 then
      raise Void_Linked_List_Error;
    end if;
    if Pos > Ll.Num + 1 then
      raise Index_Out_Of_Bounds_Error;
    end if;

    if Pos = 1 then
      Ll.Current := Ll.First;
    elsif Pos = Ll.Num then
      Ll.Current := Ll.Last;
    elsif Pos = Ll.Num + 1 then
      Ll.Current := null;
    else
      Ll.Current := Get_Node(Pos, Ll);
    end if;
  end Set;

  ----------
  -- Save --
  ----------

  procedure Save(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    Push(Ll.Current, Ll.Saved);
  end Save;

  -------------
  -- Restore --
  -------------

  procedure Restore(Ll: in Linked_List) is
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;
    if Is_Empty(Ll.Saved) then
      raise No_Saved_Current_Position_Error;
    end if;

    Ll.Current := Pop(Ll.Saved);
  end Restore;

  -----------
  -- Clear --
  -----------

  procedure Clear(Ll: in Linked_List) is
    P, Q: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    P := Ll.First;
    while P /= null loop
      Q := P.Next;
      Dispose(P);
      P := Q;
    end loop;
    Ll.First := null;
    Ll.Last := null;
    Ll.Current:= null;
    Clear(Ll.Saved);
    Ll.Num := 0;
  end Clear;

  -----------
  -- Clone --
  -----------

  function Clone(Ll: in Linked_List) return Linked_List is
    Ll_Clone: Linked_List;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    Initialize(Ll_Clone);
    Save(Ll);
    Reset(Ll);
    while Has_Next(Ll) loop
      Add_Last(Next(Ll), Ll_Clone);
    end loop;
    Restore(Ll);
    Reset(Ll_Clone);
    return Ll_Clone;
  end Clone;

  ----------
  -- Join --
  ----------

  function Join(Ll1, Ll2: in Linked_List) return Linked_List is
    Jl: Linked_List;
  begin
    if Ll1 = null or Ll2 = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    Jl := Clone(Ll1);
    Save(Ll2);
    Reset(Ll2);
    while Has_Next(Ll2) loop
      Add_Last(Next(Ll2), Jl);
    end loop;
    Restore(Ll2);
    Reset(Jl);
    return Jl;
  end Join;

  ------------------
  -- Generic_Sort --
  ------------------

  procedure Generic_Sort(Ll: in Linked_List) is
    procedure Insertion_Sort is new Generic_Insertion_Sort(Lower);
    procedure Minheap_Sort is new Generic_Minheap_Sort(Lower);

    Cutoff: constant := 50;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    if Ll.Num in 2..Cutoff then
      Insertion_Sort(Ll.First, Ll.Num);
      Reset(Ll);
      Clear(Ll.Saved);
    elsif Ll.Num > Cutoff then
      Minheap_Sort(Ll);
      Reset(Ll);
      Clear(Ll.Saved);
    end if;
  end Generic_Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(Ll: in Linked_List; Lower: in Comparator) is
    procedure Sorter is new Generic_Sort(Lower.all);
  begin
    Sorter(Ll);
  end Sort;

  --------------------------------
  -- Remove_Adjacent_Duplicates --
  --------------------------------

  procedure Remove_Adjacent_Duplicates(Ll: in Linked_List) is
    P, Q: Pnode;
    N: Natural;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    N := Ll.Num;
    if N >= 2 then
      P := Ll.First;
      Q := P.Next;
      for I in 2..N-1 loop
        if P.Value = Q.Value then
          P.Next := Q.Next;
          Q.Next.Prev := P;
          Ll.Num := Ll.Num - 1;
          if Ll.Current = Q then
            Ll.Current := Q.Next;
          end if;
          Dispose(Q);
          Q := P.Next;
        else
          P := Q;
          Q := Q.Next;
        end if;
      end loop;
      if P.Value = Q.Value then
        Remove_Last(Ll);
      end if;
      Reset(Ll);
      Clear(Ll.Saved);
    end if;
  end Remove_Adjacent_Duplicates;

  ----------
  -- Swap --
  ----------

  procedure Swap(P1, P2: in Pnode) is
    Tmp: Item;
  begin
    Tmp := P1.Value;
    P1.Value := P2.Value;
    P2.Value := Tmp;
  end Swap;

  ----------------------------
  -- Generic_Insertion_Sort --
  ----------------------------

  procedure Generic_Insertion_Sort(Left: in Pnode; Num: in Positive) is
    P, J: Pnode;
    Tmp: Item;
  begin
    if Num >= 2 then
      P := Left.Next;
      for I in 2..Num loop
        J := P;
        Tmp := P.Value;
        while J /= Left and then Lower(Tmp, J.Prev.Value) loop
          J.Value := J.Prev.Value;
          J := J.Prev;
        end loop;
        J.Value := Tmp;
        P := P.Next;
      end loop;
    end if;
  end Generic_Insertion_Sort;

  --------------------------
  -- Generic_Minheap_Sort --
  --------------------------

  procedure Generic_Minheap_Sort(Ll: in Linked_List) is
    package Minheap_Items is new Minheaps(Item, Lower);
    use Minheap_Items;

    H: Minheap;
    P: Pnode;
  begin
    if Ll = null then
      raise Uninitialized_Linked_List_Error;
    end if;

    if Ll.Num >= 2 then
      Initialize(H, Ll.Num);
      P := Ll.First;
      while P /= null loop
        Add(P.Value, H);
        P := P.Next;
      end loop;
      P := Ll.First;
      while not Is_Empty(H) loop
        P.Value := Delete_Minimum(H);
        P := P.Next;
      end loop;
      Free(H);
    end if;
  end Generic_Minheap_Sort;

end Linked_Lists;
