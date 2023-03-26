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


-- @filename Finite_Disjoint_Lists.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 15/12/2017
-- @brief Treatment of Lists of Lists with Finite Disjoint Elements

with Ada.Unchecked_Deallocation;

package body Finite_Disjoint_Lists is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(List_Recs, PList_Recs);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(List_Of_Lists_Rec, List_Of_Lists);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Lol: out List_Of_Lists; Num_Elements: in Positive; Lol_Ini_Type: in List_Of_Lists_Initialization := Unassigned_Initialization) is
    L: List;
  begin
    -- Create List of Lists
    Lol := new List_Of_Lists_Rec(Num_Elements);

    -- Link all data
    for I in 1..Num_Elements loop
      Lol.Elements(I).List_Index := Unassigned;
      Lol.Elements(I).Prev := I - 1;
      Lol.Elements(I).Next := I + 1;
    end loop;
    Lol.Elements(1).Prev := Void;
    Lol.Elements(Num_Elements).Next := Void;

    -- Initialize all data to the unassigned list
    Lol.Lists := new List_Recs(0..Num_Elements);
    Lol.Lists(0).Id := Unassigned;
    Lol.Lists(0).Control.Num := Num_Elements;
    Lol.Lists(0).Control.First := 1;
    Lol.Lists(0).Control.Last := Num_Elements;
    Lol.Lists(0).Control.Current := Void;
    Initialize(Lol.Lists(0).Control.Saved);
    Lol.Lists(0).Prev := Void;
    Lol.Lists(0).Next := Void;

    -- Link all lists and initialize them without data
    for I in 1..Num_Elements loop
      Lol.Lists(I).Id := Unassigned;
      Lol.Lists(I).Control.Num := 0;
      Lol.Lists(I).Control.First := Void;
      Lol.Lists(I).Control.Last := Void;
      Lol.Lists(I).Control.Current := Void;
      Initialize(Lol.Lists(I).Control.Saved);
      Lol.Lists(I).Prev := I - 1;
      Lol.Lists(I).Next := I + 1;
    end loop;
    Lol.Lists(1).Prev := Void;
    Lol.Lists(Num_Elements).Next := Void;

    -- Initialize List of Lists
    Lol.Current_Id := Unassigned;
    Lol.Used.Num := 0;
    Lol.Used.First := Void;
    Lol.Used.Last := Void;
    Lol.Used.Current := Void;
    Initialize(Lol.Used.Saved);
    Lol.Free.Num := Num_Elements;
    Lol.Free.First := 1;
    Lol.Free.Last := Num_Elements;
    Lol.Free.Current := Void;
    Initialize(Lol.Free.Saved);

    -- Initialize List of Lists according to Initialization Type
    case Lol_Ini_Type is
      when Unassigned_Initialization =>
        null;
      when Isolated_Initialization =>
        for I in 1..Num_Elements loop
          L := New_List(Lol);
          Move(Get_Element(Lol, I), L);
        end loop;
      when Together_Initialization =>
        L := New_List(Lol);
        for I in 1..Num_Elements loop
          Move(Get_Element(Lol, I), L);
        end loop;
    end case;
  end Initialize;

  ------------------
  -- Reinitialize --
  ------------------

  procedure Reinitialize(Lol: in out List_Of_Lists; Lol_Ini_Type: in List_Of_Lists_Initialization := Unassigned_Initialization) is
    L: List;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    case Lol_Ini_Type is
      when Unassigned_Initialization =>
        Clear(Lol);
      when Isolated_Initialization =>
        Clear(Lol);
        for I in 1..Lol.Size loop
          L := New_List(Lol);
          Move(Get_Element(Lol, I), L);
        end loop;
      when Together_Initialization =>
        Clear(Lol);
        L := New_List(Lol);
        for I in 1..Lol.Size loop
          Move(Get_Element(Lol, I), L);
        end loop;
    end case;
  end Reinitialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Lol: in List_Of_Lists) return Boolean is
  begin
    return Lol /= null;
  end Is_Initialized;

  ------------------------------
  -- Increase_Number_Of_Lists --
  ------------------------------

  procedure Increase_Number_Of_Lists(Lol: in List_Of_Lists; Inc: in Positive := Default_Inc) is
    Ls: Plist_Recs;
    Num_Lists_Old, Num_Lists: Positive;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    -- Create new Lists array and copy old info
    Num_Lists_Old := Lol.Lists'Length - 1;
    Num_Lists := Num_Lists_Old + Inc;
    Ls := Lol.Lists;
    Lol.Lists := new List_Recs(0..Num_Lists);
    Lol.Lists(0..Num_Lists_Old) := Ls(0..Num_Lists_Old);
    Dispose(Ls);

    -- Link all new lists and initialize them without data
    for I in Num_Lists_Old + 1..Num_Lists loop
      Lol.Lists(I).Id := Unassigned;
      Lol.Lists(I).Control.Num := 0;
      Lol.Lists(I).Control.First := Void;
      Lol.Lists(I).Control.Last := Void;
      Lol.Lists(I).Control.Current := Void;
      Initialize(Lol.Lists(I).Control.Saved);
      Lol.Lists(I).Prev := I - 1;
      Lol.Lists(I).Next := I + 1;
    end loop;
    Lol.Lists(Num_Lists_Old + 1).Prev := Void;
    Lol.Lists(Num_Lists).Next := Void;

    -- Initialize List of Lists
    if Lol.Free.Num = 0 then
      Lol.Free.Num := Inc;
      Lol.Free.First := Num_Lists_Old + 1;
      Lol.Free.Last := Num_Lists;
      Lol.Free.Current := Void;
    else
      Lol.Lists(Num_Lists_Old + 1).Prev := Lol.Free.Last;
      Lol.Lists(Lol.Free.Last).Next := Num_Lists_Old + 1;
      Lol.Free.Num := Lol.Free.Num + Inc;
      Lol.Free.Last := Num_Lists;
    end if;
  end Increase_Number_Of_Lists;

  ----------
  -- Free --
  ----------

  procedure Free(Lol: in out List_Of_Lists) is
  begin
    if Lol /= null then
      for I in Lol.Lists'Range loop
        Free(Lol.Lists(I).Control.Saved);
      end loop;
      Dispose(Lol.Lists);
      Free(Lol.Used.Saved);
      Free(Lol.Free.Saved);
      Dispose(Lol);
      Lol := null;
    end if;
  end Free;

  -----------
  -- Clone --
  -----------

  function Clone(Lol: in List_Of_Lists) return List_Of_Lists is
    Lol_Clone: List_Of_Lists;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Lol_Clone := new List_Of_Lists_Rec(Lol.Size);
    Lol_Clone.Elements := Lol.Elements;
    Lol_Clone.Lists := new List_Recs(Lol.Lists'Range);
    Lol_Clone.Lists.all := Lol.Lists.all;
    for I in Lol.Lists'Range loop
      Lol_Clone.Lists(I).Control.Saved := Clone(Lol.Lists(I).Control.Saved);
    end loop;
    Lol_Clone.Current_Id := Lol.Current_Id;
    Lol_Clone.Used := Lol.Used;
    Lol_Clone.Used.Saved := Clone(Lol.Used.Saved);
    Lol_Clone.Free := Lol.Free;
    Lol_Clone.Free.Saved := Clone(Lol.Free.Saved);

    return Lol_Clone;
  end Clone;

  ------------------------
  -- Number_Of_Elements --
  ------------------------

  function Number_Of_Elements(Lol: in List_Of_Lists) return Natural is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    return Lol.Size;
  end Number_Of_Elements;

  ---------------------
  -- Number_Of_Lists --
  ---------------------

  function Number_Of_Lists(Lol: in List_Of_Lists) return Natural is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    return Lol.Used.Num;
  end Number_Of_Lists;

  --------------------
  -- Has_Unassigned --
  --------------------

  function Has_Unassigned(Lol: in List_Of_Lists) return Boolean is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    return Lol.Lists(0).Control.Num > 0;
  end Has_Unassigned;

  ---------------------
  -- Unassigned_List --
  ---------------------

  function Unassigned_List(Lol: in List_Of_Lists) return List is
    L: List;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    L.Lol := Lol;
    L.Id := Lol.Lists(Unassigned).Id;
    L.Index := Unassigned;
    return L;
  end Unassigned_List;

  --------------
  -- New_List --
  --------------

  function New_List(Lol: in List_Of_Lists) return List is
    L: List;
    Nxt: Natural;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;
    if Lol.Free.Num = 0 then
      Increase_Number_Of_Lists(Lol);
    end if;

    -- Generate new Id for the new List
    Lol.Current_Id := Lol.Current_Id + 1;

    -- Initialize List to be returned to the first Free List
    L.Lol := Lol;
    L.Id := Lol.Current_Id;
    L.Index := Lol.Free.First;
    Nxt := Lol.Lists(L.Index).Next;

    -- Add the new List at the end of the Used linked list
    if Lol.Used.Num = 0 then
      Lol.Used.First := L.Index;
      Lol.Used.Last := L.Index;
      Lol.Used.Current := Void;
      Lol.Lists(L.Index).Prev := Void;
      Lol.Lists(L.Index).Next := Void;
    else
      Lol.Lists(Lol.Used.Last).Next := L.Index;
      Lol.Lists(L.Index).Prev := Lol.Used.Last;
      Lol.Lists(L.Index).Next := Void;
      Lol.Used.Last := L.Index;
    end if;
    Lol.Used.Num := Lol.Used.Num + 1;

    -- Remove the new List from the begining of the Free linked list
    Lol.Free.Num := Lol.Free.Num - 1;
    if Lol.Free.Num = 0 then
      Lol.Free.First := Void;
      Lol.Free.Last := Void;
      Lol.Free.Current := Void;
    else
      Lol.Free.First := Nxt;
      if Lol.Free.Current = L.Index then
        Lol.Free.Current := Void;
      end if;
    end if;
    Lol.Lists(Nxt).Prev := Void;

    -- Initialize List_Rec of the new List
    Lol.Lists(L.Index).Id := Lol.Current_Id;
    Lol.Lists(L.Index).Control.Num := 0;
    Lol.Lists(L.Index).Control.First := Void;
    Lol.Lists(L.Index).Control.Last:= Void;
    Lol.Lists(L.Index).Control.Current:= Void;
    Clear(Lol.Lists(L.Index).Control.Saved);

    return L;
  end New_List;

  ------------
  -- Get_Id --
  ------------

  function Get_Id(L: in List) return Natural is
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    return L.Id;
  end Get_Id;

  ----------------------
  -- List_Of_Lists_Of --
  ----------------------

  function List_Of_Lists_Of(L: in List) return List_Of_Lists is
  begin
    return L.Lol;
  end List_Of_Lists_Of;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(L: in List; Lol: in List_Of_Lists) return Boolean is
  begin
    return L.Lol = Lol and then L.Lol.Lists(L.Index).Id = L.Id;
  end Belongs_To;

  ------------
  -- Remove --
  ------------

  procedure Remove(L: in List) is
    Prv, Nxt: Natural;
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;
    if L.Index = 0 then
      raise Unremovable_Unassigned_List_Error;
    end if;

    -- Remove all Elements of the List
    Move(L, Unassigned_List(L.Lol));

    -- Initializations
    L.Lol.Lists(L.Index).Id := Unassigned;
    Prv := L.Lol.Lists(L.Index).Prev;
    Nxt := L.Lol.Lists(L.Index).Next;

    -- Remove the List from the Used linked list
    L.Lol.Used.Num := L.Lol.Used.Num - 1;
    if L.Lol.Used.Num = 0 then
      L.Lol.Used.First := Void;
      L.Lol.Used.Last := Void;
      L.Lol.Used.Current := Void;
    else
      if Prv = Void then
        L.Lol.Used.First := Nxt;
      else
        L.Lol.Lists(Prv).Next := Nxt;
      end if;
      if Nxt = Void then
        L.Lol.Used.Last := Prv;
      else
        L.Lol.Lists(Nxt).Prev := Prv;
      end if;
      if L.Lol.Used.Current = L.Index then
        if Prv = Void then
          L.Lol.Used.Current := Void;
        else
          L.Lol.Used.Current := Prv;
        end if;
      end if;
    end if;

    -- Add the List to the end of the Free linked list
    if L.Lol.Free.Num = 0 then
      L.Lol.Free.First := L.Index;
      L.Lol.Free.Last := L.Index;
      L.Lol.Free.Current := Void;
      L.Lol.Lists(L.Index).Prev := Void;
      L.Lol.Lists(L.Index).Next := Void;
    else
      L.Lol.Lists(L.Lol.Free.Last).Next := L.Index;
      L.Lol.Lists(L.Index).Prev := L.Lol.Free.Last;
      L.Lol.Lists(L.Index).Next := Void;
      L.Lol.Free.Last := L.Index;
    end if;
    L.Lol.Free.Num := L.Lol.Free.Num + 1;

    -- Initialize List_Rec of the removed List
    L.Lol.Lists(L.Index).Id := Unassigned;
    L.Lol.Lists(L.Index).Control.Num := 0;
    L.Lol.Lists(L.Index).Control.First := Void;
    L.Lol.Lists(L.Index).Control.Last:= Void;
    L.Lol.Lists(L.Index).Control.Current:= Void;
    Clear(L.Lol.Lists(L.Index).Control.Saved);

  end Remove;

  ------------------
  -- Remove_Empty --
  ------------------

  procedure Remove_Empty(Lol: in List_Of_Lists) is
    L: List;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    -- Set Current List to the closest non-empty one
    while Has_Next_List(Lol) loop
      L := Get_List(Lol);
      exit when Number_Of_Elements(L) > 0;
      Next_List(Lol);
    end loop;

    -- Remove Void Lists
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      if Number_Of_Elements(L) = 0 then
        Remove(L);
      end if;
    end loop;
    Restore(Lol);
  end Remove_Empty;

  -----------
  -- Clear --
  -----------

  procedure Clear(Lol: in List_Of_Lists) is
    L: List;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      Remove(L);
    end loop;
    Clear(Lol.Used.Saved);
    Clear(Lol.Free.Saved);
  end Clear;

  -------------------
  -- Has_Next_List --
  -------------------

  function Has_Next_List(Lol: in List_Of_Lists) return Boolean is
    Has: Boolean := False;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    if Lol.Used.Num > 0 then
      if Lol.Used.Current = Void then
        Has := True;
      else
        Has := Lol.Lists(Lol.Used.Current).Next /= Void;
      end if;
    end if;
    return Has;
  end Has_Next_List;

  --------------
  -- Get_List --
  --------------

  function Get_List(Lol: in List_Of_Lists) return List is
    L: List;
    P: Natural;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;
    if Lol.Used.Num = 0 then
      raise No_More_Lists_Error;
    elsif Lol.Used.Current = Lol.Used.Last then
      raise No_More_Lists_Error;
    end if;

    if Lol.Used.Current = Void then
      P := Lol.Used.First;
    else
      P := Lol.Lists(Lol.Used.Current).Next;
    end if;
    L.Lol := Lol;
    L.Id := Lol.Lists(P).Id;
    L.Index := P;
    return L;
  end Get_List;

  ---------------
  -- Next_List --
  ---------------

  function Next_List(Lol: in List_Of_Lists) return List is
    L: List;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;
    if Lol.Used.Num = 0 then
      raise No_More_Lists_Error;
    elsif Lol.Used.Current = Lol.Used.Last then
      raise No_More_Lists_Error;
    end if;

    if Lol.Used.Current = Void then
      Lol.Used.Current := Lol.Used.First;
    else
      Lol.Used.Current := Lol.Lists(Lol.Used.Current).Next;
    end if;
    L.Lol := Lol;
    L.Id := Lol.Lists(Lol.Used.Current).Id;
    L.Index := Lol.Used.Current;
    return L;
  end Next_List;

  ---------------
  -- Next_List --
  ---------------

  procedure Next_List(Lol: in List_Of_Lists) is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;
    if Lol.Used.Num = 0 then
      raise No_More_Lists_Error;
    elsif Lol.Used.Current = Lol.Used.Last then
      raise No_More_Lists_Error;
    end if;

    if Lol.Used.Current = Void then
      Lol.Used.Current := Lol.Used.First;
    else
      Lol.Used.Current := Lol.Lists(Lol.Used.Current).Next;
    end if;
  end Next_List;

  -----------
  -- Reset --
  -----------

  procedure Reset(Lol: in List_Of_Lists) is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Lol.Used.Current := Void;
  end Reset;

  ----------
  -- Save --
  ----------

  procedure Save(Lol: in List_Of_Lists) is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Push(Lol.Used.Current, Lol.Used.Saved);
  end Save;

  -------------
  -- Restore --
  -------------

  procedure Restore(Lol: in List_Of_Lists) is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;
    if Is_Empty(Lol.Used.Saved) then
      raise No_Saved_Current_Position_Error;
    end if;

    Lol.Used.Current := Pop(Lol.Used.Saved);
  end Restore;

  -----------------
  -- Get_Element --
  -----------------

  function Get_Element(Lol: in List_Of_Lists; P: in Positive) return Element is
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    if P > Lol.Size then
      raise Index_Error;
    end if;
    return Element'(Lol, P);
  end Get_Element;

  --------------
  -- Index_Of --
  --------------

  function Index_Of(E: in Element) return Positive is
  begin
    return E.Index;
  end Index_Of;

  ------------------------
  -- Number_Of_Elements --
  ------------------------

  function Number_Of_Elements(L: in List) return Natural is
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    return L.Lol.Lists(L.Index).Control.Num;
  end Number_Of_Elements;

  ----------------------
  -- Has_Next_Element --
  ----------------------

  function Has_Next_Element(L: in List) return Boolean is
    Has: Boolean := False;
    C: List_Control;
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    C := L.Lol.Lists(L.Index).Control;
    if C.Num > 0 then
      if C.Current = Void then
        Has := True;
      else
        Has := L.Lol.Elements(C.Current).Next /= Void;
      end if;
    end if;
    return Has;
  end Has_Next_Element;

  -----------------
  -- Get_Element --
  -----------------

  function Get_Element(L: in List) return Element is
    C: List_Control;
    P: Natural;
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;
    C := L.Lol.Lists(L.Index).Control;
    if C.Num = 0 then
      raise No_More_Elements_Error;
    elsif C.Current = C.Last then
      raise No_More_Elements_Error;
    end if;

    if C.Current = Void then
      P := C.First;
    else
      P := L.Lol.Elements(C.Current).Next;
    end if;
    return Element'(L.Lol, P);
  end Get_Element;

  ------------------
  -- Next_Element --
  ------------------

  function Next_Element(L: in List) return Element is
    C: List_Control;
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;
    C := L.Lol.Lists(L.Index).Control;
    if C.Num = 0 then
      raise No_More_Elements_Error;
    elsif C.Current = C.Last then
      raise No_More_Elements_Error;
    end if;

    if C.Current = Void then
      C.Current := C.First;
    else
      C.Current := L.Lol.Elements(C.Current).Next;
    end if;
    L.Lol.Lists(L.Index).Control.Current := C.Current;
    return Element'(L.Lol, C.Current);
  end Next_Element;

  ------------------
  -- Next_Element --
  ------------------

  procedure Next_Element(L: in List) is
    C: List_Control;
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;
    C := L.Lol.Lists(L.Index).Control;
    if C.Num = 0 then
      raise No_More_Elements_Error;
    elsif C.Current = C.Last then
      raise No_More_Elements_Error;
    end if;

    if C.Current = Void then
      C.Current := C.First;
    else
      C.Current := L.Lol.Elements(C.Current).Next;
    end if;
    L.Lol.Lists(L.Index).Control.Current := C.Current;
  end Next_Element;

  -----------
  -- Reset --
  -----------

  procedure Reset(L: in List) is
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    L.Lol.Lists(L.Index).Control.Current := Void;
  end Reset;

  ----------
  -- Save --
  ----------

  procedure Save(L: in List) is
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    Push(L.Lol.Lists(L.Index).Control.Current, L.Lol.Lists(L.Index).Control.Saved);
  end Save;

  -------------
  -- Restore --
  -------------

  procedure Restore(L: in List) is
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;
    if Is_Empty(L.Lol.Lists(L.Index).Control.Saved) then
      raise No_Saved_Current_Position_Error;
    end if;

    L.Lol.Lists(L.Index).Control.Current := Pop(L.Lol.Lists(L.Index).Control.Saved);
  end Restore;

  ----------
  -- Move --
  ----------

  procedure Move(E: in Element; L: in List) is
    Prv, Nxt: Natural;
    Idx, Lst: Positive;
    Li: Natural;
  begin
    if E.Lol /= L.Lol then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    -- Initializations
    Idx := E.Index;
    Li := L.Lol.Elements(Idx).List_Index;
    Prv := L.Lol.Elements(Idx).Prev;
    Nxt := L.Lol.Elements(Idx).Next;

    -- Check whether the Element is already in the List
    if Li = L.Index then
      return;
    end if;

    -- Remove the Element from the origin List
    L.Lol.Lists(Li).Control.Num := L.Lol.Lists(Li).Control.Num - 1;
    if L.Lol.Lists(Li).Control.Num = 0 then
      L.Lol.Lists(Li).Control.First := Void;
      L.Lol.Lists(Li).Control.Last := Void;
      L.Lol.Lists(Li).Control.Current := Void;
    else
      if Prv = Void then
        L.Lol.Lists(Li).Control.First := Nxt;
      else
        L.Lol.Elements(Prv).Next := Nxt;
      end if;
      if Nxt = Void then
        L.Lol.Lists(Li).Control.Last:= Prv;
      else
        L.Lol.Elements(Nxt).Prev := Prv;
      end if;
      if L.Lol.Lists(Li).Control.Current = Idx then
        if Prv = Void then
          L.Lol.Lists(Li).Control.Current := Void;
        else
          L.Lol.Lists(Li).Control.Current := Prv;
        end if;
      end if;
    end if;

    -- Add the Element to the end of the destination List
    L.Lol.Elements(Idx).List_Index := L.Index;
    if L.Lol.Lists(L.Index).Control.Num = 0 then
      L.Lol.Lists(L.Index).Control.First := Idx;
      L.Lol.Lists(L.Index).Control.Last := Idx;
      L.Lol.Lists(L.Index).Control.Current := Void;
      L.Lol.Elements(Idx).Prev := Void;
      L.Lol.Elements(Idx).Next := Void;
    else
      Lst := L.Lol.Lists(L.Index).Control.Last;
      L.Lol.Elements(Lst).Next := Idx;
      L.Lol.Elements(Idx).Prev := Lst;
      L.Lol.Elements(Idx).Next := Void;
      L.Lol.Lists(L.Index).Control.Last := Idx;
    end if;
    L.Lol.Lists(L.Index).Control.Num := L.Lol.Lists(L.Index).Control.Num + 1;

  end Move;

  ----------
  -- Move --
  ----------

  procedure Move(From: in List; To: in List) is
    Lf: List renames From;
    Lt: List renames To;
    E: Element;
  begin
    if Lf.Lol /= Lt.Lol then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;
    if Lf.Lol.Lists(Lf.Index).Id /= Lf.Id then
      raise Not_A_List_Error;
    end if;
    if Lt.Lol.Lists(Lt.Index).Id /= Lt.Id then
      raise Not_A_List_Error;
    end if;

    -- Check whether the Lists are the same
    if Lf.Index = Lt.Index then
      return;
    end if;

    -- Check whether the From List is empty
    if Lf.Lol.Lists(Lf.Index).Control.Num = 0 then
      return;
    end if;

    -- Move all the Elements
    Reset(Lf);
    while Has_Next_Element(Lf) loop
      E := Next_Element(Lf);
      Move(E, Lt);
    end loop;

  end Move;

  ----------------------
  -- List_Of_Lists_Of --
  ----------------------

  function List_Of_Lists_Of(E: in Element) return List_Of_Lists is
  begin
    return E.Lol;
  end List_Of_Lists_Of;

  -------------
  -- List_Of --
  -------------

  function List_Of(E: in Element) return List is
    L: List;
  begin
    L.Lol := E.Lol;
    L.Index := E.Lol.Elements(E.Index).List_Index;
    L.Id := E.Lol.Lists(L.Index).Id;
    return L;
  end List_Of;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(E: in Element; Lol: in List_Of_Lists) return Boolean is
  begin
    return E.Lol = Lol;
  end Belongs_To;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(E: in Element; L: in List) return Boolean is
  begin
    if not Belongs_To(E, L.Lol) then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    return L.Index = L.Lol.Elements(E.Index).List_Index;
  end Belongs_To;

  --------------
  -- Unassign --
  --------------

  procedure Unassign(E: in Element) is
  begin
    Move(E, Unassigned_List(E.Lol));
  end Unassign;

end Finite_Disjoint_Lists;
