-- Radalib, Copyright (c) 2018 by
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


-- @filename Finite_Disjoint_Lists-Algorithms.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 5/10/2005
-- @revision 19/12/2014
-- @brief Implementation of Lists of Lists Algorithms

with Utils; use Utils;
with Arrays_Utils_Integer; use Arrays_Utils_Integer;
with Linked_Lists;

package body Finite_Disjoint_Lists.Algorithms is

  -- Sort_List utils
  package Int_Linked_Lists is new Linked_Lists(Integer);
  use Int_Linked_Lists;

  function Lt(Left, Right: in Integer) return Boolean;
  pragma Inline(Lt);

  function Lt(Left, Right: in Integer) return Boolean is
  begin
    return Left < Right;
  end Lt;

  -- Sort_By_Size utils
  type Size_Rec is record
    Size: Natural;
    Index: Natural;
  end record;

  package Size_Linked_Lists is new Linked_Lists(Size_Rec);
  use Size_Linked_Lists;

  function Gt(Left, Right: in Size_Rec) return Boolean;
  pragma Inline(Gt);

  function Gt(Left, Right: in Size_Rec) return Boolean is
  begin
    return Left.Size > Right.Size;
  end Gt;

  ---------------
  -- Sort_List --
  ---------------

  procedure Sort_List(L: in List) is
    Ll: Int_Linked_Lists.Linked_List;
    S: List;
    E: Element;
  begin
    if L.Lol.Lists(L.Index).Id /= L.Id then
      raise Not_A_List_Error;
    end if;

    if Number_Of_Elements(L) > 1 then
      S := New_List(L.Lol);
      Initialize(Ll);
      Reset(L);
      while Has_Next_Element(L) loop
        E := Next_Element(L);
        Add_Last(Index_Of(E), Ll);
        Move(E, S);
      end loop;
      Sort(Ll, Lt'Access);
      Reset(Ll);
      while Has_Next(Ll) loop
        E := Get_Element(L.Lol, Next(Ll));
        Move(E, L);
      end loop;
      Clear(L.Lol.Lists(L.Index).Control.Saved);
      Reset(L);
      Free(Ll);
      Remove(S);
    end if;
  end Sort_List;

  ----------------
  -- Sort_Lists --
  ----------------

  procedure Sort_Lists(Lol: in List_Of_Lists) is
    L: List;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      Sort_List(L);
    end loop;
    Restore(Lol);
    Sort_List(Unassigned_List(Lol));
  end Sort_Lists;

  ------------------
  -- Sort_By_Size --
  ------------------

  procedure Sort_By_Size(Lol: in List_Of_Lists) is
    Ll: Size_Linked_Lists.Linked_List;
    L: List;
    Sr: Size_Rec;
    I, Iprev: Natural;
  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    if Lol.Used.Num >= 2 then
      Initialize(Ll);

      Save(Lol);
      Reset(Lol);
      while Has_Next_List(Lol) loop
        L := Next_List(Lol);
        Sr := (Lol.Lists(L.Index).Control.Num, L.Index);
        Add_Last(Sr, Ll);
      end loop;
      Restore(Lol);
      Sort(Ll, Gt'Access);

      Iprev := Void;
      Reset(Ll);
      while Has_Next(Ll) loop
        Sr := Next(Ll);
        I := Sr.Index;
        Lol.Lists(I).Prev := Iprev;
        if Iprev = Void then
          Lol.Used.First := I;
        else
          Lol.Lists(Iprev).Next := I;
        end if;
        Iprev := I;
      end loop;
      Lol.Lists(Iprev).Next := Void;
      Lol.Used.Last := Iprev;
      Clear(Lol.Used.Saved);
      Reset(Lol);

      Free(Ll);
    end if;
  end Sort_By_Size;

  ------------------------
  -- Maximal_Refinement --
  ------------------------

  function Maximal_Refinement(Lol1, Lol2: in List_Of_Lists) return List_Of_Lists is
    Lol_Mr: List_Of_Lists;
    Comms: PPsIntegers;
    Index: PIntegers;
    N: Positive;
    L: List;
    E: Element;
    I: Positive;
    Id: Natural;
  begin
    if Lol1 = null or Lol2 = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;
    if Lol1.Size /= Lol2.Size then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;
    if Has_Unassigned(Lol1) or Has_Unassigned(Lol2) then
      raise Unassigned_Elements_Error;
    end if;

    N := Lol1.Size;
    Initialize(Lol_Mr, N, Unassigned_Initialization);
    Comms := Alloc_Rectangular(1, N, 1, 2);

    Save(Lol1);
    Reset(Lol1);
    while Has_Next_List(Lol1) loop
      L := Next_List(Lol1);
      Id := Get_Id(L);
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        E := Next_Element(L);
        I := Index_Of(E);
        Comms(I)(1) := Id;
      end loop;
    end loop;
    Restore(Lol1);

    Save(Lol2);
    Reset(Lol2);
    while Has_Next_List(Lol2) loop
      L := Next_List(Lol2);
      Id := Get_Id(L);
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        E := Next_Element(L);
        I := Index_Of(E);
        Comms(I)(2) := Id;
      end loop;
    end loop;
    Restore(Lol2);

    Sort(Comms, Index);
    for J in 1..N loop
      if J = 1 or else Comms(J).all /= Comms(J - 1).all then
        L := New_List(Lol_Mr);
      end if;
      E := Get_Element(Lol_Mr, Index(J));
      Move(E, L);
    end loop;
    Free(Comms);
    Free(Index);

    return Lol_Mr;
  end Maximal_Refinement;

  --------------------------------------
  -- Generic_All_Partitions_Traversal --
  --------------------------------------

  procedure Generic_All_Partitions_Traversal(Num_Elements: in Positive) is

    procedure Add_Next(Lol: in List_Of_Lists; I: in Positive) is
      E: Element;
      L: List;
    begin
      E := Get_Element(Lol, I);
      Save(Lol);
      Reset(Lol);
      while Has_Next_List(Lol) loop
        L := Next_List(Lol);
        Move(E, L);
        if I < Number_Of_Elements(Lol) then
          Add_Next(Lol, I + 1);
        else
          Handler(Lol);
        end if;
      end loop;
      Restore(Lol);
      L := New_List(Lol);
      Move(E, L);
      if I < Number_Of_Elements(Lol) then
        Add_Next(Lol, I + 1);
      else
        Handler(Lol);
      end if;
      Remove(L);
    end Add_Next;

    Lol: List_Of_Lists;

  begin
    Initialize(Lol, Num_Elements);
    Add_Next(Lol, 1);
    Free(Lol);
  end Generic_All_Partitions_Traversal;

  ------------------------------
  -- All_Partitions_Traversal --
  ------------------------------

  procedure All_Partitions_Traversal(Num_Elements: in Positive; Handler: in List_Of_Lists_Handler) is
    procedure Traversal is new Generic_All_Partitions_Traversal(Handler.all);
  begin
    Traversal(Num_Elements);
  end All_Partitions_Traversal;

  -----------------------------------
  -- Generic_All_Subsets_Traversal --
  -----------------------------------

  procedure Generic_All_Subsets_Traversal(Num_Elements: in Positive) is
    procedure Traversal is new Generic_All_Unassigned_Subsets_Traversal(Handler);
    Lol: List_Of_Lists;
  begin
    Initialize(Lol, Num_Elements);
    Traversal(Lol);
    Free(Lol);
  end Generic_All_Subsets_Traversal;

  ---------------------------
  -- All_Subsets_Traversal --
  ---------------------------

  procedure All_Subsets_Traversal(Num_Elements: in Positive; Handler: in List_Handler) is
    procedure Traversal is new Generic_All_Subsets_Traversal(Handler.all);
  begin
    Traversal(Num_Elements);
  end All_Subsets_Traversal;

  -------------------------------
  -- Generic_Subsets_Traversal --
  -------------------------------

  procedure Generic_Subsets_Traversal(Num_Elements: in Positive; Max_Size: in Natural) is
    procedure Traversal is new Generic_Unassigned_Subsets_Traversal(Handler);
    Lol: List_Of_Lists;
  begin
    Initialize(Lol, Num_Elements);
    Traversal(Lol, Max_Size);
    Free(Lol);
  end Generic_Subsets_Traversal;

  -----------------------
  -- Subsets_Traversal --
  -----------------------

  procedure Subsets_Traversal(Num_Elements: in Positive; Max_Size: in Natural; Handler: in List_Handler) is
    procedure Traversal is new Generic_Subsets_Traversal(Handler.all);
  begin
    Traversal(Num_Elements, Max_Size);
  end Subsets_Traversal;

  ----------------------------------------------
  -- Generic_All_Unassigned_Subsets_Traversal --
  ----------------------------------------------

  procedure Generic_All_Unassigned_Subsets_Traversal(Lol: in List_Of_Lists) is

    procedure Add_Next(L, X, U: in List) is
      N: Natural;
      E: Element;
    begin
      N := Number_Of_Elements(U);
      if N = 0 then
        Handler(L);
      else
        Reset(U);
        E := Next_Element(U);
        Move(E, L);
        Add_Next(L, X, U);
        Move(E, X);
        Add_Next(L, X, U);
        Move(E, U);
      end if;
    end Add_Next;

    L, X, U: List;

  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Next_List(Lol);
    end Loop;
    L := New_List(Lol);
    X := New_List(Lol);
    U := Unassigned_List(Lol);
    Add_Next(L, X, U);
    Remove(L);
    Remove(X);
    Restore(Lol);
  end Generic_All_Unassigned_Subsets_Traversal;

  --------------------------------------
  -- All_Unassigned_Subsets_Traversal --
  --------------------------------------

  procedure All_Unassigned_Subsets_Traversal(Lol: in List_Of_Lists; Handler: in List_Handler) is
    procedure Traversal is new Generic_All_Unassigned_Subsets_Traversal(Handler.all);
  begin
    Traversal(Lol);
  end All_Unassigned_Subsets_Traversal;

  ------------------------------------------
  -- Generic_Unassigned_Subsets_Traversal --
  ------------------------------------------

  procedure Generic_Unassigned_Subsets_Traversal(Lol: in List_Of_Lists; Max_Size: in Natural) is

    procedure Add_Next(L, X, U: in List) is
      Nu, Nl: Natural;
      E: Element;
    begin
      Nu := Number_Of_Elements(U);
      Nl := Number_Of_Elements(L);
      if Nu = 0 or Nl = Max_Size then
        Handler(L);
      else
        Reset(U);
        E := Next_Element(U);
        Move(E, L);
        Add_Next(L, X, U);
        Move(E, X);
        Add_Next(L, X, U);
        Move(E, U);
      end if;
    end Add_Next;

    L, X, U: List;

  begin
    if Lol = null then
      raise Uninitialized_List_Of_Lists_Error;
    end if;

    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Next_List(Lol);
    end Loop;
    L := New_List(Lol);
    X := New_List(Lol);
    U := Unassigned_List(Lol);
    Add_Next(L, X, U);
    Remove(L);
    Remove(X);
    Restore(Lol);
  end Generic_Unassigned_Subsets_Traversal;

  ----------------------------------
  -- Unassigned_Subsets_Traversal --
  ----------------------------------

  procedure Unassigned_Subsets_Traversal(Lol: in List_Of_Lists; Max_Size: in Natural; Handler: in List_Handler) is
    procedure Traversal is new Generic_Unassigned_Subsets_Traversal(Handler.all);
  begin
    Traversal(Lol, Max_Size);
  end Unassigned_Subsets_Traversal;

end Finite_Disjoint_Lists.Algorithms;
