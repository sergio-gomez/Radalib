-- Radalib, Copyright (c) 2021 by
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


-- @filename Arrays_Utils.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/07/2009
-- @revision 18/12/2014
-- @brief Arrays utils

package body Arrays_Utils is

  -----------
  -- Alloc --
  -----------

  function Alloc(First, Last: in Integer; Ini_Val: in Item) return PItems is
    Res: PItems;
  begin
    Res := Alloc(First, Last);
    Res.all := (others => Ini_Val);
    return Res;
  end Alloc;

  -----------
  -- Alloc --
  -----------

  function Alloc(First1, Last1, First2, Last2: in Integer; Ini_Val: in Item) return PItemss is
    Res: PItemss;
  begin
    Res := Alloc(First1, Last1, First2, Last2);
    Res.all := (others => (others => Ini_Val));
    return Res;
  end Alloc;

  -----------
  -- Alloc --
  -----------

  function Alloc(First, Last: in Integer; Ini_Val: in Item) return PItemss is
    Res: PItemss;
  begin
    Res := Alloc(First, Last);
    Res.all := (others => (others => Ini_Val));
    return Res;
  end Alloc;

  ------------
  -- Column --
  ------------

  function Column(A: in Itemss; Index: in Integer) return Items is
    Res: Items(A'Range(1));
  begin
    if Index not in A'Range(2) then
      raise Array_Index_Error;
    end if;
    for I in A'Range(1) loop
      Res(I) := A(I, Index);
    end loop;
    return Res;
  end Column;

  ------------
  -- Column --
  ------------

  function Column(A: in PItemss; Index: in Integer) return PItems is
    Res: PItems;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if Index not in A'Range(2) then
      raise Array_Index_Error;
    end if;
    Res := Alloc(A'First(1), A'Last(1));
    for I in A'Range(1) loop
      Res(I) := A(I, Index);
    end loop;
    return Res;
  end Column;

  ---------
  -- Row --
  ---------

  function Row(A: in Itemss; Index: in Integer) return Items is
    Res: Items(A'Range(2));
  begin
    if Index not in A'Range(1) then
      raise Array_Index_Error;
    end if;
    for I in A'Range(2) loop
      Res(I) := A(Index, I);
    end loop;
    return Res;
  end Row;

  ---------
  -- Row --
  ---------

  function Row(A: in PItemss; Index: in Integer) return PItems is
    Res: PItems;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if Index not in A'Range(1) then
      raise Array_Index_Error;
    end if;
    Res := Alloc(A'First(2), A'Last(2));
    for I in A'Range(2) loop
      Res(I) := A(Index, I);
    end loop;
    return Res;
  end Row;

  --------------
  -- Diagonal --
  --------------

  function Diagonal(A: in Itemss) return Items is
  begin
    if A'Length(1) <= A'Length(2) then
      declare
        Res: Items(A'Range(1));
        J: Integer;
      begin
        for I in A'Range(1) loop
          J := A'First(2) + (I - A'First(1));
          Res(I) := A(I, J);
        end loop;
        return Res;
      end;
    else
      declare
        Res: Items(A'Range(2));
        I: Integer;
      begin
        for J in A'Range(2) loop
          I := A'First(1) + (J - A'First(2));
          Res(J) := A(I, J);
        end loop;
        return Res;
      end;
    end if;
  end Diagonal;

  --------------
  -- Diagonal --
  --------------

  function Diagonal(A: in PItemss) return PItems is
    Res: PItems;
    I, J: Integer;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if A'Length(1) <= A'Length(2) then
      Res := Alloc(A'First(1), A'Last(1));
      for I in A'Range(1) loop
        J := A'First(2) + (I - A'First(1));
        Res(I) := A(I, J);
      end loop;
    else
      Res := Alloc(A'First(2), A'Last(2));
      for J in A'Range(2) loop
        I := A'First(1) + (J - A'First(2));
        Res(J) := A(I, J);
      end loop;
    end if;
    return Res;
  end Diagonal;

  ------------------
  -- Set_Diagonal --
  ------------------

  procedure Set_Diagonal(A: in out Itemss; Diag: in Item) is
    I, J: Integer;
  begin
    if A'Length(1) <= A'Length(2) then
      for I in A'Range(1) loop
        J := A'First(2) + (I - A'First(1));
        A(I, J) := Diag;
      end loop;
    else
      for J in A'Range(2) loop
        I := A'First(1) + (J - A'First(2));
        A(I, J) := Diag;
      end loop;
    end if;
  end Set_Diagonal;

  ------------------
  -- Set_Diagonal --
  ------------------

  procedure Set_Diagonal(A: in PItemss; Diag: in Item) is
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    Set_Diagonal(A.all, Diag);
  end Set_Diagonal;

  ------------------
  -- Set_Diagonal --
  ------------------

  procedure Set_Diagonal(A: in out Itemss; Diag: in Items) is
    I, J, K: Integer;
  begin
    if A'Length(1) <= A'Length(2) then
      if A'Length(1) /= Diag'Length then
        raise Incompatible_Arrays_Error;
      end if;
      for I in A'Range(1) loop
        J := A'First(2) + (I - A'First(1));
        K := Diag'First + (I - A'First(1));
        A(I, J) := Diag(K);
      end loop;
    else
      if A'Length(2) /= Diag'Length then
        raise Incompatible_Arrays_Error;
      end if;
      for J in A'Range(2) loop
        I := A'First(1) + (J - A'First(2));
        K := Diag'First + (J - A'First(2));
        A(I, J) := Diag(K);
      end loop;
    end if;
  end Set_Diagonal;

  ------------------
  -- Set_Diagonal --
  ------------------

  procedure Set_Diagonal(A: in PItemss; Diag: in PItems) is
  begin
    if A = null or Diag = null then
      raise Uninitialized_Array_Error;
    end if;
    Set_Diagonal(A.all, Diag.all);
  end Set_Diagonal;

  -----------
  -- Slice --
  -----------

  function Slice(A: in Items; First, Last: in Integer) return Items is
  begin
    if (First not in A'Range) or (Last not in A'Range) then
      raise Array_Index_Error;
    end if;
    return A(First..Last);
  end Slice;

  -----------
  -- Slice --
  -----------

  function Slice(A: in PItems; First, Last: in Integer) return PItems is
    Res: PItems;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if (First not in A'Range) or (Last not in A'Range) then
      raise Array_Index_Error;
    end if;
    Res := Alloc(First, Last);
    Res.all := A(First..Last);
    return Res;
  end Slice;

  -----------
  -- Slice --
  -----------

  function Slice(A: in Itemss; First1, Last1, First2, Last2: in Integer) return Itemss is
    Res: Itemss(First1..Last1, First2..Last2);
  begin
    if (First1 not in A'Range(1)) or (Last1 not in A'Range(1)) or
       (First2 not in A'Range(2)) or (Last2 not in A'Range(2)) then
      raise Array_Index_Error;
    end if;
    for I in Res'Range(1) loop
      for J in Res'Range(2) loop
        Res(I, J) := A(I, J);
      end loop;
    end loop;
    return Res;
  end Slice;

  -----------
  -- Slice --
  -----------

  function Slice(A: in PItemss; First1, Last1, First2, Last2: in Integer) return PItemss is
    Res: PItemss;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if (First1 not in A'Range(1)) or (Last1 not in A'Range(1)) or
       (First2 not in A'Range(2)) or (Last2 not in A'Range(2)) then
      raise Array_Index_Error;
    end if;
    Res := Alloc(First1, Last1, First2, Last2);
    for I in Res'Range(1) loop
      for J in Res'Range(2) loop
        Res(I, J) := A(I, J);
      end loop;
    end loop;
    return Res;
  end Slice;

  -----------
  -- Slice --
  -----------

  function Slice(A: in Itemss; First, Last: in Integer) return Itemss is
    Res: Itemss(First..Last, First..Last);
  begin
    if (First not in A'Range(1)) or (Last not in A'Range(1)) or
       (First not in A'Range(2)) or (Last not in A'Range(2)) then
      raise Array_Index_Error;
    end if;
    for I in Res'Range(1) loop
      for J in Res'Range(2) loop
        Res(I, J) := A(I, J);
      end loop;
    end loop;
    return Res;
  end Slice;

  -----------
  -- Slice --
  -----------

  function Slice(A: in PItemss; First, Last: in Integer) return PItemss is
    Res: PItemss;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if (First not in A'Range(1)) or (Last not in A'Range(1)) or
       (First not in A'Range(2)) or (Last not in A'Range(2)) then
      raise Array_Index_Error;
    end if;
    Res := Alloc(First, Last);
    for I in Res'Range(1) loop
      for J in Res'Range(2) loop
        Res(I, J) := A(I, J);
      end loop;
    end loop;
    return Res;
  end Slice;

  ----------
  -- Swap --
  ----------

  procedure Swap(A: in out Items; P1, P2: in Integer) is
    Tmp: Item;
  begin
    if P1 not in A'Range or P2 not in A'Range then
      raise Array_Index_Error;
    end if;
    Tmp := A(P1);
    A(P1) := A(P2);
    A(P2) := Tmp;
  end Swap;

  ----------
  -- Swap --
  ----------

  procedure Swap(A: in PItems; P1, P2: in Integer) is
    Tmp: Item;
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    if P1 not in A'Range or P2 not in A'Range then
      raise Array_Index_Error;
    end if;
    Tmp := A(P1);
    A(P1) := A(P2);
    A(P2) := Tmp;
  end Swap;

  ----------
  -- Swap --
  ----------

  procedure Swap(A: in out Items) is
    P1, P2: Integer;
  begin
    P1 := A'First;
    P2 := A'Last;
    while P1 < P2 loop
      Unchecked_Swap(A, P1, P2);
      P1 := P1 + 1;
      P2 := P2 - 1;
    end loop;
  end Swap;

  ----------
  -- Swap --
  ----------

  procedure Swap(A: in PItems) is
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    Swap(A.all);
  end Swap;

  --------------------
  -- Unchecked_Swap --
  --------------------

  procedure Unchecked_Swap(A: in out Items; P1, P2: in Integer) is
    Tmp: constant Item := A(P1);
  begin
    A(P1) := A(P2);
    A(P2) := Tmp;
  end Unchecked_Swap;

  --------------------
  -- Unchecked_Swap --
  --------------------

  procedure Unchecked_Swap(A: in out PsItems; P1, P2: in Integer) is
    Tmp: constant PItems := A(P1);
  begin
    A(P1) := A(P2);
    A(P2) := Tmp;
  end Unchecked_Swap;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in out Items) is
  begin
    if A'Length >= 2 then
      Quick_Sort(A, A'First, A'Last);
      Insertion_Sort(A, A'First, A'Last);
    end if;
  end Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in out Items; Index: out Integers) is
    Offset: constant Integer := Index'First - A'First;
  begin
    if A'Length /= Index'Length then
      raise Incompatible_Arrays_Error;
    end if;
    for I in A'Range loop
      Index(I + Offset) := I;
    end loop;
    if A'Length >= 2 then
      Quick_Sort(A, Index, A'First, A'Last);
      Insertion_Sort(A, Index, A'First, A'Last);
    end if;
  end Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in PItems) is
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    Sort(A.all);
  end Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in PItems; Index: out PIntegers) is
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    Index := Alloc(A'First, A'Last);
    Sort(A.all, Index.all);
  end Sort;

  ---------
  -- "<" --
  ---------

  function "<"(Left, Right: in PItems) return Boolean is
    J: Integer;
  begin
    if Left = null or Right = null then
      raise Uninitialized_Array_Error;
    end if;

    for I in Left'Range loop
      J := Right'First + (I - Left'First);
      if J in Right'Range then
        if Left(I) < Right(J) then
          return True;
        elsif Right(J) < Left(I) then
          return False;
        end if;
      else
        return False;
      end if;
    end loop;
    if Left'Length < Right'Length then
      return True;
    end if;
    return False;
  end "<";

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in out PSItems) is
  begin
    if A'Length >= 2 then
      Quick_Sort(A, A'First, A'Last);
      Insertion_Sort(A, A'First, A'Last);
    end if;
  end Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in out PsItems; Index: out Integers) is
    Offset: constant Integer := Index'First - A'First;
  begin
    if A'Length /= Index'Length then
      raise Incompatible_Arrays_Error;
    end if;
    for I in A'Range loop
      Index(I + Offset) := I;
    end loop;
    if A'Length >= 2 then
      Quick_Sort(A, Index, A'First, A'Last);
      Insertion_Sort(A, Index, A'First, A'Last);
    end if;
  end Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in PPsItems) is
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    Sort(A.all);
  end Sort;

  ----------
  -- Sort --
  ----------

  procedure Sort(A: in PPsItems; Index: out PIntegers) is
  begin
    if A = null then
      raise Uninitialized_Array_Error;
    end if;
    Index := Alloc(A'First, A'Last);
    Sort(A.all, Index.all);
  end Sort;

  --------------------
  -- Insertion_Sort --
  --------------------

  procedure Insertion_Sort(A: in out Items; Left, Right: in Integer) is
    J: Integer;
    Tmp: Item;
  begin
    for I in (Left + 1)..Right loop
      Tmp := A(I);
      J := I - 1;
      while J >= Left and then Tmp < A(J) loop
        A(J + 1) := A(J);
        J := J - 1;
      end loop;
      A(J + 1) := Tmp;
    end loop;
  end Insertion_Sort;

  --------------------
  -- Insertion_Sort --
  --------------------

  procedure Insertion_Sort(A: in out Items; Index: in out Integers; Left, Right: in Integer) is
    Offset: constant Integer := Index'First - A'First;
    J, Tmi: Integer;
    Tmp: Item;
  begin
    for I in (Left + 1)..Right loop
      Tmp := A(I);
      Tmi := Index(I + Offset);
      J := I - 1;
      while J >= Left and then Tmp < A(J) loop
        A(J + 1) := A(J);
        Index(J + 1 + Offset) := Index(J + Offset);
        J := J - 1;
      end loop;
      A(J + 1) := Tmp;
      Index(J + 1 + Offset) := Tmi;
    end loop;
  end Insertion_Sort;

  --------------------
  -- Insertion_Sort --
  --------------------

  procedure Insertion_Sort(A: in out PsItems; Left, Right: in Integer) is
    J: Integer;
    Tmp: PItems;
  begin
    for I in (Left + 1)..Right loop
      Tmp := A(I);
      J := I - 1;
      while J >= Left and then Tmp < A(J) loop
        A(J + 1) := A(J);
        J := J - 1;
      end loop;
      A(J + 1) := Tmp;
    end loop;
  end Insertion_Sort;

  --------------------
  -- Insertion_Sort --
  --------------------

  procedure Insertion_Sort(A: in out PsItems; Index: in out Integers; Left, Right: in Integer) is
    Offset: constant Integer := Index'First - A'First;
    J, Tmi: Integer;
    Tmp: PItems;
  begin
    for I in (Left + 1)..Right loop
      Tmp := A(I);
      Tmi := Index(I + Offset);
      J := I - 1;
      while J >= Left and then Tmp < A(J) loop
        A(J + 1) := A(J);
        Index(J + 1 + Offset) := Index(J + Offset);
        J := J - 1;
      end loop;
      A(J + 1) := Tmp;
      Index(J + 1 + Offset) := Tmi;
    end loop;
  end Insertion_Sort;

  ----------------
  -- Quick_Sort --
  ----------------

  procedure Quick_Sort(A: in out Items; Left, Right: in Integer) is
    Cutoff: constant := 10;
    Num: constant Integer := Right - Left + 1;
    I, J: Integer;
    Pivot: Item;

    procedure Median3;
    pragma Inline(Median3);

    procedure Median3 is
      Center: constant Integer := (Left + Right) / 2;
    begin
      if A(Center) < A(Left) then
        Unchecked_Swap(A, Center, Left);
      end if;
      if A(Right) < A(Left) then
        Unchecked_Swap(A, Right, Left);
      end if;
      if A(Right) < A(Center) then
        Unchecked_Swap(A, Right, Center);
      end if;
      Pivot := A(Center);
      Unchecked_Swap(A, Center, Right);
    end Median3;
  begin
    if Num > Cutoff then
      Median3;
      I := Left;
      J := Right;
      loop
        loop
          I := I + 1;
          exit when not (A(I) < Pivot);
        end loop;
        loop
          J := J - 1;
          exit when not (Pivot < A(J));
        end loop;
        exit when J <= I;
        Unchecked_Swap(A, I, J);
      end loop;
      Unchecked_Swap(A, I, Right);
      Quick_Sort(A, Left, I - 1);
      Quick_Sort(A, I + 1, Right);
    end if;
  end Quick_Sort;

  ----------------
  -- Quick_Sort --
  ----------------

  procedure Quick_Sort(A: in out Items; Index: in out Integers; Left, Right: in Integer) is
    Cutoff: constant := 10;
    Offset: constant Integer := Index'First - A'First;
    Num: constant Integer := Right - Left + 1;
    I, J: Integer;
    Pivot: Item;

    procedure Index_Swap(B: in out Integers; P1, P2: in Integer);
    pragma Inline(Index_Swap);

    procedure Index_Swap(B: in out Integers; P1, P2: in Integer) is
      Tmp: constant Integer := B(P1 + Offset);
    begin
      B(P1 + Offset) := B(P2 + Offset);
      B(P2 + Offset) := Tmp;
    end Index_Swap;

    procedure Median3;
    pragma Inline(Median3);

    procedure Median3 is
      Center: constant Integer := (Left + Right) / 2;
    begin
      if A(Center) < A(Left) then
        Unchecked_Swap(A, Center, Left);
        Index_Swap(Index, Center, Left);
      end if;
      if A(Right) < A(Left) then
        Unchecked_Swap(A, Right, Left);
        Index_Swap(Index, Right, Left);
      end if;
      if A(Right) < A(Center) then
        Unchecked_Swap(A, Right, Center);
        Index_Swap(Index, Right, Center);
      end if;
      Pivot := A(Center);
      Unchecked_Swap(A, Center, Right);
      Index_Swap(Index, Center, Right);
    end Median3;
  begin
    if Num > Cutoff then
      Median3;
      I := Left;
      J := Right;
      loop
        loop
          I := I + 1;
          exit when not (A(I) < Pivot);
        end loop;
        loop
          J := J - 1;
          exit when not (Pivot < A(J));
        end loop;
        exit when J <= I;
        Unchecked_Swap(A, I, J);
        Index_Swap(Index, I, J);
      end loop;
      Unchecked_Swap(A, I, Right);
      Index_Swap(Index, I, Right);
      Quick_Sort(A, Index, Left, I - 1);
      Quick_Sort(A, Index, I + 1, Right);
    end if;
  end Quick_Sort;

  ----------------
  -- Quick_Sort --
  ----------------

  procedure Quick_Sort(A: in out PsItems; Left, Right: in Integer) is
    Cutoff: constant := 10;
    Num: constant Integer := Right - Left + 1;
    I, J: Integer;
    Pivot: PItems;

    procedure Median3;
    pragma Inline(Median3);

    procedure Median3 is
      Center: constant Integer := (Left + Right) / 2;
    begin
      if A(Center) < A(Left) then
        Unchecked_Swap(A, Center, Left);
      end if;
      if A(Right) < A(Left) then
        Unchecked_Swap(A, Right, Left);
      end if;
      if A(Right) < A(Center) then
        Unchecked_Swap(A, Right, Center);
      end if;
      Pivot := A(Center);
      Unchecked_Swap(A, Center, Right);
    end Median3;
  begin
    if Num > Cutoff then
      Median3;
      I := Left;
      J := Right;
      loop
        loop
          I := I + 1;
          exit when not (A(I) < Pivot);
        end loop;
        loop
          J := J - 1;
          exit when not (Pivot < A(J));
        end loop;
        exit when J <= I;
        Unchecked_Swap(A, I, J);
      end loop;
      Unchecked_Swap(A, I, Right);
      Quick_Sort(A, Left, I - 1);
      Quick_Sort(A, I + 1, Right);
    end if;
  end Quick_Sort;

  ----------------
  -- Quick_Sort --
  ----------------

  procedure Quick_Sort(A: in out PsItems; Index: in out Integers; Left, Right: in Integer) is
    Cutoff: constant := 10;
    Offset: constant Integer := Index'First - A'First;
    Num: constant Integer := Right - Left + 1;
    I, J: Integer;
    Pivot: PItems;

    procedure Index_Swap(B: in out Integers; P1, P2: in Integer);
    pragma Inline(Index_Swap);

    procedure Index_Swap(B: in out Integers; P1, P2: in Integer) is
      Tmp: constant Integer := B(P1 + Offset);
    begin
      B(P1 + Offset) := B(P2 + Offset);
      B(P2 + Offset) := Tmp;
    end Index_Swap;

    procedure Median3;
    pragma Inline(Median3);

    procedure Median3 is
      Center: constant Integer := (Left + Right) / 2;
    begin
      if A(Center) < A(Left) then
        Unchecked_Swap(A, Center, Left);
        Index_Swap(Index, Center, Left);
      end if;
      if A(Right) < A(Left) then
        Unchecked_Swap(A, Right, Left);
        Index_Swap(Index, Right, Left);
      end if;
      if A(Right) < A(Center) then
        Unchecked_Swap(A, Right, Center);
        Index_Swap(Index, Right, Center);
      end if;
      Pivot := A(Center);
      Unchecked_Swap(A, Center, Right);
      Index_Swap(Index, Center, Right);
    end Median3;
  begin
    if Num > Cutoff then
      Median3;
      I := Left;
      J := Right;
      loop
        loop
          I := I + 1;
          exit when not (A(I) < Pivot);
        end loop;
        loop
          J := J - 1;
          exit when not (Pivot < A(J));
        end loop;
        exit when J <= I;
        Unchecked_Swap(A, I, J);
        Index_Swap(Index, I, J);
      end loop;
      Unchecked_Swap(A, I, Right);
      Index_Swap(Index, I, Right);
      Quick_Sort(A, Index, Left, I - 1);
      Quick_Sort(A, Index, I + 1, Right);
    end if;
  end Quick_Sort;

end Arrays_Utils;
