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


-- @filename Disjoint_Sets.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/11/2014
-- @revision 16/11/2014
-- @brief Disjoint Sets based on Forests for fast Join operations

with Ada.Unchecked_Deallocation;
with Ada.Containers.Ordered_Maps;

package body Disjoint_Sets is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Disjoint_Set_Recs, Disjoint_Set);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Ds: out Disjoint_Set; Size: in Positive) is
  begin
    Ds := new Disjoint_Set_Recs(1..Size);
    for I in Ds'Range loop
      Ds(I) := (Parent => I, Rank => 0, Size => 1);
    end loop;
  end Initialize;

  ------------------
  -- Reinitialize --
  ------------------

  procedure Reinitialize(Ds: in Disjoint_Set) is
  begin
    if Ds = null then
      raise Uninitialized_Disjoint_Set_Error;
    end if;

    for I in Ds'Range loop
      Ds(I) := (Parent => I, Rank => 0, Size => 1);
    end loop;
  end Reinitialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Ds: in Disjoint_Set) return Boolean is
  begin
    return Ds /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(Ds: in out Disjoint_Set) is
  begin
    if Ds /= null then
      Dispose(Ds);
      Ds := null;
    end if;
  end Free;

  -----------
  -- Clone --
  -----------

  function Clone(Ds: in Disjoint_Set) return Disjoint_Set is
    Ds_Clone: Disjoint_Set;
  begin
    if Ds = null then
      raise Uninitialized_Disjoint_Set_Error;
    end if;

    Ds_Clone := new Disjoint_Set_Recs(Ds'Range);
    Ds_Clone.all := Ds.all;
    return Ds_Clone;
  end Clone;

  ----------
  -- Size --
  ----------

  function Size(Ds: in Disjoint_Set) return Natural is
  begin
    if Ds = null then
      raise Uninitialized_Disjoint_Set_Error;
    end if;

    return Ds'Length;
  end Size;

  -----------------
  -- Get_Element --
  -----------------

  function Get_Element(Ds: in Disjoint_Set; P: in Positive) return Element is
  begin
    if Ds = null then
      raise Uninitialized_Disjoint_Set_Error;
    end if;

    if P not in Ds'Range then
      raise Index_Error;
    end if;
    return Element'(Ds => Ds, Index => P);
  end Get_Element;

  --------------
  -- Index_Of --
  --------------

  function Index_Of(E: in Element) return Positive is
  begin
    return E.Index;
  end Index_Of;

  ---------------------
  -- Disjoint_Set_Of --
  ---------------------

  function Disjoint_Set_Of(E: in Element) return Disjoint_Set is
  begin
    return E.Ds;
  end Disjoint_Set_Of;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(E: in Element; Ds: in Disjoint_Set) return Boolean is
  begin
    return E.Ds = Ds;
  end Belongs_To;

  ------------------
  -- Component_Of --
  ------------------

  function Component_Of(E: in Element) return Component is
    C: Component;
    Parent_Index: Positive;
  begin
    Parent_Index := E.Ds(E.Index).Parent;
    if Parent_Index = E.Index then
      return E;
    else
      C := Component_Of(Component'(E.Ds, Parent_Index));
      if Parent_Index /= C.Index then
        E.Ds(E.Index).Parent := C.Index;
        E.Ds(Parent_Index).Size := E.Ds(Parent_Index).Size - E.Ds(E.Index).Size;
      end if;
      return C;
    end if;
  end Component_Of;

  --------------------
  -- Component_Size --
  --------------------

  function Component_Size(E: in Element) return Natural is
    C: Component;
  begin
    C := Component_Of(E);
    return E.Ds(C.Index).Size;
  end Component_Size;

  ------------------
  -- Subtree_Size --
  ------------------

  function Subtree_Size(E: in Element) return Natural is
  begin
    return E.Ds(E.Index).Size;
  end Subtree_Size;

  ---------------------
  -- Share_Component --
  ---------------------

  function Share_Component(E1, E2: in Element) return Boolean is
  begin
    if E1.Ds /= E2.Ds then
      raise Incompatible_Elements_Error;
    end if;

    return Component_Of(E1) = Component_Of(E2);
  end Share_Component;

  ----------
  -- Join --
  ----------

  procedure Join(E1, E2: in Element) is
    C1, C2: Component;
    Rank1, Rank2: Natural;
  begin
    if E1.Ds /= E2.Ds then
      raise Incompatible_Elements_Error;
    end if;

    if E1 /= E2 then
      C1 := Component_Of(E1);
      C2 := Component_Of(E2);
      if C1 /= C2 then
        Rank1 := E1.Ds(C1.Index).Rank;
        Rank2 := E2.Ds(C2.Index).Rank;
        if Rank1 < Rank2 then
          E1.Ds(C1.Index).Parent := C2.Index;
          E2.Ds(C2.Index).Size := E2.Ds(C2.Index).Size + E1.Ds(C1.Index).Size;
        elsif Rank1 > Rank2 then
          E2.Ds(C2.Index).Parent := C1.Index;
          E1.Ds(C1.Index).Size := E1.Ds(C1.Index).Size + E2.Ds(C2.Index).Size;
        else
          E2.Ds(C2.Index).Parent := C1.Index;
          E1.Ds(C1.Index).Size := E1.Ds(C1.Index).Size + E2.Ds(C2.Index).Size;
          E1.Ds(C1.Index).Rank := E1.Ds(C1.Index).Rank + 1;
        end if;
      end if;
    end if;
  end Join;

  ---------------
  -- To_Forest --
  ---------------

  function To_Forest(Ds: in Disjoint_Set) return Forest is
    use Nodes_Lists;
    type Nodes is array(Positive range <>) of Node;
    type PNodes is access Nodes;

    procedure Dispose is new Ada.Unchecked_Deallocation(Nodes, PNodes);

    Nd: PNodes;
    Fr: Forest;
  begin
    if Ds = null then
      raise Uninitialized_Disjoint_Set_Error;
    end if;

    Nd := new Nodes(Ds'Range);
    for I in Nd'Range loop
      Nd(I) := New_Node(I);
    end loop;
    Initialize(Fr);

    for I in Ds'Range loop
      if Ds(I).Parent = I then
        Add_Last(Nd(I), Fr);
      else
        Add_Child(Nd(Ds(I).Parent), Nd(I));
      end if;
    end loop;

    Dispose(Nd);
    return Fr;
  end To_Forest;

  ---------------
  -- To_Forest --
  ---------------

  function To_List_Of_Lists(Ds: in Disjoint_Set) return List_Of_Lists is
    package List_Maps is new Ada.Containers.Ordered_Maps(Integer, List);
    use List_Maps;

    Lol: List_Of_Lists;
    M: Map;
    N: Natural;
    C: Component;
  begin
    if Ds = null then
      raise Uninitialized_Disjoint_Set_Error;
    end if;

    N := Ds'Length;
    Initialize(Lol, N, Unassigned_Initialization);

    for I in Ds'Range loop
      if Ds(I).Parent = I then
        Insert(M, I, New_List(Lol));
      end if;
    end loop;
    for I in Ds'Range loop
      C := Component_Of(Element'(Ds => Ds, Index => I));
      Move(Get_Element(Lol, I), List_Maps.Element(M, C.Index));
    end loop;

    return Lol;
  end To_List_Of_Lists;

end Disjoint_Sets;
