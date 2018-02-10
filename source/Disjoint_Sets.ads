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


-- @filename Disjoint_Sets.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/11/2014
-- @revision 16/11/2014
-- @brief Disjoint Sets based on Forests for fast Join operations

with Trees_Integer; use Trees_Integer;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;

package Disjoint_Sets is

  -- Represents an Element
  type Element is private;

  -- Represents a Disjoint Component
  subtype Component is Element;

  -- Represents a Disjoint Set
  type Disjoint_Set is private;

  Index_Error: exception;
  Uninitialized_Disjoint_Set_Error: exception;
  Incompatible_Elements_Error: exception;


  -- Purpose : Initialize a Disjoint Set to a given number of Elements
  -- Note    : All Elements are initially isolated
  --
  -- Ds      : The Disjoint Set
  -- Size    : The Number of Elements
  procedure Initialize(Ds: out Disjoint_Set; Size: in Positive);

  -- Purpose : Reinitialize a Disjoint Set
  -- Note    : All Elements become isolated
  --
  -- Ds      : The Disjoint Set
  -- raises  : Uninitialized_Disjoint_Set_Error
  procedure Reinitialize(Ds: in Disjoint_Set);

  -- Purpose : To know whether a Disjoint Set is Initialized
  --
  -- Ds      : The Disjoint Set
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Ds: in Disjoint_Set) return Boolean;

  -- Purpose : Deallocate all the space used by a Disjoint Set
  --
  -- Ds      : The Disjoint Set
  procedure Free(Ds: in out Disjoint_Set);

  -- Purpose : Create a copy of a Disjoint Set
  --
  -- Ds      : The Disjoint Set
  -- return  : The Clone
  -- raises  : Uninitialized_Disjoint_Set_Error
  function Clone(Ds: in Disjoint_Set) return Disjoint_Set;

  -- Purpose : Obtain the total number of Elements in the Disjoint Set
  --
  -- Ds      : The Disjoint Set
  -- return  : The number of Elements
  -- raises  : Uninitialized_Disjoint_Set_Error
  function Size(Ds: in Disjoint_Set) return Natural;

  -- Purpose : Obtain an Element from an Index
  --
  -- Ds      : The Disjoint Set
  -- P       : The Index
  -- return  : The Element
  -- raises  : Uninitialized_Disjoint_Set_Error
  -- raises  : Index_Error
  function Get_Element(Ds: in Disjoint_Set; P: in Positive) return Element;

  -- Purpose : Obtain the Index of an Element
  --
  -- E       : The Element
  -- return  : The Index
  function Index_Of(E: in Element) return Positive;

  -- Purpose : Obtain the Disjoint Set associated to an Element
  --
  -- E       : The Element
  -- return  : The Disjoint Set
  function Disjoint_Set_Of(E: in Element) return Disjoint_Set;

  -- Purpose : To know whether an Element belongs to a Disjoint Set
  --
  -- E       : The Element
  -- Ds      : The Disjoint Set
  -- return  : True if Element belongs to the Disjoint Set
  function Belongs_To(E: in Element; Ds: in Disjoint_Set) return Boolean;

  -- Purpose : Obtain the Component to which an Element belongs to
  -- Note    : Path compression applied
  --
  -- E       : The Element
  -- return  : The Component
  function Component_Of(E: in Element) return Component;

  -- Purpose : Obtain the number of Elements in the Component holding a given Element
  -- Note    : It is possible to apply this function to a Component instead of an Element
  -- Note    : Path compression applied
  --
  -- E       : The Element
  -- return  : The number of Elements of the Component
  function Component_Size(E: in Element) return Natural;

  -- Purpose : Obtain the number of Elements in the Subtree of given Element
  -- Note    : It is possible to apply this function to a Component instead of an Element
  --
  -- E       : The Element
  -- return  : The number of Elements of the Subtree
  function Subtree_Size(E: in Element) return Natural;

  -- Purpose : To know whether two Elements belong to the same Component
  -- Note    : Path compression applied to both Elements
  --
  -- E1      : First Element
  -- E2      : Second Element
  -- return  : True if Elements belongs to the same Component
  -- raises  : Incompatible_Elements_Error
  function Share_Component(E1, E2: in Element) return Boolean;

  -- Purpose : Join the Sets of two Elements
  --
  -- E1      : First Element
  -- E2      : Second Element
  -- raises  : Incompatible_Elements_Error
  procedure Join(E1, E2: in Element);

  -- Purpose : Create a Forest corresponding to a Disjoint Set
  -- Note    : Each Tree of the Forest corresponds to a Set of the Disjoint Set
  -- Note    : Changes to the Forest and/or Disjoint Set are not synchronized
  -- Note    : Each Node of the Forest only holds the Index of an Element
  --
  -- Ds      : The Disjoint Set
  -- return  : The Forest
  -- raises  : Uninitialized_Disjoint_Set_Error
  function To_Forest(Ds: in Disjoint_Set) return Forest;

  -- Purpose : Create a List of Lists corresponding to a Disjoint Set
  -- Note    : Each List of the List of Lists corresponds to a Set of the Disjoint Set
  -- Note    : Changes to the List of Lists and/or Disjoint Set are not synchronized
  -- Note    : There is a one-to-one correspondence between Elements of the Disjoint Set and the List of Lists
  --
  -- Ds      : The Disjoint Set
  -- return  : The List of Lists
  -- raises  : Uninitialized_Disjoint_Set_Error
  function To_List_Of_Lists(Ds: in Disjoint_Set) return List_Of_Lists;


private

  --------------
  -- Elements --
  --------------

  type Element is record
    Ds: Disjoint_Set;
    Index: Positive;
  end record;

  -------------------
  -- Disjoint Sets --
  -------------------

  type Disjoint_Set_Rec is record
    Parent: Positive;
    Rank: Natural;
    Size: Positive;
  end record;

  type Disjoint_Set_Recs is array(Positive range <>) of Disjoint_Set_Rec;
  type Disjoint_Set is access Disjoint_Set_Recs;

end Disjoint_Sets;
