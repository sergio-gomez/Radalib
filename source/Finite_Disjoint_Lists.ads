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


-- @filename Finite_Disjoint_Lists.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 08/04/2012
-- @brief Treatment of Lists of Lists with Finite Disjoint Elements

with Stacks;

package Finite_Disjoint_Lists is

  -- Represents an Element
  type Element is private;

  -- Represents a List of a finite number of Elements
  type List is private;

  -- Represents a List of Lists
  -- All Lists are pairwise disjoint
  -- Elements which do not belong to any List are members of the Unassigned List
  type List_Of_Lists is private;

  -- Initialization possibilities for Lists of Lists
  type List_Of_Lists_Initialization is (Unassigned_Initialization, Isolated_Initialization, Together_Initialization);

  Index_Error: exception;
  Uninitialized_List_Of_Lists_Error: exception;
  Not_A_List_Error: exception;
  No_More_Elements_Error: exception;
  No_More_Lists_Error: exception;
  Incompatible_Lists_Of_Lists_Error: exception;
  Unremovable_Unassigned_List_Error: exception;
  No_Saved_Current_Position_Error: exception;


  -- Purpose : Initialize a List of Lists to a given number of Elements
  --
  -- Lol          : The List of Lists
  -- Num_Elements : The number of Elements
  -- Lol_Ini_Type : The type of Initialization
  procedure Initialize(Lol: out List_Of_Lists; Num_Elements: in Positive; Lol_Ini_Type: in List_Of_Lists_Initialization := Unassigned_Initialization);

  -- Purpose : Reinitialize a List of Lists
  --
  -- Lol          : The List of Lists
  -- Lol_Ini_Type : The type of Initialization
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Reinitialize(Lol: in out List_Of_Lists; Lol_Ini_Type: in List_Of_Lists_Initialization := Unassigned_Initialization);

  -- Purpose : To know whether a List of Lists is Initialized
  --
  -- Lol     : The List of Lists
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Lol: in List_Of_Lists) return Boolean;

  -- Purpose : Deallocate all the space used by a List of Lists
  --
  -- Lol     : The List of Lists
  procedure Free(Lol: in out List_Of_Lists);

  -- Purpose : Create a copy of a List of Lists
  -- Note    : Current positions and Saved positions are also cloned
  --
  -- Lol     : The List of Lists
  -- return  : The Clone
  -- raises  : Uninitialized_List_Of_Lists_Error
  function Clone(Lol: in List_Of_Lists) return List_Of_Lists;

  -- Purpose : Obtain the total number of Elements in the List of Lists
  --
  -- Lol     : The List of Lists
  -- return  : The number of Elements
  -- raises  : Uninitialized_List_Of_Lists_Error
  function Number_Of_Elements(Lol: in List_Of_Lists) return Natural;

  -- Purpose : Obtain the number of Lists in the List of Lists
  -- Note    : The Unassigned List of Elements is not counted
  --
  -- Lol     : The List of Lists
  -- return  : The number of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  function Number_Of_Lists(Lol: in List_Of_Lists) return Natural;

  -- Purpose : To know if a List of Lists has Unassigned Elements
  --
  -- Lol     : The List of Lists
  -- return  : True if has Unassigned Elements
  -- raises  : Uninitialized_List_Of_Lists_Error
  function Has_Unassigned(Lol: in List_Of_Lists) return Boolean;

  -- Purpose : Obtain the Unassigned List of Elements
  --
  -- Lol     : The List of Lists
  -- return  : The Unassigned List
  -- raises  : Uninitialized_List_Of_Lists_Error
  function Unassigned_List(Lol: in List_Of_Lists) return List;

  -- Purpose : Create a new void List
  --
  -- Lol     : The List of Lists
  -- return  : The new List
  -- raises  : Uninitialized_List_Of_Lists_Error
  function New_List(Lol: in List_Of_Lists) return List;

  -- Purpose : Obtain the Id of a List
  -- Note    : Each different List in a List of Lists has a different Id
  --
  -- L       : The List
  -- return  : The Id
  -- raises  : Not_A_List_Error
  function Get_Id(L: in List) return Natural;

  -- Purpose : Obtain the List of Lists associated to a List
  --
  -- L       : The List
  -- return  : The List of Lists
  function List_Of_Lists_Of(L: in List) return List_Of_Lists;

  -- Purpose : To know whether a List belongs to a List of Lists
  --
  -- L       : The List
  -- Lol     : The List of Lists
  -- return  : True if List belongs to the List of Lists
  function Belongs_To(L: in List; Lol: in List_Of_Lists) return Boolean;

  -- Purpose : Remove a List from the List of Lists
  -- Note    : The Elements of the List are moved to the Unassigned List
  -- Note    : Void unused Lists should be removed
  --
  -- L       : The List
  -- raises  : Not_A_List_Error
  -- raises  : Unremovable_Unassigned_List_Error
  procedure Remove(L: in List);

  -- Purpose : Remove all Empty Lists
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Remove_Empty(Lol: in List_Of_Lists);

  -- Purpose : Remove all Lists
  -- Note    : The Elements of the Lists are moved to the Unassigned List
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Clear(Lol: in List_Of_Lists);

  -- Purpose : To know if there are more Lists left in a List of Lists
  -- Note    : The Unassigned List is not considered
  --
  -- Lol     : The List of Lists
  -- return  : True if more Lists left
  -- raises  : Uninitialized_List_Of_Lists_Error
  function Has_Next_List(Lol: in List_Of_Lists) return Boolean;

  -- Purpose : Obtain the current List in a List of Lists
  -- Note    : The Unassigned List is not considered
  --
  -- Lol     : The List of Lists
  -- return  : The next List
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : No_More_Lists_Error
  function Get_List(Lol: in List_Of_Lists) return List;

  -- Purpose : Obtain and skip the next List in a List of Lists
  -- Note    : The Unassigned List is not considered
  --
  -- Lol     : The List of Lists
  -- return  : The next List
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : No_More_Lists_Error
  function Next_List(Lol: in List_Of_Lists) return List;

  -- Purpose : Skip the next List in a List of Lists
  -- Note    : The Unassigned List is not considered
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : No_More_Lists_Error
  procedure Next_List(Lol: in List_Of_Lists);

  -- Purpose : Set a List of Lists to the first List
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Reset(Lol: in List_Of_Lists);

  -- Purpose : Save the position to the current List in a List of Lists
  -- Note    : There is a stack of saved positions
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Save(Lol: in List_Of_Lists);

  -- Purpose : Restore the position in a List of Lists to the previously saved List
  -- Note    : Saved position must exist, no check is made
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : No_Saved_Current_Position_Error
  procedure Restore(Lol: in List_Of_Lists);

  -- Purpose : Obtain an Element from an index
  --
  -- Lol     : The List of Lists
  -- P       : The index
  -- return  : The Element
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : Index_Error
  function Get_Element(Lol: in List_Of_Lists; P: in Positive) return Element;

  -- Purpose : Obtain the index of an Element
  --
  -- E       : The Element
  -- return  : The index
  function Index_Of(E: in Element) return Positive;

  -- Purpose : Obtain the number of Elements of a List
  --
  -- L       : The List
  -- return  : The number of elements
  -- raises  : Not_A_List_Error
  function Number_Of_Elements(L: in List) return Natural;

  -- Purpose : To know if there are more Elements left in a List
  --
  -- L       : The List
  -- return  : True if more Elements left
  -- raises  : Not_A_List_Error
  function Has_Next_Element(L: in List) return Boolean;

  -- Purpose : Obtain the current Element in a List
  --
  -- L       : The List
  -- return  : The next Elements
  -- raises  : Not_A_List_Error
  -- raises  : No_More_Elements_Error
  function Get_Element(L: in List) return Element;

  -- Purpose : Obtain and skip the next Element in a List
  --
  -- L       : The List
  -- return  : The next Elements
  -- raises  : Not_A_List_Error
  -- raises  : No_More_Elements_Error
  function Next_Element(L: in List) return Element;

  -- Purpose : Skip the next Element in a List
  --
  -- L       : The List
  -- raises  : Not_A_List_Error
  -- raises  : No_More_Elements_Error
  procedure Next_Element(L: in List);

  -- Purpose : Set the List to the first Element
  --
  -- L       : The List
  -- raises  : Not_A_List_Error
  procedure Reset(L: in List);

  -- Purpose : Save the position to the current Element in a List
  -- Note    : There is a stack of saved positions
  --
  -- L       : The List
  -- raises  : Not_A_List_Error
  procedure Save(L: in List);

  -- Purpose : Restore the position in a List to the previously saved Element
  -- Note    : Saved position must exist, no check is made
  --
  -- L       : The List
  -- raises  : Not_A_List_Error
  -- raises  : No_Saved_Current_Position_Error
  procedure Restore(L: in List);

  -- Purpose : Move an Element to a different List
  -- Note    : The Element is removed from the original List
  --
  -- E       : The Element
  -- L       : The destination List
  -- raises  : Not_A_List_Error
  -- raises  : Incompatible_Lists_Of_Lists_Error
  procedure Move(E: in Element; L: in List);

  -- Purpose : Move all the Elements of a List to a different List
  -- Note    : The From List is not removed
  --
  -- From    : The List of Elements to move
  -- To      : The destination List
  -- raises  : Not_A_List_Error
  -- raises  : Incompatible_Lists_Of_Lists_Error
  procedure Move(From: in List; To: in List);

  -- Purpose : Obtain the List of Lists associated to an Element
  --
  -- E       : The Element
  -- return  : The List of Lists
  function List_Of_Lists_Of(E: in Element) return List_Of_Lists;

  -- Purpose : Obtain the list of an Element
  --
  -- E       : The Element
  -- return  : The List
  function List_Of(E: in Element) return List;

  -- Purpose : To know whether an Element belongs to a List of Lists
  --
  -- E       : The Element
  -- Lol     : The List of Lists
  -- return  : True if Element belongs to the List of Lists
  function Belongs_To(E: in Element; Lol: in List_Of_Lists) return Boolean;

  -- Purpose : To know whether an Element belongs to a List
  --
  -- E       : The Element
  -- L       : The List
  -- return  : True if Element belongs to the List
  -- raises  : Not_A_List_Error
  -- raises  : Incompatible_Lists_Of_Lists_Error
  function Belongs_To(E: in Element; L: in List) return Boolean;

  -- Purpose : Move an Element to the Unassigned List
  -- Note    : The Element is removed from the original List
  --
  -- E       : The Element
  procedure Unassign(E: in Element);


private

  Void: constant := 0;
  Unassigned: constant := 0;

  type List_Of_Lists_Rec;
  type List_Of_Lists is access List_Of_Lists_Rec;

  --------------
  -- Elements --
  --------------

  type Element is record
    Lol: List_Of_Lists;
    Index: Positive;
  end record;

  type Element_Rec is record
    List_Index: Natural := Unassigned;
    Prev: Natural := Void;
    Next: Natural := Void;
  end record;

  type Element_Recs is array(Positive range <>) of Element_Rec;

  -----------
  -- Lists --
  -----------

  type List is record
    Lol: List_Of_Lists;
    Id: Natural := Unassigned;
    Index: Natural := Unassigned;
  end record;

  package Cursor_Stacks is new Stacks(Natural); use Cursor_Stacks;

  type List_Control is record
    Num: Natural := 0;
    First: Natural;
    Last: Natural;
    Current: Natural;
    Saved: Cursor_Stacks.Stack;
  end record;

  type List_Rec is record
    Id: Natural := Unassigned;
    Control: List_Control;
    Prev: Natural;
    Next: Natural;
  end record;

  type List_Recs is array(Natural range <>) of List_Rec;
  type PList_Recs is access List_Recs;

  ---------------------------
  -- Finite Disjoint Lists --
  ---------------------------

  -- Each List represents a linked list of Elements
  -- The number of Lists increases as needed
  -- The maximum number of non-empty Lists equals the number of Elements
  -- There are two linked lists of Lists: the Used and the Free
  type List_Of_Lists_Rec(Size: Positive) is record
    Elements: Element_Recs(1..Size);
    Lists: PList_Recs;
    Current_Id: Natural := Unassigned;
    Used: List_Control;
    Free: List_Control;
  end record;


  Default_Inc: Positive := 10;

  -- Purpose : Increase the space for the Lists
  -- Note    : The old Lists are not affected
  --
  -- Lol     : The List of Lists
  -- Inc     : The number of space for new Lists added
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Increase_Number_Of_Lists(Lol: in List_Of_Lists; Inc: in Positive := Default_Inc);

end Finite_Disjoint_Lists;
