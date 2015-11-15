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


-- @filename Linked_Lists.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/10/2004
-- @revision 08/04/2012
-- @brief Treatment of Linked Lists

with Stacks;

generic
  type Item is private;
package Linked_Lists is

  -- Represents a Linked List of Items
  type Linked_List is private;

  -- Type of functions to compare Items
  type Comparator is access function(Left, Right: in Item) return Boolean;


  Uninitialized_Linked_List_Error: exception;
  Index_Out_Of_Bounds_Error: exception;
  Void_Linked_List_Error: exception;
  No_More_Items_Error: exception;
  No_Saved_Current_Position_Error: exception;


  -- Purpose : Initialize a Linked List
  --
  -- Ll      : The Linked List
  procedure Initialize(Ll: out Linked_List);

  -- Purpose : To know whether a Linked List is Initialized
  --
  -- Ll      : The Linked List
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Ll: in Linked_List) return Boolean;

  -- Purpose : Deallocate all the space used by a Linked List
  --
  -- Ll      : The Linked List
  procedure Free(Ll: in out Linked_List);

  -- Purpose : Insert an Item to beginning of a Linked List
  --
  -- E       : The Item
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Add_First(E: in Item; Ll: in Linked_List);

  -- Purpose : Append an Item to end of a Linked List
  --
  -- E       : The Item
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Add_Last(E: in Item; Ll: in Linked_List);

  -- Purpose : Insert an Item to the specified position in a Linked List
  -- Note    : The position has to be in the range 1..Size(Ll)+1
  --
  -- E       : The Item
  -- Pos     : The position
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Index_Out_Of_Bounds_Error
  procedure Add(E: in Item; Pos: in Positive; Ll: in Linked_List);

  -- Purpose : Insert an Item to the current position in a Linked List
  -- Note    : The inserted Item becomes the current Item
  --
  -- E       : The Item
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Add(E: in Item; Ll: in Linked_List);

  -- Purpose : Obtain the total number of Items in the Linked List
  --
  -- Ll      : The Linked List
  -- return  : The number of Elements
  -- raises  : Uninitialized_Linked_List_Error
  function Size(Ll: in Linked_List) return Natural;

  -- Purpose : To know whether an Item belongs to a Linked List
  --
  -- E       : The Item
  -- Ll      : The Linked List
  -- return  : True if Item belongs to the Linked List
  -- raises  : Uninitialized_Linked_List_Error
  function Belongs_To(E: in Item; Ll: in Linked_List) return Boolean;

  -- Purpose : Obtain the Position of an Item in a Linked List
  -- Note    : Returns 0 if Item not found
  --
  -- E       : The Item
  -- Ll      : The Linked List
  -- return  : The Position
  -- raises  : Uninitialized_Linked_List_Error
  function Position(E: in Item; Ll: in Linked_List) return Natural;

  -- Purpose : Obtain the first Item in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : The first Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  function Get_First(Ll: in Linked_List) return Item;

  -- Purpose : Obtain the last Item in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : The last Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  function Get_Last(Ll: in Linked_List) return Item;

  -- Purpose : Obtain the Item at the specified position in a Linked List
  -- Note    : The position has to be in the range 1..Size(Ll)
  --
  -- Pos     : The position
  -- Ll      : The Linked List
  -- return  : The Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : Index_Out_Of_Bounds_Error
  function Get(Pos: in Positive; Ll: in Linked_List) return Item;

  -- Purpose : Obtain the current Item in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : The current Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : No_More_Items_Error
  function Get(Ll: in Linked_List) return Item;

  -- Purpose : Replace the first Item in a Linked List
  --
  -- Ll      : The Linked List
  -- E       : The replacing Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  procedure Replace_First(Ll: in Linked_List; E: in Item);

  -- Purpose : Replace the last Item in a Linked List
  --
  -- Ll      : The Linked List
  -- E       : The replacing Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  procedure Replace_Last(Ll: in Linked_List; E: in Item);

  -- Purpose : Replace the Item at the specified position in a Linked List
  -- Note    : The position has to be in the range 1..Size(Ll)
  --
  -- Pos     : The position
  -- Ll      : The Linked List
  -- E       : The replacing Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : Index_Out_Of_Bounds_Error
  procedure Replace(Pos: in Positive; Ll: in Linked_List; E: in Item);

  -- Purpose : Replace the current Item in a Linked List
  --
  -- Ll      : The Linked List
  -- E       : The replacing Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : No_More_Items_Error
  procedure Replace(Ll: in Linked_List; E: in Item);

  -- Purpose : Remove the first Item in a Linked List
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  procedure Remove_First(Ll: in Linked_List);

  -- Purpose : Remove the last Item in a Linked List
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  procedure Remove_Last(Ll: in Linked_List);

  -- Purpose : Remove the Item at the specified position from a Linked List
  -- Note    : The position has to be in the range 1..Size(Ll)
  --
  -- Pos     : The position
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : Index_Out_Of_Bounds_Error
  procedure Remove(Pos: in Positive; Ll: in Linked_List);

  -- Purpose : Remove the Item at the current position in a Linked List
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : No_More_Items_Error
  procedure Remove(Ll: in Linked_List);

  -- Purpose : Remove all instances of an Item in a Linked List
  --
  -- E       : The Item
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Remove_All(E: in Item; Ll: in Linked_List);

  -- Purpose : To know if there are next Items left in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : True if there are next Items left
  -- raises  : Uninitialized_Linked_List_Error
  function Has_Next(Ll: in Linked_List) return Boolean;

  -- Purpose : To know if there are previous Items left in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : True if there are previous Items
  -- raises  : Uninitialized_Linked_List_Error
  function Has_Prev(Ll: in Linked_List) return Boolean;

  -- Purpose : Obtain and skip the next Item in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : The next Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : No_More_Items_Error
  function Next(Ll: in Linked_List) return Item;

  -- Purpose : Skip the next Item in a Linked List
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : No_More_Items_Error
  procedure Next(Ll: in Linked_List);

  -- Purpose : Obtain and skip the previous Item in a Linked List
  --
  -- Ll      : The Linked List
  -- return  : The previous Item
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : No_More_Items_Error
  function Prev(Ll: in Linked_List) return Item;

  -- Purpose : Skip the previous Item in a Linked List
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : No_More_Items_Error
  procedure Prev(Ll: in Linked_List);

  -- Purpose : Set the Linked List to the first Item
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Reset(Ll: in Linked_List);

  -- Purpose : Set the Linked List to the first Item
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  procedure Set_First(Ll: in Linked_List);

  -- Purpose : Set the Linked List to the last Item
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  procedure Set_Last(Ll: in Linked_List);

  -- Purpose : Set the Linked List to the end
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Set_End(Ll: in Linked_List);

  -- Purpose : Set the Linked List to the specified position in a Linked List
  -- Note    : The position has to be in the range 1..Size(Ll)+1
  --
  -- Pos     : The position
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : Void_Linked_List_Error
  -- raises  : Index_Out_Of_Bounds_Error
  procedure Set(Pos: in Positive; Ll: in Linked_List);

  -- Purpose : Save the current position in a Linked List
  -- Note    : There is a stack of saved positions
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Save(Ll: in Linked_List);

  -- Purpose : Restore the current position in a Linked List to the previously saved one
  -- Note    : Saved position must exist, no check is made
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  -- raises  : No_Saved_Current_Position_Error
  procedure Restore(Ll: in Linked_List);

  -- Purpose : Remove all the Items in a Linked List
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Clear(Ll: in Linked_List);

  -- Purpose : Create a copy of a Linked List
  -- Note    : Current position and Saved positions are not cloned
  --
  -- Ll      : The Linked List
  -- return  : The Clone
  -- raises  : Uninitialized_Linked_List_Error
  function Clone(Ll: in Linked_List) return Linked_List;

  -- Purpose : Create a new Linked List by concatenation of the two given Linked Lists
  --
  -- Ll1     : The first Linked List
  -- Ll2     : The second Linked List
  -- return  : The concatenation of the Linked Lists
  -- raises  : Uninitialized_Linked_List_Error
  function Join(Ll1, Ll2: in Linked_List) return Linked_List;

  generic
    with function Lower(Left, Right: in Item) return Boolean is <>;
  -- Purpose : Sort a Linked List using the Quicksort algorithm
  -- Note    : Current position and Saved positions are lost
  --
  -- Ll      : The Linked List
  -- Lower   : The function to know whether the first Item is smaller than the second
  -- raises  : Uninitialized_Linked_List_Error
  procedure Generic_Sort(Ll: in Linked_List);

  -- Purpose : Sort a Linked List using the Quicksort algorithm
  -- Note    : Current position and Saved positions are lost
  --
  -- Ll      : The Linked List
  -- Lower   : The function to know whether the first Item is smaller than the second
  -- raises  : Uninitialized_Linked_List_Error
  procedure Sort(Ll: in Linked_List; Lower: in Comparator);

  -- Purpose : Remove adjacent duplicates of each Item in a Linked List
  -- Note    : If the Linked List is previously sorted, all duplicates are removed
  -- Note    : Current position and Saved positions are lost
  --
  -- Ll      : The Linked List
  -- raises  : Uninitialized_Linked_List_Error
  procedure Remove_Adjacent_Duplicates(Ll: in Linked_List);


private

  -----------
  -- Nodes --
  -----------

  type Node;
  type Pnode is access Node;

  type Node is record
    Value: Item;
    Next: Pnode;
    Prev: Pnode;
  end record;

  ------------------
  -- Linked Lists --
  ------------------

  package Pnode_Stacks is new Stacks(Pnode); use Pnode_Stacks;

  type Linked_List_Rec is record
    First: Pnode;
    Last: Pnode;
    Current: Pnode;
    Saved: Pnode_Stacks.Stack;
    Num: Natural := 0;
  end record;

  type Linked_List is access Linked_List_Rec;

  -- Purpose : Get the Node at the specified position in the Linked List
  -- Note    : returns null if the position is out of bounds
  --
  -- Pos     : The position
  -- Ll      : The Linked List
  -- return  : The access to the Node
  -- raises  : Uninitialized_Linked_List_Error
  function Get_Node(Pos: in Positive; Ll: in Linked_List) return Pnode;

  -- Purpose : Swap the values of the given Nodes
  -- Note    : Both Nodes must exist, no check is performed
  --
  -- P1      : First Node
  -- P2      : Second Node
  procedure Swap(P1, P2: in Pnode);
  pragma Inline(Swap);

  generic
    with function Lower(Left, Right: in Item) return Boolean is <>;
  -- Purpose : Sort a slice of a Linked List using the Insertion algorithm
  -- Note    : All Nodes must exist, no check is performed
  --
  -- Left    : Left Node
  -- Num     : Number of Nodes
  -- Lower   : The function to know whether the first Item is smaller than the second
  procedure Generic_Insertion_Sort(Left: in Pnode; Num: in Positive);

  generic
    with function Lower(Left, Right: in Item) return Boolean is <>;
  -- Purpose : Sort a slice of a Linked List using the Quicksort algorithm
  -- Note    : All Nodes must exist, no check is performed
  --
  -- Left    : Left Node
  -- Right   : Right Node
  -- Num     : Number of Nodes
  -- Lower   : The function to know whether the first Item is smaller than the second
  procedure Generic_Quick_Sort(Left, Right: in Pnode; Num: in Positive);

end Linked_Lists;
