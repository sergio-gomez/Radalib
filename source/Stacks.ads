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


-- @filename Stacks.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 4/04/2005
-- @revision 08/04/2012
-- @brief Treatment of Stacks

generic
  type Item is private;
package Stacks is

  -- Represents a Stack
  type Stack is private;

  Uninitialized_Stack_Error: exception;
  Void_Stack_Error: exception;


  -- Purpose : Initialize a Stack
  --
  -- St      : The Stack
  procedure Initialize(St: out Stack);

  -- Purpose : To know whether a Stack is Initialized
  --
  -- St      : The Stack
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(St: in Stack) return Boolean;

  -- Purpose : Deallocate all the space used by a Stack
  --
  -- St      : The Stack
  procedure Free(St: in out Stack);

  -- Purpose : Push an Item into a Stack
  --
  -- E       : The Item
  -- St      : The Stack
  -- raises  : Uninitialized_Stack_Error
  procedure Push(E: in Item; St: in Stack);

  -- Purpose : Pop the Top Item of a Stack
  --
  -- St      : The Stack
  -- raises  : Uninitialized_Stack_Error
  -- raises  : Void_Stack_Error
  procedure Pop(St: in Stack);

  -- Purpose : Get and Pop the Top Item of a Stack
  --
  -- St      : The Stack
  -- return  : The Top Item
  -- raises  : Uninitialized_Stack_Error
  -- raises  : Void_Stack_Error
  function Pop(St: in Stack) return Item;

  -- Purpose : Get the Top Item of a Stack
  --
  -- St      : The Stack
  -- return  : The Top Item
  -- raises  : Uninitialized_Stack_Error
  -- raises  : Void_Stack_Error
  function Top(St: in Stack) return Item;

  -- Purpose : Obtain the total number of Items in a Stack
  --
  -- St      : The Stack
  -- return  : The number of Elements
  -- raises  : Uninitialized_Stack_Error
  function Size(St: in Stack) return Natural;

  -- Purpose : To know whether a Stack is Empty
  --
  -- St      : The Stack
  -- return  : True if Stack Empty
  -- raises  : Uninitialized_Stack_Error
  function Is_Empty(St: in Stack) return Boolean;

  -- Purpose : Remove all the Items in a Stack
  --
  -- St      : The Stack
  -- raises  : Uninitialized_Stack_Error
  procedure Clear(St: in Stack);

  -- Purpose : Create a copy of a Stack
  --
  -- St      : The Stack
  -- return  : The Clone
  -- raises  : Uninitialized_Stack_Error
  function Clone(St: in Stack) return Stack;


private

  -----------
  -- Nodes --
  -----------

  type Node;
  type Pnode is access Node;

  type Node is record
    Value: Item;
    Next: Pnode;
  end record;

  ------------
  -- Stacks --
  ------------

  type Stack_Rec is record
    Top: Pnode;
    Num: Natural := 0;
  end record;

  type Stack is access Stack_Rec;

end Stacks;
