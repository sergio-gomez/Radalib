-- Radalib, Copyright (c) 2017 by
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


-- @filename Queues.ads
-- @author Javier Borge
-- @version 1.0
-- @date 5/10/2007
-- @revision 08/04/2012
-- @brief Treatment of Queues

generic
  type Item is private;
package Queues is

  -- Represents a Queue
  type Queue is private;

  Uninitialized_Queue_Error: exception;
  Void_Queue_Error: exception;


  -- Purpose : Initialize a Queue
  --
  -- Q       : The Queue
  procedure Initialize(Q: out Queue);

  -- Purpose : To know whether a Queue is Initialized
  --
  -- Q       : The Queue
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Q: in Queue) return Boolean;

  -- Purpose : Deallocate all the space used by a Queue
  --
  -- Q       : The Queue
  procedure Free(Q: in out Queue);

  -- Purpose : Enqueue an Item into a Queue
  --
  -- E       : The Item
  -- Q       : The Queue
  -- raises  : Uninitialized_Queue_Error
  procedure Enqueue(E: in Item; Q: in Queue);

  -- Purpose : Dequeue the Head Item of a Queue
  --
  -- Q       : The Queue
  -- raises  : Uninitialized_Queue_Error
  -- raises  : Void_Queue_Error
  procedure Dequeue(Q: in Queue);

  -- Purpose : Dequeue the Head Item of a Queue
  --
  -- Q       : The Queue
  -- return  : The head item
  -- raises  : Uninitialized_Queue_Error
  -- raises  : Void_Queue_Error
  function Dequeue(Q: in Queue) return Item;

  -- Purpose : Get the Head Item of a Queue
  --
  -- Q       : The Queue
  -- return  : The head item
  -- raises  : Uninitialized_Queue_Error
  -- raises  : Void_Queue_Error
  function Head(Q: in Queue) return Item;

  -- Purpose : Obtain the total number of Items in a Queue
  --
  -- Q       : The Queue
  -- return  : The number of Elements
  -- raises  : Uninitialized_Queue_Error
  function Size(Q: in Queue) return Natural;

  -- Purpose : To know whether a Queue is empty
  --
  -- Q       : The Queue
  -- return  : True if Queue Empty
  -- raises  : Uninitialized_Queue_Error
  function Is_Empty(Q: in Queue) return Boolean;

  -- Purpose : Remove all the Items in a Queue
  --
  -- Q       : The Queue
  -- raises  : Uninitialized_Queue_Error
  procedure Clear(Q: in Queue);

  -- Purpose : Create a copy of a Queue
  --
  -- Q       : The Queue
  -- return  : The Clone
  -- raises  : Uninitialized_Queue_Error
  function Clone(Q: in Queue) return Queue;


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
  -- Queues --
  ------------

  type Queue_Rec is record
    First : Pnode;
    Last  : Pnode;
    Num   : Natural := 0;
  end record;

  type Queue is access Queue_Rec;

end Queues;
