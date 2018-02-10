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


-- @filename Minheaps.ads
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 22/10/2007
-- @revision 21/01/2018
-- @brief Treatment of Minheaps

generic
  type Item is private;
  with function "<"(E1: in Item; E2: in Item) return Boolean is <>;
package Minheaps is

  -- Represents a Minheap
  type Minheap is private;

  Uninitialized_Minheap_Error: exception;
  Void_Minheap_Error         : exception;
  Overloaded_Minheap_Error   : exception;


  -- Purpose : Initialize a Minheap
  --
  -- M       : The Minheap
  -- Size    : The Size
  procedure Initialize(M: out Minheap; Size: in Positive);

  -- Purpose : To know whether a Minheap is Initialized
  --
  -- M       : The Minheap
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(M: in Minheap) return Boolean;

  -- Purpose : Deallocate all the space used by a Minheap
  --
  -- M       : The Minheap
  procedure Free(M: in out Minheap);

  -- Purpose : Obtain the Size of a Minheap
  --
  -- M       : The Minheap
  -- return  : The Size
  -- raises  : Uninitialized_Minheap_Error
  function Size(M: in Minheap) return Natural;

  -- Purpose : Add an Item to the Minheap
  --
  -- E       : The Item
  -- M       : The Minheap
  -- raises  : Uninitialized_Minheap_Error
  -- raises  : Overloaded_Minheap_Error
  procedure Add(E: in Item; M: in Minheap);

  -- Purpose : Delete the Minimum Element of the Minheap
  --
  -- M       : The Minheap
  -- raises  : Uninitialized_Minheap_Error
  -- raises  : Void_Minheap_Error
  procedure Delete_Minimum(M: in Minheap);

  -- Purpose : Get and Delete the Minimum Element of the Minheap
  --
  -- M       : The Minheap
  -- return  : The Minimum Element
  -- raises  : Uninitialized_Minheap_Error
  -- raises  : Void_Minheap_Error
  function Delete_Minimum(M: in Minheap) return Item;

  -- Purpose : Get the Minimum Element of the Minheap
  --
  -- M       : The Minheap
  -- return  : The Item
  -- raises  : Uninitialized_Minheap_Error
  -- raises  : Void_Minheap_Error
  function Minimum(M: in Minheap) return Item;

  -- Purpose : Obtain the Number of Elements in a Minheap
  --
  -- M       : The Minheap
  -- return  : The Number of Elements
  -- raises  : Uninitialized_Minheap_Error
  function Number_Of_Elements(M: in Minheap) return Natural;

  -- Purpose : To know whether a Minheap is empty
  --
  -- M       : The Minheap
  -- return  : True if Minheap Empty
  -- raises  : Uninitialized_Minheap_Error
  function Is_Empty(M: in Minheap) return Boolean;

  -- Purpose : Remove all the Items in a Minheap
  --
  -- M       : The Minheap
  -- raises  : Uninitialized_Minheap_Error
  procedure Clear(M: in Minheap);

  -- Purpose : Create a copy of a Minheap
  --
  -- M       : The Minheap
  -- return  : The Clone
  -- raises  : Uninitialized_Minheap_Error
  function Clone(M: in Minheap) return Minheap;


  procedure Delete_Min(M: in Minheap) renames Delete_Minimum;
  function Delete_Min(M: in Minheap) return Item renames Delete_Minimum;
  function Min(M: in Minheap) return Item renames Minimum;
  function Head(M: in Minheap) return Item renames Minimum;

private

  --------------
  -- Minheaps --
  --------------

  type List is array(Positive range <>) of Item;

  type Minheap_Rec(Size: Positive) is record
    Elements : List(1..Size);
    Num      : Natural := 0;
  end record;

  type Minheap is access Minheap_Rec;

end Minheaps;
