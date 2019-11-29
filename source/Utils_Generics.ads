-- Radalib, Copyright (c) 2019 by
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


-- @filename Utils_Generics.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 24/03/2010
-- @revision 18/12/2014
-- @brief Several Generics Utils

package Utils_Generics is

  -- Purpose : Allocate one-dimensional arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
  function Alloc_1D_Array(First, Last: in Integer) return PItems;

  -- Purpose : Allocate two-dimensional squared arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  generic
    type Item is private;
    type Itemss is array(Integer range <>, Integer range <>) of Item;
    type PItemss is access Itemss;
  function Alloc_2D_Squared_Array(First, Last: in Integer) return PItemss;

  -- Purpose : Allocate two-dimensional rectangular arrays
  --
  -- First1  : The First index of first dimension
  -- Last1   : The Last index of first dimension
  -- First2  : The First index of second dimension
  -- Last2   : The Last index of second dimension
  -- return  : The access to the array
  generic
    type Item is private;
    type Itemss is array(Integer range <>, Integer range <>) of Item;
    type PItemss is access Itemss;
  function Alloc_2D_Rectangular_Array(First1, Last1, First2, Last2: in Integer) return PItemss;

  -- Purpose : Allocate two-dimensional squared arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
    type PsItems is array(Integer range <>) of PItems;
    type PPsItems is access PsItems;
  function Alloc_Irregular_Squared_Array(First, Last: in Integer) return PPsItems;

  -- Purpose : Allocate two-dimensional rectangular arrays
  --
  -- First1  : The First index of first dimension
  -- Last1   : The Last index of first dimension
  -- First2  : The First index of second dimension
  -- Last2   : The Last index of second dimension
  -- return  : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
    type PsItems is array(Integer range <>) of PItems;
    type PPsItems is access PsItems;
  function Alloc_Irregular_Rectangular_Array(First1, Last1, First2, Last2: in Integer) return PPsItems;

  -- Purpose : Allocate upper triangular arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
    type PsItems is array(Integer range <>) of PItems;
    type PPsItems is access PsItems;
  function Alloc_Upper_Triangular_Array(First, Last: in Integer) return PPsItems;

  -- Purpose : Allocate lower triangular arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
    type PsItems is array(Integer range <>) of PItems;
    type PPsItems is access PsItems;
  function Alloc_Lower_Triangular_Array(First, Last: in Integer) return PPsItems;

  -- Purpose : Deallocate the space used by one-dimensional arrays
  --
  -- P       : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
  procedure Free_1D_Array(P: in out PItems);

  -- Purpose : Deallocate the space used two-dimensional arrays
  --
  -- P       : The access to the array
  generic
    type Item is private;
    type Itemss is array(Integer range <>, Integer range <>) of Item;
    type PItemss is access Itemss;
  procedure Free_2D_Array(P: in out PItemss);

  -- Purpose : Deallocate the space used by two-dimensional irregular arrays
  --
  -- P       : The access to the array
  generic
    type Item is private;
    type Items is array(Integer range <>) of Item;
    type PItems is access Items;
    type PsItems is array(Integer range <>) of PItems;
    type PPsItems is access PsItems;
  procedure Free_2D_Irregular_Array(P: in out PPsItems);

end Utils_Generics;
