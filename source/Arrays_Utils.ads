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


-- @filename Arrays_Utils.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/07/2009
-- @revision 18/12/2014
-- @brief Arrays utils

with Utils; use Utils;

generic
  type Item is private;
  type Items is array(Integer range <>) of Item;
  type Itemss is array(Integer range <>, Integer range <>) of Item;
  type PItems is access Items;
  type PItemss is access Itemss;
  type PsItems is array (Integer range <>) of PItems;
  type PPsItems is access PsItems;
  with function Alloc(First, Last: in Integer) return PItems is <>;
  with function Alloc(First1, Last1, First2, Last2: in Integer) return PItemss is <>;
  with function Alloc(First, Last: in Integer) return PItemss is <>;
  with function "<"(Left, Right: in Item) return Boolean is <>;
package Arrays_Utils is

  Uninitialized_Array_Error: exception;
  Incompatible_Arrays_Error: exception;
  Array_Index_Error: exception;

  -- Purpose : Allocate an initialized vector of Items
  --
  -- First   : The First index
  -- Last    : The Last index
  -- Ini_Val : Initial value for the elements of the vector
  -- return  : The access to the vector of Items
  function Alloc(First, Last: in Integer; Ini_Val: in Item) return PItems;

  -- Purpose : Allocate an initialized matrix of Items
  --
  -- First1  : The First index of first dimension
  -- Last1   : The Last index of first dimension
  -- First2  : The First index of second dimension
  -- Last2   : The Last index of second dimension
  -- Ini_Val : Initial value for the elements of the matrix
  -- return  : The access to the matrix of Items
  function Alloc(First1, Last1, First2, Last2: in Integer; Ini_Val: in Item) return PItemss;

  -- Purpose : Allocate an initialized squared matrix of Items
  --
  -- First   : The First index of both dimensions
  -- Last    : The Last index of both dimensions
  -- Ini_Val : Initial value for the elements of the squared matrix
  -- return  : The access to the matrix of Items
  function Alloc(First, Last: in Integer; Ini_Val: in Item) return PItemss;

  -- Purpose : Obtain a Column of a matrix of Items
  --
  -- A       : The matrix
  -- Index   : The index of the column
  -- return  : The column vector
  -- raises  : Array_Index_Error
  function Column(A: in Itemss; Index: in Integer) return Items;

  -- Purpose : Obtain a Column of a matrix of Items
  --
  -- A       : The matrix
  -- Index   : The index of the column
  -- return  : The column vector
  -- raises  : Uninitialized_Array_Error
  -- raises  : Array_Index_Error
  function Column(A: in PItemss; Index: in Integer) return PItems;

  -- Purpose : Obtain a Row of a matrix of Items
  --
  -- A       : The matrix
  -- Index   : The index of the row
  -- return  : The row vector
  -- raises  : Array_Index_Error
  function Row(A: in Itemss; Index: in Integer) return Items;

  -- Purpose : Obtain a Row of a matrix of Items
  --
  -- A       : The matrix
  -- Index   : The index of the row
  -- return  : The row vector
  -- raises  : Uninitialized_Array_Error
  -- raises  : Array_Index_Error
  function Row(A: in PItemss; Index: in Integer) return PItems;

  -- Purpose : Obtain the Diagonal of a matrix of Items
  -- Note    : Non-squared matrices also possible
  --
  -- A       : The squared matrix
  -- return  : The diagonal vector
  function Diagonal(A: in Itemss) return Items;

  -- Purpose : Obtain the Diagonal of a matrix of Items
  -- Note    : Non-squared matrices also possible
  --
  -- A       : The squared matrix
  -- return  : The diagonal vector
  -- raises  : Uninitialized_Array_Error
  function Diagonal(A: in PItemss) return PItems;

  -- Purpose : Set the Diagonal of a matrix to a given Item
  -- Note    : Non-squared matrices also possible
  --
  -- A       : The squared matrix
  -- Diag    : The diagonal Item
  procedure Set_Diagonal(A: in out Itemss; Diag: in Item);

  -- Purpose : Set the Diagonal of a matrix to a given Item
  -- Note    : Non-squared matrices also possible
  --
  -- A       : The squared matrix
  -- Diag    : The diagonal Item
  -- raises  : Uninitialized_Array_Error
  procedure Set_Diagonal(A: in PItemss; Diag: in Item);

  -- Purpose : Set the Diagonal of a matrix to a given vector
  -- Note    : Non-squared matrices also possible
  --
  -- A       : The squared matrix
  -- Diag    : The diagonal vector
  -- raises  : Incompatible_Arrays_Error
  procedure Set_Diagonal(A: in out Itemss; Diag: in Items);

  -- Purpose : Set the Diagonal of a matrix to a given vector
  -- Note    : Non-squared matrices also possible
  --
  -- A       : The squared matrix
  -- Diag    : The diagonal vector
  -- raises  : Uninitialized_Array_Error
  -- raises  : Incompatible_Arrays_Error
  procedure Set_Diagonal(A: in PItemss; Diag: in PItems);

  -- Purpose : Obtain a Slice of a vector of Items
  --
  -- A       : The vector
  -- First   : The First index of the slice
  -- Last    : The Last index of the slice
  -- return  : The column vector
  -- raises  : Array_Index_Error
  function Slice(A: in Items; First, Last: in Integer) return Items;

  -- Purpose : Obtain a Slice of a vector of Items
  --
  -- A       : The vector
  -- First   : The First index of the slice
  -- Last    : The Last index of the slice
  -- return  : The slice vector
  -- raises  : Uninitialized_Array_Error
  -- raises  : Array_Index_Error
  function Slice(A: in PItems; First, Last: in Integer) return PItems;

  -- Purpose : Obtain a Slice of a matrix of Items
  --
  -- A       : The matrix
  -- First1  : The First index of first dimension of the slice
  -- Last1   : The Last index of first dimension of the slice
  -- First2  : The First index of second dimension of the slice
  -- Last2   : The Last index of second dimension of the slice
  -- return  : The slice matrix
  -- raises  : Array_Index_Error
  function Slice(A: in Itemss; First1, Last1, First2, Last2: in Integer) return Itemss;

  -- Purpose : Obtain a Slice of a matrix of Items
  --
  -- A       : The matrix
  -- First1  : The First index of first dimension of the slice
  -- Last1   : The Last index of first dimension of the slice
  -- First2  : The First index of second dimension of the slice
  -- Last2   : The Last index of second dimension of the slice
  -- return  : The slice matrix
  -- raises  : Uninitialized_Array_Error
  -- raises  : Array_Index_Error
  function Slice(A: in PItemss; First1, Last1, First2, Last2: in Integer) return PItemss;

  -- Purpose : Obtain a squared Slice of a matrix of Items
  --
  -- A       : The matrix
  -- First   : The First index of both dimensions of the slice
  -- Last    : The Last index of both dimensions of the slice
  -- return  : The squared slice matrix
  -- raises  : Array_Index_Error
  function Slice(A: in Itemss; First, Last: in Integer) return Itemss;

  -- Purpose : Obtain a squared Slice of a matrix of Items
  --
  -- A       : The matrix
  -- First   : The First index of both dimensions of the slice
  -- Last    : The Last index of both dimensions of the slice
  -- return  : The squared slice matrix
  -- raises  : Uninitialized_Array_Error
  -- raises  : Array_Index_Error
  function Slice(A: in PItemss; First, Last: in Integer) return PItemss;

  -- Purpose : Swap the values at the given positions of a vector
  --
  -- A       : The vector
  -- P1      : The First position
  -- P2      : The Second position
  -- raises  : Array_Index_Error
  procedure Swap(A: in out Items; P1, P2: in Integer);

  -- Purpose : Swap the values at the given positions of a vector
  --
  -- A       : The vector
  -- P1      : The First position
  -- P2      : The Second position
  -- raises  : Uninitialized_Array_Error
  -- raises  : Array_Index_Error
  procedure Swap(A: in PItems; P1, P2: in Integer);

  -- Purpose : Reverse the values of a vector
  --
  -- A       : The vector
  procedure Swap(A: in out Items);

  -- Purpose : Reverse the values of a vector
  --
  -- A       : The vector
  -- raises  : Uninitialized_Array_Error
  procedure Swap(A: in PItems);

  -- Purpose : Swap the values at the given positions of a vector
  -- Note    : Both positions must exist, no check is performed
  --
  -- A       : The vector
  -- P1      : The First position
  -- P2      : The Second position
  procedure Unchecked_Swap(A: in out Items; P1, P2: in Integer);
  pragma Inline(Unchecked_Swap);

  -- Purpose : Swap the values at the given positions of a vector
  -- Note    : Both positions must exist, no check is performed
  --
  -- A       : The vector
  -- P1      : The First position
  -- P2      : The Second position
  procedure Unchecked_Swap(A: in out PsItems; P1, P2: in Integer);
  pragma Inline(Unchecked_Swap);

  -- Purpose : Sort a vector of Items
  --
  -- A       : The vector
  procedure Sort(A: in out Items);

  -- Purpose : Sort a vector of Items
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- raises  : Incompatible_Arrays_Error
  procedure Sort(A: in out Items; Index: out Integers);

  -- Purpose : Sort a vector of Items
  --
  -- A       : The vector
  -- raises  : Uninitialized_Array_Error
  procedure Sort(A: in PItems);

  -- Purpose : Sort a vector of Items
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- raises  : Uninitialized_Array_Error
  procedure Sort(A: in PItems; Index: out PIntegers);

  -- Purpose : Compare two vectors of Items in lexicographical order
  -- Note    : The vectors may be of different size
  --
  -- Left    : The Left vector
  -- Right   : The Right vector
  -- raises  : Uninitialized_Array_Error
  function "<"(Left, Right: in PItems) return Boolean;

  -- Purpose : Sort a vector of Items in lexicographical order
  --
  -- A       : The vector
  procedure Sort(A: in out PsItems);

  -- Purpose : Sort a vector of Items in lexicographical order
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- raises  : Incompatible_Arrays_Error
  procedure Sort(A: in out PsItems; Index: out Integers);

  -- Purpose : Sort a vector of Items in lexicographical order
  --
  -- A       : The vector
  -- raises  : Uninitialized_Array_Error
  procedure Sort(A: in PPsItems);

  -- Purpose : Sort a vector of Items in lexicographical order
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- raises  : Uninitialized_Array_Error
  procedure Sort(A: in PPsItems; Index: out PIntegers);


private

  -- Purpose : Sort a slice of a vector using the Insertion algorithm
  -- Note    : Both positions must exist, no check is performed
  --
  -- A       : The vector
  -- Left    : Left position
  -- Right   : Right position
  procedure Insertion_Sort(A: in out Items; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Insertion algorithm
  -- Note    : Both positions must exist, no check is performed
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- Left    : Left position
  -- Right   : Right position
  procedure Insertion_Sort(A: in out Items; Index: in out Integers; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Insertion algorithm
  -- Note    : Both positions must exist, no check is performed
  --
  -- A       : The vector
  -- Left    : Left position
  -- Right   : Right position
  procedure Insertion_Sort(A: in out PsItems; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Insertion algorithm
  -- Note    : Both positions must exist, no check is performed
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- Left    : Left position
  -- Right   : Right position
  procedure Insertion_Sort(A: in out PsItems; Index: in out Integers; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Quicksort algorithm
  -- Note    : Both positions must exist, no check is performed
  -- Note    : Not fully sorted! Insertion Sort expected to complete the sorting
  --
  -- A       : The vector
  -- Left    : Left position
  -- Right   : Right position
  procedure Quick_Sort(A: in out Items; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Quicksort algorithm
  -- Note    : Both positions must exist, no check is performed
  -- Note    : Not fully sorted! Insertion Sort expected to complete the sorting
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- Left    : Left position
  -- Right   : Right position
  procedure Quick_Sort(A: in out Items; Index: in out Integers; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Quicksort algorithm
  -- Note    : Both positions must exist, no check is performed
  -- Note    : Not fully sorted! Insertion Sort expected to complete the sorting
  --
  -- A       : The vector
  -- Left    : Left position
  -- Right   : Right position
  procedure Quick_Sort(A: in out PsItems; Left, Right: in Integer);

  -- Purpose : Sort a slice of a vector using the Quicksort algorithm
  -- Note    : Both positions must exist, no check is performed
  -- Note    : Not fully sorted! Insertion Sort expected to complete the sorting
  --
  -- A       : The vector
  -- Index   : Original Index of the sorted Items
  -- Left    : Left position
  -- Right   : Right position
  procedure Quick_Sort(A: in out PsItems; Index: in out Integers; Left, Right: in Integer);

end Arrays_Utils;
