-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


-- @filename Vectors.ads
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 04/04/2008
-- @brief Vector operations

with Ada.Numerics.Float_Random;        use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

generic

   type Float_Type is digits <>;

package Vectors is

   -------------------------------------------------------------------------

   type Vector is limited private;

   procedure Copy(
      vec_ori: in     Vector;             -- vector original
      vec_cop:    out Vector);            -- vector copy

   procedure Allocate(
      vec    :    out Vector;             -- the vector
      nb_row : in     Integer);           -- number of rows

   procedure Deallocate(
      vec    : in out Vector);            -- the vector

   function Dimension(
      vec    : in     Vector)             -- the vector
      return Integer;                     -- number of rows

   function Get(
      vec    : in     Vector;             -- the vector
      row    : in     Integer)            -- row number
      return Float_Type;                  -- value

   procedure Set(
      vec    : in out Vector;             -- the vector
      row    : in     Integer;            -- row number
      val    : in     Float_Type);        -- value

   procedure Initialise(
      vec    : in out Vector;             -- the vector
      val    : in     Float_Type);        -- value

   procedure Initialise_Random(
      vec    : in out Vector;             -- the vector
      gen    : in out Generator;          -- random numbers generator
      val_min: in     Float_Type;         -- value minimum
      val_max: in     Float_Type);        -- value maximum

   procedure Signs(
      vec_ori: in     Vector;             -- vector original
      vec_sgn:    out Vector);            -- vector of signs

   procedure Add(
      vec1   : in     Vector;             -- the vector 1
      vec2   : in     Vector;             -- the vector 2
      vec_add:    out Vector);            -- vector add

   procedure Subtract(
      vec1   : in     Vector;             -- the vector 1
      vec2   : in     Vector;             -- the vector 2
      vec_sub:    out Vector);            -- vector subtract

   function Dot_Product(
      vec1   : in     Vector;             -- the vector 1
      vec2   : in     Vector)             -- the vector 2
      return Float_Type;                  -- dot product

   function Sum(
      vec    : in     Vector)             -- the vector
      return Float_Type;                  -- sum

   function Norm(
      vec    : in     Vector)             -- the vector
      return Float_Type;                  -- norm

   procedure Normalise(
      vec_ori: in     Vector;             -- vector original
      vec_nrm:    out Vector);            -- vector normalised

   procedure Put(
      fil    : in out File_Type;          -- the file
      vec    : in     Vector);            -- the vector

   -------------------------------------------------------------------------

private

   -------------------------------------------------------------------------

   type Array1 is array (Integer range <>) of Float_Type;

   type Ptr_Array1 is access Array1;

   type Vector is record
      nb_row : Integer := 0;         -- number of rows
      val    : Ptr_Array1 := null;   -- values
   end record;

   -------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(Array1, Ptr_Array1);

   -------------------------------------------------------------------------

   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions(Float_Type);
   use Elementary_Functions;

   package Float_Type_IO is new Ada.Text_IO.Float_IO(Float_Type);
   use Float_Type_IO;

   -------------------------------------------------------------------------

end Vectors;
