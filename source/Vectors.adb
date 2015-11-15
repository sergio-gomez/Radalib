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


-- @filename Vectors.adb
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 04/04/2008
-- @brief Vector operations

package body Vectors is

   -------------------------------------------------------------------------
   -- Add --
   ---------

   procedure Add(
      vec1   : in     Vector;             -- the vector 1
      vec2   : in     Vector;             -- the vector 2
      vec_add:    out Vector)             -- vector add
   is
   begin
      Allocate(vec_add, vec1.nb_row);
      for r in 1 .. vec1.nb_row loop
         vec_add.val(r) := vec1.val(r) + vec2.val(r);
      end loop;
   end Add;

   -------------------------------------------------------------------------
   -- Allocate --
   --------------

   procedure Allocate(
      vec    :    out Vector;             -- the vector
      nb_row : in     Integer)            -- number of rows
   is
   begin
      Deallocate(vec);
      vec.nb_row := nb_row;
      vec.val := new Array1(1..nb_row);
   end Allocate;

   -------------------------------------------------------------------------
   -- Copy --
   ----------

   procedure Copy(
      vec_ori: in     Vector;             -- vector original
      vec_cop:    out Vector)             -- vector copy
   is
   begin
      Allocate(vec_cop, vec_ori.nb_row);
      for r in 1 .. vec_ori.nb_row loop
         vec_cop.val(r) := vec_ori.val(r);
      end loop;
   end Copy;

   -------------------------------------------------------------------------
   -- Deallocate --
   ----------------

   procedure Deallocate(
      vec    : in out Vector)             -- the vector
   is
   begin
      vec.nb_row := 0;
      Free(vec.val);
      vec.val := null;
   end Deallocate;

   -------------------------------------------------------------------------
   -- Dimension --
   ---------------

   function Dimension(
      vec    : in     Vector)             -- the vector
      return Integer                      -- number of rows
   is
   begin
      return vec.nb_row;
   end Dimension;

   -------------------------------------------------------------------------
   -- Dot_Product --
   -----------------

   function Dot_Product(
      vec1   : in     Vector;             -- the vector 1
      vec2   : in     Vector)             -- the vector 2
      return Float_Type                   -- dot product
   is
      dot_pro: Float_Type;         -- dot product
   begin
      dot_pro := 0.0;
      for r in 1 .. vec1.nb_row loop
         dot_pro := dot_pro + vec1.val(r) * vec2.val(r);
      end loop;
      return dot_pro;
   end Dot_Product;

   -------------------------------------------------------------------------
   -- Get --
   ---------

   function Get(
      vec    : in     Vector;             -- the vector
      row    : in     Integer)            -- row number
      return Float_Type                   -- value
   is
   begin
      return vec.val(row);
   end Get;

   -------------------------------------------------------------------------
   -- Initialise --
   ----------------

   procedure Initialise(
      vec    : in out Vector;             -- the vector
      val    : in     Float_Type)         -- value
   is
   begin
      for r in 1 .. vec.nb_row loop
         vec.val(r) := val;
      end loop;
   end Initialise;

   -------------------------------------------------------------------------
   -- Initialise_Random --
   -----------------------

   procedure Initialise_Random(
      vec    : in out Vector;             -- the vector
      gen    : in out Generator;          -- random numbers generator
      val_min: in     Float_Type;         -- value minimum
      val_max: in     Float_Type)         -- value maximum
   is
      val    : Float_Type;         -- value
   begin
      for r in 1 .. vec.nb_row loop
         val := val_min + Float_Type(Random(gen)) * (val_max - val_min);
         vec.val(r) := val;
      end loop;
   end Initialise_Random;

   -------------------------------------------------------------------------
   -- Norm --
   ----------

   function Norm(
      vec    : in     Vector)             -- the vector
      return Float_Type                   -- norm
   is
   begin
      return Sqrt(Dot_Product(vec, vec));
   end Norm;

   -------------------------------------------------------------------------
   -- Normalise --
   ---------------

   procedure Normalise(
      vec_ori: in     Vector;             -- vector original
      vec_nrm:    out Vector)             -- vector normalised
   is
      Epsilon: constant Float_Type := 1.0E-6;
      nrm    : Float_Type;         -- norm
   begin
      nrm := Norm(vec_ori);
      Allocate(vec_nrm, vec_ori.nb_row);
      if nrm < Epsilon then
         Initialise(vec_nrm, 0.0);
      else
         for r in 1 .. vec_ori.nb_row loop
            vec_nrm.val(r) := vec_ori.val(r) / nrm;
         end loop;
      end if;
   end Normalise;

   -------------------------------------------------------------------------
   -- Put --
   ---------

   procedure Put(
      fil    : in out File_Type;          -- the file
      vec    : in     Vector)             -- the vector
   is
   begin
      for r in 1 .. vec.nb_row loop
         Put(fil, vec.val(r), Exp=>0);
         New_Line(fil);
      end loop;
   end Put;

   -------------------------------------------------------------------------
   -- Set --
   ---------

   procedure Set(
      vec    : in out Vector;             -- the vector
      row    : in     Integer;            -- row number
      val    : in     Float_Type)         -- value
   is
   begin
      vec.val(row) := val;
   end Set;

   -------------------------------------------------------------------------
   -- Signs --
   -----------

   procedure Signs(
      vec_ori: in     Vector;             -- vector original
      vec_sgn:    out Vector)             -- vector of signs
   is
   begin
      Allocate(vec_sgn, vec_ori.nb_row);
      for r in 1 .. vec_ori.nb_row loop
         if vec_ori.val(r) > 0.0 then
            vec_sgn.val(r) := +1.0;
         else
            vec_sgn.val(r) := -1.0;
         end if;
      end loop;
   end Signs;

   -------------------------------------------------------------------------
   -- Subtract --
   --------------

   procedure Subtract(
      vec1   : in     Vector;             -- the vector 1
      vec2   : in     Vector;             -- the vector 2
      vec_sub:    out Vector)             -- vector subtract
   is
   begin
      Allocate(vec_sub, vec1.nb_row);
      for r in 1 .. vec1.nb_row loop
         vec_sub.val(r) := vec1.val(r) - vec2.val(r);
      end loop;
   end Subtract;

   -------------------------------------------------------------------------
   -- Sum --
   ---------

   function Sum(
      vec    : in     Vector)             -- the vector
      return Float_Type                   -- sum
   is
      s      : Float_Type;         -- sum
   begin
      s := 0.0;
      for r in 1 .. vec.nb_row loop
         s := s + vec.val(r);
      end loop;
      return s;
   end Sum;

   -------------------------------------------------------------------------

end Vectors;
