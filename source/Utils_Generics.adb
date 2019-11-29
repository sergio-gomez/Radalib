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

with Ada.Unchecked_Deallocation;

package body Utils_Generics is

  --------------------
  -- Alloc_1D_Array --
  --------------------

  function Alloc_1D_Array(First, Last: in Integer) return PItems is
  begin
    return new Items(First..Last);
  end Alloc_1D_Array;

  ----------------------------
  -- Alloc_2D_Squared_Array --
  ----------------------------

  function Alloc_2D_Squared_Array(First, Last: in Integer) return PItemss is
  begin
    return new Itemss(First..Last, First..Last);
  end Alloc_2D_Squared_Array;

  --------------------------------
  -- Alloc_2D_Rectangular_Array --
  --------------------------------

  function Alloc_2D_Rectangular_Array(First1, Last1, First2, Last2: in Integer) return PItemss is
  begin
    return new Itemss(First1..Last1, First2..Last2);
  end Alloc_2D_Rectangular_Array;

  -----------------------------------
  -- Alloc_Irregular_Squared_Array --
  -----------------------------------

  function Alloc_Irregular_Squared_Array(First, Last: in Integer) return PPsItems is
    P: PPsItems;
  begin
    P := new PsItems(First..Last);
    for I in P'Range loop
      P(I) := new Items(First..Last);
    end loop;
    return P;
  end Alloc_Irregular_Squared_Array;

  ---------------------------------------
  -- Alloc_Irregular_Rectangular_Array --
  ---------------------------------------

  function Alloc_Irregular_Rectangular_Array(First1, Last1, First2, Last2: in Integer) return PPsItems is
    P: PPsItems;
  begin
    P := new PsItems(First1..Last1);
    for I in P'Range loop
      P(I) := new Items(First2..Last2);
    end loop;
    return P;
  end Alloc_Irregular_Rectangular_Array;

  ----------------------------------
  -- Alloc_Upper_Triangular_Array --
  ----------------------------------

  function Alloc_Upper_Triangular_Array(First, Last: in Integer) return PPsItems is
    P: PPsItems;
  begin
    P := new PsItems(First..Last);
    for I in P'Range loop
      P(I) := new Items(I..Last);
    end loop;
    return P;
  end Alloc_Upper_Triangular_Array;

  ----------------------------------
  -- Alloc_Lower_Triangular_Array --
  ----------------------------------

  function Alloc_Lower_Triangular_Array(First, Last: in Integer) return PPsItems is
    P: PPsItems;
  begin
    P := new PsItems(First..Last);
    for I in P'Range loop
      P(I) := new Items(First..I);
    end loop;
    return P;
  end Alloc_Lower_Triangular_Array;

  -------------------
  -- Free_1D_Array --
  -------------------

  procedure Free_1D_Array(P: in out PItems) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Items, PItems);
  begin
    if P /= null then
      Dispose(P);
      P := null;
    end if;
  end Free_1D_Array;

  -------------------
  -- Free_2D_Array --
  -------------------

  procedure Free_2D_Array(P: in out PItemss) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Itemss, PItemss);
  begin
    if P /= null then
      Dispose(P);
      P := null;
    end if;
  end Free_2D_Array;

  --------------------------
  -- Free_2D_Irregular_Array --
  --------------------------

  procedure Free_2D_Irregular_Array(P: in out PPsItems) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Items, PItems);
    procedure Dispose is new Ada.Unchecked_Deallocation(PsItems, PPsItems);
  begin
    if P /= null then
      for I in P'Range loop
        if P(I) /= null then
          Dispose(P(I));
          P(I) := null;
        end if;
      end loop;
      Dispose(P);
      P := null;
    end if;
  end Free_2D_Irregular_Array;

end Utils_Generics;
