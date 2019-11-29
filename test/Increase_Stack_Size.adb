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


-- @filename Increase_Stack_Size.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 25/08/2009
-- @revision 28/12/2017
-- @brief Test of the way to Increase the Stack Size

with Ada.Text_IO; use Ada.Text_IO;
with Arrays_Float; use Arrays_Float;
with Arrays_Utils_Float; use Arrays_Utils_Float;
with Utils; use Utils;

procedure Increase_Stack_Size is

  Wh: PFloatss;
  N: Natural;

begin
  N := 5_000;
  Put_Line("Allocating a " & I2S(N) & "x" & I2S(N) & " matrix of floats in the heap, size " & I2S(4 * N * N) & " Bytes");
  Wh := Alloc(1, N, 0.0);
  Put_Line("  OK");

  Wh.all := (others => (others => 1.0));

  Put_Line("Tranposing the matrix, using the stack to hold the matrix as parameter");
  Wh.all := Transpose(Wh.all);
  Put_Line("  OK");

  Free(Wh);
end Increase_Stack_Size;
