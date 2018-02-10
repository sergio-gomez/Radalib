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


-- @filename Minheaps_String.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 05/04/2012
-- @revision 05/04/2012
-- @brief Instantiation of Minheaps to String Items

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;
with Minheaps;

package Minheaps_String is
  new Minheaps(Item => Ustring);
