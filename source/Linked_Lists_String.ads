-- Radalib, Copyright (c) 2021 by
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


-- @filename Linked_Lists_String.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 05/04/2012
-- @revision 05/04/2012
-- @brief Instantiation of Linked Lists to String Items

with Utils; use Utils;
with Linked_Lists;

package Linked_Lists_String is
  new Linked_Lists(Item => Ustring);
