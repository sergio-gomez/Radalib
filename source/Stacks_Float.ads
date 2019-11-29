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


-- @filename Stacks_Float.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 27/07/2009
-- @revision 27/07/2009
-- @brief Instantiation of Stacks to Float Items

with Stacks;

package Stacks_Float is
  new Stacks(Item => Float);
