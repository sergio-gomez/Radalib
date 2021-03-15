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


-- @filename Statistics_Double.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/11/2007
-- @revision 08/11/2007
-- @brief Instantiation of Statistics to Double Arrays

with Utils; use Utils;
with Statistics;

package Statistics_Double is
  new Statistics(Num => Double, Nums => Doubles, PNums => PDoubles);
