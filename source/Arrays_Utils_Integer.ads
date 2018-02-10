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


-- @filename Arrays_Utils_Integer.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/07/2009
-- @revision 18/12/2014
-- @brief Instantiation of Arrays_Utils to Integer

with Utils; use Utils;
with Arrays_Utils;

package Arrays_Utils_Integer is
  new Arrays_Utils(Integer, Integers, Integerss, PIntegers, PIntegerss, PsIntegers, PPsIntegers);
