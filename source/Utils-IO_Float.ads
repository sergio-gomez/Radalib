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


-- @filename Utils-IO_Float.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 15/08/2006
-- @revision 05/04/2012
-- @brief Instantiation of Float_IO to Float

with Ada.Text_IO;

package Utils.IO_Float is
  new Ada.Text_IO.Float_IO(Float);
