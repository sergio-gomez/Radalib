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


-- @filename Data_IO_Integer.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 09/05/2013
-- @revision 14/05/2013
-- @brief Input and Output of Integer Data in List or Matrix form

with Utils.IO; use Utils.IO;

package body Data_IO_Integer is

  -----------
  -- Get_I --
  -----------

  procedure Get_I(Ft: in File_Type; X: out Integer) is
  begin
    Get_Integer(Ft, X);
  end Get_I;

end Data_IO_Integer;
