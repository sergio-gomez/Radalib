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


-- @filename Trees_Double_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 10/05/2013
-- @brief Input and Output of Trees of Doubles

with Utils.IO; use Utils.IO;

package body Trees_Double_IO is

  ----------
  -- To_S --
  ----------

  function To_S(D: in Double; Aft: in Field := Default_Double_Aft; Format: in Tree_Format) return String is
  begin
    return D2Sea(D, Aft => Aft);
  end To_S;

  -----------
  -- Get_D --
  -----------

  procedure Get_D(D: out Double; Format: in Tree_Format) is
  begin
    Get_Double(D);
  end Get_D;

end Trees_Double_IO;
