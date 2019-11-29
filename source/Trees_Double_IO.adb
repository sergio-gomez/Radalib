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


-- @filename Trees_Double_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 30/01/2016
-- @brief Input and Output of Trees of Doubles

with Utils.IO; use Utils.IO;

package body Trees_Double_IO is

  ----------
  -- To_S --
  ----------

  function To_S(D: in Double; Aft: in Field := Default_Double_Aft; Format: in Tree_Format) return String is
  begin
    case Format is
      when Json_Tree =>
        return """name"": """ & D2Sea(D, Aft => Aft) & """";
      when others =>
        return D2Sea(D, Aft => Aft);
    end case;
  end To_S;

  -----------
  -- Get_D --
  -----------

  procedure Get_D(D: out Double; Format: in Tree_Format) is
    Key, Value: Ustring;
    Found: Boolean := False;
  begin
    case Format is
      when Json_Tree =>
        loop
          Get_Pair(Key, Value, ':');
          if U2S(To_Lowercase(Key)) = "name" then
            D := U2D(Value);
            return;
          end if;
          Comments_Skip;
          if not Separator_Skip(',', Strict => True) then
            raise Trees_IO_D.Tree_IO_Error with "Name field not found";
          end if;
        end loop;
      when others =>
        Get_Double(D);
    end case;
  end Get_D;

end Trees_Double_IO;
