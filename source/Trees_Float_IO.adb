-- Radalib, Copyright (c) 2016 by
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


-- @filename Trees_Float_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 30/01/2016
-- @brief Input and Output of Trees of Floats

with Utils.IO; use Utils.IO;

package body Trees_Float_IO is

  ----------
  -- To_S --
  ----------

  function To_S(F: in Float; Aft: in Field := Default_Float_Aft; Format: in Tree_Format) return String is
  begin
    case Format is
      when Json_Tree =>
        return """name"": """ & F2Sea(F, Aft => Aft) & """";
      when others =>
        return F2Sea(F, Aft => Aft);
    end case;
  end To_S;

  -----------
  -- Get_F --
  -----------

  procedure Get_F(F: out Float; Format: in Tree_Format) is
    Key, Value: Ustring;
    Found: Boolean := False;
  begin
    case Format is
      when Json_Tree =>
        loop
          Get_Pair(Key, Value, ':');
          if U2S(To_Lowercase(Key)) = "name" then
            F := U2F(Value);
            return;
          end if;
          Comments_Skip;
          if not Separator_Skip(',', Strict => True) then
            raise Trees_IO_F.Tree_IO_Error with "Name field not found";
          end if;
        end loop;
      when others =>
        Get_Float(F);
    end case;
  end Get_F;

end Trees_Float_IO;
