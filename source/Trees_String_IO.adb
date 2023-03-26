-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


-- @filename Trees_String_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 30/01/2016
-- @brief Input and Output of Trees of Strings

with Utils.IO; use Utils.IO;

package body Trees_String_IO is

  ----------
  -- To_S --
  ----------

  function To_S(Us: in Ustring; Width: in Natural := Default_Integer_Width; Format: in Tree_Format) return String is
  begin
    case Format is
      when Json_Tree =>
        return """name"": """ & U2S(Us) & """";
      when others =>
        if Width = 0 then
          return U2S(Us);
        else
          return Right_Justify(U2S(Us), Width => Width, Pad => ' ');
        end if;
    end case;
  end To_S;

  -----------
  -- Get_S --
  -----------

  procedure Get_S(Us: out Ustring; Format: in Tree_Format) is
    Key, Value: Ustring;
    Found: Boolean := False;
  begin
    case Format is
      when Json_Tree =>
        loop
          Get_Pair(Key, Value, ':');
          if U2S(To_Lowercase(Key)) = "name" then
            Us := Value;
            return;
          end if;
          Comments_Skip;
          if not Separator_Skip(',', Strict => True) then
            raise Trees_IO_S.Tree_IO_Error with "Name field not found";
          end if;
        end loop;
      when others =>
        Get_Word(Us);
    end case;
  end Get_S;

end Trees_String_IO;
