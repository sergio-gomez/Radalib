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


-- @filename Trees_Integer_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 05/04/2012
-- @revision 30/01/2016
-- @brief Input and Output of Trees of Integers

with Utils.IO; use Utils.IO;

package body Trees_Integer_IO is

  ----------
  -- To_S --
  ----------

  function To_S(I: in Integer; Width: in Natural := Default_Integer_Width; Format: in Tree_Format) return String is
  begin
    case Format is
      when Json_Tree =>
        return """name"": """ & I2S(I) & """";
      when others =>
        if Width = 0 then
          return I2S(I);
        else
          return Right_Justify(I2S(I), Width => Width, Pad => ' ');
        end if;
    end case;
  end To_S;

  -----------
  -- Get_I --
  -----------

  procedure Get_I(I: out Integer; Format: in Tree_Format) is
    Key, Value: Ustring;
    Found: Boolean := False;
  begin
    case Format is
      when Json_Tree =>
        loop
          Get_Pair(Key, Value, ':');
          if U2S(To_Lowercase(Key)) = "name" then
            I := U2I(Value);
            return;
          end if;
          Comments_Skip;
          if not Separator_Skip(',', Strict => True) then
            raise Trees_IO_I.Tree_IO_Error with "Name field not found";
          end if;
        end loop;
      when others =>
        Get_Integer(I);
    end case;
  end Get_I;

end Trees_Integer_IO;
