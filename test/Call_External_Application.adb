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


-- @filename Call_External_Application.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 30/08/2006
-- @revision 28/08/2009
-- @brief Test of the way to Call an External Application

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Os_Lib; use GNAT.Os_Lib;

with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Call_External_Application is
  Name  : constant String := "c:\usrlocal\pstools\pslist.exe";
  Title : constant String_Access := new String'("PsList");
  Param1: constant String_Access := new String'("-m");
  Params_List: constant String_List := (1 => Title, 2 => Param1);
begin
  Put_Line("Calling " & Title.all & ":");
  Put("  " & Name);
  for I in 2..Params_List'Last loop
    Put(" " & Params_List(I).all);
  end loop;
  New_Line;

  if File_Exists(Name) then
    if Spawn(Name, Params_List) = 0 then
      null;
    end if;
  else
    Put_Line("File not found!");
  end if;
end Call_External_Application;
