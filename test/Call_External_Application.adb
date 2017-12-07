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


-- @filename Call_External_Application.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 30/08/2006
-- @revision 13/10/2016
-- @brief Test of the way to Call an External Application

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Os_Lib; use GNAT.Os_Lib;

with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Call_External_Application is

  Success: Boolean := False;
  -- Windows
  Prog_Windows        : constant String := "C:\Windows\System32\cmd.exe";
  Title_Windows       : constant String_Access := new String'("cmd -> hostname]");
  Param1_Windows      : constant String_Access := new String'("/C C:\Windows\System32\hostname.exe");
  Params_List_Windows : constant String_List := (1 => Title_Windows, 2 => Param1_Windows);
  -- Others
  Prog_Others         : constant String := "/bin/hostname";
  Param1_Others       : constant String_Access := new String'("-s");
  Params_List_Others  : constant String_List := (1 => Param1_Others);

begin
  if File_Exists(Prog_Windows) then
    Put_Line("Calling: " & Title_Windows.all);
    Put_Line("---");
    Spawn(Prog_Windows, Params_List_Windows, Success);
    Put_Line("---");
  elsif File_Exists(Prog_Others) then
    Put_Line("Calling: " & Prog_Others & " " & Param1_Others.all);
    Put_Line("---");
    Spawn(Prog_Others, Params_List_Others, Success);
    Put_Line("---");
  else
    Put_Line("External application not found!");
  end if;
  if Success then
    Put_Line("Done!");
  end if;
end Call_External_Application;
