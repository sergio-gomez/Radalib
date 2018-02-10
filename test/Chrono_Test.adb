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


-- @filename Chrono_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/12/2004
-- @revision 28/12/2017
-- @brief Test of the Chrono utils

with Ada.Text_IO; use Ada.Text_IO;
with Chrono_Utils; use Chrono_Utils;
with Utils.IO; use Utils.IO;

procedure Chrono_Test is

  Chrono: Chronometer;
  Fn: constant String := "test-chrono.txt";

begin

  Start(Chrono, 0.1);
  delay 1.5;
  Put_Elapsed(Chrono); New_Line;
  delay 1.5;
  Put_Elapsed(Chrono); New_Line;
  delay 1.5;
  Put_Elapsed(Chrono); New_Line;
  Stop(Chrono);
  delay 1.5;
  Put("Total: "); Put_Elapsed(Chrono); New_Line;
  New_Line;

  delay 1.0;

  Start(Chrono);
  delay 1.5;
  Put_Elapsed(Chrono); New_Line;
  delay 1.5;
  Put_Elapsed(Chrono); New_Line;
  delay 1.5;
  Stop(Chrono);
  Put_Elapsed(Chrono); New_Line;
  delay 1.5;
  Put("Total: "); Put_Elapsed(Chrono); New_Line;
  New_Line;

  delay 1.0;

  Start(Chrono, 0.1, "<>");
  delay 1.5;
  Put_Elapsed(Chrono, 4); New_Line;
  delay 1.5;
  Put_Elapsed(Chrono, 4); New_Line;
  delay 1.5;
  Stop(Chrono);
  Put_Elapsed(Chrono, 4); New_Line;
  delay 1.5;
  Put("Total: "); Put_Duration(Elapsed(Chrono), 4); New_Line;
  New_Line;

  Put("Accumulated: "); Put_Accumulated(Chrono, 2); New_Line;
  New_Line;
  Reset(Chrono);

  Start(Chrono, 0.1, "");
  delay 1.5;
  Put_Elapsed(Chrono, 2); New_Line;
  delay 1.5;
  Put_Elapsed(Chrono, 2); New_Line;
  delay 1.5;
  Stop(Chrono);
  Put_Elapsed(Chrono, 2); New_Line;
  delay 1.5;
  Put("Total: "); Put_Elapsed(Chrono, 2); New_Line;
  New_Line;

  Start(Chrono, 1.0);
  delay 65.5;
  Stop(Chrono); New_Line;
  Put("Total: "); Put_Elapsed(Chrono, 2); New_Line;
  New_Line;

  Put("Accumulated: "); Put_Accumulated(Chrono, 2); New_Line;

  Delete_File(Fn);
  Put_String_Line(Fn, "Accumulated: ");
  Put_Duration(Fn, Accumulated(Chrono), 2);

end Chrono_Test;
