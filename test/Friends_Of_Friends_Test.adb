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


-- @filename Friends_Of_Friends_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 28/12/2017
-- @brief Test of Friends of Friends Clustering

with Ada.Text_IO; use Ada.Text_IO;
with Friends_Of_Friends_Utils; use Friends_Of_Friends_Utils;

procedure Friends_Of_Friends_Test is

  use These_Data_Clusters;

  Data: Pdata_Array;

begin

  New_Line;
  Put_Line("==============================");
  Put_Line("=== fof_cubes_isolated.txt ===");
  Put_Line("==============================");
  New_Line;
  Read_Data("test-fof_cubes_isolated.txt", 200, Data);
  Make_Fof(Data, 0.2);
  Make_Fof(Data, 0.5);
  Make_Fof(Data, 0.99);
  Make_Fof(Data, 1.01);
  Make_Fof(Data, 1.25);
  Make_Fof(Data, 10.0);
  Free(Data);

  New_Line;
  Put_Line("===============================");
  Put_Line("=== fof_cubes_connected.txt ===");
  Put_Line("===============================");
  New_Line;
  Read_Data("test-fof_cubes_connected.txt", 208, Data);
  Make_Fof(Data, 0.2);
  Make_Fof(Data, 0.5);
  Make_Fof(Data, 0.99);
  Make_Fof(Data, 1.01);
  Make_Fof(Data, 1.25);
  Make_Fof(Data, 10.0);
  Free(Data);

end Friends_Of_Friends_Test;
