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


-- @filename Data_IO_Integer.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 09/05/2013
-- @revision 16/05/2013
-- @brief Input and Output of Integer Data in List or Matrix form

with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Data_IO;

package Data_IO_Integer is

  procedure Get_I(Ft: in File_Type; X: out Integer);

  package Data_IO_I is new Data_IO(Item     => Integer,
                                   Itemss   => Integerss,
                                   PItemss  => PIntegerss,
                                   No_Item  => Integer'First,
                                   Alloc    => Alloc,
                                   Is_Item  => Is_Integer,
                                   Get_Item => Get_I);

  use Data_IO_I;

  -- Purpose : Get Integer Data from a File in List of Matrix form
  -- Note    : In list form, no symmetrization is performed, and repeated elements are overwritten
  -- Note    : In matrix form, the names may appear in first row, first column, both, or none of them
  -- Note    : The No_Value parameter should preferably be a value not in the Data File
  --
  -- Fn      : The File Name
  -- Data    : The Data
  -- Col_Name: The Columns Names
  -- Row_Name: The Rows Names
  -- No_Value: The No Data flag Value
  -- Form    : The Data Form
  -- raises  : Data_IO_Error
  procedure Get_Data(Fn: in String; Data: out PIntegerss; Col_Name, Row_Name: out PUstrings; No_Value: in Integer := Integer'First; Form: in Data_Form := Auto) renames Generic_Get_Data;

  -- Purpose : Get Integer Data from a File in List of Matrix form
  -- Note    : In list form, no symmetrization is performed, and repeated elements are overwritten
  -- Note    : In matrix form, the names may appear in first row, first column, both, or none of them
  -- Note    : The No_Value parameter should preferably be a value not in the Data File
  --
  -- Ft      : The File Type
  -- Data    : The Data
  -- Col_Name: The Columns Names
  -- Row_Name: The Rows Names
  -- No_Value: The No Data flag Value
  -- Form    : The Data Form
  -- raises  : Data_IO_Error
  procedure Get_Data(Ft: in out File_Type; Data: out PIntegerss; Col_Name, Row_Name: out PUstrings; No_Value: in Integer := Integer'First; Form: in Data_Form := Auto) renames Generic_Get_Data;

end Data_IO_Integer;
