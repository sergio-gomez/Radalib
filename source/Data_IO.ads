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


-- @filename Data_IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/05/2013
-- @revision 18/01/2016
-- @brief Input and Output of Data in List or Matrix form

with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;

generic
  type Item is private;
  type Itemss is array(Integer range <>, Integer range <>) of Item;
  type PItemss is access Itemss;
  No_Item: Item;
  with function Alloc(First1, Last1, First2, Last2: in Integer) return PItemss is <>;
  with function Is_Item(U: in Ustring) return Boolean is <>;
  with procedure Get_Item(Ft: in File_Type; X: out Item) is <>;
package Data_IO is

  type Data_Form is (Auto, Matrix_Form, List_Form);

  Data_IO_Error: exception;

  -- Purpose : Get Data from a File in List or Matrix form
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
  procedure Generic_Get_Data(Fn: in String; Data: out PItemss; Col_Name, Row_Name: out PUstrings; No_Value: in Item := No_Item; Form: in Data_Form := Auto);

  -- Purpose : Get Data from a File in List or Matrix form
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
  procedure Generic_Get_Data(Ft: in out File_Type; Data: out PItemss; Col_Name, Row_Name: out PUstrings; No_Value: in Item := No_Item; Form: in Data_Form := Auto);

end Data_IO;
