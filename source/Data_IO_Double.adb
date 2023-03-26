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


-- @filename Data_IO_Double.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 09/05/2013
-- @revision 16/05/2013
-- @brief Input and Output of Double Data in List or Matrix form

with Utils.IO; use Utils.IO;

package body Data_IO_Double is

  -----------
  -- Get_D --
  -----------

  procedure Get_D(Ft: in File_Type; X: out Double) is
  begin
    Get_Double(Ft, X);
  end Get_D;

  --------------
  -- Get_Data --
  --------------

  procedure Get_Data(Fn: in String; Data: out PDoubless; Col_Name, Row_Name: out PUstrings; Decimals: out Natural; No_Value: in Double := Double'First; Form: in Data_Form := Auto) is
    Ft: File_Type;
  begin
    Open(Ft, In_File, Fn);
    Get_Data(Ft, Data, Col_Name, Row_Name, Decimals, No_Value, Form);
    Close(Ft);
  end Get_Data;

  --------------
  -- Get_Data --
  --------------

  procedure Get_Data(Ft: in out File_Type; Data: out PDoubless; Col_Name, Row_Name: out PUstrings; Decimals: out Natural; No_Value: in Double := Double'First; Form: in Data_Form := Auto) is

    procedure Get_D_And_Decimals(Ft: in File_Type; X: out Double) is
      W: Word_Access;
      Dec: Natural;
      I: Positive;
    begin
      Get_Word(Ft, W);
      X := S2D(W.all);
      I := 1;
      while I <= W'Length loop
        if W(I) = '.' then
          I := I + 1;
          exit;
        end if;
        I := I + 1;
      end loop;
      Dec := 0;
      while I <= W'Length and then W(I) in '0'..'9' loop
        Dec := Dec + 1;
        I := I + 1;
      end loop;
      if Dec > Decimals then
        Decimals := Dec;
      end if;
      Free_Word(W);
    end Get_D_And_Decimals;

    package Data_IO_D_Dec is new Data_IO(Item     => Double,
                                         Itemss   => Doubless,
                                         PItemss  => PDoubless,
                                         No_Item  => Double'First,
                                         Alloc    => Alloc,
                                         Is_Item  => Is_Real,
                                         Get_Item => Get_D_And_Decimals);

    function To_Data_Form(Form: in Data_Form) return Data_IO_D_Dec.Data_Form is
    begin
      case Form is
        when Auto =>
          return Data_IO_D_Dec.Auto;
        when Matrix_Form =>
          return Data_IO_D_Dec.Matrix_Form;
        when List_Form =>
          return Data_IO_D_Dec.List_Form;
      end case;
    end To_Data_Form;

  begin
    Decimals := 0;
    Data_IO_D_Dec.Generic_Get_Data(Ft, Data, Col_Name, Row_Name, No_Value, To_Data_Form(Form));
  end Get_Data;

end Data_IO_Double;
