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


-- @filename Data_IO_Float.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 09/05/2013
-- @revision 16/05/2013
-- @brief Input and Output of Float Data in List or Matrix form

with Utils.IO; use Utils.IO;

package body Data_IO_Float is

  -----------
  -- Get_F --
  -----------

  procedure Get_F(Ft: in File_Type; X: out Float) is
  begin
    Get_Float(Ft, X);
  end Get_F;

  --------------
  -- Get_Data --
  --------------

  procedure Get_Data(Fn: in String; Data: out PFloatss; Col_Name, Row_Name: out PUstrings; Decimals: out Natural; No_Value: in Float := Float'First; Form: in Data_Form := Auto) is
    Ft: File_Type;
  begin
    Open(Ft, In_File, Fn);
    Get_Data(Ft, Data, Col_Name, Row_Name, Decimals, No_Value, Form);
    Close(Ft);
  end Get_Data;

  --------------
  -- Get_Data --
  --------------

  procedure Get_Data(Ft: in out File_Type; Data: out PFloatss; Col_Name, Row_Name: out PUstrings; Decimals: out Natural; No_Value: in Float := Float'First; Form: in Data_Form := Auto) is

    procedure Get_F_And_Decimals(Ft: in File_Type; X: out Float) is
      W: Word_Access;
      Dec: Natural;
      I: Positive;
    begin
      Get_Word(Ft, W);
      X := S2F(W.all);
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
    end Get_F_And_Decimals;

    package Data_IO_F_Dec is new Data_IO(Item     => Float,
                                         Itemss   => Floatss,
                                         PItemss  => PFloatss,
                                         No_Item  => Float'First,
                                         Alloc    => Alloc,
                                         Is_Item  => Is_Real,
                                         Get_Item => Get_F_And_Decimals);

    function To_Data_Form(Form: in Data_Form) return Data_IO_F_Dec.Data_Form is
    begin
      case Form is
        when Auto =>
          return Data_IO_F_Dec.Auto;
        when Matrix_Form =>
          return Data_IO_F_Dec.Matrix_Form;
        when List_Form =>
          return Data_IO_F_Dec.List_Form;
      end case;
    end To_Data_Form;

  begin
    Decimals := 0;
    Data_IO_F_Dec.Generic_Get_Data(Ft, Data, Col_Name, Row_Name, No_Value, To_Data_Form(Form));
  end Get_Data;

end Data_IO_Float;
