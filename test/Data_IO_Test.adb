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


-- @filename Data_IO_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 09/05/2013
-- @revision 28/12/2017
-- @brief Test of Data_IO packages

with Ada.Text_Io; use Ada.Text_Io;
with Utils; use Utils;
with Data_IO_Integer; use Data_IO_Integer;
with Data_IO_Double; use Data_IO_Double;

procedure Data_IO_Test is

  use Data_IO_D;

  procedure Put_Data(Data: in PIntegerss; Col_Name, Row_Name: in PUstrings; No_Value: in Integer) is
  begin
    if Col_Name /= null then
      if Row_Name /= null then
        Put(" ");
      end if;
      for J in Col_Name'Range loop
        Put(HTab & U2S(Col_Name(J)));
      end loop;
      New_Line;
    end if;
    for I in Data'Range(1) loop
      if Row_Name /= null then
        Put(U2S(Row_Name(I)));
      end if;
      for J in Data'Range(2) loop
        if Data(I, J) /= No_Value then
          Put(Htab & I2S(Data(I, J)));
        else
          Put(Htab & "N/A");
        end if;
      end loop;
      New_Line;
    end loop;
  end Put_Data;

  procedure Put_Data(Data: in PDoubless; Col_Name, Row_Name: in PUstrings; No_Value: in Double; Aft: in Field := Default_Double_Aft) is
  begin
    if Col_Name /= null then
      if Row_Name /= null then
        Put(" ");
      end if;
      for J in Col_Name'Range loop
        Put(HTab & U2S(Col_Name(J)));
      end loop;
      New_Line;
    end if;
    for I in Data'Range(1) loop
      if Row_Name /= null then
        Put(U2S(Row_Name(I)));
      end if;
      for J in Data'Range(2) loop
        if Data(I, J) /= No_Value then
          Put(Htab & D2Se0(Data(I, J), Aft => Aft));
        else
          Put(Htab & "N/A");
        end if;
      end loop;
      New_Line;
    end loop;
  end Put_Data;

  Fn_01: constant String := "test-data_toy_01.txt";
  Fn_02: constant String := "test-data_toy_02.txt";
  Fn_03: constant String := "test-data_toy_03.txt";

  Data_I: PIntegerss;
  Data_D: PDoubless;
  Col_Name, Row_Name: PUstrings;
  Decimals: Natural;
  No_I: Integer;
  No_D: Double;

begin
  No_I := 0;
  Get_Data(Fn_01, Data_I, Col_Name, Row_Name, No_I, Data_IO_I.Auto);
  Put_Data(Data_I, Col_Name, Row_Name, No_I);
  Free(Data_I);
  Free(Col_Name);
  Free(Row_Name);
  New_Line;

  No_D := 0.0;
  Get_Data(Fn_01, Data_D, Col_Name, Row_Name, No_D, Matrix_Form);
  Put_Data(Data_D, Col_Name, Row_Name, No_D, 2);
  Free(Data_D);
  Free(Col_Name);
  Free(Row_Name);
  New_Line;

  No_D := 0.0;
  Get_Data(Fn_02, Data_D, Col_Name, Row_Name, Decimals, No_D, List_Form);
  Put_Line("Precision : " & I2S(Decimals));
  Put_Data(Data_D, Col_Name, Row_Name, No_D, Decimals);
  Free(Data_D);
  Free(Col_Name);
  Free(Row_Name);
  New_Line;

  No_D := 0.0;
  Get_Data(Fn_03, Data_D, Col_Name, Row_Name, Decimals, No_D);
  Put_Line("Precision : " & I2S(Decimals));
  Put_Data(Data_D, Col_Name, Row_Name, No_D, Decimals);
  Free(Data_D);
  Free(Col_Name);
  Free(Row_Name);
  New_Line;
end Data_IO_Test;
