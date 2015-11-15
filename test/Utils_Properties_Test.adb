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


-- @filename Utils_Properties_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 21/10/2009
-- @revision 26/10/2014
-- @brief Test of use of Properties files in Utils package

with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Utils_Properties_Test is
  Fn_In : constant String := "utils_properties_test.txt";
  Fn_Out: constant String := "utils_properties_test-out.txt";
  Props: Properties;
  Names, Values: PUstrings;
  B: Boolean;
  I: Integer;
  F: Float;
  D: Double;
begin
  Put_Line(Fn_In & " -> " & Fn_Out);
  New_Line;

  Load_Properties(Fn_In, Props);

  Put_Line("Number of properties: " & I2S(Number_Of_Properties(Props)));
  Names := Get_Property_Names(Props);
  Values := Get_Property_Values(Props);
  for I in Names'Range loop
    Put_Line(U2S(Names(I)) & " -> " & U2S(Values(I)));
  end loop;
  Free(Names);
  Free(Values);
  New_Line;

  B := Get_Property(Props, "Bool01");
  Put_Line("Bool01 = " & Capitalize(Boolean'Image(B)));

  D := Get_Property(Props, "Double02", 0.0);
  Put_Line("Double02 = " & D2Se0(D, Aft => 6));

  D := Get_Property(Props, "Double07", 0.0);
  Put_Line("Double07 = " & D2Se0(D, Aft => 6));

  I := Get_Property(Props, "Int01");
  Put_Line("Int01 = " & I2S(I));
  New_Line;

  Remove_Property(Props, "Int02");
  -- modify property
  Set_Property(Props, " Int01  ", Htab & "  111 " & Htab);
  Set_Property(Props, "Bool01", True);
  -- add property
  I := 555;
  Set_Property(Props, "Int05", I);
  F := 0.123;
  Set_Property(Props, "Float01", F, Aft => 4, Exp => 0);
  -- case-sensitivity
  I := 555555;
  Set_Property(Props, "INT05", I);

  Put_Line("Number of properties: " & I2S(Number_Of_Properties(Props)));
  Names := Get_Property_Names(Props);
  Values := Get_Property_Values(Props);
  for I in Names'Range loop
    Put_Line(U2S(Names(I)) & " -> " & U2S(Values(I)));
  end loop;
  Free(Names);
  Free(Values);
  New_Line;

  Save_Properties(Fn_Out, Props);
end Utils_Properties_Test;
