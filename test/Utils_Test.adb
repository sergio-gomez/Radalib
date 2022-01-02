-- Radalib, Copyright (c) 2022 by
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


-- @filename Utils_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/05/2005
-- @revision 07/09/2021
-- @brief Test of Utils package

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Utils.IO_Integer; use Utils.IO_Integer;
with Utils.IO_Float; use Utils.IO_Float;

procedure Utils_Test is
  Separators_Json: constant Characters := ('{', '}', '[', ']', ',', ':');
  Fn: constant String := "test-utils.txt";
  Fn_In: String := "..\test\test-utils.txt";
  Ft: File_Type;
  W: Word_Access;
  I: Integer;
  X: Float;
  D: Double;
  Us, Key, Value: Ustring;
begin
  Fn_In := Argument(1);
  Put_Line("Current Date and Time:  " & To_String(Clock));
  New_Line;

  Put_Line("Integer: " & I2S(Integer'First) & " to " & I2S(Integer'Last));
  Put_Line("Longint: " & L2S(Longint'First) & " to " & L2S(Longint'Last));
  New_Line;

  Put_Line("Left and Right Justify");
  Put_Line("<" & Left_Justify("Left", 2) & ">");
  Put_Line("<" & Left_Justify("Left", 6) & ">");
  Put_Line("<" & Left_Justify("  Left    ", 6, '*') & ">");
  Put_Line("<" & Right_Justify("Right", 0) & ">");
  Put_Line("<" & Right_Justify("Right", 10) & ">");
  Put_Line("<" & Right_Justify("  Right  ", 10, '*') & ">");
  New_Line;

  Put_Line("Conversion to String");
  Put_Line("<" & I2S(-255) & ">");
  Put_Line("<" & I2S(255, Base => 2) & ">");
  Put_Line("<" & To_String(F => 0.000001234, Exp => 2) & ">");
  Put_Line("<" & To_String(F => -1.0, Aft => 2) & ">");
  Put_Line("<" & To_String(D => 1234.5678, Aft => 6, Exp => 2) & ">");
  Put_Line("<" & To_String_Exp0(F => 333.7000, Aft => 4) & ">");
  Put_Line("<" & To_String_Short(F => 333.7000, Aft => 4) & ">");
  Put_Line("<" & To_String_Short(F => 333.7000, Aft => 0) & ">");
  Put_Line("<" & To_String_Exp_Auto(F => 123.4000e+15, Aft => 4) & ">");
  Put_Line("<" & To_String_Exp_Auto(F => 123.4000, Aft => 4) & ">");
  Put_Line("<" & To_String_Exp_Auto(F => 12.34000, Aft => 4) & ">");
  Put_Line("<" & To_String_Exp_Auto(F => 12.34000, Aft => 4, Short => True) & ">");
  Put_Line("<" & I2S(Character'Pos(Character'Val(1))) & ">");
  for Aft in 0..15 loop
    Put_Line("Aft = " & I2S(Aft));
    Put_Line("  <" & To_String_Exp_Auto(F => 123456789.0e5, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(F => 9876543.21, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(F => 123456.789, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(F => 987.654321, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(F => 0.123456789, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(F => 0.987654321e-2, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(F => 0.123456789e-4, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(D => 1234567890123.98, Aft => Aft) & ">");
    Put_Line("  <" & To_String_Exp_Auto(D => 9999999999999.99, Aft => Aft) & ">");
  end loop;
  New_Line;

  Put_Line("Floor, Round and Ceiling of Float");
  X := -2.0;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := -1.8;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := -1.5;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := -1.2;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 1.2;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 1.5;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 1.8;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 2.0;
  Put_Line(F2Se0(X, Aft => 2) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 0.999;
  Put_Line(F2Se0(X, Aft => 6) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 0.9999;
  Put_Line(F2Se0(X, Aft => 6) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 0.99999;
  Put_Line(F2Se0(X, Aft => 6) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 0.999999;
  Put_Line(F2Se0(X, Aft => 6) & ":  " & F2Se0(Floor(X), Aft => 2) & "  " & I2S(Integer(X)) & "  " & F2Se0(Round(X), Aft => 2) & "  " & F2Se0(Ceiling(X), Aft => 2));
  X := 98765432198.7654;
  Put_Line(F2Se0(X, Aft => 4) & ":  " & F2Se0(Floor(X), Aft => 4) & "  " & F2Se0(Round(X), Aft => 4) & "  " & F2Se0(Ceiling(X), Aft => 4));
  New_Line;

  Put_Line("Floor, Round and Ceiling of Double");
  D := 0.999;
  Put_Line(D2Se0(D, Aft => 8) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  D := 0.9999;
  Put_Line(D2Se0(D, Aft => 8) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  D := 0.99999;
  Put_Line(D2Se0(D, Aft => 8) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  D := 0.999999;
  Put_Line(D2Se0(D, Aft => 8) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  D := 0.9999999;
  Put_Line(D2Se0(D, Aft => 8) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  D := 0.99999999;
  Put_Line(D2Se0(D, Aft => 8) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  D := -98765432198.7654;
  Put_Line(D2Se0(D, Aft => 4) & ":  " & D2Se0(Floor(D), Aft => 4) & "  " & D2Se0(Round(D), Aft => 4) & "  " & D2Se0(Ceiling(D), Aft => 4));
  New_Line;

  D := 123.937500;
  Put_Line(D2Se0(D, Aft => 10) & ":");
  for Precision in 0..6 loop
    Put_Line("  " & I2S(Precision) & "   -> " & D2Se0(Round(D, Precision), Aft => 8));
  end loop;
  Put_Line("  4&3 -> " & D2Se0(Round(Round(D, 4), 3), Aft => 8));
  New_Line;

  D := 123.937592;
  Put_Line(D2Se0(D, Aft => 10) & ":");
  for Precision in 0..6 loop
    Put_Line("  " & I2S(Precision) & "   -> " & D2Se0(Round(D, Precision), Aft => 8));
  end loop;
  Put_Line("  4&3 -> " & D2Se0(Round(Round(D, 4), 3), Aft => 8));
  New_Line;

  D := 123.9375000001;
  Put_Line(D2Se0(D, Aft => 10) & ":");
  for Precision in 0..6 loop
    Put_Line("  " & I2S(Precision) & "   -> " & D2Se0(Round(D, Precision), Aft => 8));
  end loop;
  Put_Line("  4&3 -> " & D2Se0(Round(Round(D, 4), 3), Aft => 8));
  New_Line;

  D := 123.9374999999;
  Put_Line(D2Se0(D, Aft => 10) & ":");
  for Precision in 0..6 loop
    Put_Line("  " & I2S(Precision) & "   -> " & D2Se0(Round(D, Precision), Aft => 8));
  end loop;
  Put_Line("  4&3 -> " & D2Se0(Round(Round(D, 4), 3), Aft => 8));
  New_Line;

  for P in 0..4 loop
    Put_Line(I2S(P) & ": " & ("(.)" ** P) & ('.' ** P));
  end loop;
  New_Line;

  Put_Line("Durations");
  Put_Line(To_String(Duration'(13700.5), Aft => 0));
  Put_Line(To_String(Duration'(400.01), Aft => 2));
  Put_Line(To_String(Duration'(30.09)));
  Put_Line(To_String(Duration'(0.01)));
  Put_Line(To_String(Duration'(-0.01)));
  Put_Line(To_String(Duration'(-30.09)));
  Put_Line(To_String(Duration'(-400.01), Aft => 2));
  Put_Line(To_String(Duration'(-13700.5), Aft => 0));
  New_Line;

  Put("-123      :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("-123")) & "   "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("-123")) & "  "));
  Put("     45   :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("     45")) & "   "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("     45")) & "  "));
  Put("-123.45   :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("-123.45")) & "  "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("-123.45")) & "  "));
  Put("123.45e-5 :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("123.45e-5")) & "  "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("123.45e-5")) & "  "));
  Put(".3e+8     :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer(".3e+8")) & "  "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real(".3e+8")) & "  "));
  Put("123e5     :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("123.45e-5")) & "  "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("123.45e-5")) & "  "));
  Put("0.3e+8a   :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("0.3e+8a")) & "  "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("0.3e+8a")) & "  "));
  Put("patata    :  ");
  Put("Integer = " & To_Lowercase(Boolean'Image(Is_Integer("patata")) & "  "));
  Put_Line("Real = " & To_Lowercase(Boolean'Image(Is_Real("patata")) & "  "));
  New_Line;

  Put_Line("Original   : " & "tHIs iS a shOrt_senTence_for_test one2two");
  Put_Line("Uppercase  : " & To_Uppercase("tHIs iS a shOrt_senTence_for_test one2two"));
  Put_Line("Lowercase  : " & To_Lowercase("tHIs iS a shOrt_senTence_for_test one2two"));
  Put_Line("Capitalize : " &   Capitalize("tHIs iS a shOrt_senTence_for_test one2two"));
  Put_Line("Translate  : " &    Translate("tHIs iS a shOrt_senTence_for_test one2two", "_HISOT", " hisot") & "   (""_HISOT"" -> "" hisot"")");
  New_Line;

  if File_Exists(Fn_In) then
    Put_Line("File           : " & Fn_In);
    Put_Line("File path      : " & File_Path(Fn_In));
    Put_Line("File name      : " & File_Name(Fn_In));
    Put_Line("File base name : " & File_Base_Name(Fn_In));
    Put_Line("File extension : " & File_Extension(Fn_In));
    Put_Line("Full file name : " & Full_File_Name(Fn_In));
    Put_Line("New file name  : " & Compose_File_Name(File_Path(Fn_In), File_Base_Name(Fn_In) & "-cc", "net"));
    Put_Line("New file name  : " & Compose_File_Name(File_Path(Fn_In), File_Base_Name(Fn_In) & "-cc.net"));
    New_Line;
  end if;

  if File_Exists(Fn) then
    Put_Line("File           : " & Fn);
    Put_Line("File path      : " & File_Path(Fn));
    Put_Line("File name      : " & File_Name(Fn));
    Put_Line("File base name : " & File_Base_Name(Fn));
    Put_Line("File extension : " & File_Extension(Fn));
    Put_Line("Full file name : " & Full_File_Name(Fn));
    Put_Line("New file name  : " & Compose_File_Name(File_Path(Fn), File_Base_Name(Fn) & "-cc", "net"));
    Put_Line("New file name  : " & Compose_File_Name(File_Path(Fn), File_Base_Name(Fn) & "-cc.net"));
    New_Line;

    Open(Ft, In_File, Fn);
    Comments_Skip(Ft);

    Put_Line("Read file using a variety of tools");
    while not End_Of_Line(Ft) loop
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Float(Ft, X);
        Put(" ");
        Put(X, Fore => 0, Aft => 2, Exp => 0);
        if Round(X) = X then
          Put("! ");
        end if;
      end if;
      if Separator_Skip(Ft) then
        Put(To_Lowercase(" Si"));
      else
        Put(To_Uppercase(" No"));
      end if;
    end loop;
    Skip_Line(Ft);
    New_Line;

    while not End_Of_Line(Ft) loop
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Integer(Ft, I);
        Put(" ");
        Put(I, Width => 0);
      end if;
      if Separator_Skip(Ft, Sep => ':', Strict => True) then
        Put(" OK");
      else
        Put(" Wrong_Separator");
        Separator_Skip(Ft, Sep => ':', Strict => False);
      end if;
    end loop;
    Skip_Line(Ft);
    New_Line(2);

    while not End_Of_Line(Ft) loop
      Separator_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Word(Ft, Us);
        Put_Line(U2S(Us));
      end if;
    end loop;
    Skip_Line(Ft);
    New_Line;

    Add_Quotes('<', '>');
    Add_Quotes(''', ''');
    while not End_Of_Line(Ft) loop
      Separator_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Quoted_Word(Ft, Us);
        Put_Line(U2S(Us));
      end if;
    end loop;
    Skip_Line(Ft);
    New_Line;

    Set_Separators(Separators_Json);
    Comments_Skip(Ft);
    if Separator_Skip(Ft, '{', Strict => True) then
      loop
        Get_Pair(Ft, Key, Value, Sep => ':');
        Put_Line(U2S(Key) & ": " & U2S(Value));
        exit when not Separator_Skip(Ft, ',', Strict => True);
      end loop;
    end if;
    Separator_Skip(Ft, '}', Strict => True);
    New_Line;

    Set_Separators;
    while not End_Of_File(Ft) loop
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Word(Ft, W);
        Put(W.all);
        Free_Word(W);
      end if;
      while not End_Of_Line(Ft) loop
        Separator_Skip(Ft);
        Get_Word(Ft, W);
        if W.all'Length > 0 then
          Put(" " & W.all);
        end if;
        Free_Word(W);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
      end loop;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
      end if;
      New_Line;
    end loop;

    Close(Ft);
  end if;
end Utils_Test;
