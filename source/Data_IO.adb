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


-- @filename Data_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/05/2013
-- @revision 18/01/2016
-- @brief Input and output of data in List or Matrix form

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Exceptions; use Ada.Exceptions;

with Utils.IO; use Utils.IO;
with Linked_Lists;

package body Data_IO is

  ----------------------
  -- Generic_Get_Data --
  ----------------------

  procedure Generic_Get_Data(Fn: in String; Data: out PItemss; Col_Name, Row_Name: out PUstrings; No_Value: in Item := No_Item; Form: in Data_Form := Auto) is
    Ft: File_Type;
  begin
    Open(Ft, In_File, Fn);
    Generic_Get_Data(Ft, Data, Col_Name, Row_Name, No_Value, Form);
    Close(Ft);
  end Generic_Get_Data;

  ----------------------
  -- Generic_Get_Data --
  ----------------------

  procedure Generic_Get_Data(Ft: in out File_Type; Data: out PItemss; Col_Name, Row_Name: out PUstrings; No_Value: in Item := No_Item; Form: in Data_Form := Auto) is

    -- Hash Map to store names
    package Unbounded_Strings_Maps is new Ada.Containers.Hashed_Maps(
      Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");
    use Unbounded_Strings_Maps;

    Names_Map: Map;

    -- Linked List to sort names
    package Us_Linked_Lists is new Linked_Lists(Ustring); use Us_Linked_Lists;

    Names_List: Linked_List;
    Numeric_Names: Boolean := False;

    function Lower(Left, Right: in Ustring) return Boolean is
    begin
      if Numeric_Names then
        return S2I(U2S(Left)) < S2I(U2S(Right));
      else
        return Left < Right;
      end if;
    end Lower;


    Name_R1C1, Names_Col1, Names_Col2, Names_Row1: Boolean;
    Is_List_Form: Boolean;
    N, Nr, Nc, I, J: Natural;
    F, T: Positive;
    Us: Ustring;
    X: Item;
  begin

    -- Determine data size and names location (none, columns or rows)
    Name_R1C1 := False;
    Names_Row1 := False;
    Names_Col1 := False;
    Names_Col2 := False;
    Nc := 0;
    Comments_Skip(Ft);
    while not End_Of_Line(Ft) loop
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Word(Ft, Us);
        Nc := Nc + 1;
        Separator_Skip(Ft);
        if not Is_Item(Us) then
          if Nc = 1 then
            Name_R1C1 := True;
          else
            Names_Row1 := True;
          end if;
        end if;
      end if;
    end loop;
    if not End_Of_File(Ft) then
      Skip_Line(Ft);
    end if;
    Nr := 1;
    while not End_Of_File(Ft) loop
      Comments_Skip(Ft);
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Word(Ft, Us);
        Nr := Nr + 1;
        if not Is_Item(Us) then
          Names_Col1 := True;
        end if;
      end if;
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        Get_Word(Ft, Us);
        if not Is_Item(Us) then
          Names_Col2 := True;
        end if;
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
      end if;
    end loop;
    if Name_R1C1 and not (Names_Row1 or Names_Col1) then
      Names_Row1 := True;
      Names_Col1 := True;
    end if;

    -- Decide between list or matrix form
    Is_List_Form := False;
    if Nc = 3 then
      if Form = Matrix_Form then
        Is_List_Form := False;
      elsif Names_Col1 and Names_Col2 then
        Is_List_Form := True;
      elsif Names_Col1 or Names_Row1 then
        Is_List_Form := False;
      elsif Nr = 3 then
        Is_List_Form := False;
      else
        Is_List_Form := True;
      end if;
    end if;

    if Form = List_Form and not Is_List_Form then
      raise Data_IO_Error with "Data file does not seem to be in list form";
    end if;

    if not Is_List_Form then
      if Names_Row1 then
        Nr := Nr - 1;
      end if;
      if Names_Col1 then
        Nc := Nc - 1;
      end if;
    end if;

    -- Read names and data
    if Is_List_Form then

      -- Read names in list form
      Initialize(Names_List);
      Reset(Ft);
      N := 0;
      Numeric_Names := True;
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- From
          Get_Word(Ft, Us);
          if not Contains(Names_Map, Us) then
            N := N + 1;
            Insert(Names_Map, Us, N);
            Add_Last(Us, Names_List);
            if Numeric_Names then
              Numeric_Names := Is_Integer(Us);
            end if;
          end if;
          -- Separator
          Separator_Skip(Ft);
          -- To
          Get_Word(Ft, Us);
          if not Contains(Names_Map, Us) then
            N := N + 1;
            Insert(Names_Map, Us, N);
            Add_Last(Us, Names_List);
            if Numeric_Names then
              Numeric_Names := Is_Integer(Us);
            end if;
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

      -- Initialize data
      Col_Name := Alloc(1, N);
      Row_Name := null;
      Data := Alloc(1, N, 1, N);
      Data.all := (others => (others => No_Value));

      -- Sort names
      Sort(Names_List, Lower'access);
      Clear(Names_Map);
      I := 0;
      Save(Names_List);
      Reset(Names_List);
      while Has_Next(Names_List) loop
        I := I + 1;
        Us := Next(Names_List);
        Insert(Names_Map, Us, I);
        Col_Name(I) := Us;
      end loop;
      Restore(Names_List);
      Free(Names_List);

      -- Read data in list form
      Reset(Ft);
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- From
          Get_Word(Ft, Us);
          F := Element(Names_Map, Us);
          -- Separator
          Separator_Skip(Ft);
          -- To
          Get_Word(Ft, Us);
          T := Element(Names_Map, Us);
          -- Separator
          Separator_Skip(Ft);
          -- Item
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if not End_Of_Line(Ft) then
            Get_Item(Ft, X);
            Data(F, T) := X;
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

    else

      -- Initialize data in matrix form
      Row_Name := null;
      Col_Name := null;
      if Names_Row1 then
        Col_Name := Alloc(1, Nc);
      end if;
      if Names_Col1 then
        Row_Name := Alloc(1, Nr);
      end if;
      Data := Alloc(1, Nr, 1, Nc);
      Data.all := (others => (others => No_Value));

      -- Read names in first row
      Reset(Ft);
      if Names_Row1 then
        Comments_Skip(Ft);
        if Names_Col1 then
          Get_Word(Ft, Us);
          Separator_Skip(Ft);
        end if;
        J := 1;
        while not End_Of_Line(Ft) loop
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if not End_Of_Line(Ft) then
            Get_Word(Ft, Us);
            Col_Name(J) := Us;
            Separator_Skip(Ft);
            J := J + 1;
          end if;
        end loop;
        Skip_Line(Ft);
      end if;
      -- Read names in first column and data in matrix form
      I := 1;
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        -- Read names in first column
        if Names_Col1 then
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if not End_Of_Line(Ft) then
            Get_Word(Ft, Us);
            Row_Name(I) := Us;
            Separator_Skip(Ft);
          end if;
        end if;
        -- Read data
        J := 1;
        while not End_Of_Line(Ft) loop
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if not End_Of_Line(Ft) then
            Get_Item(Ft, X);
            Data(I, J) := X;
            Separator_Skip(Ft);
            J := J + 1;
          end if;
        end loop;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
        I := I + 1;
      end loop;

    end if;

  exception
    when E: others =>
      Put_Line("Error reading data : " & Name(Ft));
      Put_Line("Current position   : line=" & I2S(Integer(Line)) & ", col=" & I2S(Integer(Col)));
      Put(Exception_Information(E));
      raise Data_IO_Error;
  end Generic_Get_Data;

end Data_IO;
