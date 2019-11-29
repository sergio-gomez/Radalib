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


-- @filename Finite_Disjoint_Lists-IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/05/2005
-- @revision 23/09/2015
-- @brief Input and Output of Lists of Lists with Finite Disjoint Elements

with Ada.Exceptions; use Ada.Exceptions;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Utils.IO_Integer; use Utils.IO_Integer;

package body Finite_Disjoint_Lists.IO is

  ---------
  -- Get --
  ---------

  procedure Get(Lol: out List_Of_Lists) is
  begin
    Get("", Lol);
  end Get;

  ---------
  -- Get --
  ---------

  procedure Get(Lol: out List_Of_Lists; Fn: in String; Num_Skip_Lines: in Natural := 0) is
  begin
    Get(Fn, Lol, Num_Skip_Lines);
  end Get;

  ---------
  -- Get --
  ---------

  procedure Get(Fn: in String; Lol: out List_Of_Lists; Num_Skip_Lines: in Natural := 0) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    C: Character;
    Num_Elements: Natural;
    Num_Lists: Natural;
    L: List;
  begin
    if Fn /= "" then
      Open(Ft, In_File, Fn);
      Ft_Prev := Current_Input;
      Set_Input(Ft);
    end if;

    for I in 1..Num_Skip_Lines loop
      Skip_Line;
    end loop;

    C := ' ';
    while C /= ':' loop
      Get(C);
    end loop;
    Get(Num_Elements); Skip_Line;
    Initialize(Lol, Num_Elements);
    C := ' ';
    while C /= ':' loop
      Get(C);
    end loop;
    Get(Num_Lists); Skip_Line;
    if Num_Elements > 0 then
      Skip_Line;
      for I in 1..Num_Lists loop
        Get(Lol, L);
      end loop;
    end if;

    if Fn /= "" then
      Close(Ft);
      Set_Input(Ft_Prev.all);
    end if;
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Error reading partition in Lol format : " & Fn);
      Put_Line("Current position : line=" & I2S(Integer(Line(Ft))) & ", col=" & I2S(Integer(Col(Ft))));
      raise List_Of_Lists_IO_Error;
  end Get;

  ---------
  -- Get --
  ---------

  procedure Get(Ft: in File_Type; Lol: out List_Of_Lists; Num_Skip_Lines: in Natural := 0) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get("", Lol, Num_Skip_Lines);
    Set_Input(Ft_Prev.all);
  end Get;

  ---------
  -- Put --
  ---------

  procedure Put(Lol: in List_Of_Lists) is
  begin
    Put("", Lol);
  end Put;

  ---------
  -- Put --
  ---------

  procedure Put(Lol: in List_Of_Lists; Fn: in String; Mode: in File_Mode := Out_File) is
  begin
    Put(Fn, Lol, Mode);
  end Put;

  ---------
  -- Put --
  ---------

  procedure Put(Fn: in String; Lol: in List_Of_Lists; Mode: in File_Mode := Out_File) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    L: List;
  begin
    if Fn /= "" then
      if Mode = Out_File then
        Create(Ft, Mode, Fn);
      elsif Mode = Append_File then
        Open_Or_Create(Ft, Fn);
      end if;
      Ft_Prev := Current_Output;
      Set_Output(Ft);
    end if;

    Put("Number of elements: ");
    Put(Number_Of_Elements(Lol), Width => 0); New_Line;
    Put("Number of lists: ");
    Put(Number_Of_Lists(Lol), Width => 0); New_Line;
    if Number_Of_Lists(Lol) > 0 then
      New_Line;
      Save(Lol);
      Reset(Lol);
      while Has_Next_List(Lol) loop
        L := Next_List(Lol);
        Put(L);
      end loop;
      Restore(Lol);
    end if;
    L := Unassigned_List(Lol);
    if Number_Of_Elements(L) > 0 then
      New_Line;
      Put_Line("Unassigned list:");
      Put(L);
    end if;

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Put;

  ---------
  -- Put --
  ---------

  procedure Put(Ft: in File_Type; Lol: in List_Of_Lists) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Output;
    Set_Output(Ft);
    Put("", Lol);
    Set_Output(Ft_Prev.all);
  end Put;

  ---------
  -- Get --
  ---------

  procedure Get(Lol: in List_Of_Lists; L: out List) is
  begin
    Get("", Lol, L);
  end Get;

  ---------
  -- Get --
  ---------

  procedure Get(Lol: in List_Of_Lists; L: out List; Fn: in String) is
  begin
    Get(Fn, Lol, L);
  end Get;

  ---------
  -- Get --
  ---------

  procedure Get(Fn: in String; Lol: in List_Of_Lists; L: out List) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    N: Natural;
    P: Positive;
    C: Character;
  begin
    if Fn /= "" then
      Open(Ft, In_File, Fn);
      Ft_Prev := Current_Input;
      Set_Input(Ft);
    end if;

    L := New_List(Lol);
    Get_Integer(N);
    Get(C);
    for I in 1..N loop
      Get(P);
      Move(Get_Element(Lol, P), L);
    end loop;
    Skip_Line;

    if Fn /= "" then
      Close(Ft);
      Set_Input(Ft_Prev.all);
    end if;
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Error reading a list from a partition in Lol format : " & Fn);
      Put_Line("Current position : line=" & I2S(Integer(Line(Ft))) & ", col=" & I2S(Integer(Col(Ft))));
      raise List_Of_Lists_IO_Error;
  end Get;

  ---------
  -- Get --
  ---------

  procedure Get(Ft: in File_Type; Lol: in List_Of_Lists; L: out List) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get("", Lol, L);
    Set_Input(Ft_Prev.all);
  end Get;

  ---------
  -- Put --
  ---------

  procedure Put(L: in List) is
  begin
    Put("", L);
  end Put;

  ---------
  -- Put --
  ---------

  procedure Put(L: in List; Fn: in String; Mode: in File_Mode := Out_File) is
  begin
    Put(Fn, L, Mode);
  end Put;

  ---------
  -- Put --
  ---------

  procedure Put(Fn: in String; L: in List; Mode: in File_Mode := Out_File) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    E: Element;
  begin
    if Fn /= "" then
      if Mode = Out_File then
        Create(Ft, Mode, Fn);
      elsif Mode = Append_File then
        Open_Or_Create(Ft, Fn);
      end if;
      Ft_Prev := Current_Output;
      Set_Output(Ft);
    end if;

    Put(Number_Of_Elements(L), Width => 0); Put(":");
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      E := Next_Element(L);
      Put(" "); Put(Index_Of(E), Width => 0);
    end loop;
    Restore(L);
    New_Line;

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Put;

  ---------
  -- Put --
  ---------

  procedure Put(Ft: in File_Type; L: in List) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Output;
    Set_Output(Ft);
    Put("", L);
    Set_Output(Ft_Prev.all);
  end Put;

end Finite_Disjoint_Lists.IO;
