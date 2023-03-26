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


-- @filename Utils-IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/05/2005
-- @revision 30/01/2016
-- @brief Several IO Utils

with Ada.Unchecked_Deallocation;
with Ada.Directories; use Ada.Directories;
with Ada.Containers; use Ada.Containers;

package body Utils.IO is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(String, Word_Access);

  --------------------
  -- Open_Or_Create --
  --------------------

  procedure Open_Or_Create(Ft: out File_Type; Fn: in String) is
  begin
    if File_Exists(Fn) then
      Open(Ft, Append_File, Fn);
    else
      Create(Ft, Out_File, Fn);
    end if;
  end Open_Or_Create;

  -----------------
  -- File_Exists --
  -----------------

  function File_Exists(Name: in String) return Boolean is
  begin
    return Exists(Name) and then Kind(Name) = Ordinary_File;
  end File_Exists;

  ----------------------
  -- Directory_Exists --
  ----------------------

  function Directory_Exists(Name: in String) return Boolean is
  begin
    return Exists(Name) and then Kind(Name) = Directory;
  end Directory_Exists;

  -----------------
  -- Rename_File --
  -----------------

  procedure Rename_File(Old_Fn: in String; New_Fn: in String) is
  begin
    if Rename_File(Old_Fn, New_Fn) then
      null;
    end if;
  end Rename_File;

  -----------------
  -- Rename_File --
  -----------------

  function Rename_File(Old_Fn: in String; New_Fn: in String) return Boolean is
  begin
    Rename(Old_Fn, New_Fn);
    return True;
  exception
    when others => return False;
  end Rename_File;

  ---------------
  -- Copy_File --
  ---------------

  procedure Copy_File(Ori_Fn: in String; Dest_Fn: in String) is
  begin
    Copy_File(Ori_Fn, Dest_Fn, Form => "mode=overwrite");
  end Copy_File;

  ---------------
  -- Copy_File --
  ---------------

  function Copy_File(Ori_Fn: in String; Dest_Fn: in String) return Boolean is
  begin
    Copy_File(Ori_Fn, Dest_Fn, Form => "mode=overwrite");
    return True;
  exception
    when others => return False;
  end Copy_File;

  -----------------
  -- Append_File --
  -----------------

  procedure Append_File(Ori_Fn: in String; Dest_Fn: in String) is
  begin
    Copy_File(Ori_Fn, Dest_Fn, Form => "mode=append");
  end Append_File;

  -----------------
  -- Append_File --
  -----------------

  function Append_File(Ori_Fn: in String; Dest_Fn: in String) return Boolean is
  begin
    Copy_File(Ori_Fn, Dest_Fn, Form => "mode=append");
    return True;
  exception
    when others => return False;
  end Append_File;

  -----------------
  -- Delete_File --
  -----------------

  procedure Delete_File(Fn: in String) is
  begin
    if Delete_File(Fn) then
      null;
    end if;
  end Delete_File;

  -----------------
  -- Delete_File --
  -----------------

  function Delete_File(Fn: in String) return Boolean is
  begin
    Ada.Directories.Delete_File(Fn);
    return True;
  exception
    when others => return False;
  end Delete_File;

  ---------------
  -- File_Path --
  ---------------

  function File_Path(Fn: in String) return String is
  begin
    return Containing_Directory(Fn);
  end File_Path;

  ---------------
  -- File_Name --
  ---------------

  function File_Name(Fn: in String) return String is
  begin
    return Simple_Name(Fn);
  end File_Name;

  --------------------
  -- File_Base_Name --
  --------------------

  function File_Base_Name(Fn: in String) return String is
  begin
    return Base_Name(Fn);
  end File_Base_Name;

  --------------------
  -- File_Extension --
  --------------------

  function File_Extension(Fn: in String) return String is
  begin
    return Extension(Fn);
  end File_Extension;

  --------------------
  -- Full_File_Name --
  --------------------

  function Full_File_Name(Fn: in String) return String is
  begin
    return Full_Name(Fn);
  end Full_File_Name;

  -----------------------
  -- Compose_File_Name --
  -----------------------

  function Compose_File_Name(Path, Name: in String; Ext: in String := "") return String is
  begin
    return Compose(Path, Name, Ext);
  end Compose_File_Name;

  ----------------------
  -- Line_Spaces_Skip --
  ----------------------

  procedure Line_Spaces_Skip is
    C: Character;
    Eol: Boolean;
  begin
    loop
      Look_Ahead(C, Eol);
      exit when Eol or else not Is_Space(C);
      Get(C);
    end loop;
  end Line_Spaces_Skip;

  ----------------------
  -- Line_Spaces_Skip --
  ----------------------

  procedure Line_Spaces_Skip(Ft: in File_Type) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Line_Spaces_Skip;
    Set_Input(Ft_Prev.all);
  end Line_Spaces_Skip;

  -----------------------
  -- Line_Comment_Skip --
  -----------------------

  procedure Line_Comment_Skip is
    C: Character;
    Eol: Boolean;
  begin
    Look_Ahead(C, Eol);
    if not Eol and then Is_Comment(C) then
      while not End_Of_Line loop
        Get(C);
      end loop;
    end if;
  end Line_Comment_Skip;

  -----------------------
  -- Line_Comment_Skip --
  -----------------------

  procedure Line_Comment_Skip(Ft: in File_Type) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Line_Comment_Skip;
    Set_Input(Ft_Prev.all);
  end Line_Comment_Skip;

  -------------------
  -- Comments_Skip --
  -------------------

  procedure Comments_Skip is
    C: Character;
    Eol: Boolean;
  begin
    loop
      Line_Spaces_Skip;
      exit when End_Of_File;
      Look_Ahead(C, Eol);
      exit when not Eol and then not Is_Comment(C);
      Skip_Line;
    end loop;
  end Comments_Skip;

  -------------------
  -- Comments_Skip --
  -------------------

  procedure Comments_Skip(Ft: in File_Type) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Comments_Skip;
    Set_Input(Ft_Prev.all);
  end Comments_Skip;

  --------------------
  -- Separator_Skip --
  --------------------

  function Separator_Skip(Sep: in Character := ' '; Strict: in Boolean := False) return Boolean is
    Found: Boolean := False;
    C: Character;
    Eol: Boolean;
  begin
    Look_Ahead(C, Eol);
    if not Eol then
      if Is_Space(C) then
        Found := not Strict;
        Line_Spaces_Skip;
        Look_Ahead(C, Eol);
      end if;
      if not Eol and then (C = Sep or ((not Strict) and Is_Separator(C))) then
        Found := True;
        Get(C);
        Line_Spaces_Skip;
      end if;
    end if;
    return Found;
  end Separator_Skip;

  --------------------
  -- Separator_Skip --
  --------------------

  function Separator_Skip(Ft: in File_Type; Sep: in Character := ' '; Strict: in Boolean := False) return Boolean is
    Ft_Prev: File_Access;
    Found: Boolean;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Found := Separator_Skip(Sep, Strict);
    Set_Input(Ft_Prev.all);
    return Found;
  end Separator_Skip;

  --------------------
  -- Separator_Skip --
  --------------------

  procedure Separator_Skip(Sep: in Character := ' '; Strict: in Boolean := False) is
  begin
    if Separator_Skip(Sep, Strict) then
      null;
    end if;
  end Separator_Skip;

  --------------------
  -- Separator_Skip --
  --------------------

  procedure Separator_Skip(Ft: in File_Type; Sep: in Character := ' '; Strict: in Boolean := False) is
  begin
    if Separator_Skip(Ft, Sep, Strict) then
      null;
    end if;
  end Separator_Skip;

  ---------------
  -- Free_Word --
  ---------------

  procedure Free_Word(W: in out Word_Access) is
  begin
    if W /= null then
      Dispose(W);
      W := null;
    end if;
  end Free_Word;

  ---------------
  -- Word_Skip --
  ---------------

  function Word_Skip(Sep: in Character := ' ') return Boolean is
    C: Character;
    Eol: Boolean;
    Found: Boolean;
  begin
    Line_Spaces_Skip;
    Found := False;
    loop
      Look_Ahead(C, Eol);
      exit when Eol or else (Is_Space(C) or Is_Comment(C) or Is_Separator(C) or (C = Sep));
      Get(C);
      Found := True;
    end loop;
    return Found;
  end Word_Skip;

  ---------------
  -- Word_Skip --
  ---------------

  function Word_Skip(Ft: in File_Type; Sep: in Character := ' ') return Boolean is
    Ft_Prev: File_Access;
    Found: Boolean;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Found := Word_Skip(Sep);
    Set_Input(Ft_Prev.all);
    return Found;
  end Word_Skip;

  ---------------
  -- Word_Skip --
  ---------------

  procedure Word_Skip(Sep: in Character := ' ') is
  begin
    if Word_Skip(Sep) then
      null;
    end if;
  end Word_Skip;

  ---------------
  -- Word_Skip --
  ---------------

  procedure Word_Skip(Ft: in File_Type; Sep: in Character := ' ') is
  begin
    if Word_Skip(Ft, Sep) then
      null;
    end if;
  end Word_Skip;

  --------------
  -- Get_Word --
  --------------

  procedure Get_Word(Us: out Ustring; Sep: in Character := ' ') is
    C: Character;
    Eol: Boolean;
  begin
    Us := Null_Ustring;
    Line_Spaces_Skip;
    loop
      Look_Ahead(C, Eol);
      exit when Eol or else (Is_Space(C) or Is_Comment(C) or Is_Separator(C) or (C = Sep));
      Us := Us & C;
      Get(C);
    end loop;
  end Get_Word;

  --------------
  -- Get_Word --
  --------------

  procedure Get_Word(Ft: in File_Type; Us: out Ustring; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Word(Us, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Word;

  --------------
  -- Get_Word --
  --------------

  procedure Get_Word(W: out String_Access; Sep: in Character := ' ') is
    Us: Ustring;
  begin
    Get_Word(Us, Sep);
    W := new String(1..Length(Us));
    W.all := To_String(Us);
  end Get_Word;

  --------------
  -- Get_Word --
  --------------

  procedure Get_Word(Ft: in File_Type; W: out String_Access; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Word(W, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Word;

  ---------------------
  -- Get_Quoted_Word --
  ---------------------

  procedure Get_Quoted_Word(Us: out Ustring; Forbid_Quote_Mark_Inside: in Boolean := False) is
    C: Character;
    Eol, Found: Boolean;
    Qi: Integer;
  begin
    Us := Null_Ustring;
    Line_Spaces_Skip;
    Found := False;
    Look_Ahead(C, Eol);
    if Eol then
      raise Quotes_Error with "No word found";
    end if;
    for I in Quotes_Open'Range loop
      if C = Quotes_Open(I) then
        Qi := I;
        Found := True;
        exit;
      end if;
    end loop;
    if not Found then
      raise Quotes_Error with "No starting quote character";
    end if;
    Get(C);
    loop
      Look_Ahead(C, Eol);
      if Eol then
        raise Quotes_Error with "No ending quote character";
      elsif C = Quotes_Close(Qi) then
        Get(C);
        exit;
      elsif Forbid_Quote_Mark_Inside and (Is_In_Characters(C, Quotes_Open) or Is_In_Characters(C, Quotes_Close)) then
        raise Quotes_Error with "Found quote character different from the closing one";
      end if;
      Us := Us & C;
      Get(C);
    end loop;
  end Get_Quoted_Word;

  ---------------------
  -- Get_Quoted_Word --
  ---------------------

  procedure Get_Quoted_Word(Ft: in File_Type; Us: out Ustring; Forbid_Quote_Mark_Inside: in Boolean := False) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Quoted_Word(Us, Forbid_Quote_Mark_Inside);
    Set_Input(Ft_Prev.all);
  end Get_Quoted_Word;

  ---------------------
  -- Get_Quoted_Word --
  ---------------------

  procedure Get_Quoted_Word(W: out String_Access; Forbid_Quote_Mark_Inside: in Boolean := False) is
    Us: Ustring;
  begin
    Get_Quoted_Word(Us, Forbid_Quote_Mark_Inside);
    W := new String(1..Length(Us));
    W.all := To_String(Us);
  end Get_Quoted_Word;

  ---------------------
  -- Get_Quoted_Word --
  ---------------------

  procedure Get_Quoted_Word(Ft: in File_Type; W: out String_Access; Forbid_Quote_Mark_Inside: in Boolean := False) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Quoted_Word(W, Forbid_Quote_Mark_Inside);
    Set_Input(Ft_Prev.all);
  end Get_Quoted_Word;

  --------------
  -- Get_Pair --
  --------------

  procedure Get_Pair(Key, Value: out Ustring; Sep: in Character := ' ') is
    C: Character;
    Eol: Boolean;
  begin
    Key   := Null_Ustring;
    Value := Null_Ustring;
    Line_Spaces_Skip;
    Look_Ahead(C, Eol);
    if Eol then
      raise Key_Value_Pair_Error with "Cannot find key";
    elsif Is_Open_Quote(C) then
      Get_Quoted_Word(Key);
    else
      Get_Word(Key, Sep);
    end if;
    if not Separator_Skip(Sep, Strict => True) then
      raise Key_Value_Pair_Error with "Separator '" & Sep & "' not found between key and value";
    end if;
    Look_Ahead(C, Eol);
    if Eol then
      raise Key_Value_Pair_Error with "Cannot find value for key: " & U2S(Key);
    elsif Is_Open_Quote(C) then
      Get_Quoted_Word(Value);
    else
      Get_Word(Value, Sep);
    end if;
  end Get_Pair;

  --------------
  -- Get_Pair --
  --------------

  procedure Get_Pair(Ft: in File_Type; Key, Value: out Ustring; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Pair(Key, Value, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Pair;

  --------------
  -- Get_Pair --
  --------------

  procedure Get_Pair(Key, Value: out String_Access; Sep: in Character := ' ') is
    Us_Key, Us_Value: Ustring;
  begin
    Get_Pair(Us_Key, Us_Value, Sep);
    Key   := new String(1..Length(Us_Key));
    Value := new String(1..Length(Us_Value));
    Key.all   := To_String(Us_Key);
    Value.all := To_String(Us_Value);
  end Get_Pair;

  --------------
  -- Get_Pair --
  --------------

  procedure Get_Pair(Ft: in File_Type; Key, Value: out String_Access; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Pair(Key, Value, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Pair;

  -----------------
  -- Get_Integer --
  -----------------

  procedure Get_Integer(I: out Integer; Sep: in Character := ' ') is
    W: String_Access;
    Len: Positive;
  begin
    Get_Word(W, Sep);
    Get(W.all, I, Len);
    Free_Word(W);
  end Get_Integer;

  -----------------
  -- Get_Integer --
  -----------------

  procedure Get_Integer(Ft: in File_Type; I: out Integer; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Integer(I, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Integer;

  -----------------
  -- Get_Longint --
  -----------------

  procedure Get_Longint(L: out Longint; Sep: in Character := ' ') is
    W: String_Access;
    Len: Positive;
  begin
    Get_Word(W, Sep);
    Get(W.all, L, Len);
    Free_Word(W);
  end Get_Longint;

  -----------------
  -- Get_Longint --
  -----------------

  procedure Get_Longint(Ft: in File_Type; L: out Longint; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Longint(L, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Longint;

  ---------------
  -- Get_Float --
  ---------------

  procedure Get_Float(X: out Float; Sep: in Character := ' ') is
    W: String_Access;
    Len: Positive;
  begin
    Get_Word(W, Sep);
    Get(W.all, X, Len);
    Free_Word(W);
  end Get_Float;

  ---------------
  -- Get_Float --
  ---------------

  procedure Get_Float(Ft: in File_Type; X: out Float; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Float(X, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Float;

  ----------------
  -- Get_Double --
  ----------------

  procedure Get_Double(X: out Double; Sep: in Character := ' ') is
    W: String_Access;
    Len: Positive;
  begin
    Get_Word(W, Sep);
    Get(W.all, X, Len);
    Free_Word(W);
  end Get_Double;

  ----------------
  -- Get_Double --
  ----------------

  procedure Get_Double(Ft: in File_Type; X: out Double; Sep: in Character := ' ') is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Double(X, Sep);
    Set_Input(Ft_Prev.all);
  end Get_Double;

  ---------------------
  -- Put_String_Line --
  ---------------------

  procedure Put_String_Line(Fn: in String; S: in String) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put_Line(Ft, S);
    Close(Ft);
  end Put_String_Line;

  --------------
  -- Put_Word --
  --------------

  procedure Put_Word(Us: in Ustring; Width: in Natural := Default_String_Width) is
  begin
    Put(Right_Justify(U2S(Us), Width => Width, Pad => ' '));
  end Put_Word;

  --------------
  -- Put_Word --
  --------------

  procedure Put_Word(Fn: in String; Us: in Ustring; Width: in Natural := Default_String_Width) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put(Ft, Right_Justify(U2S(Us), Width => Width, Pad => ' '));
    Close(Ft);
  end Put_Word;

  --------------
  -- Put_Word --
  --------------

  procedure Put_Word(Ft: in File_Type; Us: in Ustring; Width: in Natural := Default_String_Width) is
  begin
    Put(Ft, Right_Justify(U2S(Us), Width => Width, Pad => ' '));
  end Put_Word;

  -----------------
  -- Put_Integer --
  -----------------

  procedure Put_Integer(I: in Integer; Width: in Field := Default_Integer_Width) is
  begin
    Put(I, Width => Width);
  end Put_Integer;

  -----------------
  -- Put_Integer --
  -----------------

  procedure Put_Integer(Fn: in String; I: in Integer; Width: in Field := Default_Integer_Width) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put(Ft, I, Width => Width);
    Close(Ft);
  end Put_Integer;

  -----------------
  -- Put_Integer --
  -----------------

  procedure Put_Integer(Ft: in File_Type; I: in Integer; Width: in Field := Default_Integer_Width) is
  begin
    Put(Ft, I, Width => Width);
  end Put_Integer;

  -----------------
  -- Put_Longint --
  -----------------

  procedure Put_Longint(L: in Longint; Width: in Field := Default_Longint_Width) is
  begin
    Put(L, Width => Width);
  end Put_Longint;

  -----------------
  -- Put_Longint --
  -----------------

  procedure Put_Longint(Fn: in String; L: in Longint; Width: in Field := Default_Longint_Width) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put(Ft, L, Width => Width);
    Close(Ft);
  end Put_Longint;

  -----------------
  -- Put_Longint --
  -----------------

  procedure Put_Longint(Ft: in File_Type; L: in Longint; Width: in Field := Default_Longint_Width) is
  begin
    Put(Ft, L, Width => Width);
  end Put_Longint;

  ---------------
  -- Put_Float --
  ---------------

  procedure Put_Float(X: in Float; Aft: in Field := Default_Float_Aft) is
  begin
    Put_Float(Current_Output, X, Aft => Aft);
  end Put_Float;

  ---------------
  -- Put_Float --
  ---------------

  procedure Put_Float(Fn: in String; X: in Float; Aft: in Field := Default_Float_Aft) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put_Float(Ft, X, Aft => Aft);
    Close(Ft);
  end Put_Float;

  ---------------
  -- Put_Float --
  ---------------

  procedure Put_Float(Ft: in File_Type; X: in Float; Aft: in Field := Default_Float_Aft) is
    Exp: Field := 2;
  begin
    if Aft = 0 then
      Put(Ft, Integer(X), Width => 0);
    else
      if X = 0.0 or else (Aft <= 2 and then (abs X) in 0.01 .. 100000.0) or else (Aft > 2 and then (abs X) in 0.01 .. 100.0) then
        Exp := 0;
      end if;
      Put(Ft, X, Fore => 1, Aft => Aft, Exp => Exp);
    end if;
  end Put_Float;

  ----------------
  -- Put_Double --
  ----------------

  procedure Put_Double(X: in Double; Aft: in Field := Default_Double_Aft) is
  begin
    Put_Double(Current_Output, X, Aft => Aft);
  end Put_Double;

  ----------------
  -- Put_Double --
  ----------------

  procedure Put_Double(Fn: in String; X: in Double; Aft: in Field := Default_Double_Aft) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put_Double(Ft, X, Aft => Aft);
    Close(Ft);
  end Put_Double;

  ----------------
  -- Put_Double --
  ----------------

  procedure Put_Double(Ft: in File_Type; X: in Double; Aft: in Field := Default_Double_Aft) is
    Exp: Field := 2;
  begin
    if Aft = 0 then
      Put(Ft, Integer(X), Width => 0);
    else
      if X = 0.0 or else (Aft <= 2 and then (abs X) in 0.01 .. 100000.0) or else (Aft > 2 and then (abs X) in 0.01 .. 100.0) then
        Exp := 0;
      end if;
      Put(Ft, X, Fore => 1, Aft => Aft, Exp => Exp);
    end if;
  end Put_Double;

  --------------------
  -- Put_Percentage --
  --------------------

  procedure Put_Percentage(Num: in Natural; Total: in Positive; Aft: in Field := Default_Float_Aft) is
  begin
    Put_Percentage(Current_Output, Num, Total, Aft);
  end Put_Percentage;

  --------------------
  -- Put_Percentage --
  --------------------

  procedure Put_Percentage(Fn: in String; Num: in Natural; Total: in Positive; Aft: in Field := Default_Float_Aft) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put_Percentage(Ft, Num, Total, Aft);
    Close(Ft);
  end Put_Percentage;

  --------------------
  -- Put_Percentage --
  --------------------

  procedure Put_Percentage(Ft: in File_Type; Num: in Natural; Total: in Positive; Aft: in Field := Default_Float_Aft) is
  begin
    Put(Ft, Num, Width => 0);
    Put(Ft, " of ");
    Put(Ft, Total, Width => 0);
    Put(Ft, " (");
    Put_Float(Ft, Float(100 * Num) / Float(Total), Aft => Aft);
    Put(Ft, "%)");
  end Put_Percentage;

  ------------------
  -- Put_Duration --
  ------------------

  procedure Put_Duration(D: in Duration; Aft: in Field := Default_Float_Aft) is
  begin
    Put_Duration(Current_Output, D, Aft);
  end Put_Duration;

  ------------------
  -- Put_Duration --
  ------------------

  procedure Put_Duration(Fn: in String; D: in Duration; Aft: in Field := Default_Float_Aft) is
    Ft: File_Type;
  begin
    Open_Or_Create(Ft, Fn);
    Put_Duration(Ft, D, Aft);
    Close(Ft);
  end Put_Duration;

  ------------------
  -- Put_Duration --
  ------------------

  procedure Put_Duration(Ft: in File_Type; D: in Duration; Aft: in Field := Default_Float_Aft) is
  begin
    Put(Ft, D2S(D, Aft));
  end Put_Duration;

  ---------------------
  -- Load_Properties --
  ---------------------

  procedure Load_Properties(Fn: in String; Props: out Properties) is
    Ft: File_Type;
    Name, Val: Ustring;
    C: Character;
  begin
    Props := Empty_Map;
    if File_Exists(Fn) then
      Open(Ft, In_File, Fn);
      while not End_Of_File(Ft) loop
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          Get_Word(Ft, Name, '=');
          if Separator_Skip(Ft, '=') then
            Val := Null_Ustring;
            while not End_Of_Line(Ft) loop
              Get(Ft, C);
              Val := Val & C;
            end loop;
            Name := Trim_Spaces(Name);
            Val := Trim_Spaces(Val);
            Include(Props, Name, Val);
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;
      Close(Ft);
    end if;
  exception
    when others => Props := Empty_Map;
  end Load_Properties;

  ---------------------
  -- Save_Properties --
  ---------------------

  procedure Save_Properties(Fn: in String; Props: in Properties) is
    Ft: File_Type;
    Cur: Cursor;
    Name, Val: Ustring;
  begin
    Create(Ft, Out_File, Fn);
    Cur := First(Props);
    while Has_Element(Cur) loop
      Name := Key(Cur);
      Val := Element(Cur);
      Put_Line(Ft, U2S(Name) & "=" & U2S(Val));
      Next(Cur);
    end loop;
    Close(Ft);
  end Save_Properties;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Ustring := Null_Ustring) return Ustring is
    Cur: Cursor;
  begin
    Cur := Find(Props, Trim_Spaces(Name));
    if Cur /= No_Element then
      return Element(Cur);
    else
      return Trim_Spaces(Def_Val);
    end if;
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in String := "") return Ustring is
  begin
    return Get_Property(Props, S2U(Name), S2U(Def_Val));
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in String := "") return String is
  begin
    return U2S(Get_Property(Props, S2U(Name), S2U(Def_Val)));
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Boolean := False) return Boolean is
    Cur: Cursor;
  begin
    Cur := Find(Props, Trim_Spaces(Name));
    if Cur /= No_Element then
      if To_Lowercase(U2S(Element(Cur))) = "true" then
        return True;
      elsif To_Lowercase(U2S(Element(Cur))) = "false" then
        return False;
      end if;
    end if;
    return Def_Val;
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Boolean := False) return Boolean is
  begin
    return Get_Property(Props, S2U(Name), Def_Val);
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Integer := 0) return Integer is
    Cur: Cursor;
  begin
    Cur := Find(Props, Trim_Spaces(Name));
    if Cur /= No_Element and then Is_Integer(Element(Cur)) then
      return U2I(Element(Cur));
    end if;
    return Def_Val;
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Integer := 0) return Integer is
  begin
    return Get_Property(Props, S2U(Name), Def_Val);
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Longint := 0) return Longint is
    Cur: Cursor;
  begin
    Cur := Find(Props, Trim_Spaces(Name));
    if Cur /= No_Element and then Is_Integer(Element(Cur)) then
      return U2L(Element(Cur));
    end if;
    return Def_Val;
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Longint := 0) return Longint is
  begin
    return Get_Property(Props, S2U(Name), Def_Val);
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Float := 0.0) return Float is
    Cur: Cursor;
  begin
    Cur := Find(Props, Trim_Spaces(Name));
    if Cur /= No_Element and then Is_Real(Element(Cur)) then
      return U2F(Element(Cur));
    end if;
    return Def_Val;
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Float := 0.0) return Float is
  begin
    return Get_Property(Props, S2U(Name), Def_Val);
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Double := 0.0) return Double is
    Cur: Cursor;
  begin
    Cur := Find(Props, Trim_Spaces(Name));
    if Cur /= No_Element and then Is_Real(Element(Cur)) then
      return U2D(Element(Cur));
    end if;
    return Def_Val;
  end Get_Property;

  ------------------
  -- Get_Property --
  ------------------

  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Double := 0.0) return Double is
  begin
    return Get_Property(Props, S2U(Name), Def_Val);
  end Get_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name, Value: in Ustring) is
  begin
    Include(Props, Trim_Spaces(Name), Trim_Spaces(Value));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name, Value: in String) is
  begin
    Include(Props, S2U(Trim_Spaces(Name)), S2U(Trim_Spaces(Value)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Boolean) is
  begin
    Include(Props, Trim_Spaces(Name), S2U(Capitalize(Boolean'Image(Value))));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Boolean) is
  begin
    Include(Props, S2U(Trim_Spaces(Name)), S2U(Capitalize(Boolean'Image(Value))));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Integer) is
  begin
    Include(Props, Trim_Spaces(Name), S2U(I2S(Value)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Integer) is
  begin
    Include(Props, S2U(Trim_Spaces(Name)), S2U(I2S(Value)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Longint) is
  begin
    Include(Props, Trim_Spaces(Name), S2U(L2S(Value)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Longint) is
  begin
    Include(Props, S2U(Trim_Spaces(Name)), S2U(L2S(Value)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) is
  begin
    Include(Props, Trim_Spaces(Name), S2U(F2S(Value, Aft => Aft, Exp => Exp)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) is
  begin
    Include(Props, S2U(Trim_Spaces(Name)), S2U(F2S(Value, Aft => Aft, Exp => Exp)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) is
  begin
    Include(Props, Trim_Spaces(Name), S2U(D2S(Value, Aft => Aft, Exp => Exp)));
  end Set_Property;

  ------------------
  -- Set_Property --
  ------------------

  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) is
  begin
    Include(Props, S2U(Trim_Spaces(Name)), S2U(D2S(Value, Aft => Aft, Exp => Exp)));
  end Set_Property;

  ---------------------
  -- Remove_Property --
  ---------------------

  procedure Remove_Property(Props: in out Properties; Name: in Ustring) is
  begin
    Exclude(Props, Trim_Spaces(Name));
  end Remove_Property;

  ---------------------
  -- Remove_Property --
  ---------------------

  procedure Remove_Property(Props: in out Properties; Name: in String) is
  begin
    Exclude(Props, S2U(Trim_Spaces(Name)));
  end Remove_Property;

  --------------------------
  -- Number_Of_Properties --
  --------------------------

  function Number_Of_Properties(Props: in Properties) return Natural is
  begin
    return Natural(Length(Props));
  end Number_Of_Properties;

  ------------------------
  -- Get_Property_Names --
  ------------------------

  function Get_Property_Names(Props: in Properties) return PUstrings is
    N: Natural;
    P: PUstrings;
    Cur: Cursor;
  begin
    P := null;
    N := Natural(Length(Props));
    if N > 0 then
      P := Alloc(1, N);
      Cur := First(Props);
      for I in P'Range loop
        P(I) := Key(Cur);
        Next(Cur);
      end loop;
    end if;
    return P;
  end Get_Property_Names;

  ------------------------
  -- Get_Property_Values--
  ------------------------

  function Get_Property_Values(Props: in Properties) return PUstrings is
    N: Natural;
    P: PUstrings;
    Cur: Cursor;
  begin
    P := null;
    N := Natural(Length(Props));
    if N > 0 then
      P := Alloc(1, N);
      Cur := First(Props);
      for I in P'Range loop
        P(I) := Element(Cur);
        Next(Cur);
      end loop;
    end if;
    return P;
  end Get_Property_Values;

end Utils.IO;
