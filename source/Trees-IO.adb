-- Radalib, Copyright (c) 2021 by
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


-- @filename Trees-IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 05/04/2012
-- @revision 01/02/2016
-- @brief Input and Output of Trees

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Utils; use Utils;
with Utils.IO; use Utils.IO;

package body Trees.IO is

  --------------
  -- Put_Tree --
  --------------

  procedure Put_Tree(T: in Tree; Param: in Integer; Format: in Tree_Format := Default_Tree_Format) is
  begin
    Put_Tree("", T, Param, Format => Format);
  end Put_Tree;

  --------------
  -- Put_Tree --
  --------------

  procedure Put_Tree(Ft: in File_Type; T: in Tree; Param: in Integer; Format: in Tree_Format := Default_Tree_Format) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Output;
    Set_Output(Ft);
    Put_Tree("", T, Param, Format => Format);
    Set_Output(Ft_Prev.all);
  end Put_Tree;

  --------------
  -- Put_Tree --
  --------------

  procedure Put_Tree(Fn: in String; T: in Tree; Param: in Integer; Mode: in File_Mode := Out_File; Format: in Tree_Format := Default_Tree_Format) is

    procedure Put_Node_Text(Nod: in Node; Indent: in out Natural) is
      Ch: List_Of_Nodes;
      Nch: Natural;
    begin
      Ch := Nod.Children;
      Nch := Size(Ch);
      if Nch = 0 then
        Put(Right_Justify("", Indent) & "*  ");
        Put(To_S(Nod.Value, Param, Format => Text_Tree));
        New_Line;
      else
        Put(Right_Justify("", Indent) & "+ ");
        Put(Nch, Width => 0);
        Put("  ");
        Put(To_S(Nod.Value, Param, Format => Text_Tree));
        New_Line;
        Indent := Indent + 2;
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          Put_Node_Text(Next(Ch), Indent);
        end loop;
        Restore(Ch);
        Indent := Indent - 2;
      end if;
    end Put_Node_Text;

    procedure Put_Node_Newick(Nod: in Node) is
      Ch: List_Of_Nodes;
      Nch: Natural;
    begin
      Ch := Nod.Children;
      Nch := Size(Ch);
      if Nch = 0 then
        Put(To_S(Nod.Value, Param, Format => Newick_Tree));
      else
        Put("(");
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          Put_Node_Newick(Next(Ch));
          if Has_Next(Ch) then
            Put(",");
          end if;
        end loop;
        Restore(Ch);
        Put(")");
        Put(To_S(Nod.Value, Param, Format => Newick_Tree));
      end if;
    end Put_Node_Newick;

    procedure Put_Node_Json(Nod: in Node; Indent: in out Natural) is
      Ch: List_Of_Nodes;
      Nch: Natural;
    begin
      Ch := Nod.Children;
      Nch := Size(Ch);
      if Nch = 0 then
        Put(Right_Justify("", Indent));
        Put("{" & To_S(Nod.Value, Param, Format => Json_Tree) & ", ""size"": 1}");
      else
        Put_Line(Right_Justify("", Indent) & "{");
        Put_Line(Right_Justify("", Indent) & " " & To_S(Nod.Value, Param, Format => Json_Tree) & ",");
        Put_Line(Right_Justify("", Indent) & " ""children"": [");
        Indent := Indent + 2;
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          Put_Node_Json(Next(Ch), Indent);
          if Has_Next(Ch) then
            Put(",");
          end if;
          New_Line;
        end loop;
        Restore(Ch);
        Indent := Indent - 2;
        Put_Line(Right_Justify("", Indent) & " ]");
        Put(Right_Justify("", Indent) & "}");
      end if;
    end Put_Node_Json;

    Ft: File_Type;
    Ft_Prev: File_Access;
    Indent: Natural := 0;
  begin
    if Is_Empty(T) then
      raise Empty_Tree_Error;
    end if;

    if Fn /= "" then
      if Mode = Out_File then
        Create(Ft, Mode, Fn);
      elsif Mode = Append_File then
        Open_Or_Create(Ft, Fn);
      end if;
      Ft_Prev := Current_Output;
      Set_Output(Ft);
    end if;

    case Format is
      when Text_Tree =>
        Put_Node_Text(T, Indent);
      when Newick_Tree =>
        Put_Node_Newick(T);
        Put_Line(";");
      when Json_Tree =>
        Put_Node_Json(T, Indent);
        New_Line;
    end case;

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Put_Tree;

  --------------
  -- Get_Tree --
  --------------

  procedure Get_Tree(T: out Tree) is
  begin
    Get_Tree("", T);
  end Get_Tree;

  --------------
  -- Get_Tree --
  --------------

  procedure Get_Tree(Ft: in File_Type; T: out Tree) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Tree("", T);
    Set_Input(Ft_Prev.all);
  end Get_Tree;

  --------------
  -- Get_Tree --
  --------------

  procedure Get_Tree(Fn: in String; T: out Tree) is

    procedure Get_Node_Text(St: in out Subtree) is
      C: Character;
      Val: Node_Value;
      Nch: Natural;
      Nod: Node;
    begin
      Comments_Skip;
      if not End_Of_Line then
        Get(C);
        if C = '*' then
          Nch := 0;
          Line_Spaces_Skip;
          Get(Val, Format => Text_Tree);
        elsif C = '+' then
          Line_Spaces_Skip;
          Get(Nch);
          Line_Spaces_Skip;
          Get(Val, Format => Text_Tree);
        else
          raise Tree_IO_Error with "Lines require an initial '+' or '*'";
        end if;
        if not End_Of_File then
          Skip_Line;
        end if;
        if St = null then
          St := New_Tree(Val);
          Nod := St;
        else
          Nod := Add_Child(St, Val);
        end if;
        for I in 1..Nch loop
          Get_Node_Text(Nod);
        end loop;
      end if;
    end Get_Node_Text;

    procedure Get_Node_Newick(St: in out Subtree) is
      C: Character;
      Eol: Boolean;
      Val: Node_Value;
      Nod: Node;
    begin
      Look_Ahead(C, Eol);
      if Eol then
        raise Tree_IO_Error;
      elsif C = '(' then
        Get(C);
        if St = null then
          St := New_Tree;
          Nod := St;
        else
          Nod := Add_Child(St);
        end if;
        while C /= ')' loop
          Look_Ahead(C, Eol);
          if Eol then
            raise Tree_IO_Error with "Input ended prematurely";
          elsif C = ',' then
            Add_Child(Nod);
          elsif C /= '(' then
            Get(Val, Format => Newick_Tree);
            Add_Child(Nod, Val);
          else -- C = '('
            Get_Node_Newick(Nod);
          end if;
          Look_Ahead(C, Eol);
          if Eol then
            raise Tree_IO_Error with "Input ended prematurely";
          elsif C = ',' then
            Get(C);
          end if;
        end loop;
        Get(C);
        Look_Ahead(C, Eol);
        if Eol then
          raise Tree_IO_Error with "Input ended prematurely";
        elsif C /= ')' and C /= ',' and C /= ';' then
          Get(Val, Format => Newick_Tree);
          Set_Value(Nod, Val);
        end if;
      end if;
    end Get_Node_Newick;

    procedure Get_Node_Json(St: in out Subtree) is
      C: Character;
      Eol: Boolean;
      Val: Node_Value;
      Nod: Node;
      Us: Ustring;
    begin
      Comments_Skip;
      Look_Ahead(C, Eol);
      if Eol then
        raise Tree_IO_Error;
      elsif C = '{' then
        Get(C);
        Comments_Skip;
        Get(Val, Format => Json_Tree);
        if St = null then
          St := New_Tree(Val);
          Nod := St;
        else
          Nod := Add_Child(St, Val);
        end if;
        loop
          Comments_Skip;
          Look_Ahead(C, Eol);
          if Eol then
            raise Tree_IO_Error with "Input ended prematurely";
          end if;
          Get(C);
          if C = ',' then
            Comments_Skip;
            Get_Quoted_Word(Us);
            Comments_Skip;
            if not Separator_Skip(':', Strict => True) then
              raise Tree_IO_Error with "Missing ':' after field named '" & U2S(Us) & "'";
            end if;
            if U2S(To_Lowercase(Us)) = "children" then
              Comments_Skip;
              if not Separator_Skip('[', Strict => True) then
                raise Tree_IO_Error with "Missing '[' after 'children' field";
              end if;
              loop
                Get_Node_Json(Nod);
                Comments_Skip;
                Look_Ahead(C, Eol);
                if Eol then
                  raise Tree_IO_Error with "Input ended prematurely";
                end if;
                Get(C);
                if C = ']' then
                  exit;
                elsif C /= ',' then
                  raise Tree_IO_Error with "Missing ',' or ']' after reading a child";
                end if;
              end loop;
            else
              Get_Word(Us);
            end if;
          elsif C = '}' then
            exit;
          else
            raise Tree_IO_Error with "Missing ',' or '}' after reading node value";
          end if;
        end loop;
      end if;
    end Get_Node_Json;

    Ft: File_Type;
    Ft_Prev: File_Access;
    C: Character;
    Eol: Boolean;
    Separators_Old   : constant Characters := Get_Separators;
    Separators_Text  : constant Characters(1..1) := (1 => ' ');
    Separators_Newick: constant Characters := ('(', ')', ',', ';');
    Separators_Json  : constant Characters := ('{', '}', '[', ']', ',', ':');
    Comments_Old     : constant Characters := Get_Comments;
    Comments_Text    : constant Characters(1..1) := (1 => '#');
    Comments_Newick  : constant Characters(1..1) := (1 => '#');
    Comments_Json    : constant Characters(1..1) := (1 => '#');
  begin
    if Fn /= "" then
      Open(Ft, In_File, Fn);
      Ft_Prev := Current_Input;
      Set_Input(Ft);
    end if;

    T := null;

    Comments_Skip;
    if not End_Of_File then
      Look_Ahead(C, Eol);
      case C is
        when '+' | '*' =>
          Set_Separators(Separators_Text);
          Set_Comments(Comments_Text);
          Get_Node_Text(T);
          Set_Comments(Comments_Old);
          Set_Separators(Separators_Old);
        when '(' =>
          Set_Separators(Separators_Newick);
          Set_Comments(Comments_Newick);
          Get_Node_Newick(T);
          Set_Comments(Comments_Old);
          Set_Separators(Separators_Old);
        when '{' =>
          Set_Separators(Separators_Json);
          Set_Comments(Comments_Json);
          Get_Node_Json(T);
          Set_Comments(Comments_Old);
          Set_Separators(Separators_Old);
        when others =>
          raise Tree_IO_Error with "Trees start with '*' or '+' in Text format, '(' in Newick format or '{' in JSON format";
      end case;
    end if;

    if Fn /= "" then
      Close(Ft);
      Set_Input(Ft_Prev.all);
    end if;
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Error reading tree : " & Name(Ft));
      Put_Line("Current position : line=" & I2S(Integer(Line(Ft))) & ", col=" & I2S(Integer(Col(Ft))));
      raise Tree_IO_Error;
  end Get_Tree;

end Trees.IO;
