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


-- @filename Dendrograms_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 01/02/2016
-- @brief Input and Output of Dendrograms

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Utils.IO; use Utils.IO;
with Dendrograms.Structure; use Dendrograms.Structure;

package body Dendrograms.IO is

  ----------
  -- To_S --
  ----------

  function To_S(Nod_Inf: in Node_Info; Precision: in Natural; Format: in Tree_Format) return String is

    function Trans(U: in Ustring) return Ustring is
    begin
      return S2U(Translate(U2S(U), " ,;:|[](){}", "_____<><><>"));
    end Trans;

    Name, Us: Ustring;
    Position, Height, Width, Length, Margin: Double;
    Is_Leaf: Boolean;
    Num_Leaves: Natural;
    Id: Integer;
  begin
    Get_Node_Info(Nod_Inf, Name, Position, Height, Width, Length, Margin, Is_Leaf, Num_Leaves, Id);
    case Format is
      when Text_Tree =>
        if Is_Leaf then
          Us := Trans(Name);
        else
          Us := S2U("[" & D2Se0(Height, Aft => Precision) & ", " & D2Se0(Height + Margin, Aft => Precision) & "]");
          Us := Us & S2U("  " & I2S(Num_Leaves));
          if Name /= Null_Ustring then
            Us := Us & S2U("  ") & Trans(Name);
          end if;
        end if;
      when Newick_Tree =>
        Us := Trans(Name) & S2U(":" & D2Se0(abs Length, Aft => Precision));
      when Json_Tree =>
        Us := """name"": """ & Trans(Name) & """"
              & ", ""height"": " & D2Se0(abs Height, Aft => Precision)
              & ", ""margin"": " & D2Se0(abs Margin, Aft => Precision)
              & ", ""length"": " & D2Se0(abs Length, Aft => Precision);
    end case;
    return U2S(Us);
  end To_S;

  -------------------
  -- Get_Node_Info --
  -------------------

  procedure Get_Node_Info(Nod_Inf: out Node_Info; Format: in Tree_Format) is
    Us, Key, Value: Ustring;
    Name: Ustring;
    Height, Length, Margin: Double;
    Num_Leaves: Natural;
    C: Character;
    Eol, Has_Name, Has_Length: Boolean;
    D: Double;
  begin
    Nod_Inf := Void_Node_Info;
    case Format is
      when Text_Tree =>
        Look_Ahead(C, Eol);
        if Eol then
          Set_Leaf(Nod_Inf, True);
        elsif C = '[' then
          Separator_Skip('[');
          Get_Double(Height);
          Separator_Skip(',');
          Get_Double(D);
          Separator_Skip(']');
          Get_Word(Us);
          if Is_Integer(Us) then
            Num_Leaves := U2I(Us);
            Line_Spaces_Skip;
            Get_Word(Us);
          else
            Num_Leaves := 0;
          end if;
          Name := Us;
          Margin := D - Height;
          Set_Leaf(Nod_Inf, False);
          Set_Height(Nod_Inf, Height);
          Set_Margin(Nod_Inf, Margin);
          Set_Num_Leaves(Nod_Inf, Num_Leaves);
          Set_Name(Nod_Inf, Name);
        else
          Get_Word(Name);
          Set_Leaf(Nod_Inf, True);
          Set_Num_Leaves(Nod_Inf, 1);
          Set_Name(Nod_Inf, Name);
        end if;
      when Newick_Tree =>
        Look_Ahead(C, Eol);
        if C = ':' then
          Separator_Skip(':');
          Get_Double(Length, ')');
          Length := -Length;
          Set_Length(Nod_Inf, Length);
        else
          Get_Word(Name, ':');
          Set_Name(Nod_Inf, Name);
          Look_Ahead(C, Eol);
          if C = ':' then
            Separator_Skip(':');
            Get_Double(Length, ')');
            Length := -Length;
            Set_Length(Nod_Inf, Length);
          end if;
        end if;
      when Json_Tree =>
        Has_Name := False;
        Has_Length := False;
        loop
          Get_Pair(Key, Value, ':');
          if U2S(To_Lowercase(Key)) = "name" then
            Set_Name(Nod_Inf, Value);
            Has_Name := True;
          elsif U2S(To_Lowercase(Key)) = "height" then
            Set_Height(Nod_Inf, U2D(Value));
          elsif U2S(To_Lowercase(Key)) = "margin" then
            Set_Margin(Nod_Inf, U2D(Value));
          elsif U2S(To_Lowercase(Key)) = "length" then
            Set_Length(Nod_Inf, U2D(Value));
            Has_Length := True;
          end if;
          exit when Has_Name and Has_Length;  -- Height and Margin must be between Name and Length
          Comments_Skip;
          if not Separator_Skip(',', Strict => True) then
            raise Trees_IO_Dendro.Tree_IO_Error with "Name field not found";
          end if;
        end loop;
    end case;
  end Get_Node_Info;

  --------------------
  -- Get_Dendrogram --
  --------------------

  procedure Get_Dendrogram(T: out Dendrogram) is
  begin
    Get_Dendrogram("", T);
  end Get_Dendrogram;

  --------------------
  -- Get_Dendrogram --
  --------------------

  procedure Get_Dendrogram(Ft: in File_Type; T: out Dendrogram) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Input;
    Set_Input(Ft);
    Get_Dendrogram("", T);
    Set_Input(Ft_Prev.all);
  end Get_Dendrogram;

  --------------------
  -- Get_Dendrogram --
  --------------------

  procedure Get_Dendrogram(Fn: in String; T: out Dendrogram) is
    Format: Tree_Format;
    Ft: File_Type;
    Ft_Prev: File_Access;
    C: Character;
    Eol: Boolean;
  begin
    -- Get Tree Format
    if Fn /= "" then
      Open(Ft, In_File, Fn);
      Ft_Prev := Current_Input;
      Set_Input(Ft);
    end if;

    Comments_Skip;
    if not End_Of_File then
      Look_Ahead(C, Eol);
      case C is
        when '+' | '*' =>
          Format := Text_Tree;
        when '(' =>
          Format := Newick_Tree;
        when '{' =>
          Format := Json_Tree;
        when others =>
          raise Trees_IO_Dendro.Tree_IO_Error with "Trees start with '*' or '+' in Text format, or '(' in Newick format";
      end case;
    end if;

    if Fn /= "" then
      Close(Ft);
      Set_Input(Ft_Prev.all);
    end if;

    -- Get Tree
    Get_Tree(Fn, T);

    -- Dendrogram information completion
    case Format is
      when Text_Tree =>
        Complete_Information_Text(T);
      when Newick_Tree =>
        Complete_Information_Newick(T);
      when Json_Tree =>
        Complete_Information_Json(T);
    end case;
    Set_Positions_And_Widths(T);

  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & "Error reading dendrogram : " & Name(Ft));
      Put_Line("Current position : line=" & I2S(Integer(Line(Ft))) & ", col=" & I2S(Integer(Col(Ft))));
      raise Trees_IO_Dendro.Tree_IO_Error;
  end Get_Dendrogram;

  ------------------------------
  -- Put_Dendrogram_Structure --
  ------------------------------

  procedure Put_Dendrogram_Structure(T: in Dendrogram; Aft: in Field := Default_Double_Aft) is
  begin
    Put_Dendrogram_Structure("", T, Aft => Aft);
  end Put_Dendrogram_Structure;

  ------------------------------
  -- Put_Dendrogram_Structure --
  ------------------------------

  procedure Put_Dendrogram_Structure(Ft: in File_Type; T: in Dendrogram; Aft: in Field := Default_Double_Aft) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Output;
    Set_Output(Ft);
    Put_Dendrogram_Structure("", T, Aft => Aft);
    Set_Output(Ft_Prev.all);
  end Put_Dendrogram_Structure;

  ------------------------------
  -- Put_Dendrogram_Structure --
  ------------------------------

  procedure Put_Dendrogram_Structure(Fn: in String; T: in Dendrogram; Mode: in File_Mode := Out_File; Aft: in Field := Default_Double_Aft) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    Ds: Dendrogram_Structure;
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

    -- Put_Line("#Name" & Htab & "Pos"    & Htab & "Height" & Htab & "Width" & Htab & "Length"
    --                  & Htab & "Margin" & Htab & "Kind"   & Htab & "Id"    & Htab & "Parent_Id");
    Ds := Get_Dendrogram_Structure(T);
    for I in Ds'Range loop
      Put(U2S(Ds(I).Name));
      Put(Htab & D2Se0(Ds(I).Position, Aft => Aft));
      Put(Htab & D2Se0(Ds(I).Height, Aft => Aft));
      Put(Htab & D2Se0(Ds(I).Width, Aft => Aft));
      Put(Htab & D2Se0(Ds(I).Length, Aft => Aft));
      Put(Htab & D2Se0(Ds(I).Margin, Aft => Aft));
      Put(Htab & Capitalize(Node_Type'Image(Ds(I).Kind)));
      Put(Htab & I2S(Ds(I).Node_Id));
      Put(Htab & I2S(Ds(I).Parent_Id));
      New_Line;
    end loop;
    Free(Ds);

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Put_Dendrogram_Structure;

  ------------------------------
  -- Put_Dendrogram_Plot_Info --
  ------------------------------

  procedure Put_Dendrogram_Plot_Info(T: in Dendrogram; Include_Bands: Boolean := True; Aft: in Field := Default_Double_Aft) is
  begin
    Put_Dendrogram_Plot_Info("", T, Include_Bands, Aft => Aft);
  end Put_Dendrogram_Plot_Info;

  ------------------------------
  -- Put_Dendrogram_Plot_Info --
  ------------------------------

  procedure Put_Dendrogram_Plot_Info(Ft: in File_Type; T: in Dendrogram; Include_Bands: Boolean := True; Aft: in Field := Default_Double_Aft) is
    Ft_Prev: File_Access;
  begin
    Ft_Prev := Current_Output;
    Set_Output(Ft);
    Put_Dendrogram_Plot_Info("", T, Include_Bands, Aft => Aft);
    Set_Output(Ft_Prev.all);
  end Put_Dendrogram_Plot_Info;

  ------------------------------
  -- Put_Dendrogram_Plot_Info --
  ------------------------------

  procedure Put_Dendrogram_Plot_Info(Fn: in String; T: in Dendrogram; Include_Bands: Boolean := True; Mode: in File_Mode := Out_File; Aft: in Field := Default_Double_Aft) is
    Ft: File_Type;
    Ft_Prev: File_Access;
    Dp: Dendrogram_Plot_Info;
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

    -- Put_Line("#kind" & Htab & "Id" & Htab & "Pos1" & Htab & "Height1" & Htab & "Pos2" & Htab & "Height2" & Htab & "Name");
    Dp := Get_Dendrogram_Plot_Info(T, Include_Bands);
    for I in Dp'Range loop
      Put(Capitalize(Plot_Info_Type'Image(Dp(I).Kind)));
      Put(Htab & I2S(Dp(I).Id));
      Put(Htab & D2Se0(Dp(I).Position1, Aft => Aft));
      Put(Htab & D2Se0(Dp(I).Height1, Aft => Aft));
      Put(Htab & D2Se0(Dp(I).Position2, Aft => Aft));
      Put(Htab & D2Se0(Dp(I).Height2, Aft => Aft));
      Put(Htab & U2S(Dp(I).Name));
      New_Line;
    end loop;
    Free(Dp);

    if Fn /= "" then
      Close(Ft);
      Set_Output(Ft_Prev.all);
    end if;
  end Put_Dendrogram_Plot_Info;

  -------------------------------
  -- Complete_Information_Text --
  -------------------------------

  procedure Complete_Information_Text(T: in Dendrogram) is

    Min_H: Double := Double'Last;
    Max_H: Double := Double'First;
    Num_Hp_Gt_Hn, Num_Hp_Lt_Hn: Natural := 0;
    H_Root, H_Leaves: Double;

    procedure Update_Internal_Node_Info(Nod: in Node) is
      Parent_Inf, Nod_Inf: Node_Info;
      Hp, Hn, Lnp: Double;
    begin
      -- Set Length of Internal Nodes, determine Max and Min Heights, and decide if Proximities Dendrogram
      if Is_Root(Nod) then
        Nod_Inf := Value(Nod);
        Hn := Get_Height(Nod_Inf);
        if Min_H > Hn then
          Min_H := Hn;
        end if;
        if Max_H < Hn then
          Max_H := Hn;
        end if;
      elsif Is_Internal(Nod) then
        Parent_Inf := Value(Get_Parent(Nod));
        Nod_Inf := Value(Nod);
        Hp := Get_Height(Parent_Inf);
        Hn := Get_Height(Nod_Inf);
        Lnp := Hp - Hn;
        Set_Length(Nod_Inf, Lnp);
        Set_Value(Nod, Nod_Inf);
        if Min_H > Hn then
          Min_H := Hn;
        end if;
        if Max_H < Hn then
          Max_H := Hn;
        end if;
        if Hp > Hn then
          Num_Hp_Gt_Hn := Num_Hp_Gt_Hn + 1;
        elsif Hp < Hn then
          Num_Hp_Lt_Hn := Num_Hp_Lt_Hn + 1;
        end if;
      end if;
    end Update_Internal_Node_Info;

    procedure Update_Leaf_Info(Nod: in Node) is
      Parent_Inf, Nod_Inf: Node_Info;
      Hp, Lnp: Double;
    begin
      -- Set Height and Length of Leaves
      if Has_Parent(Nod) and Is_Leaf(Nod) then
        Parent_Inf := Value(Get_Parent(Nod));
        Nod_Inf := Value(Nod);
        Hp := Get_Height(Parent_Inf);
        Lnp := Hp - H_Leaves;
        Set_Height(Nod_Inf, H_Leaves);
        Set_Length(Nod_Inf, Lnp);
        Set_Value(Nod, Nod_Inf);
      end if;
    end Update_Leaf_Info;

    procedure Update_Internal_Nodes is new Generic_Depth_First_Preorder_Traversal(Update_Internal_Node_Info);
    procedure Update_Leaves is new Generic_Depth_First_Preorder_Traversal(Update_Leaf_Info);

  begin
    Update_Internal_Nodes(T);

    H_Root := Get_Height(Value(T));
    if H_Root = Max_H then
      H_Leaves := 0.0;
    elsif H_Root = Min_H then
      H_Leaves := Max_H + 0.05 * (Max_H - Min_H);
    elsif Num_Hp_Gt_Hn >= Num_Hp_Lt_Hn then
      H_Leaves := 0.0;
    else
      H_Leaves := Max_H + 0.05 * (Max_H - Min_H);
    end if;

    Update_Leaves(T);
    Set_Id(T);
    Set_Num_Leaves(T);
  end Complete_Information_Text;

  ---------------------------------
  -- Complete_Information_Newick --
  ---------------------------------

  procedure Complete_Information_Newick(T: in Dendrogram) is
  begin
    Set_Heights(T);
    Set_Id(T);
    Set_Num_Leaves(T);
  end Complete_Information_Newick;

  ------------------------------------------
  -- Complete_Information_Json --
  ------------------------------------------

  procedure Complete_Information_Json(T: in Dendrogram) is

    use Nodes_Lists;

    function Has_Heights(T: in Dendrogram) return Boolean is
      Ln: List_Of_Nodes;
      Hr: Double;
      Has: Boolean;
    begin
      Has := False;
      Hr := Get_Height(Value(T));
      if Hr /= Get_Height(Void_Node_Info) then
        Has := True;
      else
        Get_Leaves(T, Ln);
        Save(Ln);
        Reset(Ln);
        while Has_Next(Ln) loop
          if Get_Height(Value(Next(Ln))) /= Hr then
            Has := True;
            exit;
          end if;
        end loop;
        Restore(Ln);
        Free(Ln);
      end if;
      return Has;
    end Has_Heights;

  begin
    if not Has_Heights(T) then
      Set_Heights(T);
    end if;
    Set_Id(T);
    Set_Num_Leaves(T);
  end Complete_Information_Json;

  ------------
  -- Set_Id --
  ------------

  procedure Set_Id(T: in Dendrogram) is

    Id: Integer := 0;

    procedure Update_Leaf_Id(Nod: in Node) is
      Nod_Inf: Node_Info;
    begin
      if Is_Leaf(Nod) then
        Id := Id + 1;
        Nod_Inf := Value(Nod);
        Set_Id(Nod_Inf, Id);
        Set_Value(Nod, Nod_Inf);
      end if;
    end Update_Leaf_Id;

    procedure Update_Internal_Node_Id(Nod: in Node) is
      Nod_Inf: Node_Info;
    begin
      if not Is_Leaf(Nod) then
        Id := Id + 1;
        Nod_Inf := Value(Nod);
        Set_Id(Nod_Inf, Id);
        Set_Value(Nod, Nod_Inf);
      end if;
    end Update_Internal_Node_Id;

    procedure Update_Leaves is new Generic_Depth_First_Postorder_Traversal(Update_Leaf_Id);
    procedure Update_Internal_Nodes is new Generic_Depth_First_Postorder_Traversal(Update_Internal_Node_Id);

  begin
    Update_Leaves(T);
    Update_Internal_Nodes(T);
  end Set_Id;

  --------------------
  -- Set_Num_Leaves --
  --------------------

  procedure Set_Num_Leaves(T: in Dendrogram) is

    procedure Reset_Node_Num_Leaves(Nod: in Node) is
      Nod_Inf: Node_Info;
      Num_Leaves: Natural;
    begin
      Nod_Inf := Value(Nod);
      if Is_Leaf(Nod) then
        Num_Leaves := 1;
      else
        Num_Leaves := 0;
      end if;
      Set_Leaf(Nod_Inf, Is_Leaf(Nod));
      Set_Num_Leaves(Nod_Inf, Num_Leaves);
      Set_Value(Nod, Nod_Inf);
    end Reset_Node_Num_Leaves;

    procedure Update_Parent_Num_Leaves(Nod: in Node) is
      Parent_Nod: Node;
      Parent_Inf: Node_Info;
      Num_Leaves: Natural;
    begin
      if not Is_Root(Nod) then
        Parent_Nod := Get_Parent(Nod);
        Parent_Inf := Value(Parent_Nod);
        Num_Leaves := Get_Num_Leaves(Parent_Inf) + Get_Num_Leaves(Value(Nod));
        Set_Num_Leaves(Parent_Inf, Num_Leaves);
        Set_Value(Parent_Nod, Parent_Inf);
      end if;
    end Update_Parent_Num_Leaves;

    procedure Reset_Nodes is new Generic_Depth_First_Postorder_Traversal(Reset_Node_Num_Leaves);
    procedure Update_Nodes is new Generic_Depth_First_Postorder_Traversal(Update_Parent_Num_Leaves);

  begin
    Reset_Nodes(T);
    Update_Nodes(T);
  end Set_Num_Leaves;

  -----------------
  -- Set_Heights --
  -----------------

  procedure Set_Heights(T: in Dendrogram) is

    procedure Update_Node_Info(Nod: in Node) is
      Parent_Inf, Nod_Inf: Node_Info;
      Lnp, Hp, Hn: Double;
    begin
      -- Set as Height the distance to Root
      Nod_Inf := Value(Nod);
      if Is_Root(Nod) then
        Hn := 0.0;
        Set_Height(Nod_Inf, Hn);
      else
        Parent_Inf := Value(Get_Parent(Nod));
        Hp := Get_Height(Parent_Inf);
        Lnp := Get_Length(Nod_Inf);
        Hn := Hp - Lnp;
        Set_Height(Nod_Inf, Hn);
      end if;
      Set_Value(Nod, Nod_Inf);
    end Update_Node_Info;

    procedure Update_Nodes is new Generic_Depth_First_Preorder_Traversal(Update_Node_Info);

  begin
    Update_Nodes(T);
  end Set_Heights;

end Dendrograms.IO;
