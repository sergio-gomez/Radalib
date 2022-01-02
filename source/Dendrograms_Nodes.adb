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


-- @filename Dendrograms_Nodes.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/05/2013
-- @revision 09/03/2015
-- @brief Definition of Nodes Info for Dendrograms

package body Dendrograms_Nodes is

  -------------------
  -- Set_Node_Info --
  -------------------

  procedure Set_Node_Info(Nod_Inf: out Node_Info; Name: in String; Position, Height, Width, Length, Margin: in Double; Is_Leaf: in Boolean; Num_Leaves: in Natural; Id: in Integer) is
  begin
    Nod_Inf := (S2U(Name), Position, Height, Width, Length, Margin, Is_Leaf, Num_Leaves, Id);
    if Position < 0.0 or Width < 0.0 then
      raise Node_Info_Error;
    end if;
    if Is_Leaf and Width /= 0.0 then
      raise Node_Info_Error;
    end if;
  end Set_Node_Info;

  -------------------
  -- Set_Node_Info --
  -------------------

  procedure Set_Node_Info(Nod_Inf: out Node_Info; Name: in Ustring; Position, Height, Width, Length, Margin: in Double; Is_Leaf: in Boolean; Num_Leaves: in Natural; Id: in Integer) is
  begin
    Nod_Inf := (Name, Position, Height, Width, Length, Margin, Is_Leaf, Num_Leaves, Id);
    if Position < 0.0 or Width < 0.0 then
      raise Node_Info_Error;
    end if;
    if Is_Leaf and Width /= 0.0 then
      raise Node_Info_Error;
    end if;
  end Set_Node_Info;

  -------------------
  -- Get_Node_Info --
  -------------------

  procedure Get_Node_Info(Nod_Inf: in Node_Info; Name: out String; Position, Height, Width, Length, Margin: out Double; Is_Leaf: out Boolean; Num_Leaves: out Natural; Id: out Integer) is
  begin
    Name       := U2S(Nod_Inf.Name);
    Position   := Nod_Inf.Position;
    Height     := Nod_Inf.Height;
    Width      := Nod_Inf.Width;
    Length     := Nod_Inf.Length;
    Margin     := Nod_Inf.Margin;
    Is_Leaf    := Nod_Inf.Is_Leaf;
    Num_Leaves := Nod_Inf.Num_Leaves;
    Id         := Nod_Inf.Id;
  end Get_Node_Info;

  -------------------
  -- Get_Node_Info --
  -------------------

  procedure Get_Node_Info(Nod_Inf: in Node_Info; Name: out Ustring; Position, Height, Width, Length, Margin: out Double; Is_Leaf: out Boolean; Num_Leaves: out Natural; Id: out Integer) is
  begin
    Name       := Nod_Inf.Name;
    Position   := Nod_Inf.Position;
    Height     := Nod_Inf.Height;
    Width      := Nod_Inf.Width;
    Length     := Nod_Inf.Length;
    Margin     := Nod_Inf.Margin;
    Is_Leaf    := Nod_Inf.Is_Leaf;
    Num_Leaves := Nod_Inf.Num_Leaves;
    Id         := Nod_Inf.Id;
  end Get_Node_Info;

  --------------
  -- Set_Name --
  --------------

  procedure Set_Name(Nod_Inf: out Node_Info; Name: in String) is
  begin
    Nod_Inf.Name := S2U(Name);
  end Set_Name;

  --------------
  -- Set_Name --
  --------------

  procedure Set_Name(Nod_Inf: out Node_Info; Name: in Ustring) is
  begin
    Nod_Inf.Name := Name;
  end Set_Name;

  --------------
  -- Get_Name --
  --------------

  function Get_Name(Nod_Inf: in Node_Info) return String is
  begin
    return U2S(Nod_Inf.Name);
  end Get_Name;

  --------------
  -- Get_Name --
  --------------

  function Get_Name(Nod_Inf: in Node_Info) return Ustring is
  begin
    return Nod_Inf.Name;
  end Get_Name;

  ------------------
  -- Set_Position --
  ------------------

  procedure Set_Position(Nod_Inf: out Node_Info; Position: in Double) is
  begin
    Nod_Inf.Position:= Position;
    if Position < 0.0 then
      raise Node_Info_Error;
    end if;
  end Set_Position;

  ------------------
  -- Get_Position --
  ------------------

  function Get_Position(Nod_Inf: in Node_Info) return Double is
  begin
    return Nod_Inf.Position;
  end Get_Position;

  ----------------
  -- Set_Height --
  ----------------

  procedure Set_Height(Nod_Inf: out Node_Info; Height: in Double) is
  begin
    Nod_Inf.Height := Height;
  end Set_Height;

  ----------------
  -- Get_Height --
  ----------------

  function Get_Height(Nod_Inf: in Node_Info) return Double is
  begin
    return Nod_Inf.Height;
  end Get_Height;

  ---------------
  -- Set_Width --
  ---------------

  procedure Set_Width(Nod_Inf: out Node_Info; Width: in Double) is
  begin
    Nod_Inf.Width:= Width;
    if Width < 0.0 or (Nod_Inf.Is_Leaf and Width /= 0.0) then
      raise Node_Info_Error;
    end if;
  end Set_Width;

  ---------------
  -- Get_Width --
  ---------------

  function Get_Width(Nod_Inf: in Node_Info) return Double is
  begin
    return Nod_Inf.Width;
  end Get_Width;

  ----------------
  -- Set_Length --
  ----------------

  procedure Set_Length(Nod_Inf: out Node_Info; Length: in Double) is
  begin
    Nod_Inf.Length:= Length;
  end Set_Length;

  ----------------
  -- Get_Length --
  ----------------

  function Get_Length(Nod_Inf: in Node_Info) return Double is
  begin
    return Nod_Inf.Length;
  end Get_Length;

  ----------------
  -- Set_Margin --
  ----------------

  procedure Set_Margin(Nod_Inf: out Node_Info; Margin: in Double) is
  begin
    Nod_Inf.Margin := Margin;
  end Set_Margin;

  ----------------
  -- Get_Margin --
  ----------------

  function Get_Margin(Nod_Inf: in Node_Info) return Double is
  begin
    return Nod_Inf.Margin;
  end Get_Margin;

  --------------
  -- Set_Leaf --
  --------------

  procedure Set_Leaf(Nod_Inf: out Node_Info; Is_Leaf: in Boolean) is
  begin
    Nod_Inf.Is_Leaf := Is_Leaf;
  end Set_Leaf;

  --------------
  -- Set_Leaf --
  --------------

  function Is_Leaf(Nod_Inf: in Node_Info) return Boolean is
  begin
    return Nod_Inf.Is_Leaf;
  end Is_Leaf;

  --------------------
  -- Set_Num_Leaves --
  --------------------

  procedure Set_Num_Leaves(Nod_Inf: out Node_Info; Num_Leaves: in Natural) is
  begin
    Nod_Inf.Num_Leaves := Num_Leaves;
  end Set_Num_Leaves;

  --------------------
  -- Get_Num_Leaves --
  --------------------

  function Get_Num_Leaves(Nod_Inf: in Node_Info) return Natural is
  begin
    return Nod_Inf.Num_Leaves;
  end Get_Num_Leaves;

  ------------
  -- Set_Id --
  ------------

  procedure Set_Id(Nod_Inf: out Node_Info; Id: in Integer) is
  begin
    Nod_Inf.Id := Id;
  end Set_Id;

  ------------
  -- Get_Id --
  ------------

  function Get_Id(Nod_Inf: in Node_Info) return Integer is
  begin
    return Nod_Inf.Id;
  end Get_Id;


end Dendrograms_Nodes;
