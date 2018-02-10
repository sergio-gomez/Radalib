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


-- @filename Dendrograms_Nodes.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/05/2013
-- @revision 09/03/2015
-- @brief Definition of Nodes Info for Dendrograms

with Utils; use Utils;

package Dendrograms_Nodes is

  -- Definitions
  type Node_Info is private;

  Void_Node_Info: constant Node_Info;

  Node_Info_Error: exception;

  -- Purpose : Set the characteristics of a Node in a Dendrogram
  -- Note    : Height may be negative in Dendrograms of Similarities with UC, WC and WR
  -- Note    : Width is zero for Leaves
  -- Note    : Position and Width cannot be negative
  -- Note    : Length maybe negative with Similarities or in Inversions with Distances
  -- Note    : Margin is negative or zero in Dendrograms of Similarities
  --
  -- Nod_Inf : The Node Info
  -- Name    : The Name
  -- Position: The Position
  -- Height  : The Height
  -- Width   : The Width
  -- Length  : The Length to its Parent
  -- Margin  : The Margin
  -- Is_Leaf : The Leaf status
  -- Num_Leaves: The Number of Leaves in the Node's Subtree
  -- Id      : The Id
  -- raises  : Node_Error
  procedure Set_Node_Info(Nod_Inf: out Node_Info; Name: in String; Position, Height, Width, Length, Margin: in Double; Is_Leaf: in Boolean; Num_Leaves: in Natural; Id: in Integer);
  procedure Set_Node_Info(Nod_Inf: out Node_Info; Name: in Ustring; Position, Height, Width, Length, Margin: in Double; Is_Leaf: in Boolean; Num_Leaves: in Natural; Id: in Integer);

  -- Purpose : Get the characteristics of a Node in a Dendrogram
  --
  -- Nod_Inf : The Node Info
  -- Name    : The Name
  -- Position: The Position
  -- Height  : The Height
  -- Width   : The Width
  -- Length  : The Length to its Parent
  -- Margin  : The Margin
  -- Is_Leaf : The Leaf status
  -- Num_Leaves: The Number of Leaves in the Node's Subtree
  -- Id      : The Id
  -- raises  : Node_Error
  procedure Get_Node_Info(Nod_Inf: in Node_Info; Name: out String; Position, Height, Width, Length, Margin: out Double; Is_Leaf: out Boolean; Num_Leaves: out Natural; Id: out Integer);
  procedure Get_Node_Info(Nod_Inf: in Node_Info; Name: out Ustring; Position, Height, Width, Length, Margin: out Double; Is_Leaf: out Boolean; Num_Leaves: out Natural; Id: out Integer);

  -- Purpose : Set the Name of a Node
  --
  -- Nod_Inf : The Node Info
  -- Name    : The Name
  procedure Set_Name(Nod_Inf: out Node_Info; Name: in String);
  procedure Set_Name(Nod_Inf: out Node_Info; Name: in Ustring);

  -- Purpose : Get the Name of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Name
  function Get_Name(Nod_Inf: in Node_Info) return String;
  function Get_Name(Nod_Inf: in Node_Info) return Ustring;

  -- Purpose : Set the Position of a Node
  -- Note    : Position cannot be negative
  --
  -- Nod_Inf : The Node Info
  -- Position: The Position
  -- raises  : Node_Error
  procedure Set_Position(Nod_Inf: out Node_Info; Position: in Double);

  -- Purpose : Get the Position of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Position
  function Get_Position(Nod_Inf: in Node_Info) return Double;

  -- Purpose : Set the Height of a Node
  -- Note    : Height may be negative in Dendrograms of Similarities with UC, WC and WR
  --
  -- Nod_Inf : The Node Info
  -- Height  : The Height
  procedure Set_Height(Nod_Inf: out Node_Info; Height: in Double);

  -- Purpose : Get the Height of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Height
  function Get_Height(Nod_Inf: in Node_Info) return Double;

  -- Purpose : Set the Width of a Node
  -- Note    : Width cannot be negative, and must be zero for Leaves
  --
  -- Nod_Inf : The Node Info
  -- Width   : The Width
  -- raises  : Node_Error
  procedure Set_Width(Nod_Inf: out Node_Info; Width: in Double);

  -- Purpose : Get the Width of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Width
  function Get_Width(Nod_Inf: in Node_Info) return Double;

  -- Purpose : Set the Length of a Node
  -- Note    : Length maybe be negative
  --
  -- Nod_Inf : The Node Info
  -- Length  : The Length to its Parent
  -- raises  : Node_Error
  procedure Set_Length(Nod_Inf: out Node_Info; Length: in Double);

  -- Purpose : Get the Length of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Length
  function Get_Length(Nod_Inf: in Node_Info) return Double;

  -- Purpose : Set the Margin of a Node
  -- Note    : Margin is negative or zero in Dendrograms of Similarities
  --
  -- Nod_Inf : The Node Info
  -- Margin  : The Margin
  procedure Set_Margin(Nod_Inf: out Node_Info; Margin: in Double);

  -- Purpose : Get the Margin of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Margin
  function Get_Margin(Nod_Inf: in Node_Info) return Double;

  -- Purpose : Set a Node its Leaf status
  --
  -- Nod_Inf : The Node Info
  -- Is_Leaf : The Leaf status
  procedure Set_Leaf(Nod_Inf: out Node_Info; Is_Leaf: in Boolean);

  -- Purpose : To know whether a Node is a Leaf
  --
  -- Nod_Inf : The Node Info
  -- return  : True if Leaf
  function Is_Leaf(Nod_Inf: in Node_Info) return Boolean;

  -- Purpose : Set the Number of Leaves in the Node's Subtree
  --
  -- Nod_Inf : The Node Info
  -- Num_Leaves: The Number of Leaves in the Node's Subtree
  procedure Set_Num_Leaves(Nod_Inf: out Node_Info; Num_Leaves: in Natural);

  -- Purpose : Get the Number of Leaves in the Node's Subtree
  --
  -- Nod_Inf : The Node Info
  -- return  : The Number of Leaves in the Node's Subtree
  function Get_Num_Leaves(Nod_Inf: in Node_Info) return Natural;

  -- Purpose : Set the Numeric Id of a Node
  --
  -- Nod_Inf : The Node Info
  -- Id      : The Id
  procedure Set_Id(Nod_Inf: out Node_Info; Id: in Integer);

  -- Purpose : Get the Numeric Id of a Node
  --
  -- Nod_Inf : The Node Info
  -- return  : The Id
  function Get_Id(Nod_Inf: in Node_Info) return Integer;


private

  type Node_Info is record
    Name       : Ustring := Null_Ustring;
    Position   : Double  := 0.0;
    Height     : Double  := 0.0;
    Width      : Double  := 0.0;
    Length     : Double  := 0.0;
    Margin     : Double  := 0.0;
    Is_Leaf    : Boolean := False;
    Num_Leaves : Natural := 0;
    Id         : Integer := 0;
  end record;

  Void_Node_Info: constant Node_Info := (Null_Ustring, 0.0, 0.0, 0.0, 0.0, 0.0, False, 0, 0);

end Dendrograms_Nodes;
