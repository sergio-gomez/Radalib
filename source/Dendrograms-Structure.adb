-- Radalib, Copyright (c) 2016 by
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


-- @filename Dendrograms-Structure.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/04/2015
-- @revision 13/04/2015
-- @brief Structure of Dendrograms for Export and Plot

with Dendrograms_Nodes; use Dendrograms_Nodes;
with Dendrograms.Algorithms; use Dendrograms.Algorithms;

package body Dendrograms.Structure is

  ------------------------------
  -- Get_Dendrogram_Structure --
  ------------------------------

  function Get_Dendrogram_Structure(T: in Dendrogram) return Dendrogram_Structure is

    Ds: Dendrogram_Structure;
    Ds_Cursor: Natural := 0;
    Num_Nodes: Natural;

    procedure Fill_Node(Nod: in Node) is
      Nod_Inf: Node_Info;
    begin
      Ds_Cursor := Ds_Cursor + 1;
      Nod_Inf := Value(Nod);
      Ds(Ds_Cursor).Node_Id := Get_Id(Nod_Inf);
      if Is_Root(Nod) then
        Ds(Ds_Cursor).Parent_Id := 0;
        Ds(Ds_Cursor).Kind := Root;
      else
        Ds(Ds_Cursor).Parent_Id := Get_Id(Value(Get_Parent(Nod)));
        if Is_Leaf(Nod) then
          Ds(Ds_Cursor).Kind := Leaf;
        else
          Ds(Ds_Cursor).Kind := Cluster;
        end if;
      end if;
      Ds(Ds_Cursor).Position := Get_Position(Nod_Inf);
      Ds(Ds_Cursor).Height   := Get_Height(Nod_Inf);
      Ds(Ds_Cursor).Width    := Get_Width(Nod_Inf);
      Ds(Ds_Cursor).Length   := Get_Length(Nod_Inf);
      Ds(Ds_Cursor).Margin   := Get_Margin(Nod_Inf);
      Ds(Ds_Cursor).Name     := Get_Name(Nod_Inf);
    end Fill_Node;

    procedure Fill_Structure is new Generic_Depth_First_Postorder_Traversal(Fill_Node);

  begin
    Num_Nodes := Number_Of_Nodes(T);
    Ds := Alloc(1, Num_Nodes);
    Fill_Structure(T);
    return Ds;
  end Get_Dendrogram_Structure;

  ------------------------------
  -- Get_Dendrogram_Plot_Info --
  ------------------------------

  function Get_Dendrogram_Plot_Info(T: in Dendrogram; Include_Bands: Boolean := True) return Dendrogram_Plot_Info is

    Dp: Dendrogram_Plot_Info;
    Dp_Cursor: Natural := 0;
    Num_Items: Natural := 0;

    procedure Add_Items(Nod: in Node) is
      Nod_Inf: Node_Info;
      Width, Length, Margin: Double;
    begin
      Nod_Inf := Value(Nod);
      -- Cluster
      Width := Get_Width(Nod_Inf);
      if Width /= 0.0 then
        Num_Items := Num_Items + 1;
      end if;
      -- Member
      if not Is_Root(Nod) then
        Length := Get_Length(Nod_Inf);
        if Length /= 0.0 then
          Num_Items := Num_Items + 1;
        end if;
      end if;
      -- Band
      Margin := Get_Margin(Nod_Inf);
      if Include_Bands and Margin /= 0.0 then
        Num_Items := Num_Items + 1;
      end if;
    end Add_Items;

    procedure Fill_Cluster(Nod: in Node) is
      Nod_Inf: Node_Info;
      Pos, Height, Width: Double;
    begin
      Nod_Inf := Value(Nod);
      Width := Get_Width(Nod_Inf);
      if Width /= 0.0 then
        Pos := Get_Position(Nod_Inf);
        Height := Get_Height(Nod_Inf);
        Dp_Cursor := Dp_Cursor + 1;
        Dp(Dp_Cursor).Kind      := Cluster;
        Dp(Dp_Cursor).Id        := Get_Id(Nod_Inf);
        Dp(Dp_Cursor).Position1 := Pos - Width / 2.0;
        Dp(Dp_Cursor).Height1   := Height;
        Dp(Dp_Cursor).Position2 := Pos + Width / 2.0;
        Dp(Dp_Cursor).Height2   := Height;
        Dp(Dp_Cursor).Name      := Get_Name(Nod_Inf);
      end if;
    end Fill_Cluster;

    procedure Fill_Member(Nod: in Node) is
      Nod_Inf: Node_Info;
      Pos, Height, Length, Margin: Double;
    begin
      if not Is_Root(Nod) then
        Nod_Inf := Value(Nod);
        Pos := Get_Position(Nod_Inf);
        Height := Get_Height(Nod_Inf);
        Length := Get_Length(Nod_Inf);
        Margin := Get_Margin(Nod_Inf);
        if Length /= 0.0 then
          Dp_Cursor := Dp_Cursor + 1;
          Dp(Dp_Cursor).Kind      := Member;
          Dp(Dp_Cursor).Id        := Get_Id(Nod_Inf);
          Dp(Dp_Cursor).Position1 := Pos;
          if Include_Bands and Margin /= 0.0 then
            Dp(Dp_Cursor).Height1 := Height + Margin;
          else
            Dp(Dp_Cursor).Height1 := Height;
          end if;
          Dp(Dp_Cursor).Position2 := Pos;
          Dp(Dp_Cursor).Height2   := Height + Length;
          Dp(Dp_Cursor).Name      := Get_Name(Nod_Inf);
        end if;
      end if;
    end Fill_Member;

    procedure Fill_Band(Nod: in Node) is
      Nod_Inf: Node_Info;
      Pos, Height, Width, Margin: Double;
    begin
      Nod_Inf := Value(Nod);
      Margin := Get_Margin(Nod_Inf);
      if Include_Bands and Margin /= 0.0 then
        Pos := Get_Position(Nod_Inf);
        Height := Get_Height(Nod_Inf);
        Width := Get_Width(Nod_Inf);
        Dp_Cursor := Dp_Cursor + 1;
        Dp(Dp_Cursor).Kind      := Band;
        Dp(Dp_Cursor).Id        := Get_Id(Nod_Inf);
        Dp(Dp_Cursor).Position1 := Pos - Width / 2.0;
        Dp(Dp_Cursor).Height1   := Height;
        Dp(Dp_Cursor).Position2 := Pos + Width / 2.0;
        Dp(Dp_Cursor).Height2   := Height + Margin;
        Dp(Dp_Cursor).Name      := Get_Name(Nod_Inf);
      end if;
    end Fill_Band;

    procedure Count_Items   is new Generic_Depth_First_Postorder_Traversal(Add_Items);
    procedure Fill_Clusters is new Generic_Depth_First_Postorder_Traversal(Fill_Cluster);
    procedure Fill_Members  is new Generic_Depth_First_Postorder_Traversal(Fill_Member);
    procedure Fill_Bands    is new Generic_Depth_First_Postorder_Traversal(Fill_Band);

  begin
    Count_Items(T);
    Dp := Alloc(1, Num_Items);
    Fill_Clusters(T);
    Fill_Members(T);
    if Include_Bands then
      Fill_Bands(T);
    end if;
    return Dp;
  end Get_Dendrogram_Plot_Info;

end Dendrograms.Structure;
