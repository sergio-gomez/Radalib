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


-- @filename Dendrograms.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 09/03/2015
-- @revision 09/03/2015
-- @brief Instantiation of Trees to Dendrograms Node Info Values

with Utils; use Utils;

package body Dendrograms is

  ------------------------------
  -- Set_Positions_And_Widths --
  ------------------------------

  procedure Set_Positions_And_Widths(T: in Dendrogram) is

    Pos_Leaf: Double := 0.0;

    procedure Update_Node_Info(Nod: in Node) is
      Children: List_Of_Nodes;
      Nod_Inf, Child_Inf: Node_Info;
      Pos, Pos_Min, Pos_Max: Double;
    begin
      if Is_Leaf(Nod) then
        Pos_Leaf := Pos_Leaf + 1.0;
        Nod_Inf := Value(Nod);
        Set_Position(Nod_Inf, Pos_Leaf);
        Set_Width(Nod_Inf, 0.0);
        Set_Value(Nod, Nod_Inf);
      else
        Pos_Min := Double'Last;
        Pos_Max := 0.0;
        Children := Get_Children(Nod);
        Save(Children);
        Reset(Children);
        while Has_Next(Children) loop
          Child_Inf := Value(Next(Children));
          Pos := Get_Position(Child_Inf);
          if Pos > Pos_Max then
            Pos_Max := Pos;
          end if;
          if Pos < Pos_Min then
            Pos_Min := Pos;
          end if;
        end loop;
        Restore(Children);
        Pos := (Pos_Min + Pos_Max) / 2.0;
        Nod_Inf := Value(Nod);
        Set_Position(Nod_Inf, Pos);
        Set_Width(Nod_Inf, Pos_Max - Pos_Min);
        Set_Value(Nod, Nod_Inf);
      end if;
    end Update_Node_Info;

    procedure Update_Nodes is new Generic_Depth_First_Postorder_Traversal(Update_Node_Info);

  begin
    Update_Nodes(T);
  end Set_Positions_And_Widths;

end Dendrograms;
