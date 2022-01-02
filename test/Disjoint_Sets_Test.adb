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


-- @filename Disjoint_Sets_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/11/2014
-- @revision 28/12/2017
-- @brief Test of Disjoint_Sets package

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Disjoint_Sets; use Disjoint_Sets;
with Trees_Integer; use Trees_Integer;
with Trees_Integer_IO; use Trees_Integer_IO;
with Finite_Disjoint_Lists; --use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO; use Finite_Disjoint_Lists.IO;
with Utils; use Utils;

procedure Disjoint_Sets_Test is

  use Nodes_Lists;

  procedure Put(Ds: in Disjoint_Set) is
    Fr: Forest;
    Tr: Tree;
    Lol: Finite_Disjoint_Lists.List_Of_Lists;
  begin
    Fr := To_Forest(Ds);
    Put_Line("Forest of " & I2S(Size(Fr)) & " trees");
    Save(Fr);
    Reset(fr);
    while Has_Next(Fr) loop
      Tr := Next(Fr);
      Put_Line("  Component represented by " & I2S(Value(Tr)) & " of size " & I2S(Number_Of_Nodes(Tr)));
      Put_Tree(Tr);
      Free(Tr);
    end loop;
    Restore(Fr);
    Free(Fr);
    Put_Line("---");
    Put_Line("Subtree sizes");
    for I in 1..Size(Ds) loop
      Put_Line("  " & I2S(I) & " of size " & I2S(Subtree_Size(Get_Element(Ds, I))));
    end loop;
    Put_Line("---");
    Put_Line("Finite Disjoint List");
    Lol := To_List_Of_Lists(Ds);
    Put(Lol);
    Finite_Disjoint_Lists.Free(Lol);
  end Put;

  Fn: constant String := "test-disjoint_sets.txt";
  Total: constant Positive := 9;
  Num: Natural := 5;

  Ds: Disjoint_Set;
  E, E1, E2: Element;
  C: Component;

  procedure Put_Title(S: in String) is
  begin
    Put_Line("----------- " & S & " -----------");
  end Put_Title;

begin

  Put_Title("Initialize");
  Initialize(Ds, Total);
  Put(Ds);

  Put_Title("Join");
  Join(Get_Element(Ds, 1), Get_Element(Ds, 1));
  Join(Get_Element(Ds, 1), Get_Element(Ds, 2));
  Join(Get_Element(Ds, 1), Get_Element(Ds, 3));
  Join(Get_Element(Ds, 2), Get_Element(Ds, 3));
  Join(Get_Element(Ds, 4), Get_Element(Ds, 5));
  Join(Get_Element(Ds, 6), Get_Element(Ds, 7));
  Join(Get_Element(Ds, 8), Get_Element(Ds, 7));
  Put(Ds);

  Put_Title("More Join");
  Join(Get_Element(Ds, 1), Get_Element(Ds, 4));
  Join(Get_Element(Ds, 1), Get_Element(Ds, 8));
  Put(Ds);

  Put_Title("Component_Of and Index_Of");
  E := Get_Element(Ds, 8);
  C := Component_Of(E);
  Put_Line("  the component of " & I2S(Index_Of(E)) & " is represented by "  & I2S(Index_Of(C)));
  Put(Ds);

  Put_Title("Share_Component");
  E1 := Get_Element(Ds, 1);
  E2 := Get_Element(Ds, 5);
  E  := Get_Element(Ds, 9);
  Put_Line("  elements " & I2S(Index_Of(E1)) & " and " & I2S(Index_Of(E2)) & " share component? " & Boolean'Image(Share_Component(E1, E2)));
  Put_Line("  elements " & I2S(Index_Of(E1)) & " and " & I2S(Index_Of(E))  & " share component? " & Boolean'Image(Share_Component(E1, E)));
  Put_Line("  elements " & I2S(Index_Of(E2)) & " and " & I2S(Index_Of(E))  & " share component? " & Boolean'Image(Share_Component(E2, E)));
  Put(Ds);

  Put_Title("All together");
  Join(Get_Element(Ds, 9), Get_Element(Ds, 1));
  Put(Ds);

  Free(Ds);

end Disjoint_Sets_Test;
