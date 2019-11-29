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


-- @filename Data_Clusters-Algorithms.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2004
-- @revision 30/08/2009
-- @brief Implementation of Clustering algorithms

package body Data_Clusters.Algorithms is

  --------------------------------
  -- Generic_Friends_Of_Friends --
  --------------------------------

  procedure Generic_Friends_Of_Friends(Data: in Pdata_Array; Dc: out Data_Clustering) is

    Lol: List_Of_Lists;

    function Friends_Of(L: in List) return List is
      F: constant List := New_List(Lol);
      U: constant List := Unassigned_List(Lol);
      E, Eu: Element;
    begin
      Save(L);
      Save(U);
      Reset(U);
      while Has_Next_Element(U) loop
        Eu := Next_Element(U);
        Reset(L);
        while Has_Next_Element(L) loop
          E := Next_Element(L);
          if Friends(Get_Data_Item(Dc, E), Get_Data_Item(Dc, Eu)) then
            Move(Eu, F);
          end if;
        end loop;
      end loop;
      Restore(U);
      Restore(L);
      return F;
    end Friends_Of;

    U: List; -- Unassigned List
    C: List; -- The new Cluster List
    L: List; -- The List of new Elements to be added to the Cluster
    F: List; -- Their Friends
    E: Element;

  begin
    Initialize(Dc, Data);
    Lol := Get_Clusters(Dc);
    U := Unassigned_List(Lol);
    while Number_Of_Elements(U) > 0 loop
      C := New_List(Lol);
      Reset(U);
      E := Next_Element(U);
      L := New_List(Lol);
      Move(E, L);
      while Number_Of_Elements(L) > 0 loop
        F := Friends_Of(L);
        Move(L, C);
        Remove(L);
        L := F;
      end loop;
      Remove(L);
    end loop;
  end Generic_Friends_Of_Friends;

  ------------------------
  -- Friends_Of_Friends --
  ------------------------

  procedure Friends_Of_Friends(Data: in Pdata_Array; Friends: in Comparator; Dc: out Data_Clustering) is
    procedure Fof is new Generic_Friends_Of_Friends(Friends.all);
  begin
    Fof(Data, Dc);
  end Friends_Of_Friends;

  --------------------------------------
  -- Generic_Friends_Of_Friends_Graph --
  --------------------------------------

  procedure Generic_Friends_Of_Friends_Graph(Data: in Pdata_Array; Dc: out Data_Clustering; Gr: out Graph) is

    Lol: List_Of_Lists;

    function Friends_Of(L: in List) return List is
      F: constant List := New_List(Lol);
      U: constant List := Unassigned_List(Lol);
      E, Eu: Element;
      Is_Friend: Boolean;
    begin
      Save(L);
      Save(U);
      Reset(U);
      while Has_Next_Element(U) loop
        Eu := Next_Element(U);
        Reset(L);
        Is_Friend := False;
        while Has_Next_Element(L) loop
          E := Next_Element(L);
          if Friends(Get_Data_Item(Dc, E), Get_Data_Item(Dc, Eu)) then
            Add_Edge(Get_Vertex(Gr, Index_Of(E)), Get_Vertex(Gr, Index_Of(Eu)));
            Is_Friend := True;
          end if;
        end loop;
        if Is_Friend then
          Reset(F);
          while Has_Next_Element(F) loop
            E := Next_Element(F);
            if Friends(Get_Data_Item(Dc, E), Get_Data_Item(Dc, Eu)) then
              Add_Edge(Get_Vertex(Gr, Index_Of(E)), Get_Vertex(Gr, Index_Of(Eu)));
            end if;
          end loop;
          Move(Eu, F);
        end if;
      end loop;
      Restore(U);
      Restore(L);
      return F;
    end Friends_Of;

    U: List; -- Unassigned List
    C: List; -- The new Cluster List
    L: List; -- The List of new Elements to be added to the Cluster
    F: List; -- Their Friends
    E: Element;

  begin
    Initialize(Dc, Data);
    Lol := Get_Clusters(Dc);
    Initialize(Gr, Number_Of_Elements(Lol), False);
    U := Unassigned_List(Lol);
    while Number_Of_Elements(U) > 0 loop
      C := New_List(Lol);
      Reset(U);
      E := Next_Element(U);
      L := New_List(Lol);
      Move(E, L);
      while Number_Of_Elements(L) > 0 loop
        F := Friends_Of(L);
        Move(L, C);
        Remove(L);
        L := F;
      end loop;
      Remove(L);
    end loop;
  end Generic_Friends_Of_Friends_Graph;

  ------------------------------
  -- Friends_Of_Friends_Graph --
  ------------------------------

  procedure Friends_Of_Friends_Graph(Data: in Pdata_Array; Friends: in Comparator; Dc: out Data_Clustering; Gr: out Graph) is
    procedure Fof_Graph is new Generic_Friends_Of_Friends_Graph(Friends.all);
  begin
    Fof_Graph(Data, Dc, Gr);
  end Friends_Of_Friends_Graph;

end Data_Clusters.Algorithms;
