-- Radalib, Copyright (c) 2015 by
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


-- @filename Friends_Of_Friends_Utils.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/10/2004
-- @revision 21/10/2009
-- @brief Friends of Friends Clustering Utils

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Unchecked_Deallocation;

package body Friends_Of_Friends_Utils is

  Friends_Cut: Float;

  procedure Dispose is new Ada.Unchecked_Deallocation(Data_Array, Pdata_Array);

  procedure Read_Data(Fn: in String; Num_Data: in Positive; Data: out Pdata_Array) is
    F: File_Type;
  begin
    Data := new Data_Array(1..Num_Data);
    Open(F, In_File, Fn);
    for I in Data'range loop
      for J in Data(I)'range loop
        Get(F, Data(I)(J));
      end loop;
      Skip_Line(F);
    end loop;
    Close(F);
  end Read_Data;

  function Euclidean_Friends(D1, D2: in Data_Item) return Boolean is
    Dist2: Float := 0.0;
  begin
    for J in Data_Item'range loop
      Dist2 := Dist2 + (D1(J) - D2(J))**2;
    end loop;
    return Dist2 < Friends_Cut**2;
  end Euclidean_Friends;

  procedure Put(L: in List) is
    E: Element;
  begin
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      E := Next_Element(L);
      Put(Index_Of(E), Width => 0); Put(" ");
    end loop;
    Restore(L);
    New_Line;
  end Put;

  procedure Put(Lol: in List_Of_Lists) is
    L: List;
    N: Natural := 0;
  begin
    Put("Number of lists: "); Put(Number_Of_Lists(Lol), Width => 0); New_Line;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      N := N + 1;
      Put_Line("---"); Put("List "); Put(N, Width => 0);
      Put(": "); Put(Number_Of_Elements(L), Width => 0); Put_Line(" elements");
      Put(L);
    end loop;
    Restore(Lol);
    L := Unassigned_list(Lol);
    Put_Line("---"); Put("Unassigned list");
    Put(": "); Put(Number_Of_Elements(L), Width => 0); Put_Line(" elements");
    Put(L);
  end Put;

  procedure Put(Gr: in Graph) is
    Vf, Vt: Vertex;
    E: Edge;
    El: Edges_List;
  begin
    pragma Warnings(Off, El);
    for F in 1..Number_Of_Vertices(Gr) loop
      Put(F, Width => 2); Put(" -> ");
      Vf := Get_Vertex(Gr, F);
      El := Edges_From(Vf);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vt := To(E);
        Put(Index_Of(Vt), Width => 0); Put(" ");
      end loop;
      Restore(El);
      New_Line;
    end loop;
  end Put;

  procedure Make_Fof(Data: in Pdata_Array; Cut: in Float) is
    Dc: Data_Clustering;
  begin
    Put_Line("======");
    Friends_Cut := Cut;
    Friends_Of_Friends(Data, Euclidean_Friends'Access, Dc);
    Put("Friends_Cut: "); Put(Friends_Cut, Aft => 2, Exp => 0); New_Line;
    Put(Get_Clusters(Dc)); New_Line;
    Free(Dc);
  end Make_Fof;

  procedure Make_Fof_Graph(Data: in Pdata_Array; Cut: in Float) is
    Dc: Data_Clustering;
    Gr: Graph;
  begin
    Put_Line("======");
    Friends_Cut := Cut;
    Friends_Of_Friends_Graph(Data, Euclidean_Friends'Access, Dc, Gr);
    Put("Friends_Cut: "); Put(Friends_Cut, Aft => 2, Exp => 0); New_Line;
    Put(Get_Clusters(Dc));
    Put_Line("------");
    Put(Gr);
    Free(Dc);
    Free(Gr);
  end Make_Fof_Graph;

  procedure Free(Data: in out Pdata_Array) is
  begin
    Dispose(Data);
  end Free;

end Friends_Of_Friends_Utils;
