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


-- @filename Graphs-Operations.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/08/2009
-- @revision 24/08/2020
-- @brief Implementation of Graphs operations

package body Graphs.Operations.Arrays is

  ---------
  -- "*" --
  ---------

  function "*"(Gr: in Graph; V: in Edge_Values) return Edge_Values is
    N: Natural;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr);
    if V'Length /= N then
      raise Incompatible_Dimensions_Error;
    end if;

    declare
      Offset: constant Integer := V'First - 1;
      Prod: Edge_Values(V'Range);
      J: Positive;
      El: Edges_List;
      E: Edge;
    begin
      Prod := (others => Zero_Value);
      for I in 1..N loop
        El := Edges_From(Get_Vertex(Gr, I));
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          J := Index_Of(To(E));
          Prod(I + Offset) := Prod(I + Offset) + (Value(E) * V(J + Offset));
        end loop;
        Restore(El);
      end loop;
      return Prod;
    end;
  end "*";

  ---------
  -- "*" --
  ---------

  function "*"(Gr: in Graph; V: in PEdge_Values) return PEdge_Values is
    N: Natural;
    Prod: PEdge_Values;
    Offset: Integer;
    J: Positive;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if V = null then
      raise Uninitialized_Vector_Error;
    end if;

    N := Number_Of_Vertices(Gr);
    if V'Length /= N then
      raise Incompatible_Dimensions_Error;
    end if;

    Prod := Alloc(V'First, V'Last);
    Prod.all := (others => Zero_Value);
    Offset := V'First - 1;
    for I in 1..N loop
      El := Edges_From(Get_Vertex(Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        Prod(I + Offset) := Prod(I + Offset) + (Value(E) * V(J + Offset));
      end loop;
      Restore(El);
    end loop;
    return Prod;
  end "*";

  --------------
  -- To_Array --
  --------------

  function To_Array(Gr: in Graph) return Edge_Valuess is
    N: Natural;
    J: Positive;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr);
    declare
      A: Edge_Valuess(1..N, 1..N);
    begin
      A := (others => (others => Zero_Value));
      for I in 1..N loop
        El := Edges_From(Get_Vertex(Gr, I));
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          J := Index_Of(To(E));
          A(I, J) := Value(E);
        end loop;
        Restore(El);
      end loop;
      return A;
    end;
  end To_Array;

  --------------
  -- To_Array --
  --------------

  function To_Array(Gr: in Graph) return PEdge_Valuess is
    P: PEdge_Valuess;
    N: Natural;
    J: Positive;
    El: Edges_List;
    E: Edge;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr);
    P := Alloc(1, N, 1, N);
    P.all := (others => (others => Zero_Value));
    for I in 1..N loop
      El := Edges_From(Get_Vertex(Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        P(I, J) := Value(E);
      end loop;
      Restore(El);
    end loop;
    return P;
  end To_Array;

end Graphs.Operations.Arrays;
