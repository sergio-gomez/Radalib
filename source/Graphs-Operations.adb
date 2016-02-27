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


-- @filename Graphs-Operations.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/08/2009
-- @revision 29/12/2014
-- @brief Implementation of Graphs operations

package body Graphs.Operations is

  ---------
  -- "+" --
  ---------

  function "+"(Gr_Left, Gr_Right: in Graph) return Graph is
    Gr: Graph;
    N: Natural;
    Directed: Boolean;
    Vf, Vt, Vl, Vr: Vertex;
    Ell, Elr: Edges_List;
    El, Er: Edge;
    Val: Edge_Value;
    Tfirst, T, Tl, Tr: Positive;
  begin
    if (Gr_Left = null) or (Gr_Right = null) then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr_Left);
    if Number_Of_Vertices(Gr_Right) /= N then
      raise Incompatible_Graphs_Error;
    end if;

    Directed := Gr_Left.Directed or Gr_Right.Directed;
    Initialize(Gr, N, Directed);
    for I in 1..N loop
      Vf := Get_Vertex(Gr, I);
      Vl := Get_Vertex(Gr_Left, I);
      Set_Name(Vf, Get_Name(Vl));
      Set_Tag(Vf, Get_Tag(Vl));
    end loop;

    for F in 1..N loop
      Vf := Get_Vertex(Gr, F);
      Vl := Get_Vertex(Gr_Left, F);
      Vr := Get_Vertex(Gr_Right, F);
      Ell := Edges_From(Vl);
      Elr := Edges_From(Vr);
      if Directed then
        Tfirst := 1;
      else
        Tfirst := F;
      end if;
      Save(Ell);
      Reset(Ell);
      Save(Elr);
      Reset(Elr);
      while Has_Next(Ell) loop
        El := Get(Ell);
        Tl := Index_Of(To(El));
        exit when Tfirst <= Tl;
        Next(Ell);
      end loop;
      while Has_Next(Elr) loop
        Er := Get(Elr);
        Tr := Index_Of(To(Er));
        exit when Tfirst <= Tr;
        Next(Elr);
      end loop;
      while Has_Next(Ell) and Has_Next(Elr) loop
        El := Get(Ell);
        Er := Get(Elr);
        Tl := Index_Of(To(El));
        Tr := Index_Of(To(Er));
        if Tl < Tr then
          T := Tl;
          Val := Value(El);
          Next(Ell);
        elsif Tr < Tl then
          T := Tr;
          Val := Value(Er);
          Next(Elr);
        elsif Tl = Tr then
          T := Tl;
          Val := Value(El) + Value(Er);
          Next(Ell);
          Next(Elr);
        end if;
        Vt := Get_Vertex(Gr, T);
        Add_Edge_Unchecked(Vf, Vt, Val);
      end loop;
      while Has_Next(Ell) loop
        El := Next(Ell);
        T := Index_Of(To(El));
        Val := Value(El);
        Vt := Get_Vertex(Gr, T);
        Add_Edge_Unchecked(Vf, Vt, Val);
      end loop;
      while Has_Next(Elr) loop
        Er := Next(Elr);
        T := Index_Of(To(Er));
        Val := Value(Er);
        Vt := Get_Vertex(Gr, T);
        Add_Edge_Unchecked(Vf, Vt, Val);
      end loop;
      Restore(Ell);
      Restore(Elr);
    end loop;
    Restore_Consistency(Gr);

    return Gr;
  end "+";

  ---------
  -- "*" --
  ---------

  function "*"(Gr_Left, Gr_Right: in Graph) return Graph is
    Gr: Graph;
    N: Natural;
    Directed, Is_Zero: Boolean;
    V, Vl, Vf, Vt, Vlf, Vrt: Vertex;
    Ellf, Elrt: Edges_List;
    Elf, Ert: Edge;
    Prod: Edge_Value;
    Tfirst, Klt, Krf: Positive;
  begin
    if (Gr_Left = null) or (Gr_Right = null) then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr_Left);
    if Number_Of_Vertices(Gr_Right) /= N then
      raise Incompatible_Graphs_Error;
    end if;

    Directed := (Gr_Left /= Gr_Right) or Gr_Left.Directed or Gr_Right.Directed;
    Initialize(Gr, N, Directed);
    for I in 1..N loop
      V := Get_Vertex(Gr, I);
      Vl := Get_Vertex(Gr_Left, I);
      Set_Name(Vl, Get_Name(V));
      Set_Tag(Vl, Get_Tag(V));
    end loop;

    for F in 1..N loop
      Vf := Get_Vertex(Gr, F);
      Vlf := Get_Vertex(Gr_Left, F);
      Ellf := Edges_From(Vlf);
      if Directed then
        Tfirst := 1;
      else
        Tfirst := F;
      end if;
      for T in Tfirst..N loop
        Vt := Get_Vertex(Gr, T);
        Vrt := Get_Vertex(Gr_Right, T);
        Elrt := Edges_To(Vrt);
        Is_Zero := True;
        Prod := Zero_Value;
        if Ellf.Ll = Elrt.Ll then
          Save(Ellf);
          Reset(Ellf);
          while Has_Next(Ellf) loop
            Elf := Next(Ellf);
            Is_Zero := False;
            Prod := Prod + (Value(Elf) * Value(Elf));
          end loop;
          Restore(Ellf);
        else
          Save(Ellf);
          Reset(Ellf);
          Save(Elrt);
          Reset(Elrt);
          while Has_Next(Ellf) and Has_Next(Elrt) loop
            Elf := Get(Ellf);
            Ert := Get(Elrt);
            Klt := Index_Of(To(Elf));
            Krf := Index_Of(From(Ert));
            if Klt < Krf then
              Next(Ellf);
            elsif Klt > Krf then
              Next(Elrt);
            else
              Is_Zero := False;
              Prod := Prod + (Value(Elf) * Value(Ert));
              Next(Ellf);
              Next(Elrt);
            end if;
          end loop;
          Restore(Elrt);
          Restore(Ellf);
        end if;
        if not Is_Zero then
          Add_Edge_Unchecked(Vf, Vt, Prod);
        end if;
      end loop;
    end loop;
    Restore_Consistency(Gr);

    return Gr;
  end "*";

  ----------
  -- "**" --
  ----------

  function "**"(Gr: in Graph; Power: in Natural) return Graph is
    Gr_Tmp, Gr_Pow: Graph;
    N: Natural;
    V, Vp: Vertex;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if Power = 0 then
      -- Identity matrix
      N := Number_Of_Vertices(Gr);
      Initialize(Gr_Pow, N, Directed => False);
      for I in 1..N loop
        V := Get_Vertex(Gr, I);
        Vp := Get_Vertex(Gr_Pow, I);
        Set_Name(Vp, Get_Name(V));
        Set_Tag(Vp, Get_Tag(V));
        Add_Edge(Vp, Vp);  -- default Edge Values as self-loops
      end loop;
    elsif Power = 1 then
      Gr_Pow := Clone(Gr);
    elsif Power = 2 then
      Gr_Pow := Gr * Gr;
    elsif Power = 3 then
      Gr_Tmp := Gr * Gr;
      Gr_Pow := Gr * Gr_Tmp;
      Free(Gr_Tmp);
    else
      Gr_Tmp := Gr * Gr;
      Gr_Pow := Gr_Tmp ** (Power / 2);
      Free(Gr_Tmp);
      if (Power mod 2) = 1 then
        Gr_Tmp := Gr_Pow;
        Gr_Pow := Gr * Gr_Tmp;
        Free(Gr_Tmp);
      end if;
    end if;

    return Gr_Pow;
  end "**";

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

end Graphs.Operations;
