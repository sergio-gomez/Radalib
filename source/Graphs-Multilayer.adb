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


-- @filename Graphs-Multilayer.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/10/2014
-- @revision 14/01/2015
-- @brief Multilayer Graphs

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Graphs.Multilayer is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Multiplex_Rec, Multiplex);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Mpx: out Multiplex; Num_Layers, Num_Vertices: in Positive; Directed: in Boolean) is
  begin
    Mpx := new Multiplex_Rec;
    Mpx.Layer := Alloc(1, Num_Layers);
    Mpx.Name  := Alloc(1, Num_Layers);
    for L in 1..Num_Layers loop
      Initialize(Mpx.Layer(L), Num_Vertices, Directed);
      Mpx.Name(L) := S2U(I2S(L));
    end loop;
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Mpx: in Multiplex) return Boolean is
  begin
    return Mpx /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(Mpx: in out Multiplex) is
  begin
    if Mpx /= null then
      for L in Mpx.Layer'Range loop
        Free(Mpx.Layer(L));
      end loop;
      Free(Mpx.Layer);
      Free(Mpx.Name);
      Dispose(Mpx);
      Mpx := null;
    end if;
  end Free;

  -----------
  -- Clone --
  -----------

  function Clone(Mpx: in Multiplex) return Multiplex is
    Mpx_Clone: Multiplex;
    Num_Layers: Positive;
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;

    Num_Layers := Mpx.Layer'Length;
    Mpx_Clone := new Multiplex_Rec;
    Mpx_Clone.Layer := Alloc(1, Num_Layers);
    Mpx_Clone.Name  := Alloc(1, Num_Layers);
    for L in 1..Num_Layers loop
      Mpx_Clone.Layer(L) := Clone(Mpx.Layer(L));
      Mpx_Clone.Name(L) := Mpx.Name(L);
    end loop;

    return Mpx_Clone;
  end Clone;

  ----------------------
  -- Number_Of_Layers --
  ----------------------

  function Number_Of_Layers(Mpx: in Multiplex) return Natural is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;

    return Mpx.Layer'Length;
  end Number_Of_Layers;

  ------------------------
  -- Number_Of_Vertices --
  ------------------------

  function Number_Of_Vertices(Mpx: in Multiplex) return Natural is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;

    return Number_Of_Vertices(Mpx.Layer(1));
  end Number_Of_Vertices;

  -----------------
  -- Is_Directed --
  -----------------

  function Is_Directed(Mpx: in Multiplex) return Boolean is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;

    for L in Mpx.Layer'Range loop
      if Is_Directed(Mpx.Layer(L)) then
        return True;
      end if;
    end loop;
    return False;
  end Is_Directed;

  ----------------
  -- Set_Layers --
  ----------------

  procedure Set_Layers(Mpx: in Multiplex; Grs: in PGraphs; Names: in PUstrings := null) is
    N: Natural;
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;

    if Grs /= null then
      for L in Mpx.Layer'Range loop
        Free(Mpx.Layer(L));
      end loop;
      Free(Mpx.Layer);
      Mpx.Layer := Grs;
    end if;
    if Names /= null then
      Free(Mpx.Name);
      Mpx.Name := Names;
    end if;

    if Mpx.Layer'Length /= Mpx.Name'Length then
      raise Incompatible_Multiplex_Error;
    end if;
    N := Number_Of_Vertices(Mpx);
    for L in Mpx.Layer'Range loop
      if Number_Of_Vertices(Mpx.Layer(L)) /= N then
        raise Incompatible_Multiplex_Error;
      end if;
    end loop;
  end Set_Layers;

  ---------------
  -- Set_Layer --
  ---------------

  procedure Set_Layer(Mpx: in Multiplex; Index: in Positive; Gr: in Graph; Name: in Ustring := Null_Ustring) is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;
    if Index not in Mpx.Layer'Range then
      raise Layer_Index_Error;
    end if;

    if Number_Of_Vertices(Mpx) /= Number_Of_Vertices(Gr) then
      raise Incompatible_Multiplex_Error;
    end if;
    Free(Mpx.Layer(Index));
    Mpx.Layer(Index) := Gr;
    if Name /= Null_Ustring then
      Mpx.Name(Index) := Name;
    end if;
  end Set_Layer;

  --------------------
  -- Set_Layer_Name --
  --------------------

  procedure Set_Layer_Name(Mpx: in Multiplex; Index: in Positive; Name: in Ustring) is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;
    if Index not in Mpx.Layer'Range then
      raise Layer_Index_Error;
    end if;

    Mpx.Name(Index) := Name;
  end Set_Layer_Name;

  ----------------
  -- Get_Layers --
  ----------------

  function Get_Layers(Mpx: in Multiplex) return PGraphs is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;

    return Mpx.Layer;
  end Get_Layers;

  ---------------
  -- Get_Layer --
  ---------------

  function Get_Layer(Mpx: in Multiplex; Index: in Positive) return Graph is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;
    if Index not in Mpx.Layer'Range then
      raise Layer_Index_Error;
    end if;

    return Mpx.Layer(Index);
  end Get_Layer;

  --------------------
  -- Get_Layer_Name --
  --------------------

  function Get_Layer_Name(Mpx: in Multiplex; Index: in Positive) return Ustring is
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;
    if Index not in Mpx.Layer'Range then
      raise Layer_Index_Error;
    end if;

    return Mpx.Name(Index);
  end Get_Layer_Name;

  ----------------------
  -- Aggregate_Layers --
  ----------------------

  function Aggregate_Layers(Mpx: in Multiplex; Indices: in PIntegers := null; Weighted: in Boolean := True) return Graph is
    Gr, Gr_Tmp: Graph;
    Idx: PIntegers;
  begin
    if Mpx = null then
      raise Uninitialized_Multiplex_Error;
    end if;
    if Indices /= null then
      for I in Indices'Range loop
        if Indices(I) not in Mpx.Layer'Range then
          raise Layer_Index_Error;
        end if;
      end loop;
    end if;

    if Indices = null then
      Idx := Alloc(1, Mpx.Layer'Length);
      for L in Idx'Range loop
        Idx(L) := L;
      end loop;
    else
      Idx := Alloc(1, Indices'Length);
      Idx.all := Indices.all;
    end if;

    for L in Idx'Range loop
      if L = Idx'First then
        Gr := Clone(Mpx.Layer(Idx(L)));
      else
        Gr_Tmp := Gr;
        Gr := Sum(Gr_Tmp, Mpx.Layer(Idx(L)), Weighted);
        Free(Gr_Tmp);
      end if;
    end loop;
    Free(Idx);

    return Gr;
  end Aggregate_Layers;

  ---------
  -- Sum --
  ---------

  function Sum(Gr_Left, Gr_Right: in Graph; Weighted: in Boolean := True) return Graph is
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
        if Weighted then
          Add_Edge_Unchecked(Vf, Vt, Val);
        else
          Add_Edge_Unchecked(Vf, Vt);
        end if;
      end loop;
      while Has_Next(Ell) loop
        El := Next(Ell);
        T := Index_Of(To(El));
        Val := Value(El);
        Vt := Get_Vertex(Gr, T);
        if Weighted then
          Add_Edge_Unchecked(Vf, Vt, Val);
        else
          Add_Edge_Unchecked(Vf, Vt);
        end if;
      end loop;
      while Has_Next(Elr) loop
        Er := Next(Elr);
        T := Index_Of(To(Er));
        Val := Value(Er);
        Vt := Get_Vertex(Gr, T);
        if Weighted then
          Add_Edge_Unchecked(Vf, Vt, Val);
        else
          Add_Edge_Unchecked(Vf, Vt);
        end if;
      end loop;
      Restore(Ell);
      Restore(Elr);
    end loop;
    Restore_Consistency(Gr);

    return Gr;
  end Sum;

end Graphs.Multilayer;
