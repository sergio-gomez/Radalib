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


-- @filename Graphs.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 18/11/2004
-- @revision 26/08/2016
-- @brief Treatment of Graphs

with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;
with Utils; use Utils;

package body Graphs is

  use Ada.Strings.Unbounded;
  use Unbounded_Strings_Maps;

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Graph_Rec, Graph);

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Gr: out Graph; Num_Vertices: in Positive; Directed: in Boolean) is
  begin
    Gr := new Graph_Rec(Num_Vertices);
    Gr.Directed := Directed;
    if Gr.Directed then
      for I in Gr.Vertices'Range loop
        Initialize(Gr.Vertices(I).From);
        Initialize(Gr.Vertices(I).To);
      end loop;
    else
      for I in Gr.Vertices'Range loop
        Initialize(Gr.Vertices(I).From);
        Gr.Vertices(I).To := Gr.Vertices(I).From;
      end loop;
    end if;
  end Initialize;

  --------------------
  -- Is_Initialized --
  --------------------

  function Is_Initialized(Gr: in Graph) return Boolean is
  begin
    return Gr /= null;
  end Is_Initialized;

  ----------
  -- Free --
  ----------

  procedure Free(Gr: in out Graph) is
  begin
    if Gr /= null then
      if Gr.Directed then
        for I in Gr.Vertices'Range loop
          Free(Gr.Vertices(I).From);
          Free(Gr.Vertices(I).To);
        end loop;
      else
        for I in Gr.Vertices'Range loop
          Free(Gr.Vertices(I).From);
        end loop;
      end if;
      Dispose(Gr);
      Gr := null;
    end if;
  end Free;

  -----------
  -- Clone --
  -----------

  function Clone(Gr: in Graph) return Graph is
    Gr_Clone: Graph;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Gr_Clone := new Graph_Rec(Gr.Size);
    Gr_Clone.all := Gr.all;
    Gr_Clone.Names_Map := Empty_Map;
    for I in Gr.Vertices'Range loop
      Gr_Clone.Vertices(I).From := Clone(Gr.Vertices(I).From);
      if Gr.Directed then
        Gr_Clone.Vertices(I).To := Clone(Gr.Vertices(I).To);
      else
        Gr_Clone.Vertices(I).To := Gr_Clone.Vertices(I).From;
      end if;
    end loop;

    return Gr_Clone;
  end Clone;

  ------------------------
  -- Number_Of_Vertices --
  ------------------------

  function Number_Of_Vertices(Gr: in Graph) return Natural is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    return Gr.Size;
  end Number_Of_Vertices;

  -----------------
  -- Is_Directed --
  -----------------

  function Is_Directed(Gr: in Graph) return Boolean is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    return Gr.Directed;
  end Is_Directed;

  -----------------
  -- To_Directed --
  -----------------

  procedure To_Directed(Gr: in Graph) is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if not Gr.Directed then
      Gr.Directed := True;
      for F in Gr.Vertices'Range loop
        Gr.Vertices(F).From := Clone(Gr.Vertices(F).To);
      end loop;
    end if;
  end To_Directed;

  -------------------
  -- To_Undirected --
  -------------------

  procedure To_Undirected(Gr: in Graph) is
    Llt, Llf: Linked_List;
    Ert, Erf: Edge_Rec;
    Tt, Tf: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if Gr.Directed then
      Gr.Directed := False;
      for F in Gr.Vertices'Range loop
        Llt := Gr.Vertices(F).To;
        Llf := Gr.Vertices(F).From;
        Save(Llt);
        Save(Llf);
        Reset(Llt);
        Reset(Llf);
        while Has_Next(Llt) and Has_Next(Llf) loop
          Ert := Get(Llt);
          Erf := Get(Llf);
          Tt := Ert.Vertex;
          Tf := Erf.Vertex;
          if Tt < Tf then
            Next(Llt);
          elsif Tt = Tf then
            if Ert.Value /= Erf.Value then
              raise Incompatible_Values_Error;
            end if;
            Next(Llt);
            Next(Llf);
          else
            Add(Edge_Rec'(Tf, Erf.Value), Llt);
            Next(Llf);
          end if;
        end loop;
        while Has_Next(Llf) loop
          Erf := Next(Llf);
          Add(Erf, Llt);
        end loop;
        Restore(Llt);
        Restore(Llf);
        Free(Gr.Vertices(F).From);
        Gr.Vertices(F).From := Llt;
      end loop;
    end if;
  end To_Undirected;

  ------------------
  -- Is_Symmetric --
  ------------------

  function Is_Symmetric(Gr: in Graph) return Boolean is
    Llf, Llt: Linked_List;
    Erf, Ert: Edge_Rec;
    T: Positive;
    Symmetric: Boolean;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Symmetric := True;
    if Gr.Directed then
      for F in Gr.Vertices'Range loop
        Llf := Gr.Vertices(F).To;
        Save(Llf);
        Reset(Llf);
        while Symmetric and then Has_Next(Llf) loop
          Erf := Get(Llf);
          T := Erf.Vertex;
          if T /= F then
            Llt := Gr.Vertices(T).To;
            Save(Llt);
            Reset(Llt);
            while Symmetric and then Has_Next(Llt) loop
              Ert := Next(Llt);
              if Ert.Vertex = F then
                if Ert.Value = Erf.Value then
                  exit;
                else
                  Symmetric := False;
                end if;
              elsif Ert.Vertex > F or not Has_Next(Llt) then
                Symmetric := False;
              end if;
            end loop;
            Restore(Llt);
          end if;
          Next(Llf);
        end loop;
        Restore(Llf);
      end loop;
    end if;
    return Symmetric;
  end Is_Symmetric;

  -----------------
  -- Is_Weighted --
  -----------------

  function Is_Weighted(Gr: in Graph) return Boolean is
    Llf: Linked_List;
    Erf: Edge_Rec;
    T: Positive;
    Weighted: Boolean;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Weighted := False;
    for F in Gr.Vertices'Range loop
      Llf := Gr.Vertices(F).To;
      Save(Llf);
      Reset(Llf);
      while not Weighted and then Has_Next(Llf) loop
        Erf := Next(Llf);
        T := Erf.Vertex;
        if Gr.Directed or else T <= F then
          Weighted := Erf.Value /= Default_Edge_Value;
        end if;
      end loop;
      Restore(Llf);
      exit when Weighted;
    end loop;
    return Weighted;
  end Is_Weighted;

  -------------------
  -- To_Unweighted --
  -------------------

  procedure To_Unweighted(Gr: in Graph) is
    Llf, Llt: Linked_List;
    Erf, Ert: Edge_Rec;
    T, F: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for F in Gr.Vertices'Range loop
      Llf := Gr.Vertices(F).To;
      Save(Llf);
      Reset(Llf);
      while Has_Next(Llf) loop
        Erf := Get(Llf);
        T := Erf.Vertex;
        Replace(Llf, (T, Default_Edge_Value));
        Next(Llf);
      end loop;
      Restore(Llf);
    end loop;

    if Gr.Directed then
      for T in Gr.Vertices'Range loop
        Llt := Gr.Vertices(T).From;
        Save(Llt);
        Reset(Llt);
        while Has_Next(Llt) loop
          Ert := Get(Llt);
          F := Ert.Vertex;
          Replace(Llt, (F, Default_Edge_Value));
          Next(Llt);
        end loop;
        Restore(Llt);
      end loop;
    end if;
  end To_Unweighted;

  ----------------
  -- Get_Vertex --
  ----------------

  function Get_Vertex(Gr: in Graph; P: in Positive) return Vertex is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if P > Gr.Size then
      raise Index_Error;
    end if;

    return Vertex'(Gr, P);
  end Get_Vertex;

  --------------
  -- Index_Of --
  --------------

  function Index_Of(V: in Vertex) return Positive is
  begin
    return V.Index;
  end Index_Of;

  --------------
  -- Add_Edge --
  --------------

  procedure Add_Edge(From: in Vertex; To: in Vertex; Value: in Edge_Value := Default_Edge_Value) is
    Gr: Graph;
    F, T: Positive;
    Ll: Linked_List;
    Er: Edge_Rec;
  begin
    if From.Gr /= To.Gr then
      raise Incompatible_Graphs_Error;
    end if;

    Gr := From.Gr;
    F := From.Index;
    T := To.Index;

    Ll := Gr.Vertices(F).To;
    Save(Ll);
    Reset(Ll);
    while Has_Next(Ll) loop
      Er := Get(Ll);
      if Er.Vertex = T then
        Remove(Ll);
        exit;
      elsif Er.Vertex > T then
        exit;
      end if;
      Next(Ll);
    end loop;
    Add(Edge_Rec'(T, Value), Ll);
    Restore(Ll);

    Ll := Gr.Vertices(T).From;
    Save(Ll);
    Reset(Ll);
    while Has_Next(Ll) loop
      Er := Get(Ll);
      if Er.Vertex = F then
        Remove(Ll);
        exit;
      elsif Er.Vertex > F then
        exit;
      end if;
      Next(Ll);
    end loop;
    Add(Edge_Rec'(F, Value), Ll);
    Restore(Ll);
  end Add_Edge;

  --------------
  -- Add_Edge --
  --------------

  function Add_Edge(From: in Vertex; To: in Vertex; Value: in Edge_Value := Default_Edge_Value) return Edge is
  begin
    Add_Edge(From, To, Value);
    return Edge'(From.Gr, From.Index, To.Index, Value);
  end Add_Edge;

  ------------------------
  -- Add_Edge_Unchecked --
  ------------------------

  procedure Add_Edge_Unchecked(From: in Vertex; To: in Vertex; Value: in Edge_Value := Default_Edge_Value) is
    Gr: Graph;
    F, T: Positive;
    Ll: Linked_List;
  begin
    if From.Gr /= To.Gr then
      raise Incompatible_Graphs_Error;
    end if;

    Gr := From.Gr;
    F := From.Index;
    T := To.Index;

    Ll := Gr.Vertices(F).To;
    Add_Last(Edge_Rec'(T, Value), Ll);
    Ll := Gr.Vertices(T).From;
    Add_Last(Edge_Rec'(F, Value), Ll);
  end Add_Edge_Unchecked;

  -------------------------
  -- Restore_Consistency --
  -------------------------

  procedure Restore_Consistency(Gr: in Graph) is

    function Lower(Left, Right: in Edge_Rec) return Boolean is
    begin
      return Left.Vertex < Right.Vertex;
    end Lower;

    procedure Sort is new Generic_Sort(Lower);

    procedure Remove_Consecutive_Duplicates(Ll: in Linked_List; I: in Positive; Is_From: in Boolean) is
      Er, Er_Next: Edge_Rec;
    begin
      Reset(Ll);
      if Has_Next(Ll) then
        Er := Next(Ll);
      end if;
      while Has_Next(Ll) loop
        Er_Next := Get(Ll);
        if Er = Er_Next then
          Remove(Ll);
        elsif Er.Vertex = Er_Next.Vertex then
          if Is_From then
            raise Incompatible_Values_Error with "Incompatible values for edge " & I2S(Er.Vertex) & " -> " & I2S(I);
          else
            raise Incompatible_Values_Error with "Incompatible values for edge " & I2S(I) & " -> " & I2S(Er.Vertex);
          end if;
        else
          Next(Ll);
        end if;
        Er := Er_Next;
      end loop;
      Reset(Ll);
    end Remove_Consecutive_Duplicates;

    Ll: Linked_List;

  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      Ll := Gr.Vertices(I).From;
      Sort(Ll);
      Remove_Consecutive_Duplicates(Ll, I, True);
      if Gr.Directed then
        Ll := Gr.Vertices(I).To;
        Sort(Ll);
        Remove_Consecutive_Duplicates(Ll, I, False);
      end if;
    end loop;
  end Restore_Consistency;

  -----------------
  -- Edge_Exists --
  -----------------

  function Edge_Exists(From: in Vertex; To: in Vertex) return Boolean is
  begin
    return Get_Edge_Or_No_Edge(From, To) /= No_Edge;
  end Edge_Exists;

  --------------
  -- Get_Edge --
  --------------

  function Get_Edge(From: in Vertex; To: in Vertex) return Edge is
    E: Edge;
  begin
    E := Get_Edge_Or_No_Edge(From, To);
    if E = No_Edge then
      raise No_Edge_Error;
    end if;
    return E;
  end Get_Edge;

  -------------------------
  -- Get_Edge_Or_No_Edge --
  -------------------------

  function Get_Edge_Or_No_Edge(From: in Vertex; To: in Vertex) return Edge is
    Gr: Graph;
    F, T: Positive;
    Ll: Linked_List;
    Er: Edge_Rec;
    Exists: Boolean;
  begin
    if From.Gr /= To.Gr then
      raise Incompatible_Graphs_Error;
    end if;

    Gr := From.Gr;
    F := From.Index;
    T := To.Index;
    Ll := Gr.Vertices(F).To;
    Save(Ll);
    Reset(Ll);
    Exists := False;
    while Has_Next(Ll) loop
      Er := Next(Ll);
      if Er.Vertex = T then
        Exists := True;
        exit;
      end if;
    end loop;
    Restore(Ll);

    if not Exists then
      return No_Edge;
    end if;
    return Edge'(Gr, F, T, Er.Value);
  end Get_Edge_Or_No_Edge;

  ---------------------
  -- Number_Of_Edges --
  ---------------------

  function Number_Of_Edges(Gr: in Graph) return Natural is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if Gr.Directed then
      return Total_Degree(Gr);
    else
      return (Total_Degree(Gr) + Number_Of_Self_Loops(Gr)) / 2;
    end if;
  end Number_Of_Edges;

  -----------------
  -- Remove_Edge --
  -----------------

  procedure Remove_Edge(From: in Vertex; To: in Vertex) is
    Gr: Graph;
    F, T: Positive;
    Ll: Linked_List;
    Er: Edge_Rec;
    Exists: Boolean;
  begin
    if From.Gr /= To.Gr then
      raise Incompatible_Graphs_Error;
    end if;

    Gr := From.Gr;
    F := From.Index;
    T := To.Index;

    Ll := Gr.Vertices(F).To;
    Save(Ll);
    Reset(Ll);
    Exists := False;
    while Has_Next(Ll) loop
      Er := Get(Ll);
      if Er.Vertex = T then
        Exists := True;
        Remove(Ll);
        exit;
      end if;
      Next(Ll);
    end loop;
    Restore(Ll);

    if Exists then
      Ll := Gr.Vertices(T).From;
      Save(Ll);
      Reset(Ll);
      while Has_Next(Ll) loop
        Er := Get(Ll);
        if Er.Vertex = F then
          Remove(Ll);
          exit;
        end if;
        Next(Ll);
      end loop;
      Restore(Ll);
    end if;
  end Remove_Edge;

  -----------
  -- Clear --
  -----------

  procedure Clear(Gr: in Graph) is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      Clear(Gr.Vertices(I).From);
      Clear(Gr.Vertices(I).To);
    end loop;
  end Clear;

  -------------------
  -- Has_Self_Loop --
  -------------------

  function Has_Self_Loop(V: in Vertex) return Boolean is
  begin
    return Edge_Exists(V, V);
  end Has_Self_Loop;

  -------------------
  -- Get_Self_Loop --
  -------------------

  function Get_Self_Loop(V: in Vertex) return Edge is
  begin
    return Get_Edge(V, V);
  end Get_Self_Loop;

  ------------------------------
  -- Get_Self_Loop_Or_No_Edge --
  ------------------------------

  function Get_Self_Loop_Or_No_Edge(V: in Vertex) return Edge is
  begin
    return Get_Edge_Or_No_Edge(V, V);
  end Get_Self_Loop_Or_No_Edge;

  --------------------------
  -- Number_Of_Self_Loops --
  --------------------------

  function Number_Of_Self_Loops(Gr: in Graph) return Natural is
    Num: Natural := 0;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      if Has_Self_Loop(Vertex'(Gr, I)) then
        Num := Num + 1;
      end if;
    end loop;
    return Num;
  end Number_Of_Self_Loops;

  -----------------
  -- Degree_From --
  -----------------

  function Degree_From(V: in Vertex) return Natural is
  begin
    return Size(V.Gr.Vertices(V.Index).To);
  end Degree_From;

  ---------------
  -- Degree_To --
  ---------------

  function Degree_To(V: in Vertex) return Natural is
  begin
    return Size(V.Gr.Vertices(V.Index).From);
  end Degree_To;

  ------------------
  -- Total_Degree --
  ------------------

  function Total_Degree(Gr: in Graph) return Natural is
    Total: Natural := 0;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      Total := Total + Size(Gr.Vertices(I).From);
    end loop;
    return Total;
  end Total_Degree;

  ----------------
  -- Edges_From --
  ----------------

  function Edges_From(V: in Vertex) return Edges_List is
  begin
    return Edges_List'(V.Gr, V.Index, True, V.Gr.Vertices(V.Index).To);
  end Edges_From;

  --------------
  -- Edges_To --
  --------------

  function Edges_To(V: in Vertex) return Edges_List is
  begin
    return Edges_List'(V.Gr, V.Index, False, V.Gr.Vertices(V.Index).From);
  end Edges_To;

  ---------------------
  -- Number_Of_Edges --
  ---------------------

  function Number_Of_Edges(El: in Edges_List) return Natural is
  begin
    return Size(El.Ll);
  end Number_Of_Edges;

  --------------
  -- Has_Next --
  --------------

  function Has_Next(El: in Edges_List) return Boolean is
  begin
    return Has_Next(El.Ll);
  end Has_Next;

  ---------
  -- Get --
  ---------

  function Get(El: in Edges_List) return Edge is
    Er: Edge_Rec;
    E: Edge;
  begin
    if not Has_Next(El.Ll) then
      raise No_More_Edges_Error;
    end if;

    Er := Get(El.Ll);
    E.Gr := El.Gr;
    if El.Index_Is_From then
      E.From := El.Index;
      E.To := Er.Vertex;
    else
      E.From := Er.Vertex;
      E.To := El.Index;
    end if;
    E.Value := Er.Value;
    return E;
  end Get;

  ---------
  -- Get --
  ---------

  function Get(El: in Edges_List) return Vertex is
  begin
    if not Has_Next(El.Ll) then
      raise No_More_Edges_Error;
    end if;

    return Vertex'(El.Gr, Get(El.Ll).Vertex);
  end Get;

  ----------
  -- Next --
  ----------

  function Next(El: in Edges_List) return Edge is
    Er: Edge_Rec;
    E: Edge;
  begin
    if not Has_Next(El.Ll) then
      raise No_More_Edges_Error;
    end if;

    Er := Next(El.Ll);
    E.Gr := El.Gr;
    if El.Index_Is_From then
      E.From := El.Index;
      E.To := Er.Vertex;
    else
      E.From := Er.Vertex;
      E.To := El.Index;
    end if;
    E.Value := Er.Value;
    return E;
  end Next;

  ----------
  -- Next --
  ----------

  function Next(El: in Edges_List) return Vertex is
  begin
    if not Has_Next(El.Ll) then
      raise No_More_Edges_Error;
    end if;

    return Vertex'(El.Gr, Next(El.Ll).Vertex);
  end Next;

  ----------
  -- Next --
  ----------

  procedure Next(El: in Edges_List) is
  begin
    if not Has_Next(El.Ll) then
      raise No_More_Edges_Error;
    end if;

    Next(El.Ll);
  end Next;

  -----------
  -- Reset --
  -----------

  procedure Reset(El: in Edges_List) is
  begin
    Reset(El.Ll);
  end Reset;

  ----------
  -- Save --
  ----------

  procedure Save(El: in Edges_List) is
  begin
    Save(El.Ll);
  end Save;

  -------------
  -- Restore --
  -------------

  procedure Restore(El: in Edges_List) is
  begin
    Restore(El.Ll);
  end Restore;

  ------------
  -- Remove --
  ------------

  procedure Remove(El: in Edges_List) is
  begin
    if not Has_Next(El.Ll) then
      raise No_More_Edges_Error;
    end if;

    Remove(El.Ll);
  end Remove;

  ----------
  -- From --
  ----------

  function From(E: in Edge) return Vertex is
  begin
    return Vertex'(E.Gr, E.From);
  end From;

  --------
  -- To --
  --------

  function To(E: in Edge) return Vertex is
  begin
    return Vertex'(E.Gr, E.To);
  end To;

  -----------
  -- Value --
  -----------

  function Value(E: in Edge) return Edge_Value is
  begin
    return E.Value;
  end Value;

  ---------------
  -- Set_Value --
  ---------------

  procedure Set_Value(E: in out Edge; Value: in Edge_Value := Default_Edge_Value) is
    Gr: Graph;
    F, T: Positive;
    Ll: Linked_List;
    Er: Edge_Rec;
    Exists: Boolean;
  begin
    Gr := E.Gr;
    F := E.From;
    T := E.To;

    Ll := Gr.Vertices(F).To;
    Save(Ll);
    Reset(Ll);
    Exists := False;
    while Has_Next(Ll) loop
      Er := Get(Ll);
      if Er.Vertex = T then
        Exists := True;
        exit;
      elsif Er.Vertex > T then
        Exists := False;
        exit;
      end if;
      Next(Ll);
    end loop;
    if not Exists then
      raise No_Edge_Error;
    end if;
    Replace(Ll, (T, Value));
    Restore(Ll);

    Ll := Gr.Vertices(T).From;
    Save(Ll);
    Reset(Ll);
    Exists := False;
    while Has_Next(Ll) loop
      Er := Get(Ll);
      if Er.Vertex = F then
        Exists := True;
        exit;
      elsif Er.Vertex > F then
        Exists := False;
        exit;
      end if;
      Next(Ll);
    end loop;
    if not Exists then
      raise No_Edge_Error;
    end if;
    Replace(Ll, (F, Value));
    Restore(Ll);

    E.Value := Value;
  end Set_Value;

  ------------
  -- Remove --
  ------------

  procedure Remove(E: in out Edge) is
  begin
    Remove_Edge(Vertex'(E.Gr, E.From), Vertex'(E.Gr, E.To));
  end Remove;

  --------------
  -- Set_Name --
  --------------

  procedure Set_Name(V: in Vertex; Name: in String) is
  begin
    V.Gr.Vertices(V.Index).Name := To_Unbounded_String(Name);
  end Set_Name;

  --------------
  -- Get_Name --
  --------------

  function Get_Name(V: in Vertex) return String is
  begin
    if V.Gr.Vertices(V.Index).Name = Null_Ustring then
      return I2S(V.Index);
    end if;
    return To_String(V.Gr.Vertices(V.Index).Name);
  end Get_Name;

  -------------
  -- Set_Tag --
  -------------

  procedure Set_Tag(V: in Vertex; Tag: in String) is
  begin
    V.Gr.Vertices(V.Index).Tag:= To_Unbounded_String(Tag);
  end Set_Tag;

  -------------
  -- Get_Tag --
  -------------

  function Get_Tag(V: in Vertex) return String is
  begin
    return To_String(V.Gr.Vertices(V.Index).Tag);
  end Get_Tag;

  --------------
  -- Set_Mark --
  --------------

  procedure Set_Mark(V: in Vertex; Marked: in Boolean) is
  begin
    V.Gr.Vertices(V.Index).Marked := Marked;
  end Set_Mark;

  ---------------
  -- Is_Marked --
  ---------------

  function Is_Marked(V: in Vertex) return Boolean is
  begin
    return V.Gr.Vertices(V.Index).Marked;
  end Is_Marked;

  ----------
  -- Mark --
  ----------

  procedure Mark(V: in Vertex) is
  begin
    V.Gr.Vertices(V.Index).Marked := True;
  end Mark;

  ------------
  -- Unmark --
  ------------

  procedure Unmark(V: in Vertex) is
  begin
    V.Gr.Vertices(V.Index).Marked := False;
  end Unmark;

  ----------
  -- Mark --
  ----------

  procedure Mark(Gr: in Graph) is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      Gr.Vertices(I).Marked := True;
    end loop;
  end Mark;

  ------------
  -- Unmark --
  ------------

  procedure Unmark(Gr: in Graph) is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    for I in Gr.Vertices'Range loop
      Gr.Vertices(I).Marked := False;
    end loop;
  end Unmark;

  -----------------------
  -- Update_Names_Info --
  -----------------------

  procedure Update_Names_Info(Gr: in Graph) is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if Is_Empty(Gr.Names_Map) then
      Reserve_Capacity(Gr.Names_Map, Count_Type(Gr.Size));
    else
      Clear(Gr.Names_Map);
    end if;
    for I in Gr.Vertices'Range loop
      if Contains(Gr.Names_Map, Gr.Vertices(I).Name) then
        Clear(Gr.Names_Map);
        raise Non_Unique_Names_Error;
      end if;
      Insert(Gr.Names_Map, Gr.Vertices(I).Name, I);
    end loop;
  end Update_Names_Info;

  ----------------
  -- Get_Vertex --
  ----------------

  function Get_Vertex(Gr: in Graph; Name: in String) return Vertex is
    Index: Positive;
    Uname: Unbounded_String;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if Is_Empty(Gr.Names_Map) then
      Update_Names_Info(Gr);
    end if;
    Uname := To_Unbounded_String(Name);
    if Contains(Gr.Names_Map, Uname) then
      Index := Element(Gr.Names_Map, Uname);
      return Vertex'(Gr, Index);
    else
      raise No_Name_Error;
    end if;
  end Get_Vertex;

end Graphs;

