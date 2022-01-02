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


-- @filename Graphs-Algorithms.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/08/2005
-- @revision 10/09/2020
-- @brief Implementation of Graphs algorithms

with Utils; use Utils;
with Utils_Generics; use Utils_Generics;

with Stacks_Integer; use Stacks_Integer;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Minheaps;

package body Graphs.Algorithms is

  ------------------------
  -- To_Components_Type --
  ------------------------

  function To_Components_Type(Comp_Name: in String) return Components_Type is
  begin
    if    To_Uppercase(Comp_Name) = "W" or To_Lowercase(Comp_Name) = "weak"   then
      return Weak_Components;
    elsif To_Uppercase(Comp_Name) = "S" or To_Lowercase(Comp_Name) = "strong" then
      return Strong_Components;
    else
      raise Unknown_Components_Type_Error;
    end if;
  end To_Components_Type;

  ---------------------
  -- To_Optimum_Type --
  ---------------------

  function To_Optimum_Type(Opt_Name: in String) return Optimum_Type is
  begin
    if    To_Uppercase(Opt_Name) = "MIN" or To_Lowercase(Opt_Name) = "minimum" then
      return Minimum;
    elsif To_Uppercase(Opt_Name) = "MAX" or To_Lowercase(Opt_Name) = "maximum" then
      return Maximum;
    else
      raise Unknown_Optimum_Type_Error;
    end if;
  end To_Optimum_Type;

  ----------------
  -- Symmetrize --
  ----------------

  procedure Symmetrize(Gr: in Graph) is
    Llt, Llf: Linked_Edge_Recs.Linked_List;
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
            Replace(Llt, Edge_Rec'(Tf, Ert.Value + Erf.Value));
            Next(Llt);
            Next(Llf);
          else
            Add(Edge_Rec'(Tf, Erf.Value), Llt);
            Next(Llf);
          end if;
        end loop;
        while Has_Next(Llf) loop
          Erf := Next(Llf);
          Add_Last(Erf, Llt);
        end loop;
        Restore(Llt);
        Restore(Llf);
        Free(Gr.Vertices(F).From);
        Gr.Vertices(F).From := Llt;
      end loop;
    end if;
  end Symmetrize;

  ------------------
  -- Is_Connected --
  ------------------

  function Is_Connected(Gr: in Graph; Ct: in Components_Type := Weak_Components) return Boolean is
    Lol: List_Of_Lists;
    Connected: Boolean;
  begin
    Connected_Components(Gr, Lol, Ct);
    Connected := Number_Of_Lists(Lol) = 1;
    Free(Lol);
    return Connected;
  end Is_Connected;

  --------------------------
  -- Connected_Components --
  --------------------------

  procedure Connected_Components(Gr: in Graph; Lol: out List_Of_Lists; Ct: in Components_Type := Weak_Components) is

    Unvisited: constant Integer := -1;

    procedure Depth_First_Traversal(P: in Positive; U, L: in List) is
      St: Stack;
      I: Positive;
      E: Element;
      El: Edges_List;
    begin
      Initialize(St);
      Push(P, St);
      while not Is_Empty(St) loop
        I := Pop(St);
        E := Get_Element(Lol, I);
        if Belongs_To(E, U) then
          Move(E, L);
          -- Links from
          El := Edges_From(Get_Vertex(Gr, I));
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            Push(Index_Of(Next(El)), St);
          end loop;
          Restore(El);
          if Gr.Directed then
            -- Links to
            El := Edges_To(Get_Vertex(Gr, I));
            Save(El);
            Reset(El);
            while Has_Next(El) loop
              Push(Index_Of(Next(El)), St);
            end loop;
            Restore(El);
          end if;
        elsif not Belongs_To(E, L) then
          Move(L, List_Of(E));
        end if;
      end loop;
      Free(St);
    end Depth_First_Traversal;

    procedure Tarjan(Vf: in Vertex; S: in Stack; Index, Lowlink: in PIntegers; Ind: in out Positive) is
      Ori: constant Positive := Index_Of(Vf);
      Dest, Aux: Positive;
      Vt: Vertex;
      El: Edges_List;
      E: Edge;
      L: List;
    begin
      Index(Ori) := Ind;
      Lowlink(Ori) := Ind;
      Ind := Ind + 1;
      Push(Ori, S);
      Mark(Vf);
      El := Edges_From(Vf);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vt := To(E);
        Dest := Index_Of(Vt);
        if Index(Dest) = Unvisited then
          Tarjan(Vt, S, Index, Lowlink, Ind);
          Lowlink(Ori) := Integer'Min(Lowlink(Ori), Lowlink(Dest));
        else
          if Is_Marked(Vt) then
            Lowlink(Ori) := Integer'Min(Lowlink(Ori), Index(Dest));
          end if;
        end if;
      end loop;
      Restore(El);
      if Index(Ori) = Lowlink(Ori) then
        L := New_List(Lol);
        Aux := Positive'Last;
        while Aux /= Ori loop
          Aux := Pop(S);
          Vt := Get_Vertex(Gr, Aux);
          Unmark(Vt);
          Move(Get_Element(Lol, Aux), L);
        end loop;
      end if;
    end Tarjan;

    N: Natural;
    U, L: List;
    S: Stack;
    Index, Lowlink: PIntegers;
    Ind: Positive;

  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr);
    Initialize(Lol, N);

    if Gr.Directed and Ct = Strong_Components then
      Unmark(Gr);
      Initialize(S);
      Index := Alloc(1, N);
      Lowlink := Alloc(1, N);
      Index.all := (others => Unvisited);
      Ind := 1;
      for I in 1..N loop
        if Index(I) = Unvisited then
          Tarjan(Get_Vertex(Gr, I), S, Index, Lowlink, Ind);
        end if;
      end loop;
      Free(Index);
      Free(Lowlink);
      Free(S);
      Unmark(Gr);
    else
      U := Unassigned_List(Lol);
      while Number_Of_Elements(U) > 0 loop
        Reset(U);
        L := New_List(Lol);
        Depth_First_Traversal(Index_Of(Get_Element(U)), U, L);
        if Number_Of_Elements(L) = 0 then
          Remove(L);
        end if;
      end loop;
    end if;

    Sort_Lists(Lol);
    Sort_By_Size(Lol);
  end Connected_Components;

  --------------------------
  -- Connected_Components --
  --------------------------

  procedure Connected_Components(Gr: in Graph; Lol_In: in List_Of_Lists; Lol_Out: out List_Of_Lists) is

    procedure Depth_First_Traversal(P: in Positive; U, L_Out: in List) is
      St: Stack;
      I: Positive;
      E: Element;
      L_In: List;
      El: Edges_List;
      P_Nxt: Positive;
    begin
      Initialize(St);
      Push(P, St);
      while not Is_Empty(St) loop
        I := Pop(St);
        E := Get_Element(Lol_Out, I);
        if Belongs_To(E, U) then
          Move(E, L_Out);
          L_In := List_Of(Get_Element(Lol_In, I));
          -- Links from
          El := Edges_From(Get_Vertex(Gr, I));
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            P_Nxt := Index_Of(Next(El));
            if Belongs_To(Get_Element(Lol_In, P_Nxt), L_In) then
              Push(P_Nxt, St);
            end if;
          end loop;
          Restore(El);
          if Gr.Directed then
            -- Links to
            El := Edges_To(Get_Vertex(Gr, I));
            Save(El);
            Reset(El);
            while Has_Next(El) loop
              P_Nxt := Index_Of(Next(El));
              if Belongs_To(Get_Element(Lol_In, P_Nxt), L_In) then
                Push(P_Nxt, St);
              end if;
            end loop;
            Restore(El);
          end if;
        elsif not Belongs_To(E, L_Out) then
          Move(L_Out, List_Of(E));
        end if;
      end loop;
      Free(St);
    end Depth_First_Traversal;

    U, L_Out: List;

  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Initialize(Lol_Out, Number_Of_Vertices(Gr));
    U := Unassigned_List(Lol_Out);
    while Number_Of_Elements(U) > 0 loop
      Reset(U);
      L_Out := New_List(Lol_Out);
      Depth_First_Traversal(Index_Of(Get_Element(U)), U, L_Out);
      if Number_Of_Elements(L_Out) = 0 then
        Remove(L_Out);
      end if;
    end loop;

    Sort_Lists(Lol_Out);
    Sort_By_Size(Lol_Out);
  end Connected_Components;

  --------------------------------------
  -- Update_List_Connected_Components --
  --------------------------------------

  procedure Update_List_Connected_Components(Gr: in Graph; L: in List; Ls_Out: out Linked_Lists_Of_Lists.Linked_List) is

    procedure Depth_First_Traversal(Lol: in List_Of_Lists; P: in Positive; U, L_Out: in List) is
      St: Stack;
      I: Positive;
      E: Element;
      El: Edges_List;
      P_Nxt: Positive;
    begin
      Initialize(St);
      Push(P, St);
      while not Is_Empty(St) loop
        I := Pop(St);
        E := Get_Element(Lol, I);
        if Belongs_To(E, U) then
          Move(E, L_Out);
          -- Links from
          El := Edges_From(Get_Vertex(Gr, I));
          Save(El);
          Reset(El);
          while Has_Next(El) loop
            P_Nxt := Index_Of(Next(El));
            if Belongs_To(Get_Element(Lol, P_Nxt), U) then
              Push(P_Nxt, St);
            end if;
          end loop;
          Restore(El);
          if Gr.Directed then
            -- Links to
            El := Edges_To(Get_Vertex(Gr, I));
            Save(El);
            Reset(El);
            while Has_Next(El) loop
              P_Nxt := Index_Of(Next(El));
              if Belongs_To(Get_Element(Lol, P_Nxt), U) then
                Push(P_Nxt, St);
              end if;
            end loop;
            Restore(El);
          end if;
        elsif not Belongs_To(E, L_Out) then
          Move(L_Out, List_Of(E));
        end if;
      end loop;
      Free(St);
    end Depth_First_Traversal;

    Lol: List_Of_Lists;
    U_Old, U, L_Out: List;

  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Lol := List_Of_Lists_Of(L);

    Initialize(Ls_Out);
    Add_Last(L, Ls_Out);

    if Number_Of_Elements(L) > 0 then
      U := Unassigned_List(Lol);

      -- Save previously unassigned Elements
      U_Old := New_List(Lol);
      Move(U, U_Old);

      -- Unassign Elements in input List
      Move(L, U);

      -- Try to recover the input List
      Depth_First_Traversal(Lol, Index_Of(Get_Element(U)), U, L);

      -- If not Connected, find the rest of Components
      while Number_Of_Elements(U) > 0 loop
        Reset(U);
        L_Out := New_List(Lol);
        Depth_First_Traversal(Lol, Index_Of(Get_Element(U)), U, L_Out);
        if Number_Of_Elements(L_Out) = 0 then
          Remove(L_Out);
        else
          Add_Last(L_Out, Ls_Out);
        end if;
      end loop;

      Remove(U_Old);
    end if;

  end Update_List_Connected_Components;

  ------------------
  -- Isolate_List --
  ------------------

  procedure Isolate_List(Gr: in Graph; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    V: Vertex;
    Belongs_Vf, Belongs_Vt: Boolean;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Number_Of_Vertices(Gr) /= Number_Of_Elements(Lol) then
      raise Incompatible_List_Error;
    end if;

    for I in 1..Number_Of_Vertices(Gr) loop
      V := Get_Vertex(Gr, I);
      Belongs_Vf := Belongs_To(Get_Element(Lol, I), L);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Get(El);
        Belongs_Vt := Belongs_To(Get_Element(Lol, Index_Of(To(E))), L);
        if Belongs_Vf xor Belongs_Vt then
          Remove(El);
        else
          Next(El);
        end if;
      end loop;
      Restore(El);
      if Gr.Directed then
        Belongs_Vt := Belongs_Vf;
        El := Edges_To(V);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Get(El);
          Belongs_Vf := Belongs_To(Get_Element(Lol, Index_Of(From(E))), L);
          if Belongs_Vf xor Belongs_Vt then
            Remove(El);
          else
            Next(El);
          end if;
        end loop;
        Restore(El);
      end if;
    end loop;
  end Isolate_List;

  ----------------
  -- Clear_List --
  ----------------

  procedure Clear_List(Gr: in Graph; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    V: Vertex;
    Belongs_Vf, Belongs_Vt: Boolean;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Number_Of_Vertices(Gr) /= Number_Of_Elements(Lol) then
      raise Incompatible_List_Error;
    end if;

    for I in 1..Number_Of_Vertices(Gr) loop
      V := Get_Vertex(Gr, I);
      Belongs_Vf := Belongs_To(Get_Element(Lol, I), L);
      El := Edges_From(V);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Get(El);
        Belongs_Vt := Belongs_To(Get_Element(Lol, Index_Of(To(E))), L);
        if Belongs_Vf and Belongs_Vt then
          Remove(El);
        else
          Next(El);
        end if;
      end loop;
      Restore(El);
    end loop;
  end Clear_List;

  ---------------------
  -- Create_Subgraph --
  ---------------------

  procedure Create_Subgraph(Gr: in Graph; L: in List; Sub_Gr: out Graph) is
    Lol: List_Of_Lists;
    N, N_Sub: Natural;
    New_Index: Pintegers;
    Index, I, I_From, I_To: Positive;
    El: Edges_List;
    E: Edge;
    V, V_Sub, V_From, V_To: Vertex;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    N := Number_Of_Elements(Lol);
    if Number_Of_Vertices(Gr) /= N then
      raise Incompatible_List_Error;
    end if;
    N_Sub := Number_Of_Elements(L);
    if N_Sub = 0 then
      raise Void_List_Error;
    end if;

    Initialize(Sub_Gr, N_Sub, Gr.Directed);
    New_Index := Alloc(1, N);
    New_Index.all := (others => 0);
    Index := 1;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      New_Index(I) := Index;
      V := Get_Vertex(Gr, I);
      V_Sub := Get_Vertex(Sub_Gr, Index);
      Set_Name(V_Sub, Get_Name(V));
      Set_Tag(V_Sub, Get_Tag(V));
      Index := Index + 1;
    end loop;
    Restore(L);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I_From := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Gr, I_From));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        I_To := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, I_To), L) then
          V_From := Get_Vertex(Sub_Gr, New_Index(I_From));
          V_To := Get_Vertex(Sub_Gr, New_Index(I_To));
          Add_Edge(V_From, V_To, E.Value);
        end if;
      end loop;
      Restore(El);
      if Gr.Directed then
        I_To := I_From;
        El := Edges_To(Get_Vertex(Gr, I_To));
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          I_From := Index_Of(From(E));
          if I_From /= I_TO and then Belongs_To(Get_Element(Lol, I_From), L) then
            V_From := Get_Vertex(Sub_Gr, New_Index(I_From));
            V_To := Get_Vertex(Sub_Gr, New_Index(I_To));
            Add_Edge(V_From, V_To, E.Value);
          end if;
        end loop;
        Restore(El);
      end if;
    end loop;
    Restore(L);

    Free(New_Index);
  end Create_Subgraph;

  -----------------------
  -- Renormalize_Graph --
  -----------------------

  procedure Renormalize_Graph(Gr: in Graph; Ren: in List_Of_Lists; Gr_Ren: out Graph; R: in Edge_Value := No_Value) is

    function "*"(Left: in Natural; Right: in Edge_Value) return Edge_Value is
      X, Y: Edge_Value;
      N: Natural;
    begin
      X := Right;
      N := Left;
      if N = 0 or X = Zero_Value then
        return Zero_Value;
      end if;
      Y := Zero_Value;
      while N > 1 loop
        if N mod 2 = 0 then
          X := X + X;
          N := N / 2;
        else
          Y := X + Y;
          X := X + X;
          N := (N - 1) / 2;
        end if;
      end loop;
      return X + Y;
    end "*";

    type Edge_Values is array(Integer range <>) of Edge_Value;
    type PEdge_Values is access Edge_Values;

    function Alloc is new Alloc_1D_Array(Edge_Value, Edge_Values, PEdge_Values);
    procedure Free is new Free_1D_Array(Edge_Value, Edge_Values, PEdge_Values);

    Directed: Boolean;
    N, N_Ren, Nc1_Ren: Natural;
    L: List;
    Finished: Boolean;
    El: Edges_List;
    E: Edge;
    Vi, Vj, Vi_Ren, Vj_Ren: Vertex;
    Vertex_To_List_Index: Pintegers;
    Wh: Edge_Value;
    Whs: PEdge_Values;
    Whs_Exists: PBooleans;
    I, J, I_Ren, J_Ren: Positive;
  begin
    pragma Warnings(Off, L);
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;
    if Number_Of_Vertices(Gr) /= Number_Of_Elements(Ren) then
      raise Incompatible_List_Of_Lists_Error;
    end if;

    Directed := Gr.Directed;
    N := Number_Of_Vertices(Gr);
    N_Ren:= Number_Of_Lists(Ren);
    Initialize(Gr_Ren, N_Ren, Directed);
    if Gr.Bipartite then
      Sort_By_First_Element(Ren);
      Nc1_Ren := 0;
      Finished := False;
      Save(Ren);
      Reset(Ren);
      while Has_Next_List(Ren) and not Finished loop
        L := Next_List(Ren);
        Save(L);
        Reset(L);
        if Has_Next_Element(L) then
          I := Index_Of(Get_Element(L));
          if I <= Gr.Num_Class1 then
            Nc1_Ren := Nc1_Ren + 1;
          else
            Finished := True;
          end if;
        end if;
        Restore(L);
      end loop;
      Restore(Ren);
      Set_Bipartiteness(Gr_Ren, Nc1_Ren);
    end if;

    Vertex_To_List_Index := Alloc(1, N);
    I_Ren := 1;
    Save(Ren);
    Reset(Ren);
    while Has_Next_List(Ren) loop
      L := Next_List(Ren);
      Save(L);
      Reset(L);
      if Has_Next_Element(L) then
        I := Index_Of(Get_Element(L));
        Vi := Get_Vertex(Gr, I);
        Vi_Ren := Get_Vertex(Gr_Ren, I_Ren);
        if Number_Of_Elements(L) > 1 then
          Set_Name(Vi_Ren, Get_Name(Vi) & "*");
        else
          Set_Name(Vi_Ren, Get_Name(Vi));
        end if;
        Set_Tag(Vi_Ren, Get_Tag(Vi));
      end if;
      Reset(L);
      while Has_Next_Element(L) loop
        I := Index_Of(Next_Element(L));
        Vertex_To_List_Index(I) := I_Ren;
      end loop;
      Restore(L);
      I_Ren := I_Ren + 1;
    end loop;
    Restore(Ren);

    Whs := Alloc(1, N_Ren);
    Whs_Exists := Alloc(1, N_Ren);

    I_Ren := 1;
    Save(Ren);
    Reset(Ren);
    while Has_Next_List(Ren) loop
      L := Next_List(Ren);
      Whs_Exists.all := (others => False);
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        I := Index_Of(Next_Element(L));
        Vi := Get_Vertex(Gr, I);
        El := Edges_From(Vi);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          Vj := To(E);
          J := Index_Of(Vj);
          J_Ren := Vertex_To_List_Index(J);
          if Directed or else I_Ren <= J_Ren then
            Wh := E.Value;
            if Whs_Exists(J_Ren) then
              Whs(J_Ren) := Whs(J_Ren) + Wh;
            else
              Whs(J_Ren) := Wh;
              Whs_Exists(J_Ren) := True;
            end if;
          end if;
        end loop;
        Restore(El);
      end loop;
      Restore(L);
      if R /= No_Value and R /= Zero_Value then
        if Whs_Exists(I_Ren) then
          Whs(I_Ren) := Whs(I_Ren) + Number_Of_Elements(L) * R;
        else
          Whs_Exists(I_Ren) := True;
          Whs(I_Ren) := Number_Of_Elements(L) * R;
        end if;
      end if;
      Vi_Ren := Get_Vertex(Gr_Ren, I_Ren);
      for Jr in 1 .. N_Ren loop
        if Whs_Exists(Jr) then
          Vj_Ren := Get_Vertex(Gr_Ren, Jr);
          Add_Edge(Vi_Ren, Vj_Ren, Whs(Jr));
        end if;
      end loop;
      I_Ren := I_Ren + 1;
    end loop;
    Restore(Ren);

    Free(Whs);
    Free(Whs_Exists);
    Free(Vertex_To_List_Index);
  end Renormalize_Graph;

  ---------------------------------
  -- Unrenormalize_List_Of_Lists --
  ---------------------------------

  procedure Unrenormalize_List_Of_Lists(Lol_Ren: in List_Of_Lists; Ren: in List_Of_Lists; Lol: out List_Of_Lists) is
    N: Natural;
    L_Lol_Ren, L_Ren, L_Lol: List;
    Index_Lol_Ren, Index_Ren : Positive;
  begin
    pragma Warnings(Off, L_Lol_Ren);
    pragma Warnings(Off, L_Ren);
    if Number_Of_Lists(Ren) /= Number_Of_Elements(Lol_Ren) then
      raise Incompatible_Lists_Of_Lists_Error;
    end if;

    N := Number_Of_Elements(Ren);
    Initialize(Lol, N);

    Save(Lol_Ren);
    Reset(Lol_Ren);
    while Has_Next_List(Lol_Ren) loop
      L_Lol_Ren := Next_List(Lol_Ren);
      L_Lol := New_List(Lol);
      Save(L_Lol_Ren);
      Reset(L_Lol_Ren);
      while Has_Next_Element(L_Lol_Ren) loop
        Index_Lol_Ren := Index_Of(Next_Element(L_Lol_Ren));
        Save(Ren);
        Reset(Ren);
        for I in 1..Index_Lol_Ren loop
          L_Ren := Next_List(Ren);
        end loop;
        Restore(Ren);
        Save(L_Ren);
        Reset(L_Ren);
        while Has_Next_Element(L_Ren) loop
          Index_Ren := Index_Of(Next_Element(L_Ren));
          Move(Get_Element(Lol, Index_Ren), L_Lol);
        end loop;
        Restore(L_Ren);
      end loop;
      Restore(L_Lol_Ren);
    end loop;
    Restore(Lol_Ren);
  end Unrenormalize_List_Of_Lists;

  -------------------
  -- Spanning_Tree --
  -------------------

  procedure Spanning_Tree(Gr: in Graph; Gr_Mst: out Graph; Optim: in Optimum_Type := Minimum) is

    function "<"(L, R: in Edge) return Boolean is
    begin
      case Optim is
        when Minimum =>
          return Value(L) < Value(R);
        when Maximum =>
          return Value(L) > Value(R);
      end case;
    end "<";

    package MinEdges is new Minheaps(Edge, "<"); use MinEdges;

    procedure Merge(L1, L2: in List) is
    begin
      if Number_Of_Elements(L1) > Number_Of_Elements(L2) then
        Move(L2, L1);
        Remove(L2);
      else
        Move(L1, L2);
        Remove(L1);
      end if;
    end Merge;

    V, Vf, Vt: Vertex;
    El: Edges_List;
    E: Edge;
    H: Minheap;
    Lol: List_Of_Lists;
    Lf, Lt: List;
    Ef, Et: Element;
    N, Ne, F, T: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    N := Gr.Size;

    Initialize(Gr_Mst, N, Directed => Gr.Directed);
    for I in 1..N loop
      V := Get_Vertex(Gr_Mst, I);
      Vf := Get_Vertex(Gr, I);
      Set_Name(V, Get_Name(Vf));
      Set_Tag(V, Get_Tag(Vf));
    end loop;

    Ne := Number_Of_Edges(Gr);
    Initialize(H, Ne);
    for I in 1..N loop
      Vf := Get_Vertex(Gr, I);
      El := Edges_From(Vf);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Vt := To(E);
        T := Index_Of(Vt);
        if (Gr.Directed and I /= T) or else I < T then
          Add(E, H);
        end if;
      end loop;
      Restore(El);
    end loop;

    Initialize(Lol, N, Isolated_Initialization);
    while not Is_Empty(H) loop
      E := Delete_Min(H);
      Vf := From(E);
      Vt := To(E);
      F := Index_Of(Vf);
      T := Index_Of(Vt);
      Ef := Get_Element(Lol, F);
      Et := Get_Element(Lol, T);
      Lf := List_Of(Ef);
      Lt := List_Of(Et);
      if Lf /= Lt then
        Vf := Get_Vertex(Gr_Mst, F);
        Vt := Get_Vertex(Gr_Mst, T);
        Add_Edge(Vf, Vt, Value(E));
        Merge(Lf, Lt);
      end if;
    end loop;

    Free(Lol);
    Free(H);
  end Spanning_Tree;

  -------------------------
  -- Force_Bipartiteness --
  -------------------------

  procedure Force_Bipartiteness(Gr: in Graph) is
    Lol: List_Of_Lists;
    L: List;
    E: Element;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    if Gr.Bipartite then
      Initialize(Lol, Gr.Size, Together_Initialization);
      L := New_List(Lol);
      for I in 1..Gr.Num_Class1 loop
        E := Get_Element(Lol, I);
        Move(E, L);
      end loop;
      Save(Lol);
      Reset(Lol);
      while Has_Next_List(Lol) loop
        L := Next_List(Lol);
        Clear_List(Gr, L);
      end loop;
      Restore(Lol);
    end if;
  end Force_Bipartiteness;

end Graphs.Algorithms;
