-- Radalib, Copyright (c) 2018 by
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


-- @filename Modularities_Spectral.adb
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 20/01/2018
-- @brief Spectral Modularity Optimization

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularities_Spectral is

  -------------------------
  -- Calculate_Strengths --
  -------------------------

  procedure Calculate_Strengths(
    Gr: in Graph;
    MT: in Modularity_Type;
    MI: in Modularity_Info;
    W_From: out Vector;
    W_To: out Vector)
  is
    Num: Natural;
    Vi: Vertex;
    Wi: Double;
  begin
    Num := Number_Of_Vertices(Gr);
    Allocate(W_From, Num);
    Allocate(W_To, Num);
    if MT in Unweighted_Modularity_Type then
      for I in 1 .. Num loop
        Vi := Get_Vertex(Gr, I);
        Wi := Double(Degree_From(MI, Vi));
        Set(W_From, I, Wi);
        Wi := Double(Degree_To(MI, Vi));
        Set(W_To, I, Wi);
      end loop;
    else
      for I in 1 .. Num loop
        Vi := Get_Vertex(Gr, I);
        Wi := Strength_From(MI, Vi);
        Set(W_From, I, Wi);
        Wi := Strength_To(MI, Vi);
        Set(W_To, I, Wi);
      end loop;
    end if;
  end Calculate_Strengths;

  -------------------------
  -- Calculate_Strengths --
  -------------------------

  procedure Calculate_Strengths(
    Gr: in Graph;
    MT: in Modularity_Type;
    MI: in Modularity_Info;
    L: in List;
    W2: out Double;
    W_From: out Vector;
    W_To: out Vector)
  is
    Num: Natural;
    E: Finite_Disjoint_Lists.Element;
    I: Positive;
    Vi: Vertex;
    Wj: Double;
  begin
    Num := Number_Of_Elements(L);
    Allocate(W_From, Num);
    Allocate(W_To, Num);
    Save(L);
    Reset(L);
    if MT in Unweighted_Modularity_Type then
      W2 := Double(Total_Degree(MI));
      for J in 1 .. Num loop
        E := Next_Element(L);
        I := Index_Of(E);
        Vi := Get_Vertex(Gr, I);
        Wj := Double(Degree_From(MI, Vi));
        Set(W_From, J, Wj);
        Wj := Double(Degree_To(MI, Vi));
        Set(W_To, J, Wj);
      end loop;
    else
      W2 := Total_Strength(MI);
      for J in 1 .. Num loop
        E := Next_Element(L);
        I := Index_Of(E);
        Vi := Get_Vertex(Gr, I);
        Wj := Strength_From(MI, Vi);
        Set(W_From, J, Wj);
        Wj := Strength_To(MI, Vi);
        Set(W_To, J, Wj);
      end loop;
    end if;
    Restore(L);
  end Calculate_Strengths;

  -------------------
  -- Adjacency_Row --
  -------------------

  procedure Adjacency_Row(
    Gr: in Graph;
    MT: in Modularity_Type;
    I: in Positive;
    Ai: out Vector)
  is
    Num: Natural;
    Vi, Vj: Vertex;
    ELi: Edges_List;
    Eij: Edge;
    Aij: Double;
    J: Positive;
  begin
    pragma Warnings(Off, ELi);
    Num := Number_Of_Vertices(Gr);
    Allocate(Ai, Num);
    Initialise(Ai, 0.0);
    Vi := Get_Vertex(Gr, I);
    ELi := Edges_From(Vi);
    Save(ELi);
    Reset(ELi);
    if MT in Unweighted_Modularity_Type then
      while Has_Next(ELi) loop
        Eij := Next(ELi);
        Vj := To(Eij);
        J := Index_Of(Vj);
        Set(Ai, J, 1.0);
      end loop;
    else
      while Has_Next(ELi) loop
        Eij := Next(ELi);
        Aij := Value(Eij);
        Vj := To(Eij);
        J := Index_Of(Vj);
        Set(Ai, J, Aij);
      end loop;
    end if;
    Restore(ELi);
  end Adjacency_Row;

  ----------------------
  -- Adjacency_Column --
  ----------------------

  procedure Adjacency_Column(
    Gr: in Graph;
    MT: in Modularity_Type;
    I: in Positive;
    Ai: out Vector)
  is
    Num: Natural;
    Vi, Vj: Vertex;
    ELi: Edges_List;
    Eij: Edge;
    Aij: Double;
    J: Positive;
  begin
    pragma Warnings(Off, ELi);
    Num := Number_Of_Vertices(Gr);
    Allocate(Ai, Num);
    Initialise(Ai, 0.0);
    Vi := Get_Vertex(Gr, I);
    ELi := Edges_To(Vi);
    Save(ELi);
    Reset(ELi);
    if MT in Unweighted_Modularity_Type then
      while Has_Next(ELi) loop
        Eij := Next(ELi);
        Vj := From(Eij);
        J := Index_Of(Vj);
        Set(Ai, J, 1.0);
      end loop;
    else
      while Has_Next(ELi) loop
        Eij := Next(ELi);
        Aij := Value(Eij);
        Vj := From(Eij);
        J := Index_Of(Vj);
        Set(Ai, J, Aij);
      end loop;
    end if;
    Restore(ELi);
  end Adjacency_Column;

  --------------------------------
  -- Multiply_Modularity_Matrix --
  --------------------------------

  procedure Multiply_Modularity_Matrix(
    Gr: in Graph;
    MT: in Modularity_Type;
    MI_Ini: in Modularity_Info;
    Net: in Network;
    MI: in Modularity_Info;
    Val_Bound: in Double;
    X: in Vector;
    Y: out Vector)
  is
    W2, W_Gr, W_Gr_F, W_Gr_T, WX, WX_F, WX_T, AiX, Wi_Gr, Wi, Xi, Yi: Double;
    Ai, W_From, W_To, W_From_Sub, W_To_Sub: Vector;
    Num: Natural;
    Vi: Vertex;
  begin
    Calculate_Strengths(Gr, MT, MI_Ini, Net.L, W2, W_From, W_To);
    Calculate_Strengths(Net.Gr, MT, MI, W_From_Sub, W_To_Sub);
    if not Is_Directed(Gr) then
      W_Gr := Sum(W_From);
      WX := Dot_Product(W_From, X);
      Num := Number_Of_Elements(Net.L);
      Allocate(Y, Num);
      for I in 1 .. Num loop
        Vi := Get_Vertex(Net.Gr, I);
        Xi := Get(X, I);
        Adjacency_Row(Net.Gr, MT, I, Ai);
        AiX := Dot_Product(Ai, X);
        Deallocate(Ai);
        Wi_Gr := Get(W_From_Sub, I);
        Wi := Get(W_From, I);
        Yi := AiX - (WX / W2) * Wi - Xi * (Wi_Gr - (W_Gr / W2) * Wi + Val_Bound);
        Set(Y, I, Yi);
      end loop;
    else
      W_Gr_F := Sum(W_From);
      WX_F := Dot_Product(W_From, X);
      W_Gr_T := Sum(W_To);
      WX_T := Dot_Product(W_To, X);
      Num := Number_Of_Elements(Net.L);
      Allocate(Y, Num);
      for I in 1 .. Num loop
        Vi := Get_Vertex(Net.Gr, I);
        Xi := Get(X, I);
        Adjacency_Row(Net.Gr, MT, I, Ai);
        AiX := Dot_Product(Ai, X);
        Deallocate(Ai);
        Wi_Gr := Get(W_From_Sub, I);
        Wi := Get(W_From, I);
        Yi := AiX - (WX_T / W2) * Wi - Xi * (Wi_Gr - (W_Gr_T / W2) * Wi + Val_Bound);
        Adjacency_Column(Net.Gr, MT, I, Ai);
        AiX := Dot_Product(Ai, X);
        Deallocate(Ai);
        Wi_Gr := Strength_To(MI, Vi);
        Wi_Gr := Get(W_From_Sub, I);
        Wi := Get(W_To, I);
        Yi := (Yi + AiX - (WX_F / W2) * Wi - Xi * (Wi_Gr - (W_Gr_F / W2) * Wi + Val_Bound)) / 2.0;
        Set(Y, I, Yi);
      end loop;
    end if;
    Deallocate(W_From);
    Deallocate(W_To);
    Deallocate(W_From_Sub);
    Deallocate(W_To_Sub);
  end Multiply_Modularity_Matrix;

  ---------------------
  -- Power_Iteration --
  ---------------------

  procedure Power_Iteration(
    Gen: in out Generator;
    Gr: in Graph;
    MT: in Modularity_Type;
    MI: in Modularity_Info;
    Net: in Network;
    MI_Sub: in Modularity_Info;
    Val_Bound: in Double;
    Val: out Double;
    Y_Norm: out Vector)
  is
    Epsilon: constant Double := 1.0E-6;
    Num: Natural;
    Y, X_Norm, YX: Vector;
    N_YX: Double;
  begin
    Num := Number_Of_Elements(Net.L);
    Allocate(Y, Num);
    Initialise_Random(Y, Gen, -1.0, +1.0);
    Normalise(Y, Y_Norm);
    Deallocate(Y);
    N_YX := Epsilon + 1.0;
    while N_YX > Epsilon loop
      Copy(Y_Norm, X_Norm);
      Deallocate(Y_Norm);
      Multiply_Modularity_Matrix(Gr, MT, MI, Net, MI_Sub, Val_Bound, X_Norm, Y);
      Val := Dot_Product(X_Norm, Y);
      Normalise(Y, Y_Norm);
      Deallocate(Y);
      if Val > 0.0 then
        Subtract(Y_Norm, X_Norm, YX);
      else
        Add(Y_Norm, X_Norm, YX);
      end if;
      Deallocate(X_Norm);
      N_YX := Norm(YX) / Num;
      Deallocate(YX);
    end loop;
  end Power_Iteration;

  ------------------
  -- Index_Vector --
  ------------------

  procedure Index_Vector(
    Gen: in out Generator;
    Gr: in Graph;
    MI: in Modularity_Info;
    Net: in Network;
    U: out Vector;
    MT: in Modularity_Type;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    MI_Sub: Modularity_Info;
    Val: Double;
  begin
    Initialize(MI_Sub, Net.Gr, MT, R, Pc);
    Power_Iteration(Gen, Gr, MT, MI, Net, MI_Sub, 0.0, Val, U);
    if Val < 0.0 then
      Power_Iteration(Gen, Gr, MT, MI, Net, MI_Sub, Val, Val, U);
    end if;
    Free(MI_Sub);
  end Index_Vector;

  -----------------
  -- Divide_List --
  -----------------

  procedure Divide_List(
    Lol: in out List_Of_Lists;
    L: in out List;
    U: in Vector;
    Sub_L1, Sub_L2: out List)
  is
    Num: Natural;
    E: Finite_Disjoint_Lists.Element;
    Ui: Double;
  begin
    Sub_L1 := New_List(Lol);
    Sub_L2 := New_List(Lol);
    Num := Number_Of_Elements(L);
    Save(L);
    Reset(L);
    for I in 1 .. Num loop
      E := Next_Element(L);
      Ui := Get(U, I);
      if Ui > 0.0 then
        Move(E, Sub_L1);
      else
        Move(E, Sub_L2);
      end if;
    end loop;
    Restore(L);
  end Divide_List;

  ------------------
  -- Modularities --
  ------------------

  procedure Modularities(
    MT: in Modularity_Type;
    MI: in out Modularity_Info;
    Ls: in Linked_Lists_Of_Lists.Linked_List;
    Lq: out Lists_Of_Modularities.Linked_List;
    Sub_Q: out Double)
  is
    L: List;
    Q: Modularity_Rec;
  begin
    Initialize(Lq);
    Sub_Q := 0.0;
    Save(Ls);
    Reset(Ls);
    while Has_Next(Ls) loop
      L := Next(Ls);
      Update_Modularity(MI, L, MT);
      Q := Partial_Modularity(MI, L);
      Add_Last(Q, Lq);
      Sub_Q := Sub_Q + Q.Total;
    end loop;
    Restore(Ls);
  end Modularities;

  -----------------
  -- Merge_Lists --
  -----------------

  procedure Merge_Lists(
    Lol: in out List_Of_Lists;
    Ls: in out Linked_Lists_Of_Lists.Linked_List;
    L: out List)
  is
    Sub_L: List;
  begin
    L := New_List(Lol);
    Reset(Ls);
    while Has_Next(Ls) loop
      Sub_L := Next(Ls);
      Move(Sub_L, L);
      Remove(Sub_L);
    end loop;
  end Merge_Lists;

  ------------------------
  -- Execute_Repetition --
  ------------------------

  procedure Execute_Repetition(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Gen: in out Generator;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    MI: Modularity_Info;
    Net, Sub_Net: Network;
    Lol: List_Of_Lists;
    St: Stack;
    U: Vector;
    Sub_L1, Sub_L2, L: List;
    Ls, Ls1, Ls2: Linked_Lists_Of_Lists.Linked_List;
    Lq: Lists_Of_Modularities.Linked_List;
    Sub_Q: Double;
  begin
    Initialize(MI, Gr, MT, R, Pc);
    Lol_Best := Clone(Lol_Ini);
    Connected_Components(Gr, Lol_Best, Lol);
    if Number_Of_Elements(Lol_Best) = Number_Of_Elements(Lol) then
      Free(Lol);
    else
      Free(Lol_Best);
      Lol_Best := Lol;
    end if;
    Update_Modularity(MI, Lol_Best, MT);

    Initialize(St);
    Save(Lol_Best);
    Reset(Lol_Best);
    while Has_Next_List(Lol_Best) loop
      Net.L := Next_List(Lol_Best);
      if Number_Of_Elements(Net.L) > 1 then
        Net.Q := Partial_Modularity(MI, Net.L);
        Create_Subgraph(Gr, Net.L, Net.Gr);
        Push(Net, St);
      end if;
    end loop;
    Restore(Lol_Best);

    while not Is_Empty(St) loop
      Net := Pop(St);
      Index_Vector(Gen, Gr, MI, Net, U, MT, R, Pc);
      Free(Net.Gr);
      Save_Modularity(MI, Net.L);
      Divide_List(Lol_Best, Net.L, U, Sub_L1, Sub_L2);
      Remove(Net.L);
      Deallocate(U);
      if Number_Of_Elements(Sub_L1) = 0 then
        Remove(Sub_L1);
      elsif Number_Of_Elements(Sub_L2) = 0 then
        Remove(Sub_L2);
      else
        Update_List_Connected_Components(Gr, Sub_L1, Ls1);
        Update_List_Connected_Components(Gr, Sub_L2, Ls2);
        Ls := Join(Ls1, Ls2);
        Free(Ls1);
        Free(Ls2);
        Modularities(MT, MI, Ls, Lq, Sub_Q);
        if Sub_Q <= Net.Q.Total then
          Merge_Lists(Lol_Best, Ls, L);
          Restore_Modularity(MI, L);
        else
          Reset(Ls);
          Reset(Lq);
          while Has_Next(Ls) loop
            Sub_Net.L := Next(Ls);
            Sub_Net.Q := Next(Lq);
            if Number_Of_Elements(Sub_Net.L) > 1 then
              Create_Subgraph(Gr, Sub_Net.L, Sub_Net.Gr);
              Push(Sub_Net, St);
            end if;
          end loop;
          Improvement_Action(Log_Name, Lol_Best, Total_Modularity(MI));
         end if;
         Free(Ls);
         Free(Lq);
      end if;
    end loop;
    Free(St);
    Sort_Lists(Lol_Best);
    Q_Best := Total_Modularity(MI);
    Free(MI);
  end Execute_Repetition;

  -------------------------
  -- Spectral_Modularity --
  -------------------------

  procedure Spectral_Modularity(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    Gen: Generator;
    Lol_Rep: List_Of_Lists;
    Q_Rep: Modularity_Rec;
  begin
    Reset(Gen);
    Initialize(Lol_Best, Number_Of_Vertices(Gr), Together_Initialization);
    for NR in 1 .. Number_Of_Repetitions loop
      Execute_Repetition(MT, Gr, Log_Name, Gen, Lol_Ini, Lol_Rep, Q_Rep, R, Pc);
      if NR = 1 or else Q_Rep.Total > Q_Best.Total then
        Free(Lol_Best);
        Lol_Best := Clone(Lol_Rep);
        Q_Best := Q_Rep;
      end if;
      Free(Lol_Rep);
      Repetition_Action(Log_Name, Lol_Best, Q_Best);
    end loop;
  end Spectral_Modularity;

end Modularities_Spectral;
