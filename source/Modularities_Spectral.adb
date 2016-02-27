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


-- @filename Modularities_Spectral.adb
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Spectral Modularity Optimization

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;
with Graphs_Double_Algorithms;         use Graphs_Double_Algorithms;

package body Modularities_Spectral is

   -------------------------------------------------------------------------
   -- Adjacency_Row --
   -------------------

   procedure Adjacency_Row(
      Gr: in Graph;
      I: in Positive;
      Ai: out Vector)
   is
      Num: Natural;
      Vi, Vj: Vertex;
      ELi: Edges_List;
      Eij: Edge;
      Aij: Long_Float;
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
      while Has_Next(ELi) loop
         Eij := Next(ELi);
         Aij := Value(Eij);
         Vj := To(Eij);
         J := Index_Of(Vj);
         Set(Ai, J, Aij);
      end loop;
      Restore(ELi);
   end Adjacency_Row;

   -------------------------------------------------------------------------
   -- Connected_Components --
   --------------------------

   procedure Connected_Components(
      Gr_Ini: in Graph;
      Lol: in out List_Of_Lists;
      L_Ind: in Lists_Of_Indices.Linked_List;
      L_L: out Lists_Of_Lists.Linked_List)
   is
      Lol_Conn: List_Of_Lists;
      Index, I: Positive;
      E, E_Conn: Finite_Disjoint_Lists.Element;
      Unass_L, L_Conn, L: List;
   begin
      Initialize(L_L);
      Connected_Components(Gr_Ini, Lol, Lol_Conn);
      Save(L_Ind);
      Reset(L_Ind);
      while Has_Next(L_Ind) loop
         Index := Next(L_Ind);
         E := Get_Element(Lol, Index);
         Unassign(E);
      end loop;
      Restore(L_Ind);
      Remove_Empty(Lol);
      Unass_L := Unassigned_List(Lol);
      while Number_Of_Elements(Unass_L) > 0 loop
         Reset(Unass_L);
         E := Get_Element(Unass_L);
         I := Index_Of(E);
         E_Conn := Get_Element(Lol_Conn, I);
         L_Conn := List_Of(E_Conn);
         Connected_List(L_Conn, Lol, L);
         Add_Last(L, L_L);
      end loop;
      Free(Lol_Conn);
   end Connected_Components;

   -------------------------------------------------------------------------
   -- Connected_List --
   --------------------

   procedure Connected_List(
      L_Conn: in List;
      Lol: in out List_Of_Lists;
      L: out List)
   is
      E_Conn, E: Finite_Disjoint_Lists.Element;
      I: Positive;
   begin
      L := New_List(Lol);
      Save(L_Conn);
      Reset(L_Conn);
      while Has_Next_Element(L_Conn) loop
         E_Conn := Next_Element(L_Conn);
         I := Index_Of(E_Conn);
         E := Get_Element(Lol, I);
         Move(E, L);
      end loop;
      Restore(L_Conn);
   end Connected_List;

   -------------------------------------------------------------------------
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
      Ui: Long_Float;
   begin
      Sub_L1 := New_List(Lol);
      Sub_L2 := New_List(Lol);
      Num := Number_Of_Elements(L);
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
      Remove(L);
   end Divide_List;

   -------------------------------------------------------------------------
   -- Execute_Repetition --
   ------------------------

   procedure Execute_Repetition(
      MT: in Modularity_Type;
      Gr: in Graph;
      Log_Name: in Unbounded_String;
      Gen: in out Generator;
      Lol_Best: out List_Of_Lists;
      Q_Best: out Modularity_Rec;
      R: in Double := No_Resistance;
      Pc: in Double := 1.0)
   is
      MI: Modularity_Info;
      Net, Sub_Net: Network;
      St: Stack;
      U: Vector;
      Sub_L1, Sub_L2: List;
      L_Ind: Lists_Of_Indices.Linked_List;
      L_L: Lists_Of_Lists.Linked_List;
      L_Q: Lists_Of_Modularities.Linked_List;
      Sub_Q: Long_Float;
   begin
      Initialize(MI, Gr, MT, R, Pc);
      Initialize(Lol_Best, Number_Of_Vertices(Gr), Together_Initialization);
      Reset(Lol_Best);
      Net.L := Get_List(Lol_Best);
      Update_Modularity(MI, Net.L, MT);
      Net.Q := Partial_Modularity(MI, Net.L);
      Net.Gr := Clone(Gr);
      Initialize(St);
      Push(Net, St);
      while not Is_Empty(St) loop
         Net := Pop(St);
         Index_Vector(Gen, Gr, MI, Net, U, MT, R, Pc);
         Free(Net.Gr);
         Get_Indices(Net.L, L_Ind);
         Divide_List(Lol_Best, Net.L, U, Sub_L1, Sub_L2);
         Deallocate(U);
         if Number_Of_Elements(Sub_L1) = 0 then
            Remove(Sub_L1);
         elsif Number_Of_Elements(Sub_L2) = 0 then
            Remove(Sub_L2);
         else
            Connected_Components(Gr, Lol_Best, L_Ind, L_L);
            Modularities(MT, MI, L_L, L_Q, Sub_Q);
            if Sub_Q <= Net.Q.Total then
               Merge_Lists(Lol_Best, L_L);
            else
               Reset(L_L);
               Reset(L_Q);
               while Has_Next(L_L) loop
                  Sub_Net.L := Next(L_L);
                  Sub_Net.Q := Next(L_Q);
                  if Number_Of_Elements(Sub_Net.L) > 1 then
                     Create_Subgraph(Gr, Sub_Net.L, Sub_Net.Gr);
                     Push(Sub_Net, St);
                  end if;
               end loop;
               Improvement_Action(Log_Name, Lol_Best, Total_Modularity(MI));
            end if;
            Free(L_L);
            Free(L_Q);
         end if;
         Free(L_Ind);
      end loop;
      Free(St);
      Sort_Lists(Lol_Best);
      Q_Best := Modularity(MI, Lol_Best, MT);
      Free(MI);
   end Execute_Repetition;

   -------------------------------------------------------------------------
   -- Get_Indices --
   -----------------

   procedure Get_Indices(
      L: in List;
      L_Ind: out Lists_Of_Indices.Linked_List)
   is
      Num_Elements: Natural;
      E: Finite_Disjoint_Lists.Element;
      Index: Positive;
   begin
      Initialize(L_Ind);
      Num_Elements := Number_Of_Elements(L);
      Save(L);
      Reset(L);
      for I in 1 .. Num_Elements loop
         E := Next_Element(L);
         Index := Index_Of(E);
         Add(Index, L_Ind);
      end loop;
      Restore(L);
   end Get_Indices;

   -------------------------------------------------------------------------
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
      Val: Long_Float;
   begin
      Initialize(MI_Sub, Net.Gr, MT, R, Pc);
      Power_Iteration(Gen, Gr, MI, Net, MI_Sub, 0.0, Val, U);
      if Val < 0.0 then
         Power_Iteration(Gen, Gr, MI, Net, MI_Sub, Val, Val, U);
      end if;
      Free(MI_Sub);
   end Index_Vector;

   -------------------------------------------------------------------------
   -- Merge_Lists --
   -----------------

   procedure Merge_Lists(
      Lol: in out List_Of_Lists;
      L_L: in out Lists_Of_Lists.Linked_List)
   is
      L, Sub_L: List;
   begin
      L := New_List(Lol);
      Reset(L_L);
      while Has_Next(L_L) loop
         Sub_L := Next(L_L);
         Move(Sub_L, L);
         Remove(Sub_L);
      end loop;
   end Merge_Lists;

   -------------------------------------------------------------------------
   -- Modularities --
   ------------------

   procedure Modularities(
      MT: in Modularity_Type;
      MI: in out Modularity_Info;
      L_L: in Lists_Of_Lists.Linked_List;
      L_Q: out Lists_Of_Modularities.Linked_List;
      Sub_Q: out Long_Float)
   is
      L: List;
      Q: Modularity_Rec;
   begin
      Initialize(L_Q);
      Sub_Q := 0.0;
      Save(L_L);
      Reset(L_L);
      while Has_Next(L_L) loop
         L := Next(L_L);
         Update_Modularity(MI, L, MT);
         Q := Partial_Modularity(MI, L);
         Add_Last(Q, L_Q);
         Sub_Q := Sub_Q + Q.Total;
      end loop;
      Restore(L_L);
   end Modularities;

   -------------------------------------------------------------------------
   -- Multiply_Modularity_Matrix --
   --------------------------------

   procedure Multiply_Modularity_Matrix(
      Gr: in Graph;
      MI_Ini: in Modularity_Info;
      Net: in Network;
      MI: in Modularity_Info;
      Val_Bound: in Long_Float;
      X: in Vector;
      Y: out Vector)
   is
      W2, W_Gr, WX, AiX, Wi_Gr, Wi, Xi, Yi: Long_Float;
      W, Ai: Vector;
      Num: Natural;
      Vi: Vertex;
   begin
      W2 := Total_Strength(MI_Ini);
      Strength_Vector(Gr, MI_Ini, Net.L, W);
      W_Gr := Sum(W);
      WX := Dot_Product(W, X);
      Num := Number_Of_Elements(Net.L);
      Allocate(Y, Num);
      for I in 1 .. Num loop
         Adjacency_Row(Net.Gr, I, Ai);
         AiX := Dot_Product(Ai, X);
         Deallocate(Ai);
         Vi := Get_Vertex(Net.Gr, I);
         Wi_Gr := Strength_From(MI, Vi);
         Wi := Get(W, I);
         Xi := Get(X, I);
         Yi := AiX - (WX / W2) * Wi + Xi * (-Wi_Gr + (W_Gr / W2) * Wi - Val_Bound);
         Set(Y, I, Yi);
      end loop;
      Deallocate(W);
   end Multiply_Modularity_Matrix;

   -------------------------------------------------------------------------
   -- Power_Iteration --
   ---------------------

   procedure Power_Iteration(
      Gen: in out Generator;
      Gr: in Graph;
      MI: in Modularity_Info;
      Net: in Network;
      MI_Sub: in Modularity_Info;
      Val_Bound: in Long_Float;
      Val: out Long_Float;
      Y_Norm: out Vector)
   is
      Epsilon: constant Long_Float := 1.0E-3;
      Num: Natural;
      Y, X_Norm, YX: Vector;
      N_YX: Long_Float;
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
         Multiply_Modularity_Matrix(Gr, MI, Net, MI_Sub, Val_Bound, X_Norm, Y);
         Val := Dot_Product(X_Norm, Y);
         Normalise(Y, Y_Norm);
         Deallocate(Y);
         if Val > 0.0 then
            Subtract(Y_Norm, X_Norm, YX);
         else
            Add(Y_Norm, X_Norm, YX);
         end if;
         Deallocate(X_Norm);
         N_YX := Norm(YX);
         Deallocate(YX);
      end loop;
   end Power_Iteration;

   -------------------------------------------------------------------------
   -- Spectral_Modularity --
   -------------------------

   procedure Spectral_Modularity(
      MT: in Modularity_Type;
      Gr: in Graph;
      Log_Name: in Unbounded_String;
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
         Execute_Repetition(MT, Gr, Log_Name, Gen, Lol_Rep, Q_Rep, R, Pc);
         if NR = 1 or else Q_Rep.Total > Q_Best.Total then
            Free(Lol_Best);
            Lol_Best := Clone(Lol_Rep);
            Q_Best := Q_Rep;
         end if;
         Free(Lol_Rep);
         Repetition_Action(Log_Name, Lol_Best, Q_Best);
      end loop;
   end Spectral_Modularity;

   -------------------------------------------------------------------------
   -- Strength_Vector --
   ---------------------

   procedure Strength_Vector(
      Gr: in Graph;
      MI: in Modularity_Info;
      L: in List;
      W: out Vector)
   is
      Num: Natural;
      E: Finite_Disjoint_Lists.Element;
      I: Positive;
      Vi: Vertex;
      Wj: Long_Float;
   begin
      Num := Number_Of_Elements(L);
      Allocate(W, Num);
      Save(L);
      Reset(L);
      for J in 1 .. Num loop
         E := Next_Element(L);
         I := Index_Of(E);
         Vi := Get_Vertex(Gr, I);
         Wj := Strength_From(MI, Vi);
         Set(W, J, Wj);
      end loop;
      Restore(L);
   end Strength_Vector;

   -------------------------------------------------------------------------

end Modularities_Spectral;
