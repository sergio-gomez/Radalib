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


-- @filename Modularities_Spectral.ads
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 31/08/2020
-- @brief Spectral Modularity Optimization

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Utils; use Utils;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Operations; use Graphs_Double_Operations;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;

with Linked_Lists;
with Stacks;
with Vectors;

generic

  Number_Of_Repetitions: Positive;

  with procedure Improvement_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec;
    Us: in Ustring := Null_Ustring);

  with procedure Repetition_Action(
    Log_Name: in Ustring;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

package Modularities_Spectral is

  procedure Spectral_Modularity(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

private

  type Network is record
    L: List;
    Q: Modularity_Rec;
    Gr: Graph;
  end record;

  package Stacks_Of_Networks is new Stacks(Network);
  use Stacks_Of_Networks;

  package Vectors_Of_Double is new Vectors(Double);
  use Vectors_Of_Double;

  use Linked_Lists_Of_Lists;

  package Lists_Of_Modularities is new Linked_Lists(Modularity_Rec);
  use Lists_Of_Modularities;

  procedure Calculate_Strengths(
    Gr: in Graph;
    MT: in Modularity_Type;
    MI: in Modularity_Info;
    W_From: out Vector;
    W_To: out Vector);

  procedure Calculate_Strengths(
    Gr: in Graph;
    MT: in Modularity_Type;
    MI: in Modularity_Info;
    L: in List;
    W2: out Double;
    W_From: out Vector;
    W_To: out Vector);

  procedure Adjacency_Row(
    Gr: in Graph;
    MT: in Modularity_Type;
    I: in Positive;
    Ai: out Vector);

  procedure Adjacency_Column(
    Gr: in Graph;
    MT: in Modularity_Type;
    I: in Positive;
    Ai: out Vector);

  procedure Multiply_Modularity_Matrix(
    Gr: in Graph;
    MT: in Modularity_Type;
    MI_Ini: in Modularity_Info;
    Net: in Network;
    MI: in Modularity_Info;
    Val_Bound: in Double;
    X: in Vector;
    Y: out Vector);

  procedure Power_Iteration(
    Gen: in out Generator;
    Gr: in Graph;
    MT: in Modularity_Type;
    MI: in Modularity_Info;
    Net: in Network;
    MI_Sub: in Modularity_Info;
    Val_Bound: in Double;
    Val: out Double;
    Y_Norm: out Vector);

  procedure Index_Vector(
    Gen: in out Generator;
    Gr: in Graph;
    MI: in Modularity_Info;
    Net: in Network;
    U: out Vector;
    MT: in Modularity_Type;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  procedure Divide_List(
    Lol: in out List_Of_Lists;
    L: in out List;
    U: in Vector;
    Sub_L1, Sub_L2: out List);

  procedure Modularities(
    MT: in Modularity_Type;
    MI: in out Modularity_Info;
    Ls: in Linked_Lists_Of_Lists.Linked_List;
    Lq: out Lists_Of_Modularities.Linked_List;
    Sub_Q: out Double);

  procedure Merge_Lists(
    Lol: in out List_Of_Lists;
    Ls: in out Linked_Lists_Of_Lists.Linked_List;
    L: out List);

  procedure Execute_Repetition(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Gen: in out Generator;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularities_Spectral;
