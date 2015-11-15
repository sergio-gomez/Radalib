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


-- @filename Modularities_Spectral.ads
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Spectral Modularity Optimization

with Ada.Numerics.Float_Random;        use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with Utils;                            use Utils;
with Finite_Disjoint_Lists;            use Finite_Disjoint_Lists;
with Graphs_Double;                    use Graphs_Double;
with Graphs_Double_Modularities_D;     use Graphs_Double_Modularities_D;
with Linked_Lists;
with Stacks;

with Vectors;

generic

  Number_Of_Repetitions: Positive;

  with procedure Improvement_Action(
    Log_Name: in Unbounded_String;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

  with procedure Repetition_Action(
    Log_Name: in Unbounded_String;
    Lol: in List_Of_Lists;
    Q: in Modularity_Rec);

package Modularities_Spectral is

  -------------------------------------------------------------------------

  procedure Spectral_Modularity(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Unbounded_String;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  -------------------------------------------------------------------------

private

  -------------------------------------------------------------------------

  type Network is record
    L: List;
    Q: Modularity_Rec;
    Gr: Graph;
  end record;

  -------------------------------------------------------------------------

  package Double_Elementary_Functions is
    new Ada.Numerics.Generic_Elementary_Functions(Long_Float);
  use Double_Elementary_Functions;

  package Stacks_Of_Networks is new Stacks(Network);
  use Stacks_Of_Networks;

  package Vectors_Of_Double is new Vectors(Long_Float);
  use Vectors_Of_Double;

  package Lists_Of_Indices is new Linked_Lists(Positive);
  use Lists_Of_Indices;

  package Lists_Of_Lists is new Linked_Lists(List);
  use Lists_Of_Lists;

  package Lists_Of_Modularities is new Linked_Lists(Modularity_Rec);
  use Lists_Of_Modularities;

  -------------------------------------------------------------------------

  procedure Execute_Repetition(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Unbounded_String;
    Gen: in out Generator;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  procedure Index_Vector(
    Gen: in out Generator;
    Gr: in Graph;
    MI: in Modularity_Info;
    Net: in Network;
    U: out Vector;
    MT: in Modularity_Type;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  procedure Power_Iteration(
    Gen: in out Generator;
    Gr: in Graph;
    MI: in Modularity_Info;
    Net: in Network;
    MI_Sub: in Modularity_Info;
    Val_Bound: in Long_Float;
    Val: out Long_Float;
    Y_Norm: out Vector);

  procedure Multiply_Modularity_Matrix(
    Gr: in Graph;
    MI_Ini: in Modularity_Info;
    Net: in Network;
    MI: in Modularity_Info;
    Val_Bound: in Long_Float;
    X: in Vector;
    Y: out Vector);

  procedure Strength_Vector(
    Gr: in Graph;
    MI: in Modularity_Info;
    L: in List;
    W: out Vector);

  procedure Adjacency_Row(
    Gr: in Graph;
    I: in Positive;
    Ai: out Vector);

  procedure Get_Indices(
    L: in List;
    L_Ind: out Lists_Of_Indices.Linked_List);

  procedure Divide_List(
    Lol: in out List_Of_Lists;
    L: in out List;
    U: in Vector;
    Sub_L1, Sub_L2: out List);

  procedure Connected_Components(
    Gr_Ini: in Graph;
    Lol: in out List_Of_Lists;
    L_Ind: in Lists_Of_Indices.Linked_List;
    L_L: out Lists_Of_Lists.Linked_List);

  procedure Connected_List(
    L_Conn: in List;
    Lol: in out List_Of_Lists;
    L: out List);

  procedure Modularities(
    MT: in Modularity_Type;
    MI: in out Modularity_Info;
    L_L: in Lists_Of_Lists.Linked_List;
    L_Q: out Lists_Of_Modularities.Linked_List;
    Sub_Q: out Long_Float);

  procedure Merge_Lists(
    Lol: in out List_Of_Lists;
    L_L: in out Lists_Of_Lists.Linked_List);

  -------------------------------------------------------------------------

end Modularities_Spectral;
