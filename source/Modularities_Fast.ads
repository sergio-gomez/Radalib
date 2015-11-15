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


-- @filename Modularities_Fast.ads
-- @author Javier Borge
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/11/2007
-- @revision 26/10/2014
-- @brief Newman's Fast Algorithm implementation

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Utils; use Utils;

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

package Modularities_Fast is

  procedure Newman_Fast_Algorithm(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name : in Unbounded_String;
    Lol_Ini : in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

private

  type Guanys is record
    Quant: Double;
    List1, List2: List;
  end record;

  type Lists_Vector is array (Integer range <>) of List;
  type PLists_Vector is access Lists_Vector;
  procedure Free is new Ada.Unchecked_Deallocation (Lists_Vector, PLists_Vector);

  procedure Create_Adjacent_Lists_Graph(
    Gr: in Graph;
    Lol: in List_Of_Lists;
    Gr_Adj: out Graph;
    Lists_Map: out PLists_Vector);

  procedure Initialize_Adjacent_Lists_Graph(
    Gr_Adj: in Graph;
    Lists_Map: in PLists_Vector;
    Mi: in Modularity_Info;
    Mt: in Modularity_Type);

  function Modularity_Merging_Variation(
    Mi: in Modularity_Info;
    Mt: in Modularity_Type;
    Li, Lj: in List) return Double;

  procedure Maximum_Modularity_Merging(
    Gr_Adj: in Graph;
    I_Max, J_Max: out Positive;
    Max_Delta_Q: out Double);

  procedure Merge_Adjacent_Lists(
    Gr_Adj: in Graph;
    Lists_Map: in PLists_Vector;
    I, J: in Positive;
    Mi: in Modularity_Info;
    Mt: in Modularity_Type);

  procedure Optimization_Process(
    Mi: in Modularity_Info;
    Mt: in Modularity_Type;
    Gr_Adj: in Graph;
    Lists_Map: in PLists_Vector;
    Steps: in Positive;
    Finished: out Boolean);

  function Get_Steps(Nv, Nl: in Natural) return Positive;

  procedure Execute_Repetition(
    Mt: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Unbounded_String;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularities_Fast;
