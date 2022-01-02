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


-- @filename Modularities_Louvain.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 12/12/2017
-- @revision 10/09/2020
-- @brief Louvain Algorithm implementation

with Ada.Unchecked_Deallocation;

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Operations; use Graphs_Double_Operations;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;
with Utils; use Utils;

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

package Modularities_Louvain is

  procedure Louvain_Algorithm(
    MT: in Modularity_Type;
    Gr: in Graph;
    Log_Name : in Ustring;
    Lol_Ini : in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

private

  type Lists_Vector is array (Integer range <>) of List;
  type PLists_Vector is access Lists_Vector;
  procedure Free is new Ada.Unchecked_Deallocation (Lists_Vector, PLists_Vector);

  function Modularity_Variation(
    Mi: in Modularity_Info;
    Mt: in Modularity_Type;
    Ei: in Finite_Disjoint_Lists.Element;
    Li, Lj: in List) return Double;

  procedure Optimization_Process(
    Log_Name: in Ustring;
    Mt: in Modularity_Type;
    Gr: in Graph;
    Lol: in out List_Of_Lists;
    Q: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  procedure Execute_Repetition(
    Mt: in Modularity_Type;
    Gr: in Graph;
    Log_Name: in Ustring;
    Lol_Ini: in List_Of_Lists;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularities_Louvain;
