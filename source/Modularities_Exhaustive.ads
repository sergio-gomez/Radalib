-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


-- @filename Modularities_Exhaustive.ads
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 31/08/2020
-- @brief Exhaustive search Modularity Optimization

with Utils; use Utils;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Operations; use Graphs_Double_Operations;
with Graphs_Double_Modularities_D; use Graphs_Double_Modularities_D;

package Modularities_Exhaustive is

  procedure Exhaustive_Modularity(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    MT: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

  procedure Exhaustive_Modularity(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    MT: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0);

end Modularities_Exhaustive;
