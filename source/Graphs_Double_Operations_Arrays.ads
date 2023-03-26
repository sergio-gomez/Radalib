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


-- @filename Graphs_Double_Operations_Arrays.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 29/08/2009
-- @revision 25/08/2020
-- @brief Instantiation of Graphs.Operations.Arrays to Double Edge Values

with Graphs_Double_Operations;
with Graphs.Operations.Arrays;
with Utils; use Utils;

package Graphs_Double_Operations_Arrays is
  new Graphs_Double_Operations.Arrays(Edge_Values => Doubles, PEdge_Values => PDoubles,
                                      Edge_Valuess => Doubless, PEdge_Valuess => PDoubless);
