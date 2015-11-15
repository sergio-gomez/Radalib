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


-- @filename Graphs_Float_Structure.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2009
-- @revision 10/10/2009
-- @brief Instantiation of Graphs.Structure to Float Edge Values

with Graphs_Float;
with Graphs.Structure;
with Utils; use Utils;

package Graphs_Float_Structure is
  new Graphs_Float.Structure(Zero_Value => 0.0, Edge_Valuess => Floatss, PEdge_Valuess => PFloatss);
