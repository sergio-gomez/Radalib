-- Radalib, Copyright (c) 2021 by
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


-- @filename Graphs_String.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 21/03/2008
-- @revision 26/10/2014
-- @brief Instantiation of Graphs to String Edge Values

with Utils; use Utils;
with Graphs;

package Graphs_String is
  new Graphs(Edge_Value => Ustring, Default_Edge_Value => S2U("1"));
