-- Radalib, Copyright (c) 2017 by
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


-- @filename Graphs_Double_Algorithms.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 16/08/2006
-- @revision 28/08/2009
-- @brief Instantiation of Graphs.Algorithms to Double Edge Values

with Graphs_Double;
with Graphs.Algorithms;

package Graphs_Double_Algorithms is
  new Graphs_Double.Algorithms;
