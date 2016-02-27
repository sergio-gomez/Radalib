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


-- @filename Graphs_Double_Multilayer.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/10/2014
-- @revision 13/10/2014
-- @brief Instantiation of Graphs.Multilayer to Double Edge Values

with Graphs_Double;
with Graphs.Multilayer;

package Graphs_Double_Multilayer is
  new Graphs_Double.Multilayer;
