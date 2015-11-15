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


-- @filename Graphs_String_Multilayer.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 13/10/2014
-- @revision 29/12/2014
-- @brief Instantiation of Graphs.Multilayer to String Edge Values

with Graphs_String;
with Graphs.Multilayer;
with Utils; use Utils;

package Graphs_String_Multilayer is
  new Graphs_String.Multilayer;
