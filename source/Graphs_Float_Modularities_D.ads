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


-- @filename Graphs_Float_Modularities_D.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 15/08/2006
-- @revision 01/06/2009
-- @brief Instantiation of Graphs.Modularities to Float Edge Values and Double precision

with Graphs_Float;
with Graphs.Modularities;
with Utils; use Utils;

package Graphs_Float_Modularities_D is
  new Graphs_Float.Modularities(Num => Double, Nums => Doubles, PNums => PDoubles,
                                Numss => Doubless, PNumss => PDoubless);
