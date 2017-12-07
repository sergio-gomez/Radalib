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


-- @filename Modularities_Exhaustive.adb
-- @author Alberto Fernandez
-- @author Sergio Gomez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Exhaustive search Modularity Optimization

with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Modularities_Exhaustive is

  ---------------------------
  -- Exhaustive_Modularity --
  ---------------------------

  procedure Exhaustive_Modularity(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Mt: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    Degeneration: Positive;
  begin
    Exhaustive_Modularity(Gr, Lol_Best, Q_Best, Degeneration, Mt, R, Pc);
  end Exhaustive_Modularity;

  ---------------------------
  -- Exhaustive_Modularity --
  ---------------------------

  procedure Exhaustive_Modularity(
    Gr: in Graph;
    Lol_Best: out List_Of_Lists;
    Q_Best: out Modularity_Rec;
    Degeneration: out Positive;
    Mt: in Modularity_Type := Weighted_Newman;
    R: in Double := No_Resistance;
    Pc: in Double := 1.0)
  is
    procedure Evaluate_Lol(Lol: in List_Of_Lists) is
      Degeneration_Epsilon : constant Double := 1.0E-6;
      Q: Modularity_Rec;
    begin
      Q := Modularity(Gr, Lol, Mt, R, Pc);
      if Q.Total > Q_Best.Total then
        if Q.Total - Q_Best.Total < Degeneration_Epsilon then
          Degeneration := Degeneration + 1;
        else
          Degeneration := 1;
        end if;
        Q_Best := Q;
        Free(Lol_Best);
        Lol_Best := Clone(Lol);
      elsif Q_Best.Total - Q.Total < Degeneration_Epsilon then
        Degeneration := Degeneration + 1;
      end if;
    end Evaluate_Lol;

    procedure All_Partitions_Traversal is
      new Generic_All_Partitions_Traversal(Handler => Evaluate_Lol);

    Num_Vertices: Natural;
  begin
    Num_Vertices := Number_Of_Vertices(Gr);
    Initialize(Lol_Best, Num_Vertices, Together_Initialization);
    Q_Best := (Double'First, 0.0, Double'First);
    Degeneration := 1;
    All_Partitions_Traversal(Num_Vertices);
    Sort_Lists(Lol_Best);
  end Exhaustive_Modularity;

end Modularities_Exhaustive;
