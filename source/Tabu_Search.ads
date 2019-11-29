-- Radalib, Copyright (c) 2019 by
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


-- @filename Tabu_Search.ads
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Generic Tabu Search

with Ada.Numerics.Float_Random;   use Ada.Numerics.Float_Random;

generic

   -------------------------------------------------------------------------

   Number_Of_Repetitions: Positive;

   -------------------------------------------------------------------------

   type Problem    is limited private;
   type Solution   is limited private;
   type Tabu_Moves is limited private;

   -------------------------------------------------------------------------

   with function Maximum_Of_Nonimprovements(
      pro    : in     Problem)            -- the problem
               return Natural;            -- max. iters. without improvement

   with procedure Copy_Solution(
      sol_ori: in     Solution;           -- solution original
      sol_cop:    out Solution);          -- solution copy

   with function Better_Solution(
      sol1   : in     Solution;           -- solution 1
      sol2   : in     Solution)           -- solution 2
               return Boolean;            -- sol. 1 better than sol. 2

   with procedure Improvement_Action(
      pro    : in     Problem;            -- the problem
      sol    : in     Solution);          -- the solution

   with procedure Repetition_Action(
      pro    : in     Problem;            -- the problem
      sol    : in     Solution);          -- the solution

   with procedure Deallocate_Solution(
      sol    : in out Solution);          -- the solution

   with function Number_Of_Moves(
      pro    : in     Problem)            -- the problem
               return Positive;           -- number of moves

   with procedure Solution_Neighbourhood(
      pro    : in     Problem;            -- the problem
      sol_ori: in     Solution;           -- solution original
      nm     : in     Positive;           -- number of move
      gen    : in out Generator;          -- random numbers generator
      sol_ngh:    out Solution);          -- solution neighbourhood

   with function Tabu_Tenure(
      pro    : in     Problem)            -- the problem
               return Natural;            -- tabu tenure

   with procedure Allocate_Tabu_Moves(
      nb_mov : in     Positive;           -- number of moves
      tab_mov:    out Tabu_Moves);        -- the tabu moves

   with procedure Deallocate_Tabu_Moves(
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   with procedure Set_Tabu_Move(
      val    : in     Natural;            -- value
      nm     : in     Positive;           -- number of move
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   with function Tabu_Move(
      nm     : in     Positive;           -- number of move
      tab_mov: in     Tabu_Moves)         -- the tabu moves
               return Natural;            -- value

   -------------------------------------------------------------------------

package Tabu_Search is

   -------------------------------------------------------------------------

   procedure Optimise(
      pro    : in     Problem;            -- the problem
      sol_ini: in     Solution;           -- solution initial
      sol_bst:    out Solution);          -- solution best

   -------------------------------------------------------------------------

private

   -------------------------------------------------------------------------

   procedure Execute_Repetition(
      pro    : in     Problem;            -- the problem
      sol_ini: in     Solution;           -- solution initial
      gen    : in out Generator;          -- random numbers generator
      sol_bst:    out Solution);          -- solution best

   procedure Initialise_Tabu_Moves(
      pro    : in     Problem;            -- the problem
      tab_mov:    out Tabu_Moves);        -- the tabu moves

   procedure Decrease_Tabu_Moves(
      pro    : in     Problem;            -- the problem
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   procedure Examine_Neighbourhood(
      pro    : in     Problem;            -- the problem
      sol_bst: in     Solution;           -- solution best
      sol_ori: in     Solution;           -- solution original
      gen    : in out Generator;          -- random numbers generator
      tab_mov: in out Tabu_Moves;         -- the tabu moves
      nm_bst :    out Natural;            -- number of move best
      sol_ngh:    out Solution);          -- solution neighbourhood best

   -------------------------------------------------------------------------

end Tabu_Search;
