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


-- @filename Tabu_Search.adb
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Generic Tabu Search

package body Tabu_Search is

   -------------------------------------------------------------------------
   -- Decrease_Tabu_Moves --
   -------------------------

   procedure Decrease_Tabu_Moves(
      pro    : in     Problem;            -- the problem
      tab_mov: in out Tabu_Moves)         -- the tabu moves
   is
      nb_mov : Positive;           -- number of moves
      val    : Natural;            -- value
   begin
      nb_mov := Number_Of_Moves(pro);
      for nm in 1 .. nb_mov loop
         val := Tabu_Move(nm, tab_mov);
         if val > 0 then
            Set_Tabu_Move(val-1, nm, tab_mov);
         end if;
      end loop;
   end Decrease_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Examine_Neighbourhood --
   ---------------------------

   procedure Examine_Neighbourhood(
      pro    : in     Problem;            -- the problem
      sol_bst: in     Solution;           -- solution best
      sol_ori: in     Solution;           -- solution original
      gen    : in out Generator;          -- random numbers generator
      tab_mov: in out Tabu_Moves;         -- the tabu moves
      nm_bst :    out Natural;            -- number of move best
      sol_ngh:    out Solution)           -- solution neighbourhood best
   is
      nb_mov : Positive;           -- number of moves
      sol_cur: Solution;           -- solution neighbourhood current
   begin
      nm_bst := 0;
      nb_mov := Number_Of_Moves(pro);
      Copy_Solution(sol_ori, sol_ngh);
      for nm in 1 .. nb_mov loop
         Solution_Neighbourhood(pro, sol_ori, nm, gen, sol_cur);
         if Better_Solution(sol_cur, sol_bst) then
            Set_Tabu_Move(0, nm, tab_mov);
         end if;
         if Tabu_Move(nm, tab_mov) = 0 then
            if nm_bst = 0 or else Better_Solution(sol_cur, sol_ngh) then
               nm_bst := nm;
               Deallocate_Solution(sol_ngh);
               Copy_Solution(sol_cur, sol_ngh);
            end if;
         end if;
         Deallocate_Solution(sol_cur);
      end loop;
   end Examine_Neighbourhood;

   -------------------------------------------------------------------------
   -- Execute_Repetition --
   ------------------------

   procedure Execute_Repetition(
      pro    : in     Problem;            -- the problem
      sol_ini: in     Solution;           -- solution initial
      gen    : in out Generator;          -- random numbers generator
      sol_bst:    out Solution)           -- solution best
   is
      sol_ori: Solution;           -- solution original
      tab_mov: Tabu_Moves;         -- the tabu moves
      tab_ten: Natural;            -- tabu tenure
      max_ite: Natural;            -- max. iters. without improvement
      nb_ite : Natural;            -- nb. iters. without improvement
      nm_bst : Natural;            -- number of move best
      sol_ngh: Solution;           -- solution neighbourhood
   begin
      Copy_Solution(sol_ini, sol_bst);
      Copy_Solution(sol_bst, sol_ori);
      Initialise_Tabu_Moves(pro, tab_mov);
      tab_ten := Natural'Min(Tabu_Tenure(pro), Number_Of_Moves(pro)-1);
      max_ite := Maximum_Of_Nonimprovements(pro);
      nb_ite := 0;
      while nb_ite <= max_ite loop
         Examine_Neighbourhood(pro, sol_bst, sol_ori, gen, tab_mov, nm_bst,
               sol_ngh);
         Decrease_Tabu_Moves(pro, tab_mov);
         nb_ite := nb_ite + 1;
         if nm_bst > 0 then
            Set_Tabu_Move(tab_ten, nm_bst, tab_mov);
            Deallocate_Solution(sol_ori);
            Copy_Solution(sol_ngh, sol_ori);
            if Better_Solution(sol_ngh, sol_bst) then
               Deallocate_Solution(sol_bst);
               Copy_Solution(sol_ngh, sol_bst);
               Improvement_Action(pro, sol_bst);
               nb_ite := 0;
            end if;
         end if;
         Deallocate_Solution(sol_ngh);
      end loop;
      Deallocate_Tabu_Moves(tab_mov);
      Deallocate_Solution(sol_ori);
   end Execute_Repetition;

   -------------------------------------------------------------------------
   -- Initialise_Tabu_Moves --
   ---------------------------

   procedure Initialise_Tabu_Moves(
      pro    : in     Problem;            -- the problem
      tab_mov:    out Tabu_Moves)         -- the tabu moves
   is
      nb_mov : Positive;           -- number of moves
   begin
      nb_mov := Number_Of_Moves(pro);
      Allocate_Tabu_Moves(nb_mov, tab_mov);
      for nm in 1 .. nb_mov loop
         Set_Tabu_Move(0, nm, tab_mov);
      end loop;
   end Initialise_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Optimise --
   --------------

   procedure Optimise(
      pro    : in     Problem;            -- the problem
      sol_ini: in     Solution;           -- solution initial
      sol_bst:    out Solution)           -- solution best
   is
      gen    : Generator;          -- random numbers generator
      sol_rep: Solution;           -- solution repetition
   begin
      Reset(gen);
      Copy_Solution(sol_ini, sol_bst);
      for nr in 1 .. Number_Of_Repetitions loop
         Execute_Repetition(pro, sol_ini, gen, sol_rep);
         if nr = 1 or else Better_Solution(sol_rep, sol_bst) then
            Deallocate_Solution(sol_bst);
            Copy_Solution(sol_rep, sol_bst);
         end if;
         Deallocate_Solution(sol_rep);
         Repetition_Action(pro, sol_bst);
      end loop;
   end Optimise;

   -------------------------------------------------------------------------

end Tabu_Search;
