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


with Ada.Float_Text_IO;                  use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Text_IO;                        use Ada.Text_IO;

with Tabu_Search;

procedure Tabu_Search_Test is

   -------------------------------------------------------------------------

   type Problem is record
      x_max  : Float;              -- angle maximum
      x_min  : Float;              -- angle minimum
      x_inc  : Float;              -- angle increment
   end record;

   type Solution is record
      x      : Float;              -- angle
      s_x    : Float;              -- sine of angle
   end record;

   type Tabu_Moves is array (1..2) of Natural;

   -------------------------------------------------------------------------

   function Maximum_Of_Nonimprovements(
      pro    : in     Problem)            -- the problem
               return Natural;            -- max. iters. without improvement

   procedure Copy_Solution(
      sol_ori: in     Solution;           -- solution original
      sol_cop:    out Solution);          -- solution copy

   function Better_Solution(
      sol1   : in     Solution;           -- solution 1
      sol2   : in     Solution)           -- solution 2
               return Boolean;            -- sol. 1 better than sol. 2

   procedure Improvement_Action(
      pro    : in     Problem;            -- the problem
      sol    : in     Solution);          -- the solution

   procedure Repetition_Action(
      pro    : in     Problem;            -- the problem
      sol    : in     Solution);          -- the solution

   procedure Deallocate_Solution(
      sol    : in out Solution);          -- the solution

   procedure Solution_Initial(
      pro    : in     Problem;            -- the problem
      sol_ini:    out Solution);          -- solution initial

   function Number_Of_Moves(
      pro    : in     Problem)            -- the problem
               return Positive;           -- number of moves

   procedure Solution_Neighbourhood(
      pro    : in     Problem;            -- the problem
      sol_ori: in     Solution;           -- solution original
      nm     : in     Positive;           -- number of move
      gen    : in out Generator;          -- random numbers generator
      sol_ngh:    out Solution);          -- solution neighbourhood

   function Tabu_Tenure(
      pro    : in     Problem)            -- the problem
               return Natural;            -- tabu tenure

   procedure Allocate_Tabu_Moves(
      nb_mov : in     Positive;           -- number of moves
      tab_mov:    out Tabu_Moves);        -- the tabu moves

   procedure Deallocate_Tabu_Moves(
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   procedure Set_Tabu_Move(
      val    : in     Natural;            -- value
      nm     : in     Positive;           -- number of move
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   function Tabu_Move(
      nm     : in     Positive;           -- number of move
      tab_mov: in     Tabu_Moves)         -- the tabu moves
               return Natural;            -- value

   -------------------------------------------------------------------------

   package Tabu_Sine is new Tabu_Search(
      Number_Of_Repetitions      => 2,
      Problem                    => Problem,
      Solution                   => Solution,
      Tabu_Moves                 => Tabu_Moves,
      Maximum_Of_Nonimprovements => Maximum_Of_Nonimprovements,
      Copy_Solution              => Copy_Solution,
      Better_Solution            => Better_Solution,
      Improvement_Action         => Improvement_Action,
      Repetition_Action          => Repetition_Action,
      Deallocate_Solution        => Deallocate_Solution,
      Number_Of_Moves            => Number_Of_Moves,
      Solution_Neighbourhood     => Solution_Neighbourhood,
      Tabu_Tenure                => Tabu_Tenure,
      Allocate_Tabu_Moves        => Allocate_Tabu_Moves,
      Deallocate_Tabu_Moves      => Deallocate_Tabu_Moves,
      Set_Tabu_Move              => Set_Tabu_Move,
      Tabu_Move                  => Tabu_Move);
   use Tabu_Sine;

   -------------------------------------------------------------------------
   -- Allocate_Tabu_Moves --
   -------------------------

   procedure Allocate_Tabu_Moves(
      nb_mov : in     Positive;           -- number of moves
      tab_mov:    out Tabu_Moves)         -- the tabu moves
   is
   begin
      null;
   end Allocate_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Better_Solution --
   ---------------------

   function Better_Solution(
      sol1   : in     Solution;           -- solution 1
      sol2   : in     Solution)           -- solution 2
               return Boolean             -- sol. 1 better than sol. 2
   is
   begin
      return (sol1.s_x - sol2.s_x) > 1.0E-6;
   end Better_Solution;

   -------------------------------------------------------------------------
   -- Copy_Solution --
   -------------------

   procedure Copy_Solution(
      sol_ori: in     Solution;           -- solution original
      sol_cop:    out Solution)           -- solution copy
   is
   begin
      sol_cop.x   := sol_ori.x;
      sol_cop.s_x := sol_ori.s_x;
   end Copy_Solution;

   -------------------------------------------------------------------------
   -- Deallocate_Solution --
   -------------------------

   procedure Deallocate_Solution(
      sol    : in out Solution)           -- the solution
   is
   begin
      null;
   end Deallocate_Solution;

   -------------------------------------------------------------------------
   -- Deallocate_Tabu_Moves --
   ---------------------------

   procedure Deallocate_Tabu_Moves(
      tab_mov: in out Tabu_Moves)         -- the tabu moves
   is
   begin
      null;
   end Deallocate_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Improvement_Action --
   ------------------------

   procedure Improvement_Action(
      pro    : in     Problem;            -- the problem
      sol    : in     Solution)           -- the solution
   is
   begin
      Put("Angle = ");
      Put(sol.x, Fore=>3, Aft=>1, Exp=>0);
      Put(";    Sine = ");
      Put(sol.s_x, Fore=>2, Aft=>4, Exp=>0);
      New_Line;
   end Improvement_Action;

   -------------------------------------------------------------------------
   -- Maximum_Of_Nonimprovements --
   --------------------------------

   function Maximum_Of_Nonimprovements(
      pro    : in     Problem)            -- the problem
               return Natural             -- max. iters. without improvement
   is
   begin
      return 10;
   end Maximum_Of_Nonimprovements;

   -------------------------------------------------------------------------
   -- Number_Of_Moves --
   ---------------------

   function Number_Of_Moves(
      pro    : in     Problem)            -- the problem
               return Positive            -- number of moves
   is
   begin
      return 2;
   end Number_Of_Moves;

   -------------------------------------------------------------------------
   -- Repetition_Action --
   -----------------------

   procedure Repetition_Action(
      pro    : in     Problem;            -- the problem
      sol    : in     Solution)           -- the solution
   is
   begin
      Put("Repetition best angle = ");
      Put(sol.x, Fore=>3, Aft=>1, Exp=>0);
      New_Line(2);
   end Repetition_Action;

   -------------------------------------------------------------------------
   -- Set_Tabu_Move --
   -------------------

   procedure Set_Tabu_Move(
      val    : in     Natural;            -- value
      nm     : in     Positive;           -- number of move
      tab_mov: in out Tabu_Moves)         -- the tabu moves
   is
   begin
      tab_mov(nm) := val;
   end Set_Tabu_Move;

   -------------------------------------------------------------------------
   -- Solution_Initial --
   ----------------------

   procedure Solution_Initial(
      pro    : in     Problem;            -- the problem
      sol_ini:    out Solution)           -- solution initial
   is
      gen    : Generator;          -- random numbers generator
   begin
      Reset(gen);
      sol_ini.x   := pro.x_min + (pro.x_max - pro.x_min) * Random(gen);
      sol_ini.s_x := Sin(sol_ini.x, 360.0);
   end Solution_Initial;

   -------------------------------------------------------------------------
   -- Solution_Neighbourhood --
   ----------------------------

   procedure Solution_Neighbourhood(
      pro    : in     Problem;            -- the problem
      sol_ori: in     Solution;           -- solution original
      nm     : in     Positive;           -- number of move
      gen    : in out Generator;          -- random numbers generator
      sol_ngh:    out Solution)           -- solution neighbourhood
   is
   begin
      if nm = 1 then
         sol_ngh.x := Float'Max(sol_ori.x - pro.x_inc, pro.x_min);
      else
         sol_ngh.x := Float'Min(sol_ori.x + pro.x_inc, pro.x_max);
      end if;
      sol_ngh.s_x := Sin(sol_ngh.x, 360.0);
   end Solution_Neighbourhood;

   -------------------------------------------------------------------------
   -- Tabu_Move --
   ---------------

   function Tabu_Move(
      nm     : in     Positive;           -- number of move
      tab_mov: in     Tabu_Moves)         -- the tabu moves
               return Natural             -- value
   is
   begin
      return tab_mov(nm);
   end Tabu_Move;

   -------------------------------------------------------------------------
   -- Tabu_Tenure --
   -----------------

   function Tabu_Tenure(
      pro    : in     Problem)            -- the problem
               return Natural             -- tabu tenure
   is
   begin
      return 1;
   end Tabu_Tenure;

----------------------------------------------------------------------------
-- Tabu_Search_Test --
----------------------

   pro    : Problem;            -- the problem
   sol_ini: Solution;           -- solution initial
   sol_bst: Solution;           -- solution best
begin
   pro.x_max := 360.0;
   pro.x_min :=   0.0;
   pro.x_inc :=   1.0;
   Solution_Initial(pro, sol_ini);
   Optimise(pro, sol_ini, sol_bst);
   New_Line(1);
   Put("Best angle = ");
   Put(sol_bst.x, Fore=>3, Aft=>1, Exp=>0);
   New_Line(2);
end Tabu_Search_Test;
