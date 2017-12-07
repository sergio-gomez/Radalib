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


-- @filename Modularities_Tabu.ads
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Tabu Modularity Optimization

with Ada.Numerics.Float_Random;        use Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with Utils;                            use Utils;
with Finite_Disjoint_Lists;            use Finite_Disjoint_Lists;
with Graphs_Double;                    use Graphs_Double;
with Graphs_Double_Modularities_D;     use Graphs_Double_Modularities_D;

generic

   Number_Of_Repetitions: Positive;

   with function Maximum_Of_Nonimprovements(
      nb_vrt : in     Positive)           -- number of vertices
               return Natural;            -- max. iters. without improvement

   with procedure Improvement_Action(
      fn_tmp : in     Unbounded_String;   -- file name temporal
      lol    : in     List_Of_Lists;      -- the list of lists
      q      : in     Modularity_Rec);    -- the modularity

   with procedure Repetition_Action(
      fn_tmp : in     Unbounded_String;   -- file name temporal
      lol    : in     List_Of_Lists;      -- the list of lists
      q      : in     Modularity_Rec);    -- the modularity

package Modularities_Tabu is

   Maximize: constant Boolean := True;

   -------------------------------------------------------------------------

   procedure Tabu_Modularity(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      fn_tmp : in     Unbounded_String;   -- file name temporal
      lol_ini: in     List_Of_Lists;      -- list of lists initial
      lol_bst:    out List_Of_Lists;      -- list of lists best
      q_bst  :    out Modularity_Rec;     -- modularity best
      R      : in     Double := No_Resistance;
      Pc     : in     Double := 1.0);

private

   -------------------------------------------------------------------------

   type Solution is record
      lol    : List_Of_Lists;      -- the list of lists
      mi     : Modularity_Info;    -- the modularity info
      q      : Modularity_Rec;     -- the modularity
   end record;

   type Array_Of_Natural is array (Positive range <>) of Natural;
   type Tabu_Moves is access Array_Of_Natural;

   -------------------------------------------------------------------------

   procedure Execute_Repetition(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      fn_tmp : in     Unbounded_String;   -- file name temporal
      sol_ini: in     Solution;           -- solution initial
      gen    : in out Generator;          -- random numbers generator
      sol_bst:    out Solution);          -- solution best

   procedure Copy_Solution(
      sol_ori: in     Solution;           -- solution original
      sol_cop:    out Solution);          -- solution copy

   procedure Deallocate_Solution(
      sol    : in out Solution);          -- the solution

   function Better_Solution(
      sol1   : in     Solution;           -- solution 1
      sol2   : in     Solution)           -- solution 2
               return Boolean;            -- sol. 1 better than sol. 2

   -------------------------------------------------------------------------

   procedure Initialise_Tabu_Moves(
      nb_vrt : in     Positive;           -- number of vertices
      tab_mov:    out Tabu_Moves);        -- the tabu moves

   procedure Examine_Neighbourhood(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      sol_bst: in     Solution;           -- solution best
      sol_ori: in     Solution;           -- solution original
      gen    : in out Generator;          -- random numbers generator
      tab_mov: in out Tabu_Moves;         -- the tabu moves
      nv_bst :    out Natural;            -- number of vertex best
      sol_ngh:    out Solution);          -- solution neighbourhood best

   procedure Decrease_Tabu_Moves(
      nb_vrt : in     Positive;           -- number of vertices
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   procedure Deallocate_Tabu_Moves(
      tab_mov: in out Tabu_Moves);        -- the tabu moves

   -------------------------------------------------------------------------

   procedure Solution_Neighbourhood(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      sol_ori: in     Solution;           -- solution original
      nv     : in     Positive;           -- number of vertex
      gen    : in out Generator;          -- random numbers generator
      sol_ngh:    out Solution);          -- solution neighbourhood

   -------------------------------------------------------------------------

end Modularities_Tabu;
