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


-- @filename Modularities_Tabu.adb
-- @author Alberto Fernandez
-- @version 1.0
-- @date 28/02/2007
-- @revision 26/10/2014
-- @brief Tabu Modularity Optimization

with Ada.Unchecked_Deallocation;

with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;

package body Modularities_Tabu is

   -------------------------------------------------------------------------
   -- Better_Solution --
   ---------------------

   function Better_Solution(
      sol1   : in     Solution;           -- solution 1
      sol2   : in     Solution)           -- solution 2
               return Boolean             -- sol. 1 better than sol. 2
   is
      Improvement_Tolerance: constant := 1.0e-10;
   begin
      if Maximize then
         return sol1.q.total > sol2.q.total + Improvement_Tolerance;
      else --Minimize
         return sol1.q.total < sol2.q.total - Improvement_Tolerance;
      end if;
   end Better_Solution;

   -------------------------------------------------------------------------
   -- Copy_Solution --
   -------------------

   procedure Copy_Solution(
      sol_ori: in     Solution;           -- solution original
      sol_cop:    out Solution)           -- solution copy
   is
   begin
      sol_cop.lol := Clone(sol_ori.lol);
      sol_cop.mi  := Clone(sol_ori.mi);
      sol_cop.q   := sol_ori.q;
   end Copy_Solution;

   -------------------------------------------------------------------------
   -- Deallocate_Solution --
   -------------------------

   procedure Deallocate_Solution(
      sol    : in out Solution)           -- the solution
   is
   begin
      Free(sol.lol);
      Free(sol.mi);
   end Deallocate_Solution;

   -------------------------------------------------------------------------
   -- Deallocate_Tabu_Moves --
   ---------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
      Object => Array_Of_Natural,
      Name   => Tabu_Moves);

   procedure Deallocate_Tabu_Moves(
      tab_mov: in out Tabu_Moves)         -- the tabu moves
   is
   begin
      Free(tab_mov);
   end Deallocate_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Decrease_Tabu_Moves --
   -------------------------

   procedure Decrease_Tabu_Moves(
      nb_vrt : in     Positive;           -- number of vertices
      tab_mov: in out Tabu_Moves)         -- the tabu moves
   is
   begin
      for nv in 1 .. nb_vrt loop
         if tab_mov(nv) > 0 then
            tab_mov(nv) := tab_mov(nv) - 1;
         end if;
      end loop;
   end Decrease_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Examine_Neighbourhood --
   ---------------------------

   procedure Examine_Neighbourhood(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      sol_bst: in     Solution;           -- solution best
      sol_ori: in     Solution;           -- solution original
      gen    : in out Generator;          -- random numbers generator
      tab_mov: in out Tabu_Moves;         -- the tabu moves
      nv_bst :    out Natural;            -- number of vertex best
      sol_ngh:    out Solution)           -- solution neighbourhood best
   is
      nb_vrt : Positive;           -- number of vertices
      sol_cur: Solution;           -- solution neighbourhood current
   begin
      nv_bst := 0;
      Copy_Solution(sol_ori, sol_ngh);
      nb_vrt := Number_Of_Vertices(gr);
      for nv in 1 .. nb_vrt loop
         Solution_Neighbourhood(mt, gr, sol_ori, nv, gen, sol_cur);
         if Better_Solution(sol_cur, sol_bst) then
            tab_mov(nv) := 0;
         end if;
         if tab_mov(nv) = 0 then
            if nv_bst = 0 or else Better_Solution(sol_cur, sol_ngh) then
               nv_bst := nv;
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
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      fn_tmp : in     Unbounded_String;   -- file name temporal
      sol_ini: in     Solution;           -- solution initial
      gen    : in out Generator;          -- random numbers generator
      sol_bst:    out Solution)           -- solution best
   is
      sol_ori: Solution;           -- solution original
      nb_vrt : Positive;           -- number of vertices
      tab_mov: Tabu_Moves;         -- the tabu moves
      tab_ten: Natural;            -- tabu tenure
      max_ite: Natural;            -- max. iters. without improvement
      nb_ite : Natural;            -- nb. iters. without improvement
      nv_bst : Natural;            -- number of vertex best
      sol_ngh: Solution;           -- solution neighbourhood
   begin
      Copy_Solution(sol_ini, sol_bst);
      Copy_Solution(sol_bst, sol_ori);
      nb_vrt := Number_Of_Vertices(gr);
      Initialise_Tabu_Moves(nb_vrt, tab_mov);
      tab_ten := Natural'Min(5, nb_vrt-1);
      max_ite := Maximum_Of_Nonimprovements(nb_vrt);
      nb_ite := 0;
      while nb_ite <= max_ite loop
         Examine_Neighbourhood(mt, gr, sol_bst, sol_ori, gen, tab_mov, nv_bst, sol_ngh);
         Decrease_Tabu_Moves(nb_vrt, tab_mov);
         nb_ite := nb_ite + 1;
         if nv_bst > 0 then
            tab_mov(nv_bst) := tab_ten;
            Deallocate_Solution(sol_ori);
            Copy_Solution(sol_ngh, sol_ori);
            if Better_Solution(sol_ngh, sol_bst) then
               Deallocate_Solution(sol_bst);
               Copy_Solution(sol_ngh, sol_bst);
               Improvement_Action(fn_tmp, sol_bst.lol, sol_bst.q);
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
      nb_vrt : in     Positive;           -- number of vertices
      tab_mov:    out Tabu_Moves)         -- the tabu moves
   is
   begin
      tab_mov := new Array_Of_Natural(1..nb_vrt);
      for nv in 1 .. nb_vrt loop
         tab_mov(nv) := 0;
      end loop;
   end Initialise_Tabu_Moves;

   -------------------------------------------------------------------------
   -- Solution_Neighbourhood --
   ----------------------------

   procedure List_Destination(
      lol    : in     List_Of_Lists;      -- the list of lists
      l_frm  : in     List;               -- list of element from
      edg    : in     Edges_List;         -- edges from
      ne     : in     Positive;           -- number of edge
      l_to   :    out List)               -- list of element to
   is
      vrt_to : Vertex;             -- vertex to
      elm_to : Finite_Disjoint_Lists.Element;-- element to
   begin
      Save(edg);
      Reset(edg);
      for i in 2 .. ne loop
         Next(edg);
      end loop;
      vrt_to := Next(edg);
      elm_to := Get_Element(lol, Index_Of(vrt_to));
      while Belongs_To(elm_to, l_frm) and Has_Next(edg) loop
         vrt_to := Next(edg);
         elm_to := Get_Element(lol, Index_Of(vrt_to));
      end loop;
      if Belongs_To(elm_to, l_frm) then
         l_to := New_List(lol);
      else
         l_to := List_Of(elm_to);
      end if;
      Restore(edg);
   end List_Destination;

   procedure Solution_Neighbourhood(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      sol_ori: in     Solution;           -- solution original
      nv     : in     Positive;           -- number of vertex
      gen    : in out Generator;          -- random numbers generator
      sol_ngh:    out Solution)           -- solution neighbourhood
   is
      vrt_frm: Vertex;             -- vertex from
      edg    : Edges_List;         -- edges from
      lol    : List_Of_Lists;      -- list of lists auxiliary
      elm_frm: Finite_Disjoint_Lists.Element;-- element from
      l_frm  : List;               -- list of element from
      ne     : Natural;            -- number of edge
      l_to   : List;               -- list of element to
   begin
      vrt_frm := Get_Vertex(gr, nv);
      edg := Edges_From(vrt_frm);
      if Number_Of_Edges(edg) = 0 then
         Copy_Solution(sol_ori, sol_ngh);
      else
         lol := Clone(sol_ori.lol);
         elm_frm := Get_Element(lol, nv);
         l_frm := List_Of(elm_frm);
         if Number_Of_Elements(l_frm) = 1 then
            ne := 1 + Natural(Random(gen) * Float(Number_Of_Edges(edg)-1));
         else
            ne := 0 + Natural(Random(gen) * Float(Number_Of_Edges(edg)-0));
         end if;
         if ne = 0 then
            l_to := New_List(lol);
         else
            List_Destination(lol, l_frm, edg, ne, l_to);
         end if;
         Move(elm_frm, l_to);
         if Number_Of_Elements(l_frm) = 0 then
            Remove(l_frm);
         end if;
         Connected_Components(gr, lol, sol_ngh.lol);
         sol_ngh.mi := Clone(sol_ori.mi);
         if Number_Of_Lists(lol) = Number_Of_Lists(sol_ngh.lol) then
            Update_Modularity(sol_ngh.mi, l_to, mt);
            if Belongs_To(l_frm, lol) then
               Update_Modularity(sol_ngh.mi, l_frm, mt);
            end if;
            sol_ngh.q := Total_Modularity(sol_ngh.mi);
         else
            sol_ngh.q := Modularity(sol_ngh.mi, sol_ngh.lol, mt);
         end if;
         Free(lol);
      end if;
   end Solution_Neighbourhood;

   -------------------------------------------------------------------------
   -- Tabu_Modularity --
   ---------------------

   procedure Tabu_Modularity(
      mt     : in     Modularity_Type;    -- the modularity type
      gr     : in     Graph;              -- the graph
      fn_tmp : in     Unbounded_String;   -- file name temporal
      lol_ini: in     List_Of_Lists;      -- list of lists initial
      lol_bst:    out List_Of_Lists;      -- list of lists best
      q_bst  :    out Modularity_Rec;     -- modularity best
      R      : in     Double := No_Resistance;
      Pc     : in     Double := 1.0)
   is
      sol_ini: Solution;           -- solution initial
      sol_bst: Solution;           -- solution best
      sol_rep: Solution;           -- solution repetition
      gen    : Generator;          -- random numbers generator
   begin
      Initialize(sol_ini.mi, gr, mt, R, Pc);
      sol_ini.lol := Clone(lol_ini);
      sol_ini.q := Modularity(sol_ini.mi, sol_ini.lol, mt);
      Copy_Solution(sol_ini, sol_bst);
      Reset(gen);
      for nr in 1 .. Number_Of_Repetitions loop
         Execute_Repetition(mt, gr, fn_tmp, sol_ini, gen, sol_rep);
         if nr = 1 or else Better_Solution(sol_rep, sol_bst) then
            Deallocate_Solution(sol_bst);
            Copy_Solution(sol_rep, sol_bst);
         end if;
         Deallocate_Solution(sol_rep);
         Repetition_Action(fn_tmp, sol_bst.lol, sol_bst.q);
      end loop;
      Free(sol_ini.mi);
      Free(sol_ini.lol);
      Free(sol_bst.mi);
      lol_bst := sol_bst.lol;
      q_bst := sol_bst.q;
   end Tabu_Modularity;

   -------------------------------------------------------------------------

end Modularities_Tabu;
