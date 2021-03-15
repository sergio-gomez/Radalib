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


-- @filename Trees_Float_IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 30/01/2016
-- @brief Input and Output of Trees of Floats

with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Trees_Float; use Trees_Float;
with Trees.IO;

package Trees_Float_IO is

  function To_S(F: in Float; Aft: in Field := Default_Float_Aft; Format: in Tree_Format) return String;
  procedure Get_F(F: out Float; Format: in Tree_Format);

  package Trees_IO_F is new Trees_Float.IO(To_S, Get_F);

  procedure Put_Tree(T: in Tree; Aft: in Field := Default_Float_Aft; Format: in Tree_Format := Default_Tree_Format) renames Trees_IO_F.Put_Tree;
  procedure Put_Tree(Ft: in File_Type; T: in Tree; Aft: in Field := Default_Float_Aft; Format: in Tree_Format := Default_Tree_Format) renames Trees_IO_F.Put_Tree;
  procedure Put_Tree(Fn: in String; T: in Tree; Aft: in Field := Default_Float_Aft; Mode: in File_Mode := Out_File; Format: in Tree_Format := Default_Tree_Format) renames Trees_IO_F.Put_Tree;

  procedure Get_Tree(T: out Tree) renames Trees_IO_F.Get_Tree;
  procedure Get_Tree(Ft: in File_Type; T: out Tree) renames Trees_IO_F.Get_Tree;
  procedure Get_Tree(Fn: in String; T: out Tree) renames Trees_IO_F.Get_Tree;

end Trees_Float_IO;
