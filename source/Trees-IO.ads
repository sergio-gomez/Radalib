-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


-- @filename Trees-IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 05/04/2012
-- @revision 01/02/2016
-- @brief Input and Output of Trees

with Ada.Text_IO; use Ada.Text_IO;

generic
  with function To_S(Item: in Node_Value; Param: in Integer; Format: in Tree_Format) return String is <>;
  with procedure Get(Item: out Node_Value; Format: in Tree_Format) is <>;
package Trees.IO is

  Tree_IO_Error: exception;

  -- Purpose : Put a Tree to Current Output
  -- Note    : Names of Nodes must not contain these characters (no check is made):
  -- Note    :   ' '   ','   ';'   ':'   '|'   '['    ']'   '('   ')'   '{'   '}'
  --
  -- T       : The Tree
  -- Param   : Optional Parameter
  -- Format  : Tree Format
  -- raises  : Empty_Tree_Error
  -- raises  : Tree_IO_Error
  procedure Put_Tree(T: in Tree; Param: in Integer; Format: in Tree_Format := Default_Tree_Format);

  -- Purpose : Put a Tree to a File
  -- Note    : Names of Nodes must not contain these characters (no check is made):
  -- Note    :   ' '   ','   ';'   ':'   '|'   '['    ']'   '('   ')'   '{'   '}'
  --
  -- Ft      : The File Type
  -- T       : The Tree
  -- Param   : Optional Parameter
  -- Format  : Tree Format
  -- raises  : Empty_Tree_Error
  -- raises  : Tree_IO_Error
  procedure Put_Tree(Ft: in File_Type; T: in Tree; Param: in Integer; Format: in Tree_Format := Default_Tree_Format);

  -- Purpose : Put a Tree to a File
  -- Note    : Names of Nodes must not contain these characters (no check is made):
  -- Note    :   ' '   ','   ';'   ':'   '|'   '['    ']'   '('   ')'   '{'   '}'
  --
  -- Fn      : The File Name
  -- T       : The Tree
  -- Param   : Optional Parameter
  -- Mode    : The File Mode
  -- Format  : Tree Format
  -- raises  : Empty_Tree_Error
  -- raises  : Tree_IO_Error
  procedure Put_Tree(Fn: in String; T: in Tree; Param: in Integer; Mode: in File_Mode := Out_File; Format: in Tree_Format := Default_Tree_Format);

  -- Purpose : Get a Tree from Current Input
  --
  -- T       : The Tree
  -- raises  : Tree_IO_Error
  procedure Get_Tree(T: out Tree);

  -- Purpose : Get a Tree from a File
  --
  -- Ft      : The File Type
  -- T       : The Tree
  -- raises  : Tree_IO_Error
  procedure Get_Tree(Ft: in File_Type; T: out Tree);

  -- Purpose : Get a Tree from a File
  --
  -- Fn      : The File Name
  -- T       : The Tree
  -- raises  : Tree_IO_Error
  procedure Get_Tree(Fn: in String; T: out Tree);

end Trees.IO;
