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


-- @filename Finite_Disjoint_Lists-IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/05/2005
-- @revision 23/09/2015
-- @brief Input and Output of Lists of Lists with Finite Disjoint Elements

with Ada.Text_IO; use Ada.Text_IO;

package Finite_Disjoint_Lists.IO is

  List_Of_Lists_IO_Error: exception;


  -- Purpose : Get a List of Lists from Current Input
  --
  -- Lol     : The List of Lists
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Lol: out List_Of_Lists);

  -- Purpose : Get a List of Lists from a File
  --
  -- Lol     : The List of Lists
  -- Fn      : The File Name
  -- Num_Skip_Lines: Number of lines to skip before the List of Lists
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Lol: out List_Of_Lists; Fn: in String; Num_Skip_Lines: in Natural := 0);

  -- Purpose : Get a List of Lists from a File
  --
  -- Fn      : The File Name
  -- Lol     : The List of Lists
  -- Num_Skip_Lines: Number of lines to skip before the List of Lists
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Fn: in String; Lol: out List_Of_Lists; Num_Skip_Lines: in Natural := 0);

  -- Purpose : Get a List of Lists from a File
  --
  -- Ft      : The File Type
  -- Lol     : The List of Lists
  -- Num_Skip_Lines: Number of lines to skip before the List of Lists
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Ft: in File_Type; Lol: out List_Of_Lists; Num_Skip_Lines: in Natural := 0);

  -- Purpose : Put a List of Lists to Current Output
  --
  -- Lol     : The List of Lists
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(Lol: in List_Of_Lists);

  -- Purpose : Put a List of Lists to a File
  --
  -- Lol     : The List of Lists
  -- Fn      : The File Name
  -- Mode    : The File Mode
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(Lol: in List_Of_Lists; Fn: in String; Mode: in File_Mode := Out_File);

  -- Purpose : Put a List of Lists to a File
  --
  -- Fn      : The File Name
  -- Lol     : The List of Lists
  -- Mode    : The File Mode
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(Fn: in String; Lol: in List_Of_Lists; Mode: in File_Mode := Out_File);

  -- Purpose : Put a List of Lists to a File
  --
  -- Ft      : The File Type
  -- Lol     : The List of Lists
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(Ft: in File_Type; Lol: in List_Of_Lists);

  -- Purpose : Get a List of a List of Lists from Current Input
  --
  -- Lol     : The List of Lists
  -- L       : The List
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Lol: in List_Of_Lists; L: out List);

  -- Purpose : Get a List of a List of Lists from a File
  --
  -- Lol     : The List of Lists
  -- L       : The List
  -- Fn      : The File Name
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Lol: in List_Of_Lists; L: out List; Fn: in String);

  -- Purpose : Get a List of a List of Lists from a File
  --
  -- Fn      : The File Name
  -- Lol     : The List of Lists
  -- L       : The List
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Fn: in String; Lol: in List_Of_Lists; L: out List);

  -- Purpose : Get a List of a List of Lists from a File
  --
  -- Ft      : The File Type
  -- Lol     : The List of Lists
  -- L       : The List
  -- raises  : List_Of_Lists_IO_Error
  procedure Get(Ft: in File_Type; Lol: in List_Of_Lists; L: out List);

  -- Purpose : Put a List to Current Output
  --
  -- L       : The List
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(L: in List);

  -- Purpose : Put a List to a File
  --
  -- L       : The List
  -- Fn      : The File Name
  -- Mode    : The File Mode
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(L: in List; Fn: in String; Mode: in File_Mode := Out_File);

  -- Purpose : Put a List to a File
  --
  -- Fn      : The File Name
  -- L       : The List
  -- Mode    : The File Mode
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(Fn: in String; L: in List; Mode: in File_Mode := Out_File);

  -- Purpose : Put a List to a File
  --
  -- Ft      : The File Type
  -- L       : The List
  -- raises  : List_Of_Lists_IO_Error
  procedure Put(Ft: in File_Type; L: in List);

end Finite_Disjoint_Lists.IO;
