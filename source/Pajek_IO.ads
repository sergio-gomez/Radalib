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


-- @filename Pajek_IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 15/03/2006
-- @revision 23/09/2015
-- @brief Input and output in Pajek format

with Utils; use Utils;
with Ada.Text_IO; use Ada.Text_IO;

with Graphs_Simple; use Graphs_Simple;
with Graphs_String; use Graphs_String;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Float; use Graphs_Float;
with Graphs_Double; use Graphs_Double;
with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;

package Pajek_IO is

  Unrecognized_Pajek_Format: exception;


  -- Purpose : Obtain the List index for each Element of a List of Lists
  -- Note    : Index 0 corresponds to Unassigned_List
  --
  -- Lol     : The Partition List of Lists
  -- return  : The index
  -- raises  : Unrecognized_Pajek_Format
  function Partition_Index(Lol: in List_Of_Lists) return PIntegers;

  -- Purpose : Get a Simple Graph from a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- raises  : Unrecognized_Pajek_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Graph(Fn: in String; Gr: out Graphs_Simple.Graph);

  -- Purpose : Get a String Graph from a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- raises  : Unrecognized_Pajek_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Graph(Fn: in String; Gr: out Graphs_String.Graph);

  -- Purpose : Get an Integers Graph from a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- raises  : Unrecognized_Pajek_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Graph(Fn: in String; Gr: out Graphs_Integer.Graph);

  -- Purpose : Get a Floats Graph from a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- raises  : Unrecognized_Pajek_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Graph(Fn: in String; Gr: out Graphs_Float.Graph);

  -- Purpose : Get a Doubles Graph from a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- raises  : Unrecognized_Pajek_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Graph(Fn: in String; Gr: out Graphs_Double.Graph);

  -- Purpose : Put a Simple Graph to a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  procedure Put_Graph(Fn: in String; Gr: in Graphs_Simple.Graph);

  -- Purpose : Put a String Graph to a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  procedure Put_Graph(Fn: in String; Gr: in Graphs_String.Graph);

  -- Purpose : Put an Integers Graph to a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  procedure Put_Graph(Fn: in String; Gr: in Graphs_Integer.Graph);

  -- Purpose : Put a Floats Graph to a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The width for the exponent
  procedure Put_Graph(Fn: in String; Gr: in Graphs_Float.Graph; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp);

  -- Purpose : Put a Doubles Graph to a File
  --
  -- Fn      : The File Name
  -- Gr      : The Graph
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The width for the exponent
  procedure Put_Graph(Fn: in String; Gr: in Graphs_Double.Graph; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp);

  -- Purpose : Get a Partition from a File
  --
  -- Fn      : The File Name
  -- Lol     : The Partition List of Lists
  -- raises  : Unrecognized_Pajek_Format
  procedure Get_Partition(Fn: in String; Lol: out List_Of_Lists);

  -- Purpose : Put a Partition to a File
  -- Note    : Index 0 corresponds to Unassigned_List
  --
  -- Fn      : The File Name
  -- Lol     : The Partition List of Lists
  procedure Put_Partition(Fn: in String; Lol: in List_Of_Lists);

end Pajek_IO;
