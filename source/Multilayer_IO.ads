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


-- @filename Multilayer_IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/10/2014
-- @revision 29/12/2014
-- @brief Input and output of Multilayer Graphs

with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;
with Graphs_Simple_Multilayer; use Graphs_Simple_Multilayer;
with Graphs_String_Multilayer; use Graphs_String_Multilayer;
with Graphs_Integer_Multilayer; use Graphs_Integer_Multilayer;
with Graphs_Float_Multilayer; use Graphs_Float_Multilayer;
with Graphs_Double_Multilayer; use Graphs_Double_Multilayer;

package Multilayer_IO is

  Unrecognized_Multilayer_Format: exception;


  -- Purpose : Get a Simple Multiplex from a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- raises  : Unrecognized_Multilayer_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Simple_Multilayer.Multiplex; Directed: in Boolean);

  -- Purpose : Get a String Multiplex from a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- raises  : Unrecognized_Multilayer_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_String_Multilayer.Multiplex; Directed: in Boolean);

  -- Purpose : Get an Integers Multiplex from a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- raises  : Unrecognized_Multilayer_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Integer_Multilayer.Multiplex; Directed: in Boolean);

  -- Purpose : Get a Floats Multiplex from a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- raises  : Unrecognized_Multilayer_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Float_Multilayer.Multiplex; Directed: in Boolean);

  -- Purpose : Get a Doubles Multiplex from a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- raises  : Unrecognized_Multilayer_Format
  -- raises  : Incompatible_Values_Error
  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Double_Multilayer.Multiplex; Directed: in Boolean);

  -- Purpose : Put a Simple Multiplex to a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Simple_Multilayer.Multiplex);

  -- Purpose : Put a String Multiplex to a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_String_Multilayer.Multiplex);

  -- Purpose : Put an Integers Multiplex to a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Integer_Multilayer.Multiplex);

  -- Purpose : Put a Floats Multiplex to a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The width for the exponent
  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Float_Multilayer.Multiplex; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp);

  -- Purpose : Put a Doubles Multiplex to a File
  --
  -- Fn      : The File Name
  -- Mpx     : The Multiplex
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The width for the exponent
  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Double_Multilayer.Multiplex; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp);

end Multilayer_IO;
