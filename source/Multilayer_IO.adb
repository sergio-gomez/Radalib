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


-- @filename Multilayer_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/10/2014
-- @revision 29/12/2014
-- @brief Input and output of Multilayer Graphs

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with Utils.IO; use Utils.IO;
with Graphs_Simple; use Graphs_Simple;
with Graphs_String; use Graphs_String;
with Graphs_Integer; use Graphs_Integer;
with Graphs_Float; use Graphs_Float;
with Graphs_Double; use Graphs_Double;
with Linked_Lists;

package body Multilayer_IO is

  -- Hash Map to store names
  package Unbounded_Strings_Maps is new Ada.Containers.Hashed_Maps(
    Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
    Element_Type    => Integer,
    Hash            => Ada.Strings.Unbounded.Hash,
    Equivalent_Keys => Ada.Strings.Unbounded."=");
  use Unbounded_Strings_Maps;

  --------------
  -- Get_Info --
  --------------

  procedure Get_Info(Ft: in out File_Type; Num_Layers, Num_Vertices: out Natural; Layers_Map, Vertices_Map: out Map; Layers_Names, Vertices_Names: out PUstrings) is

    -- Linked List to sort names
    package Us_Linked_Lists is new Linked_Lists(Ustring); use Us_Linked_Lists;

    Numeric: Boolean;

    function Lower(Left, Right: in Ustring) return Boolean is
    begin
      if Numeric then
        return S2I(U2S(Left)) < S2I(U2S(Right));
      else
        return Left < Right;
      end if;
    end Lower;

    Vertices_List : Linked_List;
    Layers_List: Linked_List;
    Numeric_Vertices : Boolean := False;
    Numeric_Layers: Boolean := False;
    Us: Ustring;

  begin

    -- Read and save layer and vertex names, and determine if they are numeric
    Initialize(Vertices_List);
    Initialize(Layers_List);

    Num_Layers := 0;
    Num_Vertices := 0;
    Numeric_Layers := True;
    Numeric_Vertices := True;
    while not End_Of_File(Ft) loop
      Comments_Skip(Ft);
      Line_Spaces_Skip(Ft);
      Line_Comment_Skip(Ft);
      if not End_Of_Line(Ft) then
        -- Layer
        Get_Word(Ft, Us);
        if not Contains(Layers_Map, Us) then
          Num_Layers := Num_Layers + 1;
          Insert(Layers_Map, Us, Num_Layers);
          Add_Last(Us, Layers_List);
          if Numeric_Layers then
            Numeric_Layers := Is_Integer(Us);
          end if;
        end if;
        -- Separator
        Separator_Skip(Ft);
        -- Vertex From
        Get_Word(Ft, Us);
        if not Contains(Vertices_Map, Us) then
          Num_Vertices := Num_Vertices + 1;
          Insert(Vertices_Map, Us, Num_Vertices);
          Add_Last(Us, Vertices_List);
          if Numeric_Vertices then
            Numeric_Vertices := Is_Integer(Us);
          end if;
        end if;
        -- Separator
        Separator_Skip(Ft);
        -- Vertex To
        Get_Word(Ft, Us);
        if not Contains(Vertices_Map, Us) then
          Num_Vertices := Num_Vertices + 1;
          Insert(Vertices_Map, Us, Num_Vertices);
          Add_Last(Us, Vertices_List);
          if Numeric_Vertices then
            Numeric_Vertices := Is_Integer(Us);
          end if;
        end if;
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
      end if;
    end loop;

    -- Set layers names
    Numeric := Numeric_Layers;
    Sort(Layers_List, Lower'Access);
    Clear(Layers_Map);
    Layers_Names := Alloc(1, Num_Layers);

    Save(Layers_List);
    Reset(Layers_List);
    for L in 1..Num_Layers loop
      Us := Next(Layers_List);
      Insert(Layers_Map, Us, L);
      Layers_Names(L) := Us;
    end loop;
    Restore(Layers_List);
    Free(Layers_List);

    -- Set vertices names
    Numeric := Numeric_Vertices;
    Sort(Vertices_List, Lower'Access);
    Clear(Vertices_Map);
    Vertices_Names := Alloc(1, Num_Vertices);

    Save(Vertices_List);
    Reset(Vertices_List);
    for I in 1..Num_Vertices loop
      Us := Next(Vertices_List);
      Insert(Vertices_Map, Us, I);
      Vertices_Names(I) := Us;
    end loop;
    Restore(Vertices_List);
    Free(Vertices_List);

    Reset(Ft);

  end Get_Info;

  -------------------
  -- Get_Multiplex --
  -------------------

  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Simple_Multilayer.Multiplex; Directed: in Boolean) is
    Num_Layers, Num_Vertices: Natural;
    Layers_Map, Vertices_Map: Map;
    Layers_Names, Vertices_Names: PUstrings;
    Grs: Graphs_Simple_Multilayer.PGraphs;
    Ft: File_Type;
    Us: Ustring;
    L, F, T: Integer;
  begin
    Open(Ft, In_File, Fn);
    Get_Info(Ft, Num_Layers, Num_Vertices, Layers_Map, Vertices_Map, Layers_Names, Vertices_Names);
    Initialize(Mpx, Num_Layers, Num_Vertices, Directed);

    Grs := Get_Layers(Mpx);
    for L in 1..Num_Layers loop
      Set_Layer_Name(Mpx, L, Layers_Names(L));
      for I in 1..Num_Vertices loop
        Set_Name(Get_Vertex(Grs(L), I), U2S(Vertices_Names(I)));
      end loop;
    end loop;

    -- Read network
    begin
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- Layer
          Get_Word(Ft, Us);
          L := Element(Layers_Map, Us);
          -- Vertex From
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          F := Element(Vertices_Map, Us);
          -- Vertex To
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          T := Element(Vertices_Map, Us);
          -- Add Edge
          Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T));
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

      for L in 1..Num_Layers loop
        Restore_Consistency(Grs(L));
      end loop;
    exception
      when E: others =>
        Put(Exception_Information(E));
        Put_Line(NLine & "Current position at " & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
        raise Unrecognized_Multilayer_Format;
    end;
    Close(Ft);

    Free(Layers_Names);
    Free(Vertices_Names);
  end Get_Multiplex;

  -------------------
  -- Get_Multiplex --
  -------------------

  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_String_Multilayer.Multiplex; Directed: in Boolean) is
    Num_Layers, Num_Vertices: Natural;
    Layers_Map, Vertices_Map: Map;
    Layers_Names, Vertices_Names: PUstrings;
    Grs: Graphs_String_Multilayer.PGraphs;
    Ft: File_Type;
    Us: Ustring;
    L, F, T: Integer;
    Value: Ustring;
  begin
    Open(Ft, In_File, Fn);
    Get_Info(Ft, Num_Layers, Num_Vertices, Layers_Map, Vertices_Map, Layers_Names, Vertices_Names);
    Initialize(Mpx, Num_Layers, Num_Vertices, Directed);

    Grs := Get_Layers(Mpx);
    for L in 1..Num_Layers loop
      Set_Layer_Name(Mpx, L, Layers_Names(L));
      for I in 1..Num_Vertices loop
        Set_Name(Get_Vertex(Grs(L), I), U2S(Vertices_Names(I)));
      end loop;
    end loop;

    -- Read network
    begin
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- Layer
          Get_Word(Ft, Us);
          L := Element(Layers_Map, Us);
          -- Vertex From
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          F := Element(Vertices_Map, Us);
          -- Vertex To
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          T := Element(Vertices_Map, Us);
          -- Weight and Add Edge
          Separator_Skip(Ft);
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if End_Of_Line(Ft) then
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T));
          else
            Get_Word(Ft, Value);
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T), Value);
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

      for L in 1..Num_Layers loop
        Restore_Consistency(Grs(L));
      end loop;
    exception
      when E: others =>
        Put(Exception_Information(E));
        Put_Line(NLine & "Current position at " & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
        raise Unrecognized_Multilayer_Format;
    end;
    Close(Ft);

    Free(Layers_Names);
    Free(Vertices_Names);
  end Get_Multiplex;

  -------------------
  -- Get_Multiplex --
  -------------------

  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Integer_Multilayer.Multiplex; Directed: in Boolean) is
    Num_Layers, Num_Vertices: Natural;
    Layers_Map, Vertices_Map: Map;
    Layers_Names, Vertices_Names: PUstrings;
    Grs: Graphs_Integer_Multilayer.PGraphs;
    Ft: File_Type;
    Us: Ustring;
    L, F, T: Integer;
    Value: Integer;
  begin
    Open(Ft, In_File, Fn);
    Get_Info(Ft, Num_Layers, Num_Vertices, Layers_Map, Vertices_Map, Layers_Names, Vertices_Names);
    Initialize(Mpx, Num_Layers, Num_Vertices, Directed);

    Grs := Get_Layers(Mpx);
    for L in 1..Num_Layers loop
      Set_Layer_Name(Mpx, L, Layers_Names(L));
      for I in 1..Num_Vertices loop
        Set_Name(Get_Vertex(Grs(L), I), U2S(Vertices_Names(I)));
      end loop;
    end loop;

    -- Read network
    begin
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- Layer
          Get_Word(Ft, Us);
          L := Element(Layers_Map, Us);
          -- Vertex From
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          F := Element(Vertices_Map, Us);
          -- Vertex To
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          T := Element(Vertices_Map, Us);
          -- Weight and Add Edge
          Separator_Skip(Ft);
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if End_Of_Line(Ft) then
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T));
          else
            Get_Integer(Ft, Value);
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T), Value);
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

      for L in 1..Num_Layers loop
        Restore_Consistency(Grs(L));
      end loop;
    exception
      when E: others =>
        Put(Exception_Information(E));
        Put_Line(NLine & "Current position at " & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
        raise Unrecognized_Multilayer_Format;
    end;
    Close(Ft);

    Free(Layers_Names);
    Free(Vertices_Names);
  end Get_Multiplex;

  -------------------
  -- Get_Multiplex --
  -------------------

  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Float_Multilayer.Multiplex; Directed: in Boolean) is
    Num_Layers, Num_Vertices: Natural;
    Layers_Map, Vertices_Map: Map;
    Layers_Names, Vertices_Names: PUstrings;
    Grs: Graphs_Float_Multilayer.PGraphs;
    Ft: File_Type;
    Us: Ustring;
    L, F, T: Integer;
    Value: Float;
  begin
    Open(Ft, In_File, Fn);
    Get_Info(Ft, Num_Layers, Num_Vertices, Layers_Map, Vertices_Map, Layers_Names, Vertices_Names);
    Initialize(Mpx, Num_Layers, Num_Vertices, Directed);

    Grs := Get_Layers(Mpx);
    for L in 1..Num_Layers loop
      Set_Layer_Name(Mpx, L, Layers_Names(L));
      for I in 1..Num_Vertices loop
        Set_Name(Get_Vertex(Grs(L), I), U2S(Vertices_Names(I)));
      end loop;
    end loop;

    -- Read network
    begin
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- Layer
          Get_Word(Ft, Us);
          L := Element(Layers_Map, Us);
          -- Vertex From
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          F := Element(Vertices_Map, Us);
          -- Vertex To
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          T := Element(Vertices_Map, Us);
          -- Weight and Add Edge
          Separator_Skip(Ft);
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if End_Of_Line(Ft) then
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T));
          else
            Get_Float(Ft, Value);
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T), Value);
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

      for L in 1..Num_Layers loop
        Restore_Consistency(Grs(L));
      end loop;
    exception
      when E: others =>
        Put(Exception_Information(E));
        Put_Line(NLine & "Current position at " & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
        raise Unrecognized_Multilayer_Format;
    end;
    Close(Ft);

    Free(Layers_Names);
    Free(Vertices_Names);
  end Get_Multiplex;

  -------------------
  -- Get_Multiplex --
  -------------------

  procedure Get_Multiplex(Fn: in String; Mpx: out Graphs_Double_Multilayer.Multiplex; Directed: in Boolean) is
    Num_Layers, Num_Vertices: Natural;
    Layers_Map, Vertices_Map: Map;
    Layers_Names, Vertices_Names: PUstrings;
    Grs: Graphs_Double_Multilayer.PGraphs;
    Ft: File_Type;
    Us: Ustring;
    L, F, T: Integer;
    Value: Double;
  begin
    Open(Ft, In_File, Fn);
    Get_Info(Ft, Num_Layers, Num_Vertices, Layers_Map, Vertices_Map, Layers_Names, Vertices_Names);
    Initialize(Mpx, Num_Layers, Num_Vertices, Directed);

    Grs := Get_Layers(Mpx);
    for L in 1..Num_Layers loop
      Set_Layer_Name(Mpx, L, Layers_Names(L));
      for I in 1..Num_Vertices loop
        Set_Name(Get_Vertex(Grs(L), I), U2S(Vertices_Names(I)));
      end loop;
    end loop;

    -- Read network
    begin
      while not End_Of_File(Ft) loop
        Comments_Skip(Ft);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if not End_Of_Line(Ft) then
          -- Layer
          Get_Word(Ft, Us);
          L := Element(Layers_Map, Us);
          -- Vertex From
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          F := Element(Vertices_Map, Us);
          -- Vertex To
          Separator_Skip(Ft);
          Get_Word(Ft, Us);
          T := Element(Vertices_Map, Us);
          -- Weight and Add Edge
          Separator_Skip(Ft);
          Line_Spaces_Skip(Ft);
          Line_Comment_Skip(Ft);
          if End_Of_Line(Ft) then
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T));
          else
            Get_Double(Ft, Value);
            Add_Edge_Unchecked(Get_Vertex(Grs(L), F), Get_Vertex(Grs(L), T), Value);
          end if;
        end if;
        if not End_Of_File(Ft) then
          Skip_Line(Ft);
        end if;
      end loop;

      for L in 1..Num_Layers loop
        Restore_Consistency(Grs(L));
      end loop;
    exception
      when E: others =>
        Put(Exception_Information(E));
        Put_Line(NLine & "Current position at " & Fn & ": " & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
        raise Unrecognized_Multilayer_Format;
    end;
    Close(Ft);

    Free(Layers_Names);
    Free(Vertices_Names);
  end Get_Multiplex;

  -------------------
  -- Put_Multiplex --
  -------------------

  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Simple_Multilayer.Multiplex) is
    Ft: File_Type;
    Grs: Graphs_Simple_Multilayer.PGraphs;
    El: Graphs_Simple.Edges_List;
    E: Graphs_Simple.Edge;
    Vf, Vt: Graphs_Simple.Vertex;
    Directed: Boolean;
    Layer_Name, Vf_Name, Vt_Name: Ustring;
    N: Natural;
    T: Positive;
  begin
    pragma Warnings(Off, El);
    Grs := Get_Layers(Mpx);
    Create(Ft, Out_File, Fn);
    Directed := Is_Directed(Mpx);
    for L in Grs'Range loop
      Layer_Name := Get_Layer_Name(Mpx, L);
      N := Number_Of_Vertices(Grs(L));
      for F in 1..N loop
        Vf := Get_Vertex(Grs(L), F);
        Vf_Name := S2U(Get_Name(Vf));
        El := Edges_From(Vf);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          Vt := To(E);
          Vt_Name := S2U(Get_Name(Vt));
          T := Index_Of(Vt);
          if Directed or else F <= T then
            Put(Ft, U2S(Layer_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vf_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vt_Name));
            New_Line(Ft);
          end if;
        end loop;
        Restore(El);
      end loop;
    end loop;
    Close(Ft);
  end Put_Multiplex;

  -------------------
  -- Put_Multiplex --
  -------------------

  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_String_Multilayer.Multiplex) is
    Ft: File_Type;
    Grs: Graphs_String_Multilayer.PGraphs;
    Vf, Vt: Graphs_String.Vertex;
    El: Graphs_String.Edges_List;
    E: Graphs_String.Edge;
    Directed: Boolean;
    Layer_Name, Vf_Name, Vt_Name: Ustring;
    N: Natural;
    T: Positive;
  begin
    pragma Warnings(Off, El);
    Grs := Get_Layers(Mpx);
    Create(Ft, Out_File, Fn);
    Directed := Is_Directed(Mpx);
    for L in Grs'Range loop
      Layer_Name := Get_Layer_Name(Mpx, L);
      N := Number_Of_Vertices(Grs(L));
      for F in 1..N loop
        Vf := Get_Vertex(Grs(L), F);
        Vf_Name := S2U(Get_Name(Vf));
        El := Edges_From(Vf);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          Vt := To(E);
          Vt_Name := S2U(Get_Name(Vt));
          T := Index_Of(Vt);
          if Directed or else F <= T then
            Put(Ft, U2S(Layer_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vf_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vt_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Value(E)));
            New_Line(Ft);
          end if;
        end loop;
        Restore(El);
      end loop;
    end loop;
    Close(Ft);
  end Put_Multiplex;

  -------------------
  -- Put_Multiplex --
  -------------------

  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Integer_Multilayer.Multiplex) is
    Ft: File_Type;
    Grs: Graphs_Integer_Multilayer.PGraphs;
    Vf, Vt: Graphs_Integer.Vertex;
    El: Graphs_Integer.Edges_List;
    E: Graphs_Integer.Edge;
    Directed: Boolean;
    Layer_Name, Vf_Name, Vt_Name: Ustring;
    N: Natural;
    T: Positive;
  begin
    pragma Warnings(Off, El);
    Grs := Get_Layers(Mpx);
    Create(Ft, Out_File, Fn);
    Directed := Is_Directed(Mpx);
    for L in Grs'Range loop
      Layer_Name := Get_Layer_Name(Mpx, L);
      N := Number_Of_Vertices(Grs(L));
      for F in 1..N loop
        Vf := Get_Vertex(Grs(L), F);
        Vf_Name := S2U(Get_Name(Vf));
        El := Edges_From(Vf);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          Vt := To(E);
          Vt_Name := S2U(Get_Name(Vt));
          T := Index_Of(Vt);
          if Directed or else F <= T then
            Put(Ft, U2S(Layer_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vf_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vt_Name));
            Put(Ft, " ");
            Put(Ft, I2S(Value(E)));
            New_Line(Ft);
          end if;
        end loop;
        Restore(El);
      end loop;
    end loop;
    Close(Ft);
  end Put_Multiplex;

  -------------------
  -- Put_Multiplex --
  -------------------

  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Float_Multilayer.Multiplex; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) is
    Ft: File_Type;
    Grs: Graphs_Float_Multilayer.PGraphs;
    Vf, Vt: Graphs_Float.Vertex;
    El: Graphs_Float.Edges_List;
    E: Graphs_Float.Edge;
    Directed: Boolean;
    Layer_Name, Vf_Name, Vt_Name: Ustring;
    N: Natural;
    T: Positive;
  begin
    pragma Warnings(Off, El);
    Grs := Get_Layers(Mpx);
    Create(Ft, Out_File, Fn);
    Directed := Is_Directed(Mpx);
    for L in Grs'Range loop
      Layer_Name := Get_Layer_Name(Mpx, L);
      N := Number_Of_Vertices(Grs(L));
      for F in 1..N loop
        Vf := Get_Vertex(Grs(L), F);
        Vf_Name := S2U(Get_Name(Vf));
        El := Edges_From(Vf);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          Vt := To(E);
          Vt_Name := S2U(Get_Name(Vt));
          T := Index_Of(Vt);
          if Directed or else F <= T then
            Put(Ft, U2S(Layer_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vf_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vt_Name));
            Put(Ft, " ");
            Put(Ft, F2S(Value(E), Aft => Aft, Exp => Exp));
            New_Line(Ft);
          end if;
        end loop;
        Restore(El);
      end loop;
    end loop;
    Close(Ft);
  end Put_Multiplex;

  -------------------
  -- Put_Multiplex --
  -------------------

  procedure Put_Multiplex(Fn: in String; Mpx: in Graphs_Double_Multilayer.Multiplex; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) is
    Ft: File_Type;
    Grs: Graphs_Double_Multilayer.PGraphs;
    Vf, Vt: Graphs_Double.Vertex;
    El: Graphs_Double.Edges_List;
    E: Graphs_Double.Edge;
    Directed: Boolean;
    Layer_Name, Vf_Name, Vt_Name: Ustring;
    N: Natural;
    T: Positive;
  begin
    pragma Warnings(Off, El);
    Grs := Get_Layers(Mpx);
    Create(Ft, Out_File, Fn);
    Directed := Is_Directed(Mpx);
    for L in Grs'Range loop
      Layer_Name := Get_Layer_Name(Mpx, L);
      N := Number_Of_Vertices(Grs(L));
      for F in 1..N loop
        Vf := Get_Vertex(Grs(L), F);
        Vf_Name := S2U(Get_Name(Vf));
        El := Edges_From(Vf);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          Vt := To(E);
          Vt_Name := S2U(Get_Name(Vt));
          T := Index_Of(Vt);
          if Directed or else F <= T then
            Put(Ft, U2S(Layer_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vf_Name));
            Put(Ft, " ");
            Put(Ft, U2S(Vt_Name));
            Put(Ft, " ");
            Put(Ft, D2S(Value(E), Aft => Aft, Exp => Exp));
            New_Line(Ft);
          end if;
        end loop;
        Restore(El);
      end loop;
    end loop;
    Close(Ft);
  end Put_Multiplex;

end Multilayer_IO;
