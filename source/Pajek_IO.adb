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


-- @filename Pajek_IO.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 15/03/2006
-- @revision 23/09/2015
-- @brief Input and output in Pajek format

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers.Ordered_Maps;

with Utils.IO; use Utils.IO;
with Utils.IO_Integer; use Utils.IO_Integer;
with Utils.IO_Float; use Utils.IO_Float;
with Utils.IO_Double; use Utils.IO_Double;
with Arrays_Utils_Integer; use Arrays_Utils_Integer;

package body Pajek_IO is

  ---------------------
  -- Partition_Index --
  ---------------------

  function Partition_Index(Lol: in List_Of_Lists) return PIntegers is
    N: Natural;
    Li: PIntegers;
    Idx: Natural;
    L: List;
  begin
    pragma Warnings(Off, L);
    N := Number_Of_Elements(Lol);
    Li := Alloc(1, N, 0);
    Idx := 0;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Idx := Idx + 1;
      L := Next_List(Lol);
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        Li(Index_Of(Next_Element(L))) := Idx;
      end loop;
      Restore(L);
    end loop;
    Restore(Lol);
    return Li;
  end Partition_Index;

  --------------------
  -- Get_Graph_Info --
  --------------------

  procedure Get_Graph_Info(Ft: in out File_Type; N: out Positive; Directed: out Boolean; Listed: out Boolean) is
    W: Word_Access;
    X: Integer;
  begin
    Comments_Skip(Ft);
    Get_Word(Ft, W);
    if To_Lowercase(W.all) /= "*vertices" then
      raise Unrecognized_Pajek_Format with "Pajek network files need first a '*Vertices' section";
    end if;
    Free_Word(W);
    Get_Integer(Ft, N);
    Skip_Line(Ft);

    for I in 1..N loop
      Comments_Skip(Ft);
      Get_Integer(Ft, X);
      Skip_Line(Ft);
      if I /= X then
        raise Unrecognized_Pajek_Format with "Node with index " & I2S(I) & " not found in '*Vertices' section";
      end if;
    end loop;

    Comments_Skip(Ft);
    Get_Word(Ft, W);
    Skip_Line(Ft);
    if To_Lowercase(W.all) = "*arcs" then
      Directed := True;
      Listed := False;
    elsif To_Lowercase(W.all) = "*edges" then
      Directed := False;
      Listed := False;
    elsif To_Lowercase(W.all) = "*arcslist" then
      Directed := True;
      Listed := True;
    elsif To_Lowercase(W.all) = "*edgeslist" then
      Directed := False;
      Listed := True;
    else
      raise Unrecognized_Pajek_Format with "Pajek network files need an '*Edges', '*Arcs', '*EdgesList' or '*ArcsList' section";
    end if;
    Free_Word(W);

    Reset(Ft);
  end Get_Graph_Info;

  ---------------------
  -- Get_Vertex_Info --
  ---------------------

  procedure Get_Vertex_Info(Ft: in File_Type; Name, Tag: out Ustring) is
    X: Integer;
    C: Character;
    Eol: Boolean;
  begin
    Comments_Skip(Ft);
    Get_Integer(Ft, X);
    Line_Spaces_Skip(Ft);
    Look_Ahead(Ft, C, Eol);
    if Eol then
      Name := Null_Ustring;
    elsif C = '"' then
      Get(Ft, C);
      Name := Null_Ustring;
      loop
        Look_Ahead(Ft, C, Eol);
        exit when Eol or else C = '"';
        Get(Ft, C);
        Name := Name & C;
      end loop;
      if not Eol then
        Get(Ft, C);
      end if;
    else
      Get_Word(Ft, Name);
    end if;
    Line_Spaces_Skip(Ft);
    Tag := Null_Ustring;
    while not End_Of_Line(Ft) loop
      Get(Ft, C);
      Tag := Tag & C;
    end loop;
    Tag := Trim(Tag, Both);
    Skip_Line(Ft);
  end Get_Vertex_Info;

  ---------------------
  -- Put_Vertex_Info --
  ---------------------

  procedure Put_Vertex_Info(Ft: in File_Type; I: in Integer; Name, Tag: in String) is
    Has_Space: Boolean;
  begin
    Put(Ft, I, Width => 0);
    Put(Ft, " ");
    if Name'Length = 0 then
      Put(Ft, I, Width => 0);
    else
      Has_Space := False;
      for K in Name'Range loop
        if Is_Space(Name(K)) then
          Has_Space := True;
          exit;
        end if;
      end loop;
      if Has_Space then
        Put(Ft, """" & Name & """");
      else
        Put(Ft, Name);
      end if;
    end if;
    if Tag'Length > 0 then
      Put(Ft, " ");
      Put(Ft, Tag);
    end if;
    New_Line(Ft);
  end Put_Vertex_Info;

  ---------------
  -- Get_Graph --
  ---------------

  procedure Get_Graph(Fn: in String; Gr: out Graphs_Simple.Graph) is
    Ft: File_Type;
    N: Integer;
    Directed, Listed: Boolean;
    Name, Tag: Ustring;
    From, To: Integer;
  begin
    Open(Ft, In_File, Fn);
    Get_Graph_Info(Ft, N, Directed, Listed);
    Initialize(Gr, N, Directed);

    Comments_Skip(Ft);
    Skip_Line(Ft);
    for I in 1..N loop
      Get_Vertex_Info(Ft, Name, Tag);
      Set_Name(Get_Vertex(Gr, I), To_String(Name));
      Set_Tag(Get_Vertex(Gr, I), To_String(Tag));
    end loop;

    Comments_Skip(Ft);
    Skip_Line(Ft);
    while not End_Of_File(Ft) loop
      if Listed then
        Get_Integer(Ft, From);
        loop
          Line_Spaces_Skip(Ft);
          exit when End_Of_Line(Ft);
          Get_Integer(Ft, To);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        end loop;
      else
        Get_Integer(Ft, From);
        Get_Integer(Ft, To);
        Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
        Comments_Skip(Ft);
      end if;
    end loop;
    Close(Ft);

    Restore_Consistency(Gr);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
      raise Unrecognized_Pajek_Format with "Error reading a Pajek network file";
  end Get_Graph;

  ---------------
  -- Get_Graph --
  ---------------

  procedure Get_Graph(Fn: in String; Gr: out Graphs_String.Graph) is
    Ft: File_Type;
    N: Integer;
    Directed, Listed: Boolean;
    Name, Tag, Value: Ustring;
    From, To: Integer;
  begin
    Open(Ft, In_File, Fn);
    Get_Graph_Info(Ft, N, Directed, Listed);
    Initialize(Gr, N, Directed);

    Comments_Skip(Ft);
    Skip_Line(Ft);
    for I in 1..N loop
      Get_Vertex_Info(Ft, Name, Tag);
      Set_Name(Get_Vertex(Gr, I), To_String(Name));
      Set_Tag(Get_Vertex(Gr, I), To_String(Tag));
    end loop;

    Comments_Skip(Ft);
    Skip_Line(Ft);
    while not End_Of_File(Ft) loop
      if Listed then
        Get_Integer(Ft, From);
        loop
          Line_Spaces_Skip(Ft);
          exit when End_Of_Line(Ft);
          Get_Integer(Ft, To);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        end loop;
      else
        Get_Integer(Ft, From);
        Get_Integer(Ft, To);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if End_Of_Line(Ft) then
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        else
          Get_Word(Ft, Value);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To), Value);
        end if;
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
        Comments_Skip(Ft);
      end if;
    end loop;
    Close(Ft);

    Restore_Consistency(Gr);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
      raise Unrecognized_Pajek_Format with "Error reading a Pajek network file";
  end Get_Graph;

  ---------------
  -- Get_Graph --
  ---------------

  procedure Get_Graph(Fn: in String; Gr: out Graphs_Integer.Graph) is
    Ft: File_Type;
    N: Integer;
    Directed, Listed: Boolean;
    Name, Tag: Ustring;
    From, To, Value: integer;
  begin
    Open(Ft, In_File, Fn);
    Get_Graph_Info(Ft, N, Directed, Listed);
    Initialize(Gr, N, Directed);

    Comments_Skip(Ft);
    Skip_Line(Ft);
    for I in 1..N loop
      Get_Vertex_Info(Ft, Name, Tag);
      Set_Name(Get_Vertex(Gr, I), To_String(Name));
      Set_Tag(Get_Vertex(Gr, I), To_String(Tag));
    end loop;

    Comments_Skip(Ft);
    Skip_Line(Ft);
    while not End_Of_File(Ft) loop
      if Listed then
        Get_Integer(Ft, From);
        loop
          Line_Spaces_Skip(Ft);
          exit when End_Of_Line(Ft);
          Get_Integer(Ft, To);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        end loop;
      else
        Get_Integer(Ft, From);
        Get_Integer(Ft, To);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if End_Of_Line(Ft) then
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        else
          Get_Integer(Ft, Value);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To), Value);
        end if;
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
        Comments_Skip(Ft);
      end if;
    end loop;
    Close(Ft);

    Restore_Consistency(Gr);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
      raise Unrecognized_Pajek_Format with "Error reading a Pajek network file";
  end Get_Graph;

  ---------------
  -- Get_Graph --
  ---------------

  procedure Get_Graph(Fn: in String; Gr: out Graphs_Float.Graph) is
    Ft: File_Type;
    N: Integer;
    Directed, Listed: Boolean;
    Name, Tag: Ustring;
    From, To: Integer;
    Value: Float;
  begin
    Open(Ft, In_File, Fn);
    Get_Graph_Info(Ft, N, Directed, Listed);
    Initialize(Gr, N, Directed);

    Comments_Skip(Ft);
    Skip_Line(Ft);
    for I in 1..N loop
      Get_Vertex_Info(Ft, Name, Tag);
      Set_Name(Get_Vertex(Gr, I), To_String(Name));
      Set_Tag(Get_Vertex(Gr, I), To_String(Tag));
    end loop;

    Comments_Skip(Ft);
    Skip_Line(Ft);
    while not End_Of_File(Ft) loop
      if Listed then
        Get_Integer(Ft, From);
        loop
          Line_Spaces_Skip(Ft);
          exit when End_Of_Line(Ft);
          Get_Integer(Ft, To);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        end loop;
      else
        Get_Integer(Ft, From);
        Get_Integer(Ft, To);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if End_Of_Line(Ft) then
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        else
          Get_Float(Ft, Value);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To), Value);
        end if;
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
        Comments_Skip(Ft);
      end if;
    end loop;
    Close(Ft);

    Restore_Consistency(Gr);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
      raise Unrecognized_Pajek_Format with "Error reading a Pajek network file";
  end Get_Graph;

  ---------------
  -- Get_Graph --
  ---------------

  procedure Get_Graph(Fn: in String; Gr: out Graphs_Double.Graph) is
    Ft: File_Type;
    N: Integer;
    Directed, Listed: Boolean;
    Name, Tag: Ustring;
    From, To: Integer;
    Value: Double;
  begin
    Open(Ft, In_File, Fn);
    Get_Graph_Info(Ft, N, Directed, Listed);
    Initialize(Gr, N, Directed);

    Comments_Skip(Ft);
    Skip_Line(Ft);
    for I in 1..N loop
      Get_Vertex_Info(Ft, Name, Tag);
      Set_Name(Get_Vertex(Gr, I), To_String(Name));
      Set_Tag(Get_Vertex(Gr, I), To_String(Tag));
    end loop;

    Comments_Skip(Ft);
    Skip_Line(Ft);
    while not End_Of_File(Ft) loop
      if Listed then
        Get_Integer(Ft, From);
        loop
          Line_Spaces_Skip(Ft);
          exit when End_Of_Line(Ft);
          Get_Integer(Ft, To);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        end loop;
      else
        Get_Integer(Ft, From);
        Get_Integer(Ft, To);
        Line_Spaces_Skip(Ft);
        Line_Comment_Skip(Ft);
        if End_Of_Line(Ft) then
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To));
        else
          Get_Double(Ft, Value);
          Add_Edge_Unchecked(Get_Vertex(Gr, From), Get_Vertex(Gr, To), Value);
        end if;
      end if;
      if not End_Of_File(Ft) then
        Skip_Line(Ft);
        Comments_Skip(Ft);
      end if;
    end loop;
    Close(Ft);

    Restore_Consistency(Gr);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
      raise Unrecognized_Pajek_Format with "Error reading a Pajek network file";
  end Get_Graph;

  ---------------
  -- Put_Graph --
  ---------------

  procedure Put_Graph(Fn: in String; Gr: in Graphs_Simple.Graph) is
    Ft: File_Type;
    N: Natural;
    El: Graphs_Simple.Edges_List;
    E: Graphs_Simple.Edge;
    To: Positive;
  begin
    pragma Warnings(Off, El);
    N := Number_Of_Vertices(Gr);
    Create(Ft, Out_File, Fn);

    Put(Ft, "*Vertices ");
    Put(Ft, N, Width => 0);
    New_Line(Ft);
    for I in 1..N loop
      Put_Vertex_Info(Ft, I, Get_Name(Get_Vertex(Gr, I)), Get_Tag(Get_Vertex(Gr, I)));
    end loop;

    if Is_Directed(Gr) then
      Put_Line(Ft, "*Arcs");
    else
      Put_Line(Ft, "*Edges");
    end if;
    for From in 1..N loop
      El := Edges_From(Get_Vertex(Gr, From));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        To := Index_Of(Graphs_Simple.To(E));
        if Is_Directed(Gr) or else From <= To then
          Put(Ft, From, Width => 0);
          Put(Ft, " ");
          Put(Ft, To, Width => 0);
          New_Line(Ft);
        end if;
      end loop;
      Restore(El);
    end loop;

    Close(Ft);
  end Put_Graph;

  ---------------
  -- Put_Graph --
  ---------------

  procedure Put_Graph(Fn: in String; Gr: in Graphs_String.Graph) is
    Ft: File_Type;
    N: Natural;
    El: Graphs_String.Edges_List;
    E: Graphs_String.Edge;
    To: Positive;
  begin
    N := Number_Of_Vertices(Gr);
    Create(Ft, Out_File, Fn);

    Put(Ft, "*Vertices ");
    Put(Ft, N, Width => 0);
    New_Line(Ft);
    for I in 1..N loop
      Put_Vertex_Info(Ft, I, Get_Name(Get_Vertex(Gr, I)), Get_Tag(Get_Vertex(Gr, I)));
    end loop;

    if Is_Directed(Gr) then
      Put_Line(Ft, "*Arcs");
    else
      Put_Line(Ft, "*Edges");
    end if;
    for From in 1..N loop
      El := Edges_From(Get_Vertex(Gr, From));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        To := Index_Of(Graphs_String.To(E));
        if Is_Directed(Gr) or else From <= To then
          Put(Ft, From, Width => 0);
          Put(Ft, " ");
          Put(Ft, To, Width => 0);
          Put(Ft, " ");
          Put(Ft, To_String(Value(E)));
          New_Line(Ft);
        end if;
      end loop;
      Restore(El);
    end loop;

    Close(Ft);
  end Put_Graph;

  ---------------
  -- Put_Graph --
  ---------------

  procedure Put_Graph(Fn: in String; Gr: in Graphs_Integer.Graph) is
    Ft: File_Type;
    N: Natural;
    El: Graphs_Integer.Edges_List;
    E: Graphs_Integer.Edge;
    To: Positive;
  begin
    pragma Warnings(Off, El);
    N := Number_Of_Vertices(Gr);
    Create(Ft, Out_File, Fn);

    Put(Ft, "*Vertices ");
    Put(Ft, N, Width => 0);
    New_Line(Ft);
    for I in 1..N loop
      Put_Vertex_Info(Ft, I, Get_Name(Get_Vertex(Gr, I)), Get_Tag(Get_Vertex(Gr, I)));
    end loop;

    if Is_Directed(Gr) then
      Put_Line(Ft, "*Arcs");
    else
      Put_Line(Ft, "*Edges");
    end if;
    for From in 1..N loop
      El := Edges_From(Get_Vertex(Gr, From));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        To := Index_Of(Graphs_Integer.To(E));
        if Is_Directed(Gr) or else From <= To then
          Put(Ft, From, Width => 0);
          Put(Ft, " ");
          Put(Ft, To, Width => 0);
          Put(Ft, " ");
          Put(Ft, Value(E), Width => 0);
          New_Line(Ft);
        end if;
      end loop;
      Restore(El);
    end loop;

    Close(Ft);
  end Put_Graph;

  ---------------
  -- Put_Graph --
  ---------------

  procedure Put_Graph(Fn: in String; Gr: in Graphs_Float.Graph; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) is
    Ft: File_Type;
    N: Natural;
    El: Graphs_Float.Edges_List;
    E: Graphs_Float.Edge;
    To: Positive;
  begin
    pragma Warnings(Off, El);
    N := Number_Of_Vertices(Gr);
    Create(Ft, Out_File, Fn);

    Put(Ft, "*Vertices ");
    Put(Ft, N, Width => 0);
    New_Line(Ft);
    for I in 1..N loop
      Put_Vertex_Info(Ft, I, Get_Name(Get_Vertex(Gr, I)), Get_Tag(Get_Vertex(Gr, I)));
    end loop;

    if Is_Directed(Gr) then
      Put_Line(Ft, "*Arcs");
    else
      Put_Line(Ft, "*Edges");
    end if;
    for From in 1..N loop
      El := Edges_From(Get_Vertex(Gr, From));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        To := Index_Of(Graphs_Float.To(E));
        if Is_Directed(Gr) or else From <= To then
          Put(Ft, From, Width => 0);
          Put(Ft, " ");
          Put(Ft, To, Width => 0);
          Put(Ft, " ");
          Put(Ft, Value(E), Fore => 0, Aft => Aft, Exp => Exp);
          New_Line(Ft);
        end if;
      end loop;
      Restore(El);
    end loop;

    Close(Ft);
  end Put_Graph;

  ---------------
  -- Put_Graph --
  ---------------

  procedure Put_Graph(Fn: in String; Gr: in Graphs_Double.Graph; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) is
    Ft: File_Type;
    N: Natural;
    El: Graphs_Double.Edges_List;
    E: Graphs_Double.Edge;
    To: Positive;
  begin
    pragma Warnings(Off, El);
    N := Number_Of_Vertices(Gr);
    Create(Ft, Out_File, Fn);

    Put(Ft, "*Vertices ");
    Put(Ft, N, Width => 0);
    New_Line(Ft);
    for I in 1..N loop
      Put_Vertex_Info(Ft, I, Get_Name(Get_Vertex(Gr, I)), Get_Tag(Get_Vertex(Gr, I)));
    end loop;

    if Is_Directed(Gr) then
      Put_Line(Ft, "*Arcs");
    else
      Put_Line(Ft, "*Edges");
    end if;
    for From in 1..N loop
      El := Edges_From(Get_Vertex(Gr, From));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        To := Index_Of(Graphs_Double.To(E));
        if Is_Directed(Gr) or else From <= To then
          Put(Ft, From, Width => 0);
          Put(Ft, " ");
          Put(Ft, To, Width => 0);
          Put(Ft, " ");
          Put(Ft, Value(E), Fore => 0, Aft => Aft, Exp => Exp);
          New_Line(Ft);
        end if;
      end loop;
      Restore(El);
    end loop;

    Close(Ft);
  end Put_Graph;

  -------------------
  -- Get_Partition --
  -------------------

  procedure Get_Partition(Fn: in String; Lol: out List_Of_Lists) is
    package List_Maps is new Ada.Containers.Ordered_Maps(Integer, List);
    use List_Maps;

    Ft: File_Type;
    W: Word_Access;
    N: Natural;
    Idx: Integer;
    M: Map;
  begin
    Open(Ft, In_File, Fn);

    Comments_Skip(Ft);
    Get_Word(Ft, W);
    if To_Lowercase(W.all) /= "*vertices" then
      raise Unrecognized_Pajek_Format with "Pajek partition files need first a '*Vertices' section";
    end if;
    Free_Word(W);
    Get_Integer(Ft, N);
    Skip_Line(Ft);

    Initialize(Lol, N);
    for I in 1..N loop
      Get_Integer(Ft, Idx);
      if not Contains(M, Idx) then
        Insert(M, Idx, New_List(Lol));
      end if;
      Skip_Line(Ft);
      Comments_Skip(Ft);
    end loop;

    Reset(Ft);
    Comments_Skip(Ft);
    Skip_Line(Ft);
    for I in 1..N loop
      Get_Integer(Ft, Idx);
      Move(Get_Element(Lol, I), List_Maps.Element(M, Idx));
      if I < N then
        Skip_Line(Ft);
        Comments_Skip(Ft);
      end if;
    end loop;

    Close(Ft);
  exception
    when E: others =>
      Put(Exception_Information(E));
      Put_Line(NLine & Fn & ":" & I2S(Integer(Line(Ft))) & "," & I2S(Integer(Col(Ft))));
      raise Unrecognized_Pajek_Format with "Error reading a Pajek partition file";
  end Get_Partition;

  -------------------
  -- Put_Partition --
  -------------------

  procedure Put_Partition(Fn: in String; Lol: in List_Of_Lists) is
    Ft: File_Type;
    N: Integer;
    Li: PIntegers;
  begin
    N := Number_Of_Elements(Lol);
    Li := Partition_Index(Lol);

    Create(Ft, Out_File, Fn);
    Put(Ft, "*Vertices ");
    Put(Ft, N, Width => 0);
    New_Line(Ft);
    for I in 1..N loop
      Put_Line(Ft, I2S(Li(I)));
    end loop;
    Close(Ft);

    Free(Li);
  end Put_Partition;

end Pajek_IO;
