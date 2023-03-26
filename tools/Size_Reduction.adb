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


-- @filename Size_Reduction.adb
-- @author Alberto Fernández
-- @author Sergio Gomez
-- @version 1.0
-- @date 30/11/2007
-- @revision 31/08/2020
-- @brief Reduces the size of a graph and outputs a reduced graph and the reduction Lol

with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Command_Line;                 use Ada.Command_Line;

with Finite_Disjoint_Lists;            use Finite_Disjoint_Lists;
with Finite_Disjoint_Lists.IO;         use Finite_Disjoint_Lists.IO;
with Pajek_IO;                         use Pajek_IO;
with Utils;                            use Utils;
with Graphs;
with Graphs.Algorithms;
with Graphs.Operations.Modularities;
with Graphs_Integer;
with Graphs_Integer_Algorithms;
with Graphs_Integer_Operations;
with Graphs_Integer_Modularities_D;
with Graphs_Float;
with Graphs_Float_Algorithms;
with Graphs_Float_Operations;
with Graphs_Float_Modularities_D;
with Graphs_Double;
with Graphs_Double_Algorithms;
with Graphs_Double_Operations;
with Graphs_Double_Modularities_D;

procedure Size_Reduction is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2023 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Reduction of the size of a network preserving modularity,     ==");
    Put_Line("== by elimination of simple and triangular 'hairs'               ==");
    Put_Line("== Only for Weighted_Newman (WN) modularity type                 ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  generic
    with package The_Graphs is new Graphs(<>);
    with package The_Graphs_Algorithms is new The_Graphs.Algorithms(<>);
    with package The_Graphs_Operations is new The_Graphs.Operations(<>);
    with package The_Graphs_Modularities is new The_Graphs_Operations.Modularities(<>);
  procedure Generic_Graph_Size_Reduction(Gr: in The_Graphs.Graph; Ren: out List_Of_Lists; Gr_Ren: out The_Graphs.Graph; Steps: out Natural);

  procedure Generic_Graph_Size_Reduction(Gr: in The_Graphs.Graph; Ren: out List_Of_Lists; Gr_Ren: out The_Graphs.Graph; Steps: out Natural) is
    use The_Graphs;
    use The_Graphs_Algorithms;
    use The_Graphs_Operations;
    use The_Graphs_Modularities;

    function Proper_Degree_To(V: in Vertex) return Natural is
      K: Natural;
    begin
      K := Degree_To(V);
      if Edge_Exists(V,V) then
        K := K - 1;
      end if;
      return K;
    end Proper_Degree_To;

    function Proper_Degree_From(V: in Vertex) return Natural is
      K: Natural;
    begin
      K := Degree_From(V);
      if Edge_Exists(V,V) then
        K := K - 1;
      end if;
      return K;
    end Proper_Degree_From;

    function Neighbour_To(V: in Vertex) return Vertex is
      W: Vertex;
      Found: Boolean;
      EL: Edges_List;
      E: Edge;
    begin
      Found := False;
      EL := Edges_To(V);
      Save(EL);
      Reset(EL);
      while (not Found) and Has_Next(EL) loop
        E := Next(EL);
        W := From(E);
        if W /= V then
          Found := True;
        end if;
      end loop;
      Restore(EL);
      return W;
    end Neighbour_To;

    function Neighbour_From(V: in Vertex) return Vertex is
      W: Vertex;
      Found: Boolean;
      EL: Edges_List;
      E: Edge;
    begin
      Found := False;
      EL := Edges_From(V);
      Save(EL);
      Reset(EL);
      while (not Found) and Has_Next(EL) loop
        E := Next(EL);
        W := To(E);
        if W /= V then
          Found := True;
        end if;
      end loop;
      Restore(EL);
      return W;
    end Neighbour_From;

    procedure Is_Hair(Vi: in Vertex; Hair: out Boolean; Vj: out Vertex) is
      Ki_To, Ki_From: Natural;
    begin
      Ki_To := Proper_Degree_To(Vi);
      Ki_From := Proper_Degree_From(Vi);
      if (Ki_To = 1) and (Ki_From = 0) then
        Hair := True;
        Vj := Neighbour_To(Vi);
      elsif (Ki_To = 0) and (Ki_From = 1) then
        Hair := True;
        Vj := Neighbour_From(Vi);
      elsif (Ki_To = 1) and (Ki_From = 1) then
        Vj := Neighbour_To(Vi);
        Hair := (Neighbour_From(Vi) = Vj);
      else
        Hair := False;
      end if;
    end Is_Hair;

    procedure Neighbours_From(Vi: in Vertex; Vj, Vk: out Vertex) is
      EL: Edges_List;
      E: Edge;
    begin
      EL := Edges_From(Vi);
      Save(EL);
      Reset(EL);
      E := Next(EL);
      Vj := To(E);
      E := Next(EL);
      Vk := To(E);
      if Vj = Vi then
        E := Next(EL);
        Vj := To(E);
      elsif Vk = Vi then
        E := Next(EL);
        Vk := To(E);
      end if;
      Restore(EL);
    end Neighbours_From;

    procedure Is_Triangular_Hair(Directed: in Boolean; Vi: in Vertex; Triangle: out Boolean; Vj, Vk: out Vertex) is
      Ki_To, Ki_From: Natural;
      W: Vertex;
    begin
      Ki_To := Proper_Degree_To(Vi);
      Ki_From := Proper_Degree_From(Vi);
      if (Ki_From = 2) and ((Ki_To = 0) or (not Directed)) then
        Neighbours_From(Vi, Vj, Vk);
        if (Proper_Degree_To(Vk) = 2) and ((Proper_Degree_From(Vk) = 0) or (not Directed)) then
          W := Vj;
          Vj := Vk;
          Vk := W;
        end if;
        Triangle := Edge_Exists(Vi,Vk) and Edge_Exists(Vk,Vj)
          and (Proper_Degree_To(Vj) = 2)
          and ((Proper_Degree_From(Vj) = 0) or (not Directed));
      else
        Triangle := False;
      end if;
    end Is_Triangular_Hair;

    function Satisfies_Merge_Condition(MI: in Modularity_Info; Vi: in Vertex) return Boolean is
      W2, Wi_Out, Wi_In, Wii: Double;
      Eii: Edge;
    begin
      W2     := Double(Total_Strength(MI));
      Wi_Out := Double(Strength_From(MI, Vi));
      Wi_In  := Double(Strength_To(MI, Vi));
      if Edge_Exists(Vi,Vi) then
        Eii := Get_Edge(Vi,Vi);
        Wii := Double(To_Num(Value(Eii)));
      else
        Wii := 0.0;
      end if;
      return Wii <= (Wi_Out * Wi_In) / W2;
    end Satisfies_Merge_Condition;

    procedure Try_Merging_Vertices(Vi, Vj: in Vertex; LOL: in out List_Of_Lists) is
      I, J: Positive;
      Ei, Ej: Finite_Disjoint_Lists.Element;
      Li, Lj: List;
    begin
      I := Index_Of(Vi);
      J := Index_Of(Vj);
      Ei := Get_Element(LOL, I);
      Ej := Get_Element(LOL, J);
      Li := List_Of(Ei);
      Lj := List_Of(Ej);
      if Li /= Lj then
        Move(Li, Lj);
        Remove(Li);
      end if;
    end Try_Merging_Vertices;

    procedure Renormalizing_List_Of_Lists(Gr: in Graph; LOL: out List_Of_Lists) is
      MI: Modularity_Info;
      Num_Vertices: Natural;
      Vi, Vj, Vk: Vertex;
      Hair, Triangle: Boolean;
    begin
      Initialize(MI, Gr, MT => Weighted_Newman);
      Num_Vertices := Number_Of_Vertices(Gr);
      Initialize(LOL, Num_Vertices, Isolated_Initialization);
      for I in 1 .. Num_Vertices loop
        Vi := Get_Vertex(Gr, I);
        Is_Hair(Vi, Hair, Vj);
        if Hair and then Satisfies_Merge_Condition(MI, Vi) then
          Try_Merging_Vertices(Vi, Vj, LOL);
        else
          Is_Triangular_Hair(Is_Directed(Gr), Vi, Triangle, Vj, Vk);
          if Triangle and then (Satisfies_Merge_Condition(MI, Vi)
              and Satisfies_Merge_Condition(MI, Vj)) then
            Try_Merging_Vertices(Vj, Vi, LOL);
          end if;
        end if;
      end loop;
      Free(MI);
    end Renormalizing_List_Of_Lists;

    Gr_Curr: Graph;
    Lol_Ren, Lol: List_Of_Lists;
    N: Natural;
  begin
    Gr_Curr := Clone(Gr);
    N := Number_Of_Vertices(Gr_Curr);
    Initialize(Ren, N, Isolated_Initialization);
    Steps := 0;
    loop
      Renormalizing_List_Of_Lists(Gr_Curr, Lol_Ren);
      if Number_Of_Lists(Lol_Ren) = Number_Of_Lists(Ren) then
        Free(Lol_Ren);
        exit;
      end if;
      Renormalize_Graph(Gr_Curr, Lol_Ren, Gr_Ren);
      Free(Gr_Curr);
      Gr_Curr := Gr_Ren;
      Unrenormalize_List_Of_Lists(Lol_Ren, Ren, Lol);
      Free(Lol_Ren);
      Free(Ren);
      Ren := Lol;
      Steps := Steps + 1;
    end loop;

    Gr_Ren := Gr_Curr;
    if Steps > 1 then
      Free(Gr_Curr);
      Renormalize_Graph(Gr, Ren, Gr_Ren);
    end if;
  end Generic_Graph_Size_Reduction;

  procedure Graph_Size_Reduction is new Generic_Graph_Size_Reduction(Graphs_Integer,
                                                                     Graphs_Integer_Algorithms,
                                                                     Graphs_Integer_Operations,
                                                                     Graphs_Integer_Modularities_D);
  procedure Graph_Size_Reduction is new Generic_Graph_Size_Reduction(Graphs_Float,
                                                                     Graphs_Float_Algorithms,
                                                                     Graphs_Float_Operations,
                                                                     Graphs_Float_Modularities_D);
  procedure Graph_Size_Reduction is new Generic_Graph_Size_Reduction(Graphs_Double,
                                                                     Graphs_Double_Algorithms,
                                                                     Graphs_Double_Operations,
                                                                     Graphs_Double_Modularities_D);

  Net_Sufix: constant String := ".net";
  Red_Net_Sufix: constant String := "-reduced.net";
  Red_Lol_Sufix: constant String := "-reducer.txt";
  Gr_Name, Lol_Name, Ren_Name, Wh_Id_Name: Ustring;
  Wh_Id: Numeric_Type_Id;
  Aft: Field := Default_Float_Aft;
  Gr_I, Gr_I_Ren: Graphs_Integer.Graph;
  Gr_F, Gr_F_Ren: Graphs_Float.Graph;
  Gr_D, Gr_D_Ren: Graphs_Double.Graph;
  Ren: List_Of_Lists;
  N, N_Ren, Steps: Natural;
  Ft: File_Type;

begin
  Put_Info;

  if Argument_Count = 2 then
    Gr_Name    := S2U(Argument(1) & Net_Sufix);
    Lol_Name   := S2U(Argument(1) & Red_Lol_Sufix);
    Ren_Name   := S2U(Argument(1) & Red_Net_Sufix);
    Wh_Id_Name := S2U(Argument(2));
    Aft := Default_Double_Aft;
  elsif Argument_Count = 3 then
    Gr_Name    := S2U(Argument(1) & Net_Sufix);
    Lol_Name   := S2U(Argument(1) & Red_Lol_Sufix);
    Ren_Name   := S2U(Argument(1) & Red_Net_Sufix);
    Wh_Id_Name := S2U(Argument(2));
    Aft := S2I(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name_without_ext  weights_type  [ decimal_digits ]");
    New_Line;
    Put_Line("   net_name_without_ext :  name of the network file in Pajek format without the .net extension");
    New_Line;
    Put_Line("   weights_type         :  I | F | D");
    Put_Line("                             also lowercase symbols");
    Put_Line("                             also case-insensitive full names");
    Put_Line("                             I = Integer = Int");
    Put_Line("                             F = Float");
    Put_Line("                             D = Double");
    New_Line;
    Put_Line("   decimal_digits       :  number of decimal digits for Float or Double output weights");
    Put_Line("                             ignored for Integer weights");
    Put_Line("                             default => " & I2S(Aft));
    return;
  end if;

  Put(U2S(Gr_Name) & ": ");

  Wh_Id := To_Type_Id(U2S(Wh_Id_Name));

  case Wh_Id is
    when Integer_Id..Longint_Id =>
      Get_Graph(U2S(Gr_Name), Gr_I);
      Graph_Size_Reduction(Gr_I, Ren, Gr_I_Ren, Steps);
      N     := Graphs_Integer.Number_Of_Vertices(Gr_I);
      N_Ren := Graphs_Integer.Number_Of_Vertices(Gr_I_Ren);
      if N_Ren < N then
        Put_Graph(U2S(Ren_Name), Gr_I_Ren);
      end if;
      Graphs_Integer.Free(Gr_I);
      Graphs_Integer.Free(Gr_I_Ren);
    when Float_Id =>
      Get_Graph(U2S(Gr_Name), Gr_F);
      Graph_Size_Reduction(Gr_F, Ren, Gr_F_Ren, Steps);
      N     := Graphs_Float.Number_Of_Vertices(Gr_F);
      N_Ren := Graphs_Float.Number_Of_Vertices(Gr_F_Ren);
      if N_Ren < N then
        Put_Graph(U2S(Ren_Name), Gr_F_Ren, Aft => Aft);
      end if;
      Graphs_Float.Free(Gr_F);
      Graphs_Float.Free(Gr_F_Ren);
    when Double_Id =>
      Get_Graph(U2S(Gr_Name), Gr_D);
      Graph_Size_Reduction(Gr_D, Ren, Gr_D_Ren, Steps);
      N     := Graphs_Double.Number_Of_Vertices(Gr_D);
      N_Ren := Graphs_Double.Number_Of_Vertices(Gr_D_Ren);
      if N_Ren < N then
        Put_Graph(U2S(Ren_Name), Gr_D_Ren, Aft => Aft);
      end if;
      Graphs_Double.Free(Gr_D);
      Graphs_Double.Free(Gr_D_Ren);
  end case;

  if N_Ren < N then
    Put_Line("Reduction in " & I2S(Steps) & " steps: " & I2S(N) & " -> " & I2S(N_Ren));
    Create(Ft, Out_File, U2S(Lol_Name));
    Put_Line(Ft, "---------");
    Put_Line(Ft, "Reduction partition");
    Put_Line(Ft, U2S(Gr_Name) & " -> " & U2S(Ren_Name));
    Put_Line(Ft, "---");
    Put(Ft, Ren);
    Close(Ft);
  else
    Put_Line("No reduction");
  end if;

  Free(Ren);
end Size_Reduction;
