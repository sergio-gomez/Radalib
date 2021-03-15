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


-- @filename Network_Properties.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 23/05/2011
-- @revision 26/02/2016
-- @brief Properties of a Network

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils; use Utils;
with Utils.IO; use Utils.IO;
with Pajek_IO; use Pajek_IO;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Algorithms; use Graphs_Double_Algorithms;
with Graphs_Double_Properties_D; use Graphs_Double_Properties_D;
with Statistics_Double; use Statistics_Double;
with Chrono_Utils; use Chrono_Utils;


procedure Network_Properties is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2021 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Find many global, node and edge properties of a network:      ==");
    Put_Line("==   - connectedness (weak or strong)                            ==");
    Put_Line("==   - degrees, strengths, clustering coefficients, entropies    ==");
    Put_Line("==   - assortativities, path lengths, efficiencies, diameters    ==");
    Put_Line("==   - betweenness (nodes and edges)                             ==");
    Put_Line("==   - degree distribution                                       ==");
    Put_Line("== Works with weighted and unweighted, directed and undirected,  ==");
    Put_Line("== positive and signed networks                                  ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  type Properties_Subset is (Global_Properties, Nodes_Properties, Edges_Properties, Degrees_Properties,
                             Distances_Properties, Unweighted_Properties, Fast_Properties, All_Properties);
  subtype Files_Subset is Properties_Subset range Global_Properties..Distances_Properties;
  type Properties_Subset_Selection is array(Properties_Subset) of Boolean;

  Unknown_Properties_Subset_Error: exception;

  function Get_Properties_Subset(S: in String) return Properties_Subset is
  begin
    if    To_Uppercase(S) = "G" or To_Lowercase(S) = "global"     then
      return Global_Properties;
    elsif To_Uppercase(S) = "N" or To_Lowercase(S) = "nodes"      then
      return Nodes_Properties;
    elsif To_Uppercase(S) = "E" or To_Lowercase(S) = "edges"      then
      return Edges_Properties;
    elsif To_Uppercase(S) = "D" or To_Lowercase(S) = "degrees"    then
      return Degrees_Properties;
    elsif To_Uppercase(S) = "L" or To_Lowercase(S) = "distances"  then
      return Distances_Properties;
    elsif To_Uppercase(S) = "U" or To_Lowercase(S) = "unweighted" then
      return Unweighted_Properties;
    elsif To_Uppercase(S) = "F" or To_Lowercase(S) = "fast"       then
      return Fast_Properties;
    elsif To_Uppercase(S) = "A" or To_Lowercase(S) = "all"        then
      return All_Properties;
    else
      raise Unknown_Properties_Subset_Error;
    end if;
  end Get_Properties_Subset;

  procedure Set_Properties_Subset(S: in String; Props_Selection: out Properties_Subset_Selection) is
    Ps: Properties_Subset;
  begin
    Props_Selection := (others => False);
    begin
      Ps := Get_Properties_Subset(S);
      Props_Selection(Ps) := True;
      if Ps = All_Properties then
        Props_Selection := (Files_Subset => True, others => False);
      end if;
    exception
      when Unknown_Properties_Subset_Error =>
        for I in S'Range loop
          Ps := Get_Properties_Subset(S(I..I));
          Props_Selection(Ps) := True;
          if Ps = All_Properties then
            Props_Selection := (Files_Subset => True, others => False);
          end if;
        end loop;
    end;
  end Set_Properties_Subset;

  function Num2S(D: in Double; Is_Int: in Boolean := False; Aft: Field := Default_Double_Aft) return String is
  begin
    if Is_Int then
      return I2S(Integer(D));
    else
      return D2Sea(D, Aft => Aft);
    end if;
  end Num2S;

  function Is_Integer_Graph(Gr: in Graph) return Boolean is
    Vf: Vertex;
    El: Edges_List;
    E: Edge;
    Wh: Double;
  begin
    for F in 1..Number_Of_Vertices(Gr) loop
      Vf := Get_Vertex(Gr, F);
      El := Edges_From(Vf);
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        Wh := Value(E);
        if Round(Wh) /= Wh then
          Restore(El);
          return False;
        end if;
      end loop;
      Restore(El);
    end loop;
    return True;
  end Is_Integer_Graph;

  procedure Degree_Distribution(P: in PDoubles; Freq, Cum: out PIntegers; Size: in Natural) is
    Dmax, K: Natural;
  begin
    Dmax := Integer(Max(P));
    Freq := Alloc(0, Size);
    Freq.all := (others => 0);
    for I in P'Range loop
      K := Integer(P(I));
      Freq(K) := Freq(K) + 1;
    end loop;
    Cum  := Alloc(0, Size);
    Cum.all := (others => 0);
    Cum(Dmax) := Freq(Dmax);
    for K in reverse 0..(Dmax-1) loop
      Cum(K) := Freq(K) + Cum(K + 1);
    end loop;
  end Degree_Distribution;


  Out_Global_Sufix   : constant String := "-info_global.txt";
  Out_Nodes_Sufix    : constant String := "-info_nodes.txt";
  Out_Edges_Sufix    : constant String := "-info_edges_betw.net";
  Out_Edges_Wh_Sufix : constant String := "-info_edges_betw_weigh.net";
  Out_Degrees_Sufix  : constant String := "-info_degrees.txt";
  Out_Dists_Sufix    : constant String := "-info_dists.txt";
  Out_Dists_Wh_Sufix : constant String := "-info_dists_weigh.txt";
  Default_Props      : constant Ustring := S2U("all");

  Gr: Graph;
  Fn_Net: Ustring;
  Props: Ustring;
  Aft: Field := Default_Double_Aft;

  Props_Selection: Properties_Subset_Selection;
  Fn_Base, Fn_Path: Ustring;
  Fn_Out_Global: Ustring;
  Fn_Out_Nodes: Ustring;
  Fn_Out_Edges: Ustring;
  Fn_Out_Edges_Wh: Ustring;
  Fn_Out_Degrees: Ustring;
  Fn_Out_Dists: Ustring;
  Fn_Out_Dists_Wh: Ustring;
  F_Out: File_Type;

  Directed: Boolean;
  Weighted, Weighted_Graph: Boolean;
  Signed: Boolean;
  Ints_Graph: Boolean;
  Connected_Strong, Connected_Weak: Boolean;
  Fast: Boolean;
  Both_Bet: Boolean;
  N, Ne, Nsl: Natural;
  U: Ustring;
  V: Vertex;
  Vst: Values_Statistic_Type;
  Gr_Bet, Gr_Bet_Wh: Graph;
  Din, Dinpos, Dinneg, Dout, Doutpos, Doutneg: PDoubles;
  Fin, Finpos, Finneg, Fout, Foutpos, Foutneg: PIntegers;
  Cin, Cinpos, Cinneg, Cout, Coutpos, Coutneg: PIntegers;
  Deg_Max: Natural;
  Cet: Stats.Correlation_Error_Type;
  Bet, Bet_Wh: PDoubles;
  Chrono: Chronometer;

begin
  Put_Info;

  if Argument_Count = 1 then
    Fn_Net := S2U(Argument(1));
    Props  := Default_Props;
    Aft    := Default_Double_Aft;
  elsif Argument_Count = 2 then
    Fn_Net := S2U(Argument(1));
    if Is_Integer(Argument(2)) then
      Props := Default_Props;
      Aft   := S2I(Argument(2));
    else
      Props := S2U(Argument(2));
      Aft   := Default_Double_Aft;
    end if;
  elsif Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    Props  := S2U(Argument(2));
    Aft    := S2I(Argument(3));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  [ properties ]  [ decimal_digits ]");
    New_Line;
    Put_Line("   Network Name      :  Name of the input network file in Pajek format (*.net)");
    New_Line;
    Put_Line("   Properties String :  [GNEDLUFA]+");
    Put_Line("                          also uppercase symbols");
    Put_Line("                          also single case-insensitive full names (All, Global, ...)");
    Put_Line("                            G = Global");
    Put_Line("                            N = Nodes");
    Put_Line("                            E = Edges");
    Put_Line("                            D = Degrees");
    Put_Line("                            L = Distances");
    Put_Line("                            U = Unweighted");
    Put_Line("                            F = Fast");
    Put_Line("                            A = All");
    Put_Line("                          default => All");
    Put_Line("                          properties available in each class");
    Put_Line("                            G: type and size of graph, connectedness, average and total degree and strength,");
    Put_Line("                               minimum and maximum values, asymmetry, reciprocity, assortativity,");
    Put_Line("                               average clustering coefficient, average path length, diameter, efficiency,");
    Put_Line("                               average entropy");
    Put_Line("                            N: degrees, strengths, self-loop, minimum, maximum and average values,");
    Put_Line("                               clustering coefficient, average and maximum path lengths, efficiency,");
    Put_Line("                               entropy, node betweenness");
    Put_Line("                            E: edge betweenness");
    Put_Line("                            D: degree distribution");
    Put_Line("                            L: distances between nodes");
    Put_Line("                            U: unweighted properties, excluding weighted ones");
    Put_Line("                            F: only fast calculation properties:");
    Put_Line("                               exclude average and maximum path length, diameter, efficiency,");
    Put_Line("                               betweenness and distances");
    Put_Line("                            A: all properties available; disables Unweighted and Fast");
    Put_Line("                          processed from left to right, thus AU is not equivalent to UA");
    Put_Line("                          weights should be distances to have meaningful shortest path weighted properties");
    New_Line;
    Put_Line("   Decimal Digits    :  number of decimal digits for float values");
    Put_Line("                          default => " & I2S(Default_Double_Aft));
    return;
  end if;

  Fn_Base         := S2U(File_Base_Name(U2S(Fn_Net)));
  Fn_Path         := S2U(File_Path(U2S(Fn_Net)));
  Fn_Out_Global   := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Global_Sufix));
  Fn_Out_Nodes    := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Nodes_Sufix));
  Fn_Out_Edges    := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Edges_Sufix));
  Fn_Out_Edges_Wh := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Edges_Wh_Sufix));
  Fn_Out_Degrees  := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Degrees_Sufix));
  Fn_Out_Dists    := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Dists_Sufix));
  Fn_Out_Dists_Wh := S2U(Compose_File_Name(U2S(Fn_Path), U2S(Fn_Base) & Out_Dists_Wh_Sufix));

  Set_Properties_Subset(U2S(Props), Props_Selection);

  Get_Graph(U2S(Fn_Net), Gr);

  Directed       := Is_Directed(Gr);
  Weighted_Graph := Is_Weighted(Gr);
  Signed         := Has_Links(Gr, Negative_Links);
  Ints_Graph     := Is_Integer_Graph(Gr);
  Connected_Strong := Is_Connected(Gr, Strong_Components);
  if (not Directed) or Connected_Strong then
    Connected_Weak := Connected_Strong;
  else
    Connected_Weak := Is_Connected(Gr, Weak_Components);
  end if;
  N   := Number_Of_Vertices(Gr);
  Ne  := Number_Of_Edges(Gr);
  Nsl := Number_Of_Self_Loops(Gr);

  Weighted := Weighted_Graph and not Props_Selection(Unweighted_Properties);
  Fast     := Props_Selection(Fast_Properties);
  Both_Bet := (not Fast) and Props_Selection(Nodes_Properties) and Props_Selection(Edges_Properties);

  Put_Line(U2S(Fn_Net) & ":");
  Put_Line("  Vertices   : " & HTab & I2S(N));
  Put_Line("  Edges      : " & HTab & I2S(Ne));
  Put_Line("  Self-loops : " & HTab & I2S(Nsl));
  Put_Line("  Directed   : " & HTab & Capitalize(Boolean'Image(Directed)));
  Put_Line("  Weighted   : " & HTab & Capitalize(Boolean'Image(Weighted_Graph)));
  Put_Line("  Signed     : " & HTab & Capitalize(Boolean'Image(Signed)));
  if Weighted then
    Put_Line("  Integer    : " & HTab & Capitalize(Boolean'Image(Ints_Graph)));
  end if;
  if Connected_Strong then
    Put_Line("  Connected  : " & HTab & "Strong");
  elsif Connected_Weak then
    Put_Line("  Connected  : " & HTab & "Weak");
  else
    Put_Line("  Connected  : " & HTab & "No");
  end if;
  New_Line;

  -- Global properties
  if Props_Selection(Global_Properties) then
    Put("  " & File_Name(U2S(Fn_Out_Global)) & ": ");
    Create(F_Out, Out_File, U2S(Fn_Out_Global));
    -- Basic
    Put_Line(F_Out, "File name  :" & HTab & U2S(Fn_Net));
    New_Line(F_Out);
    Put_Line(F_Out, "Directed   :" & HTab & Capitalize(Boolean'Image(Directed)));
    Put_Line(F_Out, "Weighted   :" & HTab & Capitalize(Boolean'Image(Weighted_Graph)));
    Put_Line(F_Out, "Signed     :" & HTab & Capitalize(Boolean'Image(Signed)));
    if Weighted then
      Put_Line(F_Out, "Integer    :" & HTab & Capitalize(Boolean'Image(Ints_Graph)));
    end if;
    if Connected_Strong then
      Put_Line(F_Out, "Connected  :" & HTab & "Strong");
    elsif Connected_Weak then
      Put_Line(F_Out, "Connected  :" & HTab & "Weak");
    else
      Put_Line(F_Out, "Connected  :" & HTab & "False");
    end if;
    New_Line(F_Out);
    Put_Line(F_Out, "Vertices   :" & HTab & I2S(N));
    Put_Line(F_Out, "Edges      :" & HTab & I2S(Ne));
    if Signed then
      Put_Line(F_Out, "  (+)      :" & HTab & I2S(Integer(Number_Of_Edges(Gr, Positive_Links))));
      Put_Line(F_Out, "  (-)      :" & HTab & I2S(Integer(Number_Of_Edges(Gr, Negative_Links))));
    end if;
    Put_Line(F_Out, "Self-loops :" & HTab & I2S(Nsl));
    if Signed then
      Put_Line(F_Out, "  (+)      :" & HTab & I2S(Integer(Number_Of_Self_Loops(Gr, Positive_Links))));
      Put_Line(F_Out, "  (-)      :" & HTab & I2S(Integer(Number_Of_Self_Loops(Gr, Negative_Links))));
    end if;
    New_Line(F_Out);
    Put(".");
    -- Degree
    Put_Line(F_Out, "Total degree   :" & HTab & I2S(Total_Degree(Gr)));
    if Signed then
      Put_Line(F_Out, "  (+)          :" & HTab & I2S(Integer(Total_Degree(Gr, Positive_Links))));
      Put_Line(F_Out, "  (-)          :" & HTab & I2S(Integer(Total_Degree(Gr, Negative_Links))));
    end if;
    Put_Line(F_Out, "Average degree :" & HTab & Num2S(Average_Degree(Gr), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)          :" & HTab & Num2S(Average_Degree(Gr, Positive_Links), Aft => Aft));
      Put_Line(F_Out, "  (-)          :" & HTab & Num2S(Average_Degree(Gr, Negative_Links), Aft => Aft));
    end if;
    if not Directed then
      Put_Line(F_Out, "Minimum degree :" & HTab & I2S(Integer(Minimum_Degree(Gr, To_Links))));
      if Signed then
        Put_Line(F_Out, "  (+)          :" & HTab & I2S(Integer(Minimum_Degree(Gr, To_Links, Positive_Links))));
        Put_Line(F_Out, "  (-)          :" & HTab & I2S(Integer(Minimum_Degree(Gr, To_Links, Negative_Links))));
      end if;
      Put_Line(F_Out, "Maximum degree :" & HTab & I2S(Integer(Maximum_Degree(Gr, To_Links))));
      if Signed then
        Put_Line(F_Out, "  (+)          :" & HTab & I2S(Integer(Maximum_Degree(Gr, To_Links, Positive_Links))));
        Put_Line(F_Out, "  (-)          :" & HTab & I2S(Integer(Maximum_Degree(Gr, To_Links, Negative_Links))));
      end if;
    else
      Put_Line(F_Out, "Minimum degree :");
      Put_Line(F_Out, "  (in)         :" & HTab & I2S(Integer(Minimum_Degree(Gr, To_Links))));
      if Signed then
        Put_Line(F_Out, "  (in)(+)      :" & HTab & I2S(Integer(Minimum_Degree(Gr, To_Links, Positive_Links))));
        Put_Line(F_Out, "  (in)(-)      :" & HTab & I2S(Integer(Minimum_Degree(Gr, To_Links, Negative_Links))));
      end if;
      Put_Line(F_Out, "  (out)        :" & HTab & I2S(Integer(Minimum_Degree(Gr, From_Links))));
      if Signed then
        Put_Line(F_Out, "  (out)(+)     :" & HTab & I2S(Integer(Minimum_Degree(Gr, From_Links, Positive_Links))));
        Put_Line(F_Out, "  (out)(-)     :" & HTab & I2S(Integer(Minimum_Degree(Gr, From_Links, Negative_Links))));
      end if;
      Put_Line(F_Out, "Maximum degree :");
      Put_Line(F_Out, "  (in)         :" & HTab & I2S(Integer(Maximum_Degree(Gr, To_Links))));
      if Signed then
        Put_Line(F_Out, "  (in)(+)      :" & HTab & I2S(Integer(Maximum_Degree(Gr, To_Links, Positive_Links))));
        Put_Line(F_Out, "  (in)(-)      :" & HTab & I2S(Integer(Maximum_Degree(Gr, To_Links, Negative_Links))));
      end if;
      Put_Line(F_Out, "  (out)        :" & HTab & I2S(Integer(Maximum_Degree(Gr, From_Links))));
      if Signed then
        Put_Line(F_Out, "  (out)(+)     :" & HTab & I2S(Integer(Maximum_Degree(Gr, From_Links, Positive_Links))));
        Put_Line(F_Out, "  (out)(-)     :" & HTab & I2S(Integer(Maximum_Degree(Gr, From_Links, Negative_Links))));
      end if;
    end if;
    Put(".");
    New_Line(F_Out);
    -- Strength
    if Weighted then
      Put_Line(F_Out, "Total strength   :" & HTab & Num2S(Total_Strength(Gr), Ints_Graph, Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (+)            :" & HTab & Num2S(Total_Strength(Gr, Positive_Links), Ints_Graph, Aft));
        Put_Line(F_Out, "  (-)            :" & HTab & Num2S(Total_Strength(Gr, Negative_Links), Ints_Graph, Aft));
      end if;
      Put_Line(F_Out, "Average strength :" & HTab & Num2S(Average_Strength(Gr), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (+)            :" & HTab & Num2S(Average_Strength(Gr, Positive_Links), Aft => Aft));
        Put_Line(F_Out, "  (-)            :" & HTab & Num2S(Average_Strength(Gr, Negative_Links), Aft => Aft));
      end if;
      if not Directed then
        Put_Line(F_Out, "Minimum strength :" & HTab & Num2S(Minimum_Strength(Gr, To_Links), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (+)            :" & HTab & Num2S(Minimum_Strength(Gr, To_Links, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (-)            :" & HTab & Num2S(Minimum_Strength(Gr, To_Links, Negative_Links), Ints_Graph, Aft));
        end if;
        Put_Line(F_Out, "Maximum strength :" & HTab & Num2S(Maximum_Strength(Gr, To_Links), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (+)            :" & HTab & Num2S(Maximum_Strength(Gr, To_Links, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (-)            :" & HTab & Num2S(Maximum_Strength(Gr, To_Links, Negative_Links), Ints_Graph, Aft));
        end if;
      else
        Put_Line(F_Out, "Minimum strength :");
        Put_Line(F_Out, "  (in)           :" & HTab & Num2S(Minimum_Strength(Gr, To_Links), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (in)(+)        :" & HTab & Num2S(Minimum_Strength(Gr, To_Links, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (in)(-)        :" & HTab & Num2S(Minimum_Strength(Gr, To_Links, Negative_Links), Ints_Graph, Aft));
        end if;
        Put_Line(F_Out, "  (out)          :" & HTab & Num2S(Minimum_Strength(Gr, From_Links), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (out)(+)       :" & HTab & Num2S(Minimum_Strength(Gr, From_Links, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (out)(-)       :" & HTab & Num2S(Minimum_Strength(Gr, From_Links, Negative_Links), Ints_Graph, Aft));
        end if;
        Put_Line(F_Out, "Maximum strength :");
        Put_Line(F_Out, "  (in)           :" & HTab & Num2S(Maximum_Strength(Gr, To_Links), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (in)(+)        :" & HTab & Num2S(Maximum_Strength(Gr, To_Links, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (in)(-)        :" & HTab & Num2S(Maximum_Strength(Gr, To_Links, Negative_Links), Ints_Graph, Aft));
        end if;
        Put_Line(F_Out, "  (out)          :" & HTab & Num2S(Maximum_Strength(Gr, From_Links), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (out)(+)       :" & HTab & Num2S(Maximum_Strength(Gr, From_Links, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (out)(-)       :" & HTab & Num2S(Maximum_Strength(Gr, From_Links, Negative_Links), Ints_Graph, Aft));
        end if;
      end if;
      Put(".");
      New_Line(F_Out);
      if Nsl > 0 then
        Put_Line(F_Out, "Total self-loops strength :" & HTab & Num2S(Total_Self_Loops_Strength(Gr), Ints_Graph, Aft));
        if Signed then
          Put_Line(F_Out, "  (+)                     :" & HTab & Num2S(Total_Self_Loops_Strength(Gr, Positive_Links), Ints_Graph, Aft));
          Put_Line(F_Out, "  (-)                     :" & HTab & Num2S(Total_Self_Loops_Strength(Gr, Negative_Links), Ints_Graph, Aft));
        end if;
        Put(".");
        New_Line(F_Out);
      end if;
    end if;
    -- Value
    if Weighted then
      Put_Line(F_Out, "Minimum value   :" & HTab & Num2S(Minimum_Value(Gr), Ints_Graph, Aft));
      if Signed then
        Put_Line(F_Out, "  (+)           :" & HTab & Num2S(Minimum_Value(Gr, Positive_Links), Ints_Graph, Aft));
        Put_Line(F_Out, "  (-)           :" & HTab & Num2S(Minimum_Value(Gr, Negative_Links), Ints_Graph, Aft));
      end if;
      Put_Line(F_Out, "Maximum value   :" & HTab & Num2S(Maximum_Value(Gr), Ints_Graph, Aft));
      if Signed then
        Put_Line(F_Out, "  (+)           :" & HTab & Num2S(Maximum_Value(Gr, Positive_Links), Ints_Graph, Aft));
        Put_Line(F_Out, "  (-)           :" & HTab & Num2S(Maximum_Value(Gr, Negative_Links), Ints_Graph, Aft));
      end if;
      New_Line(F_Out);
    end if;
    Close(F_Out);
    -- Asymmetry and Reciprocity
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    Put_Line(F_Out, "Asymmetry   :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => False, Weighted => False), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)       :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => False, Weighted => False, Ls => Positive_Links), Aft => Aft));
      Put_Line(F_Out, "  (-)       :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => False, Weighted => False, Ls => Negative_Links), Aft => Aft));
    end if;
    if Weighted then
      Put_Line(F_Out, "  (wh)      :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => False, Weighted => True), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)   :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => False, Weighted => True, Ls => Positive_Links), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)   :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => False, Weighted => True, Ls => Negative_Links), Aft => Aft));
      end if;
    end if;
    Put_Line(F_Out, "Reciprocity :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => False, Weighted => False), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)       :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => False, Weighted => False, Ls => Positive_Links), Aft => Aft));
      Put_Line(F_Out, "  (-)       :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => False, Weighted => False, Ls => Negative_Links), Aft => Aft));
    end if;
    if Weighted then
      Put_Line(F_Out, "  (wh)      :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => False, Weighted => True), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)   :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => False, Weighted => True, Ls => Positive_Links), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)   :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => False, Weighted => True, Ls => Negative_Links), Aft => Aft));
      end if;
    end if;
    if Nsl > 0 then
      Put_Line(F_Out, "Asymmetry ignoring self-loops   :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => True, Weighted => False), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (+)                           :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => True, Weighted => False, Ls => Positive_Links), Aft => Aft));
        Put_Line(F_Out, "  (-)                           :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => True, Weighted => False, Ls => Negative_Links), Aft => Aft));
      end if;
      if Weighted then
        Put_Line(F_Out, "  (wh)                          :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => True, Weighted => True), Aft => Aft));
        if Signed then
          Put_Line(F_Out, "  (wh)(+)                       :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => True, Weighted => True, Ls => Positive_Links), Aft => Aft));
          Put_Line(F_Out, "  (wh)(-)                       :" & HTab & Num2S(Asymmetry(Gr, Ignore_Self_Loops => True, Weighted => True, Ls => Negative_Links), Aft => Aft));
        end if;
      end if;
      Put_Line(F_Out, "Reciprocity ignoring self-loops :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => True, Weighted => False), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (+)                           :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => True, Weighted => False, Ls => Positive_Links), Aft => Aft));
        Put_Line(F_Out, "  (-)                           :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => True, Weighted => False, Ls => Negative_Links), Aft => Aft));
      end if;
      if Weighted then
        Put_Line(F_Out, "  (wh)                          :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => True, Weighted => True), Aft => Aft));
        if Signed then
          Put_Line(F_Out, "  (wh)(+)                       :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => True, Weighted => True, Ls => Positive_Links), Aft => Aft));
          Put_Line(F_Out, "  (wh)(-)                       :" & HTab & Num2S(Reciprocity(Gr, Ignore_Self_Loops => True, Weighted => True, Ls => Negative_Links), Aft => Aft));
        end if;
      end if;
    end if;
    Put(".");
    New_Line(F_Out);
    Close(F_Out);
    -- Entropy
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    if Signed then
      if (not Directed) then
        Put_Line(F_Out, "Average entropy :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => False, Normalized => False), Aft => Aft));
      else
        Put_Line(F_Out, "Average entropy :");
        Put_Line(F_Out, "  (in)          :" & HTab & Num2S(Average_Entropy(Gr, To_Links  , Weighted => False, Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (out)         :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => False, Normalized => False), Aft => Aft));
      end if;
    else
      if (not Directed) and (not Weighted) then
        Put_Line(F_Out, "Average entropy :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => False, Normalized => False), Aft => Aft));
      elsif (not Directed) and Weighted then
        Put_Line(F_Out, "Average entropy :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => False, Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (wh)          :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => True , Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (wh)(norm)    :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => True , Normalized => True ), Aft => Aft));
      elsif Directed and (not Weighted) then
        Put_Line(F_Out, "Average entropy :");
        Put_Line(F_Out, "  (in)          :" & HTab & Num2S(Average_Entropy(Gr, To_Links  , Weighted => False, Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (out)         :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => False, Normalized => False), Aft => Aft));
      elsif Directed and Weighted then
        Put_Line(F_Out, "Average entropy   :");
        Put_Line(F_Out, "  (in)            :" & HTab & Num2S(Average_Entropy(Gr, To_Links  , Weighted => False, Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (in)(wh)        :" & HTab & Num2S(Average_Entropy(Gr, To_Links  , Weighted => True , Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (in)(wh)(norm)  :" & HTab & Num2S(Average_Entropy(Gr, To_Links  , Weighted => True , Normalized => True ), Aft => Aft));
        Put_Line(F_Out, "  (out)           :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => False, Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (out)(wh)       :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => True , Normalized => False), Aft => Aft));
        Put_Line(F_Out, "  (out)(wh)(norm) :" & HTab & Num2S(Average_Entropy(Gr, From_Links, Weighted => True , Normalized => True ), Aft => Aft));
      end if;
    end if;
    Put(".");
    New_Line(F_Out);
    Close(F_Out);
    -- Assortativity
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    Put_Line(F_Out, "Assortativity (Pearson)  :" & HTab & Num2S(Assortativity(Gr, Weighted => False, Ct => Stats.Pearson), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)                    :" & HTab & Num2S(Assortativity(Gr, Weighted => False, Ls => Positive_Links, Ct => Stats.Pearson), Aft => Aft));
      Put_Line(F_Out, "  (-)                    :" & HTab & Num2S(Assortativity(Gr, Weighted => False, Ls => Negative_Links, Ct => Stats.Pearson), Aft => Aft));
    end if;
    if Weighted then
      Put_Line(F_Out, "  (wh)                   :" & HTab & Num2S(Assortativity(Gr, Weighted => True, Ct => Stats.Pearson), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)                :" & HTab & Num2S(Assortativity(Gr, Weighted => True, Ls => Positive_Links, Ct => Stats.Pearson), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)                :" & HTab & Num2S(Assortativity(Gr, Weighted => True, Ls => Negative_Links, Ct => Stats.Pearson), Aft => Aft));
      end if;
    end if;
    Put_Line(F_Out, "Assortativity (Spearman) :" & HTab & Num2S(Assortativity(Gr, Weighted => False, Ct => Stats.Spearman), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)                    :" & HTab & Num2S(Assortativity(Gr, Weighted => False, Ls => Positive_Links, Ct => Stats.Spearman), Aft => Aft));
      Put_Line(F_Out, "  (-)                    :" & HTab & Num2S(Assortativity(Gr, Weighted => False, Ls => Negative_Links, Ct => Stats.Spearman), Aft => Aft));
    end if;
    if Weighted then
      Put_Line(F_Out, "  (wh)                   :" & HTab & Num2S(Assortativity(Gr, Weighted => True, Ct => Stats.Spearman), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)                :" & HTab & Num2S(Assortativity(Gr, Weighted => True, Ls => Positive_Links, Ct => Stats.Spearman), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)                :" & HTab & Num2S(Assortativity(Gr, Weighted => True, Ls => Negative_Links, Ct => Stats.Spearman), Aft => Aft));
      end if;
    end if;
    Put(".");
    Close(F_Out);
    -- Assortativity error
    if Fast then
      Cet := Stats.Fisher_Transform;
    else
      Cet := Stats.Auto;
    end if;
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    Start(Chrono, 30.0, ".");
    Put_Line(F_Out, "Assortativity Error (Pearson)  :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => False, Ct => Stats.Pearson, Cet => Cet), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)                          :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => False, Ls => Positive_Links, Ct => Stats.Pearson, Cet => Cet), Aft => Aft));
      Put_Line(F_Out, "  (-)                          :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => False, Ls => Negative_Links, Ct => Stats.Pearson, Cet => Cet), Aft => Aft));
    end if;
    Close(F_Out);
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    if Weighted then
      Put_Line(F_Out, "  (wh)                         :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => True, Ct => Stats.Pearson, Cet => Cet), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)                      :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => True, Ls => Positive_Links, Ct => Stats.Pearson, Cet => Cet), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)                      :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => True, Ls => Negative_Links, Ct => Stats.Pearson, Cet => Cet), Aft => Aft));
      end if;
    end if;
    Close(F_Out);
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    Put_Line(F_Out, "Assortativity Error (Spearman) :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => False, Ct => Stats.Spearman, Cet => Cet), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)                          :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => False, Ls => Positive_Links, Ct => Stats.Spearman, Cet => Cet), Aft => Aft));
      Put_Line(F_Out, "  (-)                          :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => False, Ls => Negative_Links, Ct => Stats.Spearman, Cet => Cet), Aft => Aft));
    end if;
    Close(F_Out);
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    if Weighted then
      Put_Line(F_Out, "  (wh)                         :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => True, Ct => Stats.Spearman, Cet => Cet), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)                      :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => True, Ls => Positive_Links, Ct => Stats.Spearman, Cet => Cet), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)                      :" & HTab & Num2S(Assortativity_Error(Gr, Weighted => True, Ls => Negative_Links, Ct => Stats.Spearman, Cet => Cet), Aft => Aft));
      end if;
    end if;
    Stop(Chrono);
    New_Line(F_Out);
    Close(F_Out);
    -- Clustering
    Open(F_Out, Append_File, U2S(Fn_Out_Global));
    Put_Line(F_Out, "Average clustering coefficient :" & HTab & Num2S(Average_Clustering_Coefficient(Gr, Weighted => False), Aft => Aft));
    if Signed then
      Put_Line(F_Out, "  (+)                          :" & HTab & Num2S(Average_Clustering_Coefficient(Gr, Weighted => False, Ls => Positive_Links), Aft => Aft));
      Put_Line(F_Out, "  (-)                          :" & HTab & Num2S(Average_Clustering_Coefficient(Gr, Weighted => False, Ls => Negative_Links), Aft => Aft));
    end if;
    if Weighted then
      Put_Line(F_Out, "  (wh)                         :" & HTab & Num2S(Average_Clustering_Coefficient(Gr, Weighted => True), Aft => Aft));
      if Signed then
        Put_Line(F_Out, "  (wh)(+)                      :" & HTab & Num2S(Average_Clustering_Coefficient(Gr, Weighted => True, Ls => Positive_Links), Aft => Aft));
        Put_Line(F_Out, "  (wh)(-)                      :" & HTab & Num2S(Average_Clustering_Coefficient(Gr, Weighted => True, Ls => Negative_Links), Aft => Aft));
      end if;
    end if;
    Put(".");
    New_Line(F_Out);
    Close(F_Out);
    -- Path length
    if not Fast and not Signed then
      Open(F_Out, Append_File, U2S(Fn_Out_Global));
      Start(Chrono, 30.0, ".");
      if Connected_Strong then
        Put_Line(F_Out, "Average path length :" & HTab & Num2S(Average_Path_Length(Gr, Weighted => False), Aft => Aft)); Put(".");
        if Weighted then
          Put_Line(F_Out, "  (wh)              :" & HTab & Num2S(Average_Path_Length(Gr, Weighted => True), Aft => Aft)); Put(".");
        end if;
        Put_Line(F_Out, "Diameter            :" & HTab & I2S(Integer(Diameter(Gr, Weighted => False)))); Put(".");
        if Weighted then
          Put_Line(F_Out, "  (wh)              :" & HTab & Num2S(Diameter(Gr, Weighted => True), Ints_Graph, Aft)); Put(".");
        end if;
      end if;
      Put_Line(F_Out, "Efficiency          :" & HTab & Num2S(Efficiency(Gr, Weighted => False), Aft => Aft)); Put(".");
      if Weighted then
        Put_Line(F_Out, "  (wh)              :" & HTab & Num2S(Efficiency(Gr, Weighted => True), Aft => Aft)); Put(".");
      end if;
      Stop(Chrono);
      New_Line(F_Out);
      Close(F_Out);
    end if;
    New_Line;
  end if;

  -- Nodes properties
  if Props_Selection(Nodes_Properties) then
    Put("  " & File_Name(U2S(Fn_Out_Nodes)) & ": ");
    Create(F_Out, Out_File, U2S(Fn_Out_Nodes));
    -- Header
    U := S2U("Index" & HTab & "Name" & HTab & "Info");
    if Nsl > 0 then
      U := U & S2U(HTab & "Self-loop");
    end if;
    if (not Directed) and (not Signed) then
      U := U & S2U(HTab & "Degree");
    elsif Directed and (not Signed) then
      U := U & S2U(HTab & "Degree_In" & HTab & "Degree_Out");
    elsif (not Directed) and Signed then
      U := U & S2U(HTab & "Degree" & HTab & "Degree_Pos" & HTab & "Degree_Neg");
    elsif Directed and Signed then
      U := U & S2U(HTab & "Degree_In"  & HTab & "Degree_In_Pos"  & HTab & "Degree_In_Neg");
      U := U & S2U(HTab & "Degree_Out" & HTab & "Degree_Out_Pos" & HTab & "Degree_Out_Neg");
    end if;
    if Weighted then
      if (not Directed) and (not Signed) then
        U := U & S2U(HTab & "Strength");
      elsif Directed and (not Signed) then
        U := U & S2U(HTab & "Strength_In" & HTab & "Strength_Out");
      elsif (not Directed) and Signed then
        U := U & S2U(HTab & "Strength" & HTab & "Strength_Pos" & HTab & "Strength_Neg");
      elsif Directed and Signed then
        U := U & S2U(HTab & "Strength_In"  & HTab & "Strength_In_Pos"  & HTab & "Strength_In_Neg");
        U := U & S2U(HTab & "Strength_Out" & HTab & "Strength_Out_Pos" & HTab & "Strength_Out_Neg");
      end if;
    end if;
    if Weighted then
      if (not Directed) and (not Signed) then
        U := U & S2U(HTab & "Min_Value");
      elsif Directed and (not Signed) then
        U := U & S2U(HTab & "Min_Value_In" & HTab & "Min_Value_Out");
      elsif (not Directed) and Signed then
        U := U & S2U(HTab & "Min_Value" & HTab & "Min_Value_Pos" & HTab & "Min_Value_Neg");
      elsif Directed and Signed then
        U := U & S2U(HTab & "Min_Value_In"  & HTab & "Min_Value_In_Pos"  & HTab & "Min_Value_In_Neg");
        U := U & S2U(HTab & "Min_Value_Out" & HTab & "Min_Value_Out_Pos" & HTab & "Min_Value_Out_Neg");
      end if;
      if (not Directed) and (not Signed) then
        U := U & S2U(HTab & "Max_Value");
      elsif Directed and (not Signed) then
        U := U & S2U(HTab & "Max_Value_In" & HTab & "Max_Value_Out");
      elsif (not Directed) and Signed then
        U := U & S2U(HTab & "Max_Value" & HTab & "Max_Value_Pos" & HTab & "Max_Value_Neg");
      elsif Directed and Signed then
        U := U & S2U(HTab & "Max_Value_In"  & HTab & "Max_Value_In_Pos"  & HTab & "Max_Value_In_Neg");
        U := U & S2U(HTab & "Max_Value_Out" & HTab & "Max_Value_Out_Pos" & HTab & "Max_Value_Out_Neg");
      end if;
      if (not Directed) and (not Signed) then
        U := U & S2U(HTab & "Average_Value");
      elsif Directed and (not Signed) then
        U := U & S2U(HTab & "Average_Value_In" & HTab & "Average_Value_Out");
      elsif (not Directed) and Signed then
        U := U & S2U(HTab & "Average_Value" & HTab & "Average_Value_Pos" & HTab & "Average_Value_Neg");
      elsif Directed and Signed then
        U := U & S2U(HTab & "Average_Value_In"  & HTab & "Average_Value_In_Pos"  & HTab & "Average_Value_In_Neg");
        U := U & S2U(HTab & "Average_Value_Out" & HTab & "Average_Value_Out_Pos" & HTab & "Average_Value_Out_Neg");
      end if;
    end if;
    if Signed then
      if (not Directed) then
        U := U & S2U(HTab & "Entropy");
      else
        U := U & S2U(HTab & "Entropy_In"  & HTab & "Entropy_Out");
      end if;
    else
      if (not Directed) and (not Weighted) then
        U := U & S2U(HTab & "Entropy");
      elsif (not Directed) and Weighted then
        U := U & S2U(HTab & "Entropy" & HTab & "Entropy_Weighted" & HTab & "Entropy_Weighted_Norm");
      elsif Directed and (not Weighted) then
        U := U & S2U(HTab & "Entropy_In"  & HTab & "Entropy_Out");
      elsif Directed and Weighted then
        U := U & S2U(HTab & "Entropy_In"  & HTab & "Entropy_In_Weighted"  & HTab & "Entropy_In_Weighted_Norm");
        U := U & S2U(HTab & "Entropy_Out" & HTab & "Entropy_Out_Weighted" & HTab & "Entropy_Out_Weighted_Norm");
      end if;
    end if;
    U := U & S2U(HTab & "Clustering");
    if Signed then
      U := U & S2U(HTab & "Clustering_Pos" & HTab & "Clustering_Neg");
    end if;
    if Weighted then
      U := U & S2U(HTab & "Clustering_Weighted");
      if Signed then
        U := U & S2U(HTab & "Clustering_Weighted_Pos" & HTab & "Clustering_Weighted_Neg");
      end if;
    end if;
    if not Fast then
      if Connected_Strong and not Signed then
        if (not Directed) and (not Weighted) then
          U := U & S2U(HTab & "Average_Path_Length");
        elsif (not Directed) and Weighted then
          U := U & S2U(HTab & "Average_Path_Length" & HTab & "Average_Path_Length_Weighted");
        elsif Directed and (not Weighted) then
          U := U & S2U(HTab & "Average_Path_Length_In" & HTab & "Average_Path_Length_Out");
        elsif Directed and Weighted then
          U := U & S2U(HTab & "Average_Path_Length_In" & HTab & "Average_Path_Length_Out");
          U := U & S2U(HTab & "Average_Path_Length_Weighted_In" & HTab & "Average_Path_Length_Weighted_Out");
        end if;
        if (not Directed) and (not Weighted) then
          U := U & S2U(HTab & "Maximum_Path_Length");
        elsif (not Directed) and Weighted then
          U := U & S2U(HTab & "Maximum_Path_Length" & HTab & "Maximum_Path_Length_Weighted");
        elsif Directed and (not Weighted) then
          U := U & S2U(HTab & "Maximum_Path_Length_In" & HTab & "Maximum_Path_Length_Out");
        elsif Directed and Weighted then
          U := U & S2U(HTab & "Maximum_Path_Length_In" & HTab & "Maximum_Path_Length_Out");
          U := U & S2U(HTab & "Maximum_Path_Length_Weighted_In" & HTab & "Maximum_Path_Length_Weighted_Out");
        end if;
      end if;
      if not Signed then
        if (not Directed) and (not Weighted) then
          U := U & S2U(HTab & "Efficiency");
        elsif (not Directed) and Weighted then
          U := U & S2U(HTab & "Efficiency" & HTab & "Efficiency_Weighted");
        elsif Directed and (not Weighted) then
          U := U & S2U(HTab & "Efficiency_In" & HTab & "Efficiency_Out");
        elsif Directed and Weighted then
          U := U & S2U(HTab & "Efficiency_In" & HTab & "Efficiency_Out");
          U := U & S2U(HTab & "Efficiency_Weighted_In" & HTab & "Efficiency_Weighted_Out");
        end if;
      end if;
      U := U & S2U(HTab & "Betweenness");
      if Weighted and not Signed then
        U := U & S2U(HTab & "Betweenness_Weighted");
      end if;
    end if;
    Put_Line(F_Out, U2S(U));
    -- Betweenness
    if not Fast then
      Start(Chrono, 30.0, ".");
      if Both_Bet then
        Betweenness(Gr, Bet, Gr_Bet, Weighted => False, Normalized => False); Put(".");
        if Weighted and not Signed then
          Betweenness(Gr, Bet_Wh, Gr_Bet_Wh, Weighted => True, Normalized => False); Put(".");
        end if;
      else
        Bet := Vertex_Betweenness(Gr, Weighted => False, Normalized => False); Put(".");
        if Weighted and not Signed then
          Bet_Wh := Vertex_Betweenness(Gr, Weighted => True, Normalized => False); Put(".");
        end if;
      end if;
      Stop(Chrono);
    end if;
    -- Nodes
    for I in 1..N loop
      V := Get_Vertex(Gr, I);
      U := S2U(I2S(I) & HTab & Get_Name(V) & HTab & Get_Tag(V));
      if N >= 1000 and then I mod 100 = 0 then
        Put(".");
      end if;
      -- Self-loop
      if Nsl > 0 then
        if not Weighted then
          if Has_Self_Loop(V) then
            U := U & S2U(HTab & "1");
          else
            U := U & S2U(HTab & "0");
          end if;
        else
          U := U & S2U(HTab & Num2S(Self_Loop(V), Ints_Graph, Aft));
        end if;
      end if;
      -- Degree
      U := U & S2U(HTab & I2S(Degree_To(V)));
      if Signed then
        U := U & S2U(HTab & I2S(Integer(Degree_To(V, Positive_Links))));
        U := U & S2U(HTab & I2S(Integer(Degree_To(V, Negative_Links))));
      end if;
      if Directed then
        U := U & S2U(HTab & I2S(Degree_From(V)));
        if Signed then
          U := U & S2U(HTab & I2S(Integer(Degree_From(V, Positive_Links))));
          U := U & S2U(HTab & I2S(Integer(Degree_From(V, Negative_Links))));
        end if;
      end if;
      -- Strength
      if Weighted then
        U := U & S2U(HTab & Num2S(Strength_To(V), Ints_Graph, Aft));
        if Signed then
          U := U & S2U(HTab & Num2S(Strength_To(V, Positive_Links), Ints_Graph, Aft));
          U := U & S2U(HTab & Num2S(Strength_To(V, Negative_Links), Ints_Graph, Aft));
        end if;
        if Directed then
          U := U & S2U(HTab & Num2S(Strength_From(V), Ints_Graph, Aft));
          if Signed then
            U := U & S2U(HTab & Num2S(Strength_From(V, Positive_Links), Ints_Graph, Aft));
            U := U & S2U(HTab & Num2S(Strength_From(V, Negative_Links), Ints_Graph, Aft));
          end if;
        end if;
      end if;
      -- Values
      if Weighted then
        Vst := Min_Value;
        U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst), Ints_Graph, Aft));
        if Signed then
          U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst, Positive_Links), Ints_Graph, Aft));
          U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst, Negative_Links), Ints_Graph, Aft));
        end if;
        if Directed then
          U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst), Ints_Graph, Aft));
          if Signed then
            U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst, Positive_Links), Ints_Graph, Aft));
            U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst, Negative_Links), Ints_Graph, Aft));
          end if;
        end if;
        Vst := Max_Value;
        U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst), Ints_Graph, Aft));
        if Signed then
          U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst, Positive_Links), Ints_Graph, Aft));
          U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst, Negative_Links), Ints_Graph, Aft));
        end if;
        if Directed then
          U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst), Ints_Graph, Aft));
          if Signed then
            U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst, Positive_Links), Ints_Graph, Aft));
            U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst, Negative_Links), Ints_Graph, Aft));
          end if;
        end if;
        Vst := Avg_Value;
        U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst), Aft => Aft));
        if Signed then
          U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst, Positive_Links), Aft => Aft));
          U := U & S2U(HTab & Num2S(Values_Statistic_To(V, Vst, Negative_Links), Aft => Aft));
        end if;
        if Directed then
          U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst), Aft => Aft));
          if Signed then
            U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst, Positive_Links), Aft => Aft));
            U := U & S2U(HTab & Num2S(Values_Statistic_From(V, Vst, Negative_Links), Aft => Aft));
          end if;
        end if;
      end if;
      -- Entropy
      if Signed then
        if (not Directed) then
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => False, Normalized => False), Aft => Aft));
        else
          U := U & S2U(HTab & Num2S(Entropy(V, To_Links  , Weighted => False, Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => False, Normalized => False), Aft => Aft));
        end if;
      else
        if (not Directed) and (not Weighted) then
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => False, Normalized => False), Aft => Aft));
        elsif (not Directed) and Weighted then
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => False, Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => True , Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => True , Normalized => True ), Aft => Aft));
        elsif Directed and (not Weighted) then
          U := U & S2U(HTab & Num2S(Entropy(V, To_Links  , Weighted => False, Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => False, Normalized => False), Aft => Aft));
        elsif Directed and Weighted then
          U := U & S2U(HTab & Num2S(Entropy(V, To_Links  , Weighted => False, Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, To_Links  , Weighted => True , Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, To_Links  , Weighted => True , Normalized => True ), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => False, Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => True , Normalized => False), Aft => Aft));
          U := U & S2U(HTab & Num2S(Entropy(V, From_Links, Weighted => True , Normalized => True ), Aft => Aft));
        end if;
      end if;
      -- Clustering
      U := U & S2U(HTab & Num2S(Clustering_Coefficient(V, Weighted => False), Aft => Aft));
      if Signed then
        U := U & S2U(HTab & Num2S(Clustering_Coefficient(V, Weighted => False, Ls => Positive_Links), Aft => Aft));
        U := U & S2U(HTab & Num2S(Clustering_Coefficient(V, Weighted => False, Ls => Negative_Links), Aft => Aft));
      end if;
      if Weighted then
        U := U & S2U(HTab & Num2S(Clustering_Coefficient(V, Weighted => True), Aft => Aft));
        if Signed then
          U := U & S2U(HTab & Num2S(Clustering_Coefficient(V, Weighted => True, Ls => Positive_Links), Aft => Aft));
          U := U & S2U(HTab & Num2S(Clustering_Coefficient(V, Weighted => True, Ls => Negative_Links), Aft => Aft));
        end if;
      end if;
      if not Fast then
        -- Path length
        if Connected_Strong and not Signed then
          if (not Directed) and (not Weighted) then
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => False), Aft => Aft));
          elsif (not Directed) and Weighted then
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => False), Aft => Aft));
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => True) , Aft => Aft));
          elsif Directed and (not Weighted) then
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => False, Ld => To_Links  ), Aft => Aft));
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => False, Ld => From_Links), Aft => Aft));
          elsif Directed and Weighted then
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => False, Ld => To_Links  ), Aft => Aft));
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => False, Ld => From_Links), Aft => Aft));
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => True , Ld => To_Links  ), Aft => Aft));
            U := U & S2U(HTab & Num2S(Average_Path_Length(V, Weighted => True , Ld => From_Links), Aft => Aft));
          end if;
          if (not Directed) and (not Weighted) then
            U := U & S2U(HTab & I2S(Integer(Maximum_Path_Length(V, Weighted => False))));
          elsif (not Directed) and Weighted then
            U := U & S2U(HTab & I2S(Integer(Maximum_Path_Length(V, Weighted => False))));
            U := U & S2U(HTab & Num2S(Maximum_Path_Length(V, Weighted => True), Ints_Graph, Aft));
          elsif Directed and (not Weighted) then
            U := U & S2U(HTab & I2S(Integer(Maximum_Path_Length(V, Weighted => False, Ld => To_Links  ))));
            U := U & S2U(HTab & I2S(Integer(Maximum_Path_Length(V, Weighted => False, Ld => From_Links))));
          elsif Directed and Weighted then
            U := U & S2U(HTab & I2S(Integer(Maximum_Path_Length(V, Weighted => False, Ld => To_Links  ))));
            U := U & S2U(HTab & I2S(Integer(Maximum_Path_Length(V, Weighted => False, Ld => From_Links))));
            U := U & S2U(HTab & Num2S(Maximum_Path_Length(V, Weighted => True , Ld => To_Links  ), Ints_Graph, Aft));
            U := U & S2U(HTab & Num2S(Maximum_Path_Length(V, Weighted => True , Ld => From_Links), Ints_Graph, Aft));
          end if;
        end if;
        if not Signed then
          if (not Directed) and (not Weighted) then
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => False), Aft => Aft));
          elsif (not Directed) and Weighted then
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => False), Aft => Aft));
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => True) , Aft => Aft));
          elsif Directed and (not Weighted) then
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => False, Ld => To_Links  ), Aft => Aft));
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => False, Ld => From_Links), Aft => Aft));
          elsif Directed and Weighted then
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => False, Ld => To_Links  ), Aft => Aft));
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => False, Ld => From_Links), Aft => Aft));
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => True , Ld => To_Links  ), Aft => Aft));
            U := U & S2U(HTab & Num2S(Efficiency(V, Weighted => True , Ld => From_Links), Aft => Aft));
          end if;
        end if;
        -- Betweenness
        U := U & S2U(HTab & Num2S(Bet(I), Aft => Aft));
        if Weighted and not Signed then
          U := U & S2U(HTab & Num2S(Bet_Wh(I), Aft => Aft));
        end if;
      end if;
      Put_Line(F_Out, U2S(U));
    end loop;
    Close(F_Out);
    if not Fast then
      Free(Bet);
      if Weighted and not Signed then
        Free(Bet_Wh);
      end if;
    end if;
    Put_Line(".");
  end if;

  -- Edges properties
  if not Fast and Props_Selection(Edges_Properties) then
    Put("  " & File_Name(U2S(Fn_Out_Edges)) & ": ");
    if not Both_Bet then
      Start(Chrono, 30.0, ".");
      Gr_Bet := Edge_Betweenness(Gr, Weighted => False, Normalized => False);
      Stop(Chrono);
    end if;
    Put_Graph(U2S(Fn_Out_Edges), Gr_Bet, Aft => Aft);
    Free(Gr_Bet);
    Put_Line(".");
    if Weighted and not Signed then
      Put("  " & File_Name(U2S(Fn_Out_Edges_Wh)) & ": ");
      if not Both_Bet then
        Start(Chrono, 30.0, ".");
        Gr_Bet_Wh := Edge_Betweenness(Gr, Weighted => True, Normalized => False);
        Stop(Chrono);
      end if;
      Put_Graph(U2S(Fn_Out_Edges_Wh), Gr_Bet_Wh, Aft => Aft);
      Free(Gr_Bet_Wh);
      Put_Line(".");
    end if;
  end if;

  -- Degrees properties
  if Props_Selection(Degrees_Properties) then
    Put("  " & File_Name(U2S(Fn_Out_Degrees)) & ": ");
    Create(F_Out, Out_File, U2S(Fn_Out_Degrees));
    if (not Directed) and (not Signed) then
      U := S2U("Degree" & Htab & "PDF" & Htab & "CCDF");
      Put_Line(F_Out, U2S(U));
      Din := Degree_To(Gr, All_Links);
      Deg_Max := Integer(Max(Din));
      Degree_Distribution(Din, Fin, Cin, Deg_Max);
      for K in Fin'Range loop
        U := S2U(I2S(K) & HTab & I2S(Fin(K)) & HTab & I2S(Cin(K)));
        Put_Line(F_Out, U2S(U));
      end loop;
      Free(Din); Free(Fin); Free(Cin);
    elsif Directed and (not Signed) then
      U := S2U("Degree" & Htab & "In_PDF" & Htab & "Out_PDF" & Htab & "In_CCDF" & Htab & "Out_CCDF");
      Put_Line(F_Out, U2S(U));
      Din  := Degree_To(Gr, All_Links);
      Dout := Degree_From(Gr, All_Links);
      Deg_Max := Integer(Max(Doubles'(Max(Din), Max(Dout))));
      Degree_Distribution(Din, Fin, Cin, Deg_Max);
      Degree_Distribution(Dout, Fout, Cout, Deg_Max);
      for K in Fin'Range loop
        U := S2U(I2S(K) & HTab & I2S(Fin(K)) & HTab & I2S(Fout(K)) & HTab & I2S(Cin(K)) & HTab & I2S(Cout(K)));
        Put_Line(F_Out, U2S(U));
      end loop;
      Free(Din); Free(Fin); Free(Cin);
      Free(Dout); Free(Fout); Free(Cout);
    elsif (not Directed) and Signed then
      U := S2U("Degree" & Htab & "PDF"  & Htab & "Pos_PDF"  & Htab & "Neg_PDF"
                        & HTab & "CCDF" & Htab & "Pos_CCDF" & Htab & "Neg_CCDF");
      Put_Line(F_Out, U2S(U));
      Din    := Degree_To(Gr, All_Links);
      Dinpos := Degree_To(Gr, Positive_Links);
      Dinneg := Degree_To(Gr, Negative_Links);
      Deg_Max := Integer(Max(Doubles'(Max(Din), Max(Dinpos), Max(Dinneg))));
      Degree_Distribution(Din, Fin, Cin, Deg_Max);
      Degree_Distribution(Dinpos, Finpos, Cinpos, Deg_Max);
      Degree_Distribution(Dinneg, Finneg, Cinneg, Deg_Max);
      for K in Fin'Range loop
        U := S2U(I2S(K) & HTab & I2S(Fin(K)) & HTab & I2S(Finpos(K)) & HTab & I2S(Finneg(K))
                        & HTab & I2S(Cin(K)) & HTab & I2S(Cinpos(K)) & HTab & I2S(Cinneg(K)));
        Put_Line(F_Out, U2S(U));
      end loop;
      Free(Din); Free(Fin); Free(Cin);
      Free(Dinpos); Free(Finpos); Free(Cinpos);
      Free(Dinneg); Free(Finneg); Free(Cinneg);
    elsif Directed and Signed then
      U := S2U("Degree" & Htab & "In_PDF"   & Htab & "In_Pos_PDF"   & Htab & "In_Neg_PDF"
                        & Htab & "Out_PDF"  & Htab & "Out_Pos_PDF"  & Htab & "Out_Neg_PDF"
                        & HTab & "In_CCDF"  & Htab & "In_Pos_CCDF"  & Htab & "In_Neg_CCDF"
                        & HTab & "Out_CCDF" & Htab & "Out_Pos_CCDF" & Htab & "Out_Neg_CCDF");
      Put_Line(F_Out, U2S(U));
      Din     := Degree_To(Gr, All_Links);
      Dinpos  := Degree_To(Gr, Positive_Links);
      Dinneg  := Degree_To(Gr, Negative_Links);
      Dout    := Degree_From(Gr, All_Links);
      Doutpos := Degree_From(Gr, Positive_Links);
      Doutneg := Degree_From(Gr, Negative_Links);
      Deg_Max := Integer(Max(Doubles'(Max(Din), Max(Dinpos), Max(Dinneg), Max(Dout), Max(Doutpos), Max(Doutneg))));
      Degree_Distribution(Din, Fin, Cin, Deg_Max);
      Degree_Distribution(Dinpos, Finpos, Cinpos, Deg_Max);
      Degree_Distribution(Dinneg, Finneg, Cinneg, Deg_Max);
      Degree_Distribution(Dout, Fout, Cout, Deg_Max);
      Degree_Distribution(Doutpos, Foutpos, Coutpos, Deg_Max);
      Degree_Distribution(Doutneg, Foutneg, Coutneg, Deg_Max);
      for K in Fin'Range loop
        U := S2U(I2S(K) & HTab & I2S(Fin(K))  & HTab & I2S(Finpos(K))  & HTab & I2S(Finneg(K))
                        & HTab & I2S(Fout(K)) & HTab & I2S(Foutpos(K)) & HTab & I2S(Foutneg(K))
                        & HTab & I2S(Cin(K))  & HTab & I2S(Cinpos(K))  & HTab & I2S(Cinneg(K))
                        & HTab & I2S(Cout(K)) & HTab & I2S(Coutpos(K)) & HTab & I2S(Coutneg(K)));
        Put_Line(F_Out, U2S(U));
      end loop;
      Free(Din);
      Free(Dinpos);
      Free(Dinneg);
      Free(Dout);
      Free(Doutpos);
      Free(Doutneg);
    end if;
    Close(F_Out);
    Put_Line(".");
  end if;

  -- Distances properties
  if not Fast and Props_Selection(Distances_Properties) then
    Put("  " & File_Name(U2S(Fn_Out_Dists)) & ": ");
    Start(Chrono, 30.0, ".");
    Create(F_Out, Out_File, U2S(Fn_Out_Dists));
    for I in 1..N loop
      V := Get_Vertex(Gr, I);
      Dout := Shortest_Path_Length(V, Allow_Infinite_Distances => True, Weighted => False, Ld => From_Links);
      for J in 1..N loop
        if J > 1 then
          Put(F_Out, HTab);
        end if;
        if Dout(J) = Plus_Infinity then
          Put(F_Out, "+Inf");
        else
          Put(F_Out, I2S(Integer(Dout(J))));
        end if;
      end loop;
      New_Line(F_Out);
      Free(Dout);
    end loop;
    Close(F_Out);
    Stop(Chrono);
    Put_Line(".");
    if Weighted and not Signed then
      Put("  " & File_Name(U2S(Fn_Out_Dists_Wh)) & ": ");
      Start(Chrono, 30.0, ".");
      Create(F_Out, Out_File, U2S(Fn_Out_Dists_Wh));
      for I in 1..N loop
        V := Get_Vertex(Gr, I);
        Dout := Shortest_Path_Length(V, Allow_Infinite_Distances => True, Weighted => True, Ld => From_Links);
        for J in 1..N loop
          if J > 1 then
            Put(F_Out, HTab);
          end if;
          if Dout(J) = Plus_Infinity then
            Put(F_Out, "+Inf");
          else
            Put(F_Out, Num2S(Dout(J), Ints_Graph, Aft));
          end if;
        end loop;
        New_Line(F_Out);
        Free(Dout);
      end loop;
      Close(F_Out);
      Stop(Chrono);
      Put_Line(".");
    end if;
  end if;

  Free(Gr);
  New_Line;

end Network_Properties;
