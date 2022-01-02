-- Radalib, Copyright (c) 2022 by
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


-- @filename Links_Info.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 19/03/2010
-- @revision 06/03/2018
-- @brief Obtain Links Information

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Random_Numbers; use Random_Numbers;
with Graphs_Float; use Graphs_Float;
with Graphs_Float_Properties; use Graphs_Float_Properties;
with Pajek_IO; use Pajek_IO;
with Utils; use Utils;

procedure Links_Info is

  procedure Put_Info is
  begin
    New_Line(2);
    Put_Line("===================================================================");
    Put_Line("== Radalib, Copyright (c) 2022 by                                ==");
    Put_Line("==   Sergio Gomez             (sergio.gomez@urv.cat)             ==");
    Put_Line("==   Alberto Fernandez        (alberto.fernandez@urv.cat)        ==");
    Put_Line("== See LICENSE.txt                                               ==");
    Put_Line("===================================================================");
    Put_Line("== Obtain degrees and strengths of nodes attached to each link   ==");
    Put_Line("===================================================================");
    New_Line(2);
  end Put_Info;

  Max_Random_Links: constant Natural := 1_000_000;
  Default_Num_Random_Links: constant Natural := 0;

  procedure Select_Random_Links(Num_Random_Links: in out Natural; Num_Links: in Natural; Selected_Links: out PIntegers) is
    G: Generator;
    This_Link: PBooleans;
    I_Random: Positive;
  begin
    if Num_Random_Links >= Num_Links or Num_Random_Links > Max_Random_Links then
      Num_Random_Links := 0;
      Selected_Links := null;
    end if;
    if Num_Random_Links /= 0 then
      This_Link := Alloc(1, Num_Links);
      Reset(G);
      if Num_Random_Links <= Num_Links / 2 then
        This_Link.all := (others => False);
        for I in 1..Num_Random_Links loop
          I_Random := Random_Uniform(G, 1, Num_Links);
          while This_Link(I_Random) loop
            I_Random := Random_Uniform(G, 1, Num_Links);
          end loop;
          This_Link(I_Random) := True;
        end loop;
      else
        This_Link.all := (others => True);
        for I in 1..(Num_Links - Num_Random_Links) loop
          I_Random := Random_Uniform(G, 1, Num_Links);
          while not This_Link(I_Random) loop
            I_Random := Random_Uniform(G, 1, Num_Links);
          end loop;
          This_Link(I_Random) := False;
        end loop;
      end if;
      Selected_Links := Alloc(1, Num_Random_Links);
      I_Random := 1;
      for I in Selected_Links'Range loop
        while not This_Link(I_Random) loop
          I_Random :=  I_Random + 1;
        end loop;
        Selected_Links(I) := I_Random;
        I_Random :=  I_Random + 1;
      end loop;
      Free(This_Link);
    end if;
  end Select_Random_Links;

  Fn_Net: UString;
  Fn_Out: UString;
  Aft: Field;
  Num_Random_Links: Natural;
  Gr: Graph;
  Directed: Boolean;
  N, Ne: Natural;
  Kf, Kt, Sf, St: PFloats;
  Selected_Links: PIntegers;
  Ft: File_Type;
  El: Edges_List;
  E: Edge;
  J: Positive;
  Wh: Float;
  I_Selected, I_Link: Natural;

begin
  Put_Info;

  pragma Warnings(Off, El);

  if Argument_Count = 2 then
    Fn_Net := S2U(Argument(1));
    Num_Random_Links := Default_Num_Random_Links;
    Fn_Out := S2U(Argument(2));
    Aft := Default_Float_Aft;
  elsif Argument_Count = 3 then
    Fn_Net := S2U(Argument(1));
    if Is_Integer(Argument(2)) then
      Num_Random_Links := S2I(Argument(2));
      Fn_Out := S2U(Argument(3));
      Aft := Default_Float_Aft;
    else
      Num_Random_Links := Default_Num_Random_Links;
      Fn_Out := S2U(Argument(2));
      Aft := S2I(Argument(3));
    end if;
  elsif Argument_Count = 4 then
    Fn_Net := S2U(Argument(1));
    Num_Random_Links := S2I(Argument(2));
    Fn_Out := S2U(Argument(3));
    Aft := S2I(Argument(4));
  else
    Put_Line("Usage:  " & Command_Name & "  net_name  [ num_random_links ]  links_info_name  [ decimal_digits ]");
    New_Line;
    Put_Line("   net_name         :  name of the network file in Pajek format");
    New_Line;
    Put_Line("   num_random_links :  number of random links in output info file");
    Put_Line("                         0 => all links");
    Put_Line("                         num_random_links >= num_links => all links");
    Put_Line("                         num_random_links > " & I2S(Max_Random_Links) & " => all links");
    Put_Line("                         default => " & I2S(Default_Num_Random_Links));
    New_Line;
    Put_Line("   links_info_name  :  name of the file with the info of links");
    New_Line;
    Put_Line("   decimal_digits   :  number of decimal digits for float values");
    Put_Line("                         default => " & I2S(Default_Float_Aft));
    return;
  end if;

  Put_Line(U2S(Fn_Net) & " -> " & U2S(Fn_Out));
  Get_Graph(U2S(Fn_Net), Gr);
  Directed := Is_Directed(Gr);
  N := Number_Of_Vertices(Gr);
  Ne := Number_Of_Edges(Gr);
  Kf := Degree_From(Gr);
  Kt := Degree_To(Gr);
  Sf := Strength_From(Gr);
  St := Strength_To(Gr);

  Select_Random_Links(Num_Random_Links, Ne, Selected_Links);
  if Num_Random_Links = 0 then
    Put_Line("  all " & I2S(Ne) &" links");
  else
    Put_Line("  " & I2S(Num_Random_Links) & " random links");
  end if;

  I_Link := 0;
  I_Selected := 1;

  Create(Ft, Out_File, U2S(Fn_Out));
  Put_Line(Ft, "From" & HTab & "To" & HTab & "Weight" & HTab & "K_From" & HTab & "K_To" & HTab & "S_From" & HTab & "S_To");
  for I in 1..N loop
    El := Edges_From(Get_Vertex(Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      E := Next(El);
      J := Index_Of(To(E));
      if Directed or else I <= J then
        I_Link := I_Link + 1;
        if Num_Random_Links = 0 or (I_Selected <= Num_Random_Links and then I_Link >= Selected_Links(I_Selected)) then
          I_Selected := I_Selected + 1;
          Wh := Value(E);
          Put_Line(Ft, I2S(I) & HTab & I2S(J) & HTab & F2Se0(Wh, Aft => Aft)
                       & HTab & I2S(F2I(Kf(I))) & HTab & I2S(F2I(Kt(J)))
                       & HTab & F2Se0(Sf(I), Aft => Aft) & HTab & F2Se0(St(J), Aft => Aft));
        end if;
      end if;
    end loop;
    Restore(El);
  end loop;
  Close(Ft);

  Free(Selected_Links);
  Free(Kf);
  Free(Kt);
  Free(Sf);
  Free(St);
  Free(Gr);
end Links_Info;
