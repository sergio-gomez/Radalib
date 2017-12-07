-- Radalib, Copyright (c) 2017 by
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


-- @filename Graphs-Modularities.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 5/03/2006
-- @revision 23/09/2015
-- @brief Calculation of Modularities of Graphs

with Ada.Unchecked_Deallocation;
with Utils; use Utils;
with Graphs_Double; use Graphs_Double;

package body Graphs.Modularities is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Vertex_Info_Recs, PVertex_Info_Recs);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Modularity_Recs, PModularity_Recs);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Modularity_Info_Rec, Modularity_Info);

  ------------------------
  -- To_Modularity_Type --
  ------------------------

  function To_Modularity_Type(Mt_Name: in String) return Modularity_Type is
  begin
    if    To_Uppercase(Mt_Name) = "UN"   or To_Lowercase(Mt_Name) = "unweighted_newman"                  then
      return Unweighted_Newman;
    elsif To_Uppercase(Mt_Name) = "UUN"  or To_Lowercase(Mt_Name) = "unweighted_uniform_nullcase"        then
      return Unweighted_Uniform_Nullcase;
    elsif To_Uppercase(Mt_Name) = "WN"   or To_Lowercase(Mt_Name) = "weighted_newman"                    then
      return Weighted_Newman;
    elsif To_Uppercase(Mt_Name) = "WS"   or To_Lowercase(Mt_Name) = "weighted_signed"                    then
      return Weighted_Signed;
    elsif To_Uppercase(Mt_Name) = "WUN"  or To_Lowercase(Mt_Name) = "weighted_uniform_nullcase"          then
      return Weighted_Uniform_Nullcase;
    elsif To_Uppercase(Mt_Name) = "WLA"  or To_Lowercase(Mt_Name) = "weighted_local_average"             then
      return Weighted_Local_Average;
    elsif To_Uppercase(Mt_Name) = "WULA" or To_Lowercase(Mt_Name) = "weighted_uniform_local_average"     then
      return Weighted_Uniform_Local_Average;
    elsif To_Uppercase(Mt_Name) = "WLUN" or To_Lowercase(Mt_Name) = "weighted_links_unweighted_Nullcase" then
      return Weighted_Links_Unweighted_Nullcase;
    elsif To_Uppercase(Mt_Name) = "WNN"  or To_Lowercase(Mt_Name) = "weighted_no_nullcase"               then
      return Weighted_No_Nullcase;
    elsif To_Uppercase(Mt_Name) = "WLR"  or To_Lowercase(Mt_Name) = "weighted_link_rank"                 then
      return Weighted_Link_Rank;
    else
      raise Unknown_Modularity_Error;
    end if;
  end To_Modularity_Type;

  -------------
  -- To_Name --
  -------------

  function To_Name(Mt: in Modularity_Type; Short: in Boolean := False) return String is
  begin
    if Short then
      case Mt is
        when Unweighted_Newman                  => return "UN";
        when Unweighted_Uniform_Nullcase        => return "UUN";
        when Weighted_Newman                    => return "WN";
        when Weighted_Signed                    => return "WS";
        when Weighted_Uniform_Nullcase          => return "WUN";
        when Weighted_Local_Average             => return "WLA";
        when Weighted_Uniform_Local_Average     => return "WULA";
        when Weighted_Links_Unweighted_Nullcase => return "WLUN";
        when Weighted_No_Nullcase               => return "WNN";
        when Weighted_Link_Rank                 => return "WLR";
      end case;
    else
      return Capitalize(Modularity_Type'Image(Mt));
    end if;
  end To_Name;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize(Mi: out Modularity_Info; Gr: in Graph; Mt: in Modularity_Type := Weighted_Signed; R: in Num := No_Resistance; Pc: in Num := 1.0) is
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Mi := new Modularity_Info_Rec;
    Mi.Gr := Gr;
    Mi.Size := Number_Of_Vertices(Gr);
    Mi.Directed := Is_Directed(Gr);
    Mi.Signed := Has_Links(Gr, Negative_Links);
    Mi.From := new Vertex_Info_Recs(1..Mi.Size);
    if Mi.Directed then
      Mi.To := new Vertex_Info_Recs(1..Mi.Size);
    else
      Mi.To := Mi.From;
    end if;
    Mi.Lower_Q := new Modularity_Recs(1..Mi.Size);

    Mi.Resistance := R;
    Mi.Penalty_Coefficient := Pc;
    Update_Graph(Mi, Mt);
  end Initialize;

  --------------------
  -- Set_Resistance --
  --------------------

  procedure Set_Resistance(Mi: in Modularity_Info; R: in Num := No_Resistance; Mt: in Modularity_Type := Weighted_Signed) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Mi.Resistance := R;
    Update_Graph(Mi, Mt);
  end Set_Resistance;

  --------------------
  -- Get_Resistance --
  --------------------

  function Get_Resistance(Mi: in Modularity_Info) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Resistance;
  end Get_Resistance;

  -----------------------------
  -- Set_Penalty_Coefficient --
  -----------------------------

  procedure Set_Penalty_Coefficient(Mi: in Modularity_Info; Pc: in Num := 1.0) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Mi.Penalty_Coefficient := Pc;
  end Set_Penalty_Coefficient;

  -----------------------------
  -- Get_Penalty_Coefficient --
  -----------------------------

  function Get_Penalty_Coefficient(Mi: in Modularity_Info) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Penalty_Coefficient;
  end Get_Penalty_Coefficient;

  ------------------
  -- Update_Graph --
  ------------------

  procedure Update_Graph(Mi: in Modularity_Info; Mt: in Modularity_Type := Weighted_Signed) is
    Vt, Vf: Vertex;
    Ki, Kj, Tk, Tsln: Natural;
    Wii, Wi, Wj, Wla, Tw, Twp, Twn, Tla, Tula, Tsl: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    for I in Mi.From'Range loop
      Vf := Get_Vertex(Mi.Gr, I);
      Mi.From(I).K := Degree_From(Vf);
      Mi.From(I).W := Strength_From(Vf);
      Mi.From(I).W_Pos := Strength_From(Vf, Positive_Links);
      Mi.From(I).W_Neg := Strength_From(Vf, Negative_Links);
      Mi.From(I).Has_Self_Loop := Has_Self_Loop(Vf);
      Mi.From(I).Self_Loop := Self_Loop(Vf);
      if Mi.Resistance /= No_Resistance then
        if not Mi.From(I).Has_Self_Loop then
          Mi.From(I).K := Mi.From(I).K + 1;
        end if;
        Mi.From(I).W := Mi.From(I).W + Mi.Resistance;
        Wii := Mi.From(I).Self_Loop + Mi.Resistance;
        if Mi.From(I).Self_Loop >= 0.0 then
          if Wii >= 0.0 then
            Mi.From(I).W_Pos := Mi.From(I).W_Pos + Mi.Resistance;
          else
            Mi.From(I).W_Pos := Mi.From(I).W_Pos - Mi.From(I).Self_Loop;
            Mi.From(I).W_Neg := Mi.From(I).W_Neg - Wii;
          end if;
        else
          if Wii <= 0.0 then
            Mi.From(I).W_Neg := Mi.From(I).W_Neg - Mi.Resistance;
          else
            Mi.From(I).W_Neg := Mi.From(I).W_Neg + Mi.From(I).Self_Loop;
            Mi.From(I).W_Pos := Mi.From(I).W_Pos + Wii;
          end if;
        end if;
        Mi.From(I).Self_Loop := Wii;
      end if;
    end loop;
    if Mi.Directed then
      for J in Mi.To'Range loop
        Vt := Get_Vertex(Mi.Gr, J);
        Mi.To(J).K := Degree_To(Vt);
        Mi.To(J).W := Strength_To(Vt);
        Mi.To(J).W_Pos := Strength_To(Vt, Positive_Links);
        Mi.To(J).W_Neg := Strength_To(Vt, Negative_Links);
        Mi.To(J).Has_Self_Loop := Has_Self_Loop(Vt);
        Mi.To(J).Self_Loop := Self_Loop(Vt);
        if Mi.Resistance /= No_Resistance then
          if not Mi.To(J).Has_Self_Loop then
            Mi.To(J).K := Mi.To(J).K + 1;
          end if;
          Mi.To(J).W := Mi.To(J).W + Mi.Resistance;
          Wii := Mi.To(J).Self_Loop + Mi.Resistance;
          if Mi.To(J).Self_Loop >= 0.0 then
            if Wii >= 0.0 then
              Mi.To(J).W_Pos := Mi.To(J).W_Pos + Mi.Resistance;
            else
              Mi.To(J).W_Pos := Mi.To(J).W_Pos - Mi.To(J).Self_Loop;
              Mi.To(J).W_Neg := Mi.To(J).W_Neg - Wii;
            end if;
          else
            if Wii <= 0.0 then
              Mi.To(J).W_Neg := Mi.To(J).W_Neg - Mi.Resistance;
            else
              Mi.To(J).W_Neg := Mi.To(J).W_Neg + Mi.To(J).Self_Loop;
              Mi.To(J).W_Pos := Mi.To(J).W_Pos + Wii;
            end if;
          end if;
          Mi.To(J).Self_Loop := Wii;
        end if;
      end loop;
    end if;
    Tk := 0;
    Tw := 0.0;
    Twp := 0.0;
    Twn := 0.0;
    Tla := 0.0;
    Tula := 0.0;
    Tsl := 0.0;
    Tsln := 0;
    for I in Mi.From'Range loop
      Ki := Mi.From(I).K;
      Wi := Mi.From(I).W;
      Tk := Tk + Ki;
      Tw := Tw + Wi;
      Twp := Twp + Mi.From(I).W_Pos;
      Twn := Twn + Mi.From(I).W_Neg;
      Tsl := Tsl + Mi.From(I).Self_Loop;
      if Mi.From(I).Has_Self_Loop or Mi.Resistance /= No_Resistance then
        Tsln := Tsln + 1;
      end if;
      for J in Mi.To'Range loop
        Kj := Mi.To(J).K;
        Wj := Mi.To(J).W;
        if Ki + Kj > 0 then
          Wla := (Wi + Wj) / Num(Ki + Kj);
          Tla := Tla + Num(Ki * Kj) * Wla;
          Tula := Tula + Wla;
        end if;
      end loop;
    end loop;
    Mi.Two_M := Tk;
    Mi.Two_W := Tw;
    Mi.Two_W_Pos := Twp;
    Mi.Two_W_Neg := Twn;
    Mi.Two_La := Tla;
    Mi.Two_Ula := Tula;
    Mi.Self_Loops := Tsl;
    Mi.Self_Loops_N := Tsln;

    Special_Initializations(Mi, Mt);
  end Update_Graph;

  -----------------------------
  -- Special_Initializations --
  -----------------------------

  procedure Special_Initializations(Mi: in Modularity_Info; Mt: in Modularity_Type) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    case Mt is
      when Weighted_Link_Rank =>
        if Mi.Two_W_Neg > 0.0 then
          raise Incompatible_Modularity_Type_Error with "Weighted_Link_Rank cannot handle negative weights";
        end if;
        if Mi.Eigenvec /= null then
          Free(Mi.Gr_Trans);
          Free(Mi.Eigenvec);
        end if;
        Transitions_Graph(Mi);
        Left_Leading_Eigenvector(Mi.Gr_Trans, Mi.Eigenvec);
      when others =>
        null;
    end case;
  end Special_Initializations;

  ----------
  -- Free --
  ----------

  procedure Free(Mi: in out Modularity_Info) is
  begin
    if Mi /= null then
      Dispose(Mi.From);
      if Mi.Directed then
        Dispose(Mi.To);
      end if;
      Dispose(Mi.Lower_Q);
      if Mi.Eigenvec /= null then
        Free(Mi.Gr_Trans);
        Free(Mi.Eigenvec);
      end if;
      Dispose(Mi);
      Mi := null;
    end if;
  end Free;

  -----------
  -- Clone --
  -----------

  function Clone(Mi: in Modularity_Info) return Modularity_Info is
    Mi_Clone: Modularity_Info;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Mi_Clone := new Modularity_Info_Rec;
    Mi_Clone.all := Mi.all;
    Mi_Clone.From := new Vertex_Info_Recs(1..Mi.Size);
    Mi_Clone.From.all := Mi.From.all;
    if Mi_Clone.Directed then
      Mi_Clone.To := new Vertex_Info_Recs(1..Mi.Size);
      Mi_Clone.To.all := Mi.To.all;
    else
      Mi_Clone.To := Mi_Clone.From;
    end if;
    Mi_Clone.Lower_Q := new Modularity_Recs(1..Mi.Size);
    Mi_Clone.Lower_Q.all := Mi.Lower_Q.all;
    if Mi.Eigenvec /= null then
      Mi_Clone.Gr_Trans := Clone(Mi.Gr_Trans);
      Mi_Clone.Eigenvec := Alloc(1, Mi.Size);
      Mi_Clone.Eigenvec.all := Mi.Eigenvec.all;
    end if;

    return Mi_Clone;
  end Clone;

  -----------------
  -- Degree_From --
  -----------------

  function Degree_From(Mi: in Modularity_Info; V: in Vertex) return Natural is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.From(Index_Of(V)).K;
  end Degree_From;

  ---------------
  -- Degree_To --
  ---------------

  function Degree_To(Mi: in Modularity_Info; V: in Vertex) return Natural is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.To(Index_Of(V)).K;
  end Degree_To;

  ------------------
  -- Total_Degree --
  ------------------

  function Total_Degree(Mi: in Modularity_Info) return Natural is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Two_M;
  end Total_Degree;

  ---------------------
  -- Number_Of_Edges --
  ---------------------

  function Number_Of_Edges(Mi: in Modularity_Info) return Natural is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    if Mi.Directed then
      return Mi.Two_M;
    else
      return (Mi.Two_M + Mi.Self_Loops_N) / 2;
    end if;
  end Number_Of_Edges;

  --------------------------
  -- Number_Of_Self_Loops --
  --------------------------

  function Number_Of_Self_Loops(Mi: in Modularity_Info) return Natural is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Self_Loops_N;
  end Number_Of_Self_Loops;

  -------------------
  -- Strength_From --
  -------------------

  function Strength_From(Mi: in Modularity_Info; V: in Vertex) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.From(Index_Of(V)).W;
  end Strength_From;

  -----------------
  -- Strength_To --
  -----------------

  function Strength_To(Mi: in Modularity_Info; V: in Vertex) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.To(Index_Of(V)).W;
  end Strength_To;

  --------------------
  -- Total_Strength --
  --------------------

  function Total_Strength(Mi: in Modularity_Info) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Two_W;
  end Total_Strength;

  -------------------
  -- Has_Self_Loop --
  -------------------

  function Has_Self_Loop(Mi: in Modularity_Info; V: in Vertex) return Boolean is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.From(Index_Of(V)).Has_Self_Loop or Mi.Resistance /= No_Resistance;
  end Has_Self_Loop;

  ---------------
  -- Self_Loop --
  ---------------

  function Self_Loop(Mi: in Modularity_Info; V: in Vertex) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.From(Index_Of(V)).Self_Loop;
  end Self_Loop;

  -------------------------------
  -- Total_Self_Loops_Strength --
  -------------------------------

  function Total_Self_Loops_Strength(Mi: in Modularity_Info) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Self_Loops;
  end Total_Self_Loops_Strength;

  ------------------------------
  -- Left_Leading_Eigenvector --
  ------------------------------

  function Left_Leading_Eigenvector(Mi: in Modularity_Info) return PNums is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Eigenvec;
  end Left_Leading_Eigenvector;

  -----------------------
  -- Update_Modularity --
  -----------------------

  procedure Update_Modularity(Mi: in Modularity_Info; L: in List; Mt: in Modularity_Type) is
  begin
    case Mt is
      when Unweighted_Newman                  => Update_Unweighted_Newman(Mi, L);
      when Unweighted_Uniform_Nullcase        => Update_Unweighted_Uniform_Nullcase(Mi, L);
      when Weighted_Newman                    => Update_Weighted_Newman(Mi, L);
      when Weighted_Signed                    => Update_Weighted_Signed(Mi, L);
      when Weighted_Uniform_Nullcase          => Update_Weighted_Uniform_Nullcase(Mi, L);
      when Weighted_Local_Average             => Update_Weighted_Local_Average(Mi, L);
      when Weighted_Uniform_Local_Average     => Update_Weighted_Uniform_Local_Average(Mi, L);
      when Weighted_Links_Unweighted_Nullcase => Update_Weighted_Links_Unweighted_Nullcase(Mi, L);
      when Weighted_No_Nullcase               => Update_Weighted_No_Nullcase(Mi, L);
      when Weighted_Link_Rank                 => Update_Weighted_Link_Rank(Mi, L);
    end case;
  end Update_Modularity;

  -----------------------
  -- Update_Modularity --
  -----------------------

  procedure Update_Modularity(Mi: in Modularity_Info; Lol: in List_Of_Lists; Mt: in Modularity_Type) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Update_Modularity(Mi, Next_List(Lol), Mt);
    end loop;
    Restore(Lol);
  end Update_Modularity;

  ----------------------
  -- Total_Modularity --
  ----------------------

  function Total_Modularity(Mi: in Modularity_Info) return Num is
    Modularity: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Modularity := 0.0;
    for I in Mi.Lower_Q'Range loop
      Modularity := Modularity + Mi.Lower_Q(I).Total;
    end loop;
    return Modularity;
  end Total_Modularity;

  ----------------------
  -- Total_Modularity --
  ----------------------

  function Total_Modularity(Mi: in Modularity_Info) return Modularity_Rec is
    Modularity: Modularity_Rec;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Modularity.Reward := 0.0;
    Modularity.Penalty := 0.0;
    for I in Mi.Lower_Q'Range loop
      Modularity.Reward := Modularity.Reward + Mi.Lower_Q(I).Reward;
      Modularity.Penalty := Modularity.Penalty + Mi.Lower_Q(I).Penalty;
    end loop;
    Modularity.Total := Modularity.Reward - Modularity.Penalty;
    return Modularity;
  end Total_Modularity;

  ------------------------
  -- Partial_Modularity --
  ------------------------

  function Partial_Modularity(Mi: in Modularity_Info; L: in List) return Num is
    Modularity: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Modularity := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      Modularity := Modularity + Element_Modularity(Mi, Next_Element(L));
    end loop;
    Restore(L);
    return Modularity;
  end Partial_Modularity;

  ------------------------
  -- Partial_Modularity --
  ------------------------

  function Partial_Modularity(Mi: in Modularity_Info; L: in List) return Modularity_Rec is
    Modularity, M_Element: Modularity_Rec;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Modularity.Reward := 0.0;
    Modularity.Penalty := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      M_Element := Element_Modularity(Mi, Next_Element(L));
      Modularity.Reward := Modularity.Reward + M_Element.Reward;
      Modularity.Penalty := Modularity.Penalty + M_Element.Penalty;
    end loop;
    Restore(L);
    Modularity.Total := Modularity.Reward - Modularity.Penalty;
    return Modularity;
  end Partial_Modularity;

  ------------------------
  -- Element_Modularity --
  ------------------------

  function Element_Modularity(Mi: in Modularity_Info; E: in Element) return Num is
  begin
    return Element_Modularity(Mi, E).Total;
  end Element_Modularity;

  ------------------------
  -- Element_Modularity --
  ------------------------

  function Element_Modularity(Mi: in Modularity_Info; E: in Element) return Modularity_Rec is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Lower_Q(Index_Of(E));
  end Element_Modularity;

  ----------------
  -- Modularity --
  ----------------

  function Modularity(Mi: in Modularity_Info; Lol: in List_Of_Lists; Mt: in Modularity_Type) return Num is
  begin
    return Modularity(Mi, Lol, Mt).Total;
  end Modularity;

  ----------------
  -- Modularity --
  ----------------

  function Modularity(Mi: in Modularity_Info; Lol: in List_Of_Lists; Mt: in Modularity_Type) return Modularity_Rec is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Update_Modularity(Mi, Lol, Mt);
    return Total_Modularity(Mi);
  end Modularity;

  ----------------
  -- Modularity --
  ----------------

  function Modularity(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type; R: in Num := No_Resistance; Pc: in Num := 1.0) return Num is
  begin
    return Modularity(Gr, Lol, Mt, R, Pc).Total;
  end Modularity;

  ----------------
  -- Modularity --
  ----------------

  function Modularity(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type; R: in Num := No_Resistance; Pc: in Num := 1.0) return Modularity_Rec is
    Mi: Modularity_Info;
    M: Modularity_Rec;
  begin
    Initialize(Mi, Gr, Mt, R, Pc);
    M := Modularity(Mi, Lol, Mt);
    Free(Mi);
    return M;
  end Modularity;

  ------------------------------
  -- Update_Unweighted_Newman --
  ------------------------------

  procedure Update_Unweighted_Newman(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    I, J: Positive;
    Sum_K_In, Sum_A: Natural;
    Re, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Sum_K_In := 0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_K_In := Sum_K_In + Mi.To(J).K;
    end loop;
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_A := 1;
      else
        Sum_A := 0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(Next(El));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_A := Sum_A + 1;
        end if;
      end loop;
      Restore(El);
      Re := Num(Sum_A) / Num(Mi.Two_M);
      Pe := Num(Mi.From(I).K * Sum_K_In) / (Num(Mi.Two_M) * Num(Mi.Two_M));
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Unweighted_Newman;

  ----------------------------------------
  -- Update_Unweighted_Uniform_Nullcase --
  ----------------------------------------

  procedure Update_Unweighted_Uniform_Nullcase(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    I, J: Positive;
    Sum_A: Natural;
    Re, Pec, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Save(L);
    Reset(L);
    Pec := Num(Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_A := 1;
      else
        Sum_A := 0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(Next(El));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_A := Sum_A + 1;
        end if;
      end loop;
      Restore(El);
      Re := Num(Sum_A) / Num(Mi.Two_M);
      Pe := Mi.Penalty_Coefficient * Pec;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Unweighted_Uniform_Nullcase;

  ----------------------------
  -- Update_Weighted_Newman --
  ----------------------------

  procedure Update_Weighted_Newman(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_W_In, Sum_W: Num;
    Re, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Sum_W_In := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_W_In := Sum_W_In + Mi.To(J).W;
    end loop;
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + To_Num(E.Value);
        end if;
      end loop;
      Restore(El);
      Re := Sum_W / Mi.Two_W;
      Pe := Mi.From(I).W * Sum_W_In / (Mi.Two_W * Mi.Two_W);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Newman;

  ----------------------------
  -- Update_Weighted_Signed --
  ----------------------------

  procedure Update_Weighted_Signed(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_W, Sum_W_In, Sum_W_In_Pos, Sum_W_In_Neg: Num;
    Re, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Sum_W_In := 0.0;
    Sum_W_In_Pos := 0.0;
    Sum_W_In_Neg := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_W_In := Sum_W_In + Mi.To(J).W;
      Sum_W_In_Pos := Sum_W_In_Pos + Mi.To(J).W_Pos;
      Sum_W_In_Neg := Sum_W_In_Neg + Mi.To(J).W_Neg;
    end loop;
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + Value(E, All_Links);
        end if;
      end loop;
      Restore(El);
      Re := Sum_W / (Mi.Two_W_Pos + Mi.Two_W_Neg);
      Pe := 0.0;
      if Mi.Two_W_Pos > 0.0 then
        Pe := Pe + Mi.From(I).W_Pos * Sum_W_In_Pos / Mi.Two_W_Pos;
      end if;
      if Mi.Two_W_Neg > 0.0 then
        Pe := Pe - Mi.From(I).W_Neg * Sum_W_In_Neg / Mi.Two_W_Neg;
      end if;
      Pe := Pe / (Mi.Two_W_Pos + Mi.Two_W_Neg);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Signed;

  --------------------------------------
  -- Update_Weighted_Uniform_Nullcase --
  --------------------------------------

  procedure Update_Weighted_Uniform_Nullcase(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_W: Num;
    Re, Pec, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Pec := Num(Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + To_Num(E.Value);
        end if;
      end loop;
      Restore(El);
      Re := Sum_W / Mi.Two_W;
      Pe := Mi.Penalty_Coefficient * Pec;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Uniform_Nullcase;

  -----------------------------------
  -- Update_Weighted_Local_Average --
  -----------------------------------

  procedure Update_Weighted_Local_Average(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Ka: Natural;
    Sum_W, Sum_Wa_K_In, Wa: Num;
    Re, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + To_Num(E.Value);
        end if;
      end loop;
      Restore(El);
      Sum_Wa_K_In := 0.0;
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        J := Index_Of(Next_Element(L));
        Ka := Mi.From(I).K + Mi.To(J).K;
        if Ka = 0 then
          Wa := 0.0;
        else
          Wa := (Mi.From(I).W + Mi.To(J).W) / Num(Ka);
        end if;
        Sum_Wa_K_In := Sum_Wa_K_In + Num(Mi.To(J).K) * Wa;
      end loop;
      Restore(L);
      Re := Sum_W / Mi.Two_W;
      Pe := Num(Mi.From(I).K) * Sum_Wa_K_In / Mi.Two_La;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Local_Average;

  -------------------------------------------
  -- Update_Weighted_Uniform_Local_Average --
  -------------------------------------------

  procedure Update_Weighted_Uniform_Local_Average(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Ka: Natural;
    Sum_W, Sum_Wa, Wa: Num;
    Re, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + To_Num(E.Value);
        end if;
      end loop;
      Restore(El);
      Sum_Wa := 0.0;
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        J := Index_Of(Next_Element(L));
        Ka := Mi.From(I).K + Mi.To(J).K;
        if Ka = 0 then
          Wa := 0.0;
        else
          Wa := (Mi.From(I).W + Mi.To(J).W) / Num(Ka);
        end if;
        Sum_Wa := Sum_Wa + Wa;
      end loop;
      Restore(L);
      Re := Sum_W / Mi.Two_W;
      Pe := Sum_Wa / Mi.Two_Ula;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Uniform_Local_Average;

  -----------------------------------------------
  -- Update_Weighted_Links_Unweighted_Nullcase --
  -----------------------------------------------

  procedure Update_Weighted_Links_Unweighted_Nullcase(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_K_In: Natural;
    Sum_W: Num;
    Re, Pe: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Sum_K_In := 0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_K_In := Sum_K_In + Mi.To(J).K;
    end loop;
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + To_Num(E.Value);
        end if;
      end loop;
      Restore(El);
      Re := Sum_W / Mi.Two_W;
      Pe := Num(Mi.From(I).K * Sum_K_In) / (Num(Mi.Two_M) * Num(Mi.Two_M));
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Links_Unweighted_Nullcase;

  ---------------------------------
  -- Update_Weighted_No_Nullcase --
  ---------------------------------

  procedure Update_Weighted_No_Nullcase(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_W: Num;
    Re: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W := Sum_W + To_Num(E.Value);
        end if;
      end loop;
      Restore(El);
      Re := Sum_W / Mi.Two_W_Pos;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := 0.0;
      Mi.Lower_Q(I).Total := Re;
    end loop;
    Restore(L);
  end Update_Weighted_No_Nullcase;

  -------------------------------
  -- Update_Weighted_Link_Rank --
  -------------------------------

  procedure Update_Weighted_Link_Rank(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Graphs_Double.Edges_List;
    E: Graphs_Double.Edge;
    I, J: Positive;
    Sum_Eigv, Sum_Trans: Num;
    Re, Pe: Num;
  begin
    pragma Warnings(Off, El);
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;
    if Mi.Eigenvec = null then
      Special_Initializations(Mi, Weighted_Link_Rank);
    end if;

    Sum_Eigv := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_Eigv := Sum_Eigv + Mi.Eigenvec(J);
    end loop;
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      El := Edges_From(Get_Vertex(Mi.Gr_Trans, I));
      Sum_Trans := 0.0;
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_Trans := Sum_Trans + Num(Value(E));
        end if;
      end loop;
      Restore(El);
      Re := Mi.Eigenvec(I) * Sum_Trans;
      Pe := Mi.Eigenvec(I) * Sum_Eigv;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Lower_Q(I).Reward := Re;
      Mi.Lower_Q(I).Penalty := Pe;
      Mi.Lower_Q(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Link_Rank;

  -----------------------
  -- Transitions_Graph --
  -----------------------

  procedure Transitions_Graph(Mi: in Modularity_Info) is
    N: Natural;
    V: Vertex;
    Vf, Vt: Graphs_Double.Vertex;
    E: Edge;
    El: Edges_List;
    J: Positive;
    Wi, Pii, Pij: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    N := Number_Of_Vertices(Mi.Gr);
    Initialize(Mi.Gr_Trans, N, Directed => True);

    for I in 1..N loop
      V := Get_Vertex(Mi.Gr, I);
      Vf := Graphs_Double.Get_Vertex(Mi.Gr_Trans, I);
      Wi := Mi.From(I).W;
      if Wi /= 0.0 then
        if (not Mi.From(I).Has_Self_Loop) and Mi.Resistance /= No_Resistance then
          Pii := Mi.From(I).Self_Loop / Wi;
          Add_Edge(Vf, Vf, Double(Pii));
        end if;
        El := Edges_From(V);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          J := Index_Of(To(E));
          Vt := Graphs_Double.Get_Vertex(Mi.Gr_Trans, J);
          if I = J then
            Pij := Mi.From(I).Self_Loop / Wi;
          else
            Pij := To_Num(Value(E)) / Wi;
          end if;
          Add_Edge(Vf, Vt, Double(Pij));
        end loop;
        Restore(El);
      end if;
    end loop;
  end Transitions_Graph;

  ------------------------------
  -- Left_Leading_Eigenvector --
  ------------------------------

  procedure Left_Leading_Eigenvector(Gr: in Graphs_Double.Graph; Eigv: out PNums) is

    function Left_Product(Vec: in PNums; Gr: in Graphs_Double.Graph; N: in Natural) return Nums is
      Prod: Nums(1..N);
      Vt: Graphs_Double.Vertex;
      E: Graphs_Double.Edge;
      El: Graphs_Double.Edges_List;
      I: Positive;
    begin
      pragma Warnings(Off, El);
      Prod := (others => 0.0);
      for J in 1..N loop
        Vt := Get_Vertex(Gr, J);
        El := Edges_To(Vt);
        Save(El);
        Reset(El);
        while Has_Next(El) loop
          E := Next(El);
          I := Index_Of(From(E));
          Prod(J) := Prod(J) + Vec(I) * Num(Value(E));
        end loop;
        Restore(El);
      end loop;
      return Prod;
    end Left_Product;

    procedure Normalize(Vec: in PNums) is
      Norm: Num := 0.0;
    begin
      for I in Vec'Range loop
        Norm := Norm + abs Vec(I);
      end loop;
      for I in Vec'Range loop
        Vec(I) := Vec(I) / Norm;
      end loop;
    end Normalize;

    function Converged(Vec1, Vec2: in PNums) return Boolean is
      Convergence_Epsilon: constant Num := 1.0E-8;
    begin
      for I in Vec1'Range loop
        if abs (Vec1(I) - Vec2(I)) > Convergence_Epsilon then
          return False;
        end if;
      end loop;
      return True;
    end Converged;

    N: Natural;
    Eigv_Prev: PNums;

  begin
    if not Is_Initialized(Gr) then
      raise Uninitialized_Graph_Error;
    end if;

    N := Number_Of_Vertices(Gr);
    Eigv_Prev := Alloc(1, N);
    Eigv      := Alloc(1, N);
    Eigv_Prev.all := (1 => 1.0, others => 0.0);
    Eigv.all      := (others => 1.0 / Num(N));

    while not Converged(Eigv_Prev, Eigv) loop
      Eigv_Prev.all := Eigv.all;
      Eigv.all := Left_Product(Eigv, Gr, N);
      Normalize(Eigv);
    end loop;
    Free(Eigv_Prev);
  end Left_Leading_Eigenvector;

end Graphs.Modularities;
