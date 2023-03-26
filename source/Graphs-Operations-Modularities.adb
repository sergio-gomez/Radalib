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


-- @filename Graphs-Operations-Modularities.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 5/03/2006
-- @revision 26/09/2020
-- @brief Calculation of Modularities of Graphs

with Ada.Unchecked_Deallocation;
with Utils; use Utils;
with Graphs_Double; use Graphs_Double;
with Graphs_Double_Operations; use Graphs_Double_Operations;

package body Graphs.Operations.Modularities is

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
    elsif To_Uppercase(Mt_Name) = "WBPM" or To_Lowercase(Mt_Name) = "weighted_bipartite_path_motif"      then
      return Weighted_Bipartite_Path_Motif;
    elsif To_Uppercase(Mt_Name) = "WBPS" or To_Lowercase(Mt_Name) = "weighted_bipartite_path_signed"     then
      return Weighted_Bipartite_Path_Signed;
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
        when Weighted_Bipartite_Path_Motif      => return "WBPM";
        when Weighted_Bipartite_Path_Signed     => return "WBPS";
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
    Mi.Gr_Neigh := Mi.Gr;
    Mi.Size := Number_Of_Vertices(Gr);
    Mi.Directed := Is_Directed(Gr);
    Mi.Signed := Has_Links(Gr, Negative_Links);
    Mi.From := new Vertex_Info_Recs(1..Mi.Size);
    if Mi.Directed then
      Mi.To := new Vertex_Info_Recs(1..Mi.Size);
    else
      Mi.To := Mi.From;
    end if;
    Mi.Q_Node := new Modularity_Recs(1..Mi.Size);
    Mi.Q_Node_Saved := new Modularity_Recs(1..Mi.Size);

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

  procedure Set_Penalty_Coefficient(Mi: in Modularity_Info; Pc: in Num := Default_Penalty_Coefficient) is
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

  -----------------------
  -- Set_Teleportation --
  -----------------------

  procedure Set_Teleportation(Mi: in Modularity_Info; Tp: in Num := Default_Teleportation) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Mi.Teleportation := Tp;
  end Set_Teleportation;

  -----------------------
  -- Get_Teleportation --
  -----------------------

  function Get_Teleportation(Mi: in Modularity_Info) return Num is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Teleportation;
  end Get_Teleportation;

  ------------------
  -- Update_Graph --
  ------------------

  procedure Update_Graph(Mi: in Modularity_Info; Mt: in Modularity_Type := Weighted_Signed) is
    Vt, Vf: Vertex;
    Ki, Tk, Tsln: Natural;
    Kir, Kjr, Wii, Wi, Wj, Wla, Tkr, Tw, Twp, Twn, Tla, Tula, Tsl: Num;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    for I in Mi.From'Range loop
      Vf := Get_Vertex(Mi.Gr, I);
      Mi.From(I).K := Degree_From(Vf);
      Mi.From(I).Kr := Num(Mi.From(I).K);
      Mi.From(I).W := Strength_From(Vf);
      Mi.From(I).W_Pos := Strength_From(Vf, Positive_Links);
      Mi.From(I).W_Neg := Strength_From(Vf, Negative_Links);
      Mi.From(I).Has_Self_Loop := Has_Self_Loop(Vf);
      Mi.From(I).Self_Loop := Self_Loop(Vf);
      if Mi.Resistance /= No_Resistance then
        Mi.From(I).Kr := Mi.From(I).Kr + Mi.Resistance;
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
        Mi.To(J).Kr := Num(Mi.To(J).K);
        Mi.To(J).W := Strength_To(Vt);
        Mi.To(J).W_Pos := Strength_To(Vt, Positive_Links);
        Mi.To(J).W_Neg := Strength_To(Vt, Negative_Links);
        Mi.To(J).Has_Self_Loop := Has_Self_Loop(Vt);
        Mi.To(J).Self_Loop := Self_Loop(Vt);
        if Mi.Resistance /= No_Resistance then
          Mi.To(J).Kr := Mi.To(J).Kr + Mi.Resistance;
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
    Tkr := 0.0;
    Tw := 0.0;
    Twp := 0.0;
    Twn := 0.0;
    Tla := 0.0;
    Tula := 0.0;
    Tsl := 0.0;
    Tsln := 0;
    for I in Mi.From'Range loop
      Ki := Mi.From(I).K;
      Kir := Mi.From(I).Kr;
      Wi := Mi.From(I).W;
      Tk := Tk + Ki;
      Tkr := Tkr + Kir;
      Tw := Tw + Wi;
      Twp := Twp + Mi.From(I).W_Pos;
      Twn := Twn + Mi.From(I).W_Neg;
      Tsl := Tsl + Mi.From(I).Self_Loop;
      if Mi.From(I).Has_Self_Loop or Mi.Resistance /= No_Resistance then
        Tsln := Tsln + 1;
      end if;
      for J in Mi.To'Range loop
        Kjr := Mi.To(J).Kr;
        Wj := Mi.To(J).W;
        if Kir + Kjr /= 0.0 then
          Wla := (Wi + Wj) / (Kir + Kjr);
          Tla := Tla + (Kir * Kjr) * Wla;
          Tula := Tula + Wla;
        end if;
      end loop;
    end loop;
    Mi.Two_M := Tk;
    Mi.Two_Mr := Tkr;
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

    function Safe_Divide(Left, Right: Num) return Num is
    begin
      if Right = 0.0 then
        return 0.0;
      else
        return Left / Right;
      end if;
    end Safe_Divide;

    Vf: Vertex;
    Wii: Num := 0.0;
    Two_W_Path, Two_W_Path_Pos, Two_W_Path_Neg: Num := 0.0;
    Two_W_C1_F, Two_W_C1_T, Two_W_C2_F, Two_W_C2_T: Num := 0.0;
    Two_W_C1_FP, Two_W_C1_FN, Two_W_C1_TP, Two_W_C1_TN: Num := 0.0;
    Two_W_C2_FP, Two_W_C2_FN, Two_W_C2_TP, Two_W_C2_TN: Num := 0.0;
    Two_W2_C1, Two_W2_C2: Num := 0.0;
    Two_W2_C1_PP, Two_W2_C1_NN, Two_W2_C1_PN, Two_W2_C1_NP: Num := 0.0;
    Two_W2_C2_PP, Two_W2_C2_NN, Two_W2_C2_PN, Two_W2_C2_NP: Num := 0.0;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    case Mt is
      when Weighted_Link_Rank =>
        if Mi.Two_W_Neg > 0.0 then
          raise Incompatible_Modularity_Type_Error with "Weighted_Link_Rank cannot handle negative weights";
        end if;
        if Mi.Link_Rank_Eigenvec /= null then
          Free(Mi.Gr_Trans);
          Free(Mi.Link_Rank_Eigenvec);
        end if;
        Transitions_Graph(Mi);
        Link_Rank_Left_Leading_Eigenvector(Mi);
      when Weighted_Bipartite_Path_Motif | Weighted_Bipartite_Path_Signed =>
        if Is_Initialized(Mi.Gr_Path) then
          Free(Mi.Gr_Path);
        end if;
        Mi.Gr_Path := Mi.Gr ** 2;
        Mi.Gr_Neigh := Mi.Gr_Path;
        for I in Mi.From'Range loop
          Vf := Get_Vertex(Mi.Gr_Path, I);
          Two_W_Path := Two_W_Path + Strength_From(Vf);
          Two_W_Path_Pos := Two_W_Path_Pos + Strength_From(Vf, Positive_Links);
          Two_W_Path_Neg := Two_W_Path_Neg + Strength_From(Vf, Negative_Links);
          Mi.From(I).Self_Loop_Path := Self_Loop(Vf);
          if Mi.Resistance /= No_Resistance then
            Two_W_Path := Two_W_Path + Mi.Resistance;
            Wii := Mi.From(I).Self_Loop_Path + Mi.Resistance;
            if Mi.From(I).Self_Loop_Path >= 0.0 then
              if Wii >= 0.0 then
                Two_W_Path_Pos := Two_W_Path_Pos + Mi.Resistance;
              else
                Two_W_Path_Pos := Two_W_Path_Pos - Mi.From(I).Self_Loop;
                Two_W_Path_Neg := Two_W_Path_Neg - Wii;
              end if;
            else
              if Wii <= 0.0 then
                Two_W_Path_Neg := Two_W_Path_Neg - Mi.Resistance;
              else
                Two_W_Path_Neg := Two_W_Path_Neg + Mi.From(I).Self_Loop;
                Two_W_Path_Pos := Two_W_Path_Pos + Wii;
              end if;
            end if;
            Mi.From(I).Self_Loop_Path := Wii;
          end if;
          if Mi.Directed then
            Mi.To(I).Self_Loop_Path := Mi.From(I).Self_Loop_Path;
          end if;
          if I <= Mi.Gr.Num_Class1 then
            Two_W_C1_F := Two_W_C1_F + Mi.From(I).W;
            Two_W_C1_T := Two_W_C1_T + Mi.To(I).W;
            Two_W_C1_FP := Two_W_C1_FP + Mi.From(I).W_Pos;
            Two_W_C1_FN := Two_W_C1_FN + Mi.From(I).W_Neg;
            Two_W_C1_TP := Two_W_C1_TP + Mi.To(I).W_Pos;
            Two_W_C1_TN := Two_W_C1_TN + Mi.To(I).W_Neg;
            Two_W2_C1 := Two_W2_C1 + Mi.To(I).W * Mi.From(I).W;
            Two_W2_C1_PP := Two_W2_C1_PP + Mi.To(I).W_Pos * Mi.From(I).W_Pos;
            Two_W2_C1_NN := Two_W2_C1_NN + Mi.To(I).W_Neg * Mi.From(I).W_Neg;
            Two_W2_C1_PN := Two_W2_C1_PN + Mi.To(I).W_Pos * Mi.From(I).W_Neg;
            Two_W2_C1_NP := Two_W2_C1_NP + Mi.To(I).W_Neg * Mi.From(I).W_Pos;
          else
            Two_W_C2_F := Two_W_C2_F + Mi.From(I).W;
            Two_W_C2_T := Two_W_C2_T + Mi.To(I).W;
            Two_W_C2_FP := Two_W_C2_FP + Mi.From(I).W_Pos;
            Two_W_C2_FN := Two_W_C2_FN + Mi.From(I).W_Neg;
            Two_W_C2_TP := Two_W_C2_TP + Mi.To(I).W_Pos;
            Two_W_C2_TN := Two_W_C2_TN + Mi.To(I).W_Neg;
            Two_W2_C2 := Two_W2_C2 + Mi.To(I).W * Mi.From(I).W;
            Two_W2_C2_PP := Two_W2_C2_PP + Mi.To(I).W_Pos * Mi.From(I).W_Pos;
            Two_W2_C2_NN := Two_W2_C2_NN + Mi.To(I).W_Neg * Mi.From(I).W_Neg;
            Two_W2_C2_PN := Two_W2_C2_PN + Mi.To(I).W_Pos * Mi.From(I).W_Neg;
            Two_W2_C2_NP := Two_W2_C2_NP + Mi.To(I).W_Neg * Mi.From(I).W_Pos;
          end if;
        end loop;
        for I in Mi.From'Range loop
          if Mi.Gr.Bipartite then
            if I <= Mi.Gr.Num_Class1 then
              Mi.From(I).Path_Null_Factor := Safe_Divide(Safe_Divide(Two_W2_C2, Two_W_C2_T * Two_W_C2_F), Two_W_C1_F * Two_W_C1_T);
              Mi.From(I).Path_Null_Factor_PP := Safe_Divide(Safe_Divide(Two_W2_C2_PP, Two_W_C2_TP * Two_W_C2_FP), Two_W_C1_FP * Two_W_C1_TP);
              Mi.From(I).Path_Null_Factor_NN := Safe_Divide(Safe_Divide(Two_W2_C2_NN, Two_W_C2_TN * Two_W_C2_FN), Two_W_C1_FN * Two_W_C1_TN);
              Mi.From(I).Path_Null_Factor_PN := Safe_Divide(Safe_Divide(Two_W2_C2_PN, Two_W_C2_TP * Two_W_C2_FN), Two_W_C1_FP * Two_W_C1_TN);
              Mi.From(I).Path_Null_Factor_NP := Safe_Divide(Safe_Divide(Two_W2_C2_NP, Two_W_C2_TN * Two_W_C2_FP), Two_W_C1_FN * Two_W_C1_TP);
            else
              Mi.From(I).Path_Null_Factor := Safe_Divide(Safe_Divide(Two_W2_C1, Two_W_C1_T * Two_W_C1_F), Two_W_C2_F * Two_W_C2_T);
              Mi.From(I).Path_Null_Factor_PP := Safe_Divide(Safe_Divide(Two_W2_C1_PP, Two_W_C1_TP * Two_W_C1_FP), Two_W_C2_FP * Two_W_C2_TP);
              Mi.From(I).Path_Null_Factor_NN := Safe_Divide(Safe_Divide(Two_W2_C1_NN, Two_W_C1_TN * Two_W_C1_FN), Two_W_C2_FN * Two_W_C2_TN);
              Mi.From(I).Path_Null_Factor_PN := Safe_Divide(Safe_Divide(Two_W2_C1_PN, Two_W_C1_TP * Two_W_C1_FN), Two_W_C2_FP * Two_W_C2_TN);
              Mi.From(I).Path_Null_Factor_NP := Safe_Divide(Safe_Divide(Two_W2_C1_NP, Two_W_C1_TN * Two_W_C1_FP), Two_W_C2_FN * Two_W_C2_TP);
            end if;
          else
            Mi.From(I).Path_Null_Factor := Safe_Divide(Safe_Divide(Two_W2_C1, Two_W_C1_T * Two_W_C1_F), Two_W_C1_F * Two_W_C1_T);
            Mi.From(I).Path_Null_Factor_PP := Safe_Divide(Safe_Divide(Two_W2_C1_PP, Two_W_C1_TP * Two_W_C1_FP), Two_W_C1_FP * Two_W_C1_TP);
            Mi.From(I).Path_Null_Factor_NN := Safe_Divide(Safe_Divide(Two_W2_C1_NN, Two_W_C1_TN * Two_W_C1_FN), Two_W_C1_FN * Two_W_C1_TN);
            Mi.From(I).Path_Null_Factor_PN := Safe_Divide(Safe_Divide(Two_W2_C1_PN, Two_W_C1_TP * Two_W_C1_FN), Two_W_C1_FP * Two_W_C1_TN);
            Mi.From(I).Path_Null_Factor_NP := Safe_Divide(Safe_Divide(Two_W2_C1_NP, Two_W_C1_TN * Two_W_C1_FP), Two_W_C1_FN * Two_W_C1_TP);
          end if;
          if Mi.Directed then
            Mi.To(I).Path_Null_Factor := Mi.From(I).Path_Null_Factor;
            Mi.To(I).Path_Null_Factor_PP := Mi.From(I).Path_Null_Factor_PP;
            Mi.To(I).Path_Null_Factor_NN := Mi.From(I).Path_Null_Factor_NN;
            Mi.To(I).Path_Null_Factor_PN := Mi.From(I).Path_Null_Factor_PN;
            Mi.To(I).Path_Null_Factor_NP := Mi.From(I).Path_Null_Factor_NP;
          end if;
        end loop;
        Mi.Two_W_Path := Two_W_Path;
        Mi.Two_W_Path_Pos := Two_W_Path_Pos;
        Mi.Two_W_Path_Neg := Two_W_Path_Neg;
        if Mi.Gr.Bipartite then
          Mi.Two_W_Path_Null := Safe_Divide(Two_W2_C2, Two_W_C2_T * Two_W_C2_F) +
                                Safe_Divide(Two_W2_C1, Two_W_C1_T * Two_W_C1_F);
          Mi.Two_W_Path_Null_Pos := Safe_Divide(Two_W2_C2_PP, Two_W_C2_TP * Two_W_C2_FP) +
                                    Safe_Divide(Two_W2_C2_NN, Two_W_C2_TN * Two_W_C2_FN) +
                                    Safe_Divide(Two_W2_C1_PP, Two_W_C1_TP * Two_W_C1_FP) +
                                    Safe_Divide(Two_W2_C1_NN, Two_W_C1_TN * Two_W_C1_FN);
          Mi.Two_W_Path_Null_Neg := Safe_Divide(Two_W2_C2_PN, Two_W_C2_TP * Two_W_C2_FN) +
                                    Safe_Divide(Two_W2_C2_NP, Two_W_C2_TN * Two_W_C2_FP) +
                                    Safe_Divide(Two_W2_C1_PN, Two_W_C1_TP * Two_W_C1_FN) +
                                    Safe_Divide(Two_W2_C1_NP, Two_W_C1_TN * Two_W_C1_FP);
        else
          Mi.Two_W_Path_Null := Safe_Divide(Two_W2_C1, Two_W_C1_T * Two_W_C1_F);
          Mi.Two_W_Path_Null_Pos := Safe_Divide(Two_W2_C1_PP, Two_W_C1_TP * Two_W_C1_FP) +
                                    Safe_Divide(Two_W2_C1_NN, Two_W_C1_TN * Two_W_C1_FN);
          Mi.Two_W_Path_Null_Neg := Safe_Divide(Two_W2_C1_PN, Two_W_C1_TP * Two_W_C1_FN) +
                                    Safe_Divide(Two_W2_C1_NP, Two_W_C1_TN * Two_W_C1_FP);
        end if;
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
      Dispose(Mi.Q_Node);
      Dispose(Mi.Q_Node_Saved);
      if Mi.Link_Rank_Eigenvec /= null then
        Free(Mi.Gr_Trans);
        Free(Mi.Link_Rank_Eigenvec);
      end if;
      if Is_Initialized(Mi.Gr_Path) then
        Free(Mi.Gr_Path);
      end if;
      Dispose(Mi);
      Mi := null;
    end if;
  end Free;

  --------------
  -- Graph_Of --
  --------------

  function Graph_Of(Mi: in Modularity_Info) return Graph is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Gr;
  end Graph_Of;

  ---------------------
  -- Neighbors_Graph --
  ---------------------

  function Neighbors_Graph(Mi: in Modularity_Info) return Graph is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Gr_Neigh;
  end Neighbors_Graph;

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
    Mi_Clone.Gr := Clone(Mi.Gr);
    Mi_Clone.Gr_Neigh := Mi_Clone.Gr;
    Mi_Clone.From := new Vertex_Info_Recs(1..Mi.Size);
    Mi_Clone.From.all := Mi.From.all;
    if Mi_Clone.Directed then
      Mi_Clone.To := new Vertex_Info_Recs(1..Mi.Size);
      Mi_Clone.To.all := Mi.To.all;
    else
      Mi_Clone.To := Mi_Clone.From;
    end if;
    Mi_Clone.Q_Node := new Modularity_Recs(1..Mi.Size);
    Mi_Clone.Q_Node.all := Mi.Q_Node.all;
    Mi_Clone.Q_Node_Saved := new Modularity_Recs(1..Mi.Size);
    Mi_Clone.Q_Node_Saved.all := Mi.Q_Node_Saved.all;
    if Mi.Link_Rank_Eigenvec /= null then
      Mi_Clone.Gr_Trans := Clone(Mi.Gr_Trans);
      Mi_Clone.Link_Rank_Eigenvec := Alloc(1, Mi.Size);
      Mi_Clone.Link_Rank_Eigenvec.all := Mi.Link_Rank_Eigenvec.all;
    end if;
    if Is_Initialized(Mi.Gr_Path) then
      Mi_Clone.Gr_Path := Clone(Mi.Gr_Path);
      Mi_Clone.Gr_Neigh := Mi_Clone.Gr_Path;
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

  ---------------------------
  -- Link_Rank_Eigenvector --
  ---------------------------

  function Link_Rank_Eigenvector(Mi: in Modularity_Info) return PNums is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    return Mi.Link_Rank_Eigenvec;
  end Link_Rank_Eigenvector;

  ---------------------
  -- Save_Modularity --
  ---------------------

  procedure Save_Modularity(Mi: in Modularity_Info) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Mi.Q_Node_Saved.all := Mi.Q_Node.all;
  end Save_Modularity;

  ---------------------
  -- Save_Modularity --
  ---------------------

  procedure Save_Modularity(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    I: Positive;
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
      Mi.Q_Node_Saved(I) := Mi.Q_Node(I);
    end loop;
    Restore(L);
  end Save_Modularity;

  ---------------------
  -- Save_Modularity --
  ---------------------

  procedure Save_Modularity(Mi: in Modularity_Info; E: in Element) is
    I: Positive;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    I := Index_Of(E);
    Mi.Q_Node_Saved(I) := Mi.Q_Node(I);
  end Save_Modularity;

  ------------------------
  -- Restore_Modularity --
  ------------------------

  procedure Restore_Modularity(Mi: in Modularity_Info) is
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    Mi.Q_Node.all := Mi.Q_Node_Saved.all;
  end Restore_Modularity;

  ------------------------
  -- Restore_Modularity --
  ------------------------

  procedure Restore_Modularity(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    I: Positive;
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
      Mi.Q_Node(I) := Mi.Q_Node_Saved(I);
    end loop;
    Restore(L);
  end Restore_Modularity;

  ------------------------
  -- Restore_Modularity --
  ------------------------

  procedure Restore_Modularity(Mi: in Modularity_Info; E: in Element) is
    I: Positive;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    I := Index_Of(E);
    Mi.Q_Node(I) := Mi.Q_Node_Saved(I);
  end Restore_Modularity;

  ------------------------------------
  -- Update_Modularity_Move_Element --
  ------------------------------------

  procedure Update_Modularity_Move_Element(Mi: in Modularity_Info; E: in Element; L: in List; Mt: in Modularity_Type) is
    Lol: List_Of_Lists;
    Li: List;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

    if not Belongs_To(E, L) then
      Update_Modularity_Inserted_Element(Mi, E, L, Mt);
      Li := List_Of(E);
      Move(E, L);
      Update_Modularity_Removed_Element(Mi, E, Li, Mt);
    end if;
  end Update_Modularity_Move_Element;

  ----------------------------------------
  -- Update_Modularity_Inserted_Element --
  ----------------------------------------

  procedure Update_Modularity_Inserted_Element(Mi: in Modularity_Info; E: in Element; L: in List; Mt: in Modularity_Type) is
    Lol: List_Of_Lists;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;
    if Belongs_To(E, L) then
      raise Element_In_List_Error;
    end if;

    case Mt is
      when Unweighted_Newman                  => Update_Inserted_Element_Unweighted_Newman(Mi, E, L);
      when Unweighted_Uniform_Nullcase        => Update_Inserted_Element_Unweighted_Uniform_Nullcase(Mi, E, L);
      when Weighted_Newman                    => Update_Inserted_Element_Weighted_Newman(Mi, E, L);
      when Weighted_Signed                    => Update_Inserted_Element_Weighted_Signed(Mi, E, L);
      when Weighted_Uniform_Nullcase          => Update_Inserted_Element_Weighted_Uniform_Nullcase(Mi, E, L);
      when Weighted_Local_Average             => Update_Inserted_Element_Weighted_Local_Average(Mi, E, L);
      when Weighted_Uniform_Local_Average     => Update_Inserted_Element_Weighted_Uniform_Local_Average(Mi, E, L);
      when Weighted_Links_Unweighted_Nullcase => Update_Inserted_Element_Weighted_Links_Unweighted_Nullcase(Mi, E, L);
      when Weighted_No_Nullcase               => Update_Inserted_Element_Weighted_No_Nullcase(Mi, E, L);
      when Weighted_Link_Rank                 => Update_Inserted_Element_Weighted_Link_Rank(Mi, E, L);
      when Weighted_Bipartite_Path_Motif      => Update_Inserted_Element_Weighted_Bipartite_Path_Motif(Mi, E, L);
      when Weighted_Bipartite_Path_Signed     => Update_Inserted_Element_Weighted_Bipartite_Path_Signed(Mi, E, L);
    end case;
  end Update_Modularity_Inserted_Element;

  ---------------------------------------
  -- Update_Modularity_Removed_Element --
  ---------------------------------------

  procedure Update_Modularity_Removed_Element(Mi: in Modularity_Info; E: in Element; L: in List; Mt: in Modularity_Type) is
    Lol: List_Of_Lists;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;
    if Belongs_To(E, L) then
      raise Element_In_List_Error;
    end if;

    case Mt is
      when Unweighted_Newman                  => Update_Removed_Element_Unweighted_Newman(Mi, E, L);
      when Unweighted_Uniform_Nullcase        => Update_Removed_Element_Unweighted_Uniform_Nullcase(Mi, E, L);
      when Weighted_Newman                    => Update_Removed_Element_Weighted_Newman(Mi, E, L);
      when Weighted_Signed                    => Update_Removed_Element_Weighted_Signed(Mi, E, L);
      when Weighted_Uniform_Nullcase          => Update_Removed_Element_Weighted_Uniform_Nullcase(Mi, E, L);
      when Weighted_Local_Average             => Update_Removed_Element_Weighted_Local_Average(Mi, E, L);
      when Weighted_Uniform_Local_Average     => Update_Removed_Element_Weighted_Uniform_Local_Average(Mi, E, L);
      when Weighted_Links_Unweighted_Nullcase => Update_Removed_Element_Weighted_Links_Unweighted_Nullcase(Mi, E, L);
      when Weighted_No_Nullcase               => Update_Removed_Element_Weighted_No_Nullcase(Mi, E, L);
      when Weighted_Link_Rank                 => Update_Removed_Element_Weighted_Link_Rank(Mi, E, L);
      when Weighted_Bipartite_Path_Motif      => Update_Removed_Element_Weighted_Bipartite_Path_Motif(Mi, E, L);
      when Weighted_Bipartite_Path_Signed     => Update_Removed_Element_Weighted_Bipartite_Path_Signed(Mi, E, L);
    end case;
  end Update_Modularity_Removed_Element;

  -----------------------
  -- Update_Modularity --
  -----------------------

  procedure Update_Modularity(Mi: in Modularity_Info; L: in List; Mt: in Modularity_Type) is
    Lol: List_Of_Lists;
  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;
    Lol := List_Of_Lists_Of(L);
    if Mi.Size /= Number_Of_Elements(Lol) then
      raise Incompatible_Modules_Error;
    end if;

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
      when Weighted_Bipartite_Path_Motif      => Update_Weighted_Bipartite_Path_Motif(Mi, L);
      when Weighted_Bipartite_Path_Signed     => Update_Weighted_Bipartite_Path_Signed(Mi, L);
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
    for I in Mi.Q_Node'Range loop
      Modularity := Modularity + Mi.Q_Node(I).Total;
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
    for I in Mi.Q_Node'Range loop
      Modularity.Reward := Modularity.Reward + Mi.Q_Node(I).Reward;
      Modularity.Penalty := Modularity.Penalty + Mi.Q_Node(I).Penalty;
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

    return Mi.Q_Node(Index_Of(E));
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
    Sum_K_In, Sum_A: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Sum_K_In := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_K_In := Sum_K_In + Mi.To(J).Kr;
    end loop;

    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_A := Mi.Resistance;
      else
        Sum_A := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(Next(El));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_A := Sum_A + 1.0;
        end if;
      end loop;
      Restore(El);

      Re := Sum_A / Mi.Two_Mr;
      Pe := Mi.From(I).Kr * Sum_K_In / (Mi.Two_Mr * Mi.Two_Mr);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Sum_A: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Pe := Mi.Penalty_Coefficient * Num(Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_A := Mi.Resistance;
      else
        Sum_A := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(Next(El));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_A := Sum_A + 1.0;
        end if;
      end loop;
      Restore(El);

      Re := Num(Sum_A) / Mi.Two_Mr;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Lol := List_Of_Lists_Of(L);

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
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Sum_W, Sum_W_In_Pos, Sum_W_In_Neg: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Sum_W_In_Pos := 0.0;
    Sum_W_In_Neg := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_W_In_Pos := Sum_W_In_Pos + Mi.To(J).W_Pos;
      Sum_W_In_Neg := Sum_W_In_Neg + Mi.To(J).W_Neg;
    end loop;

    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Pe := Mi.Penalty_Coefficient * Num(Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Sum_W, Sum_Wa_K_In, Wa, Ka: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
        Ka := Mi.From(I).Kr + Mi.To(J).Kr;
        if Ka = 0.0 then
          Wa := 0.0;
        else
          Wa := (Mi.From(I).W + Mi.To(J).W) / Ka;
        end if;
        Sum_Wa_K_In := Sum_Wa_K_In + Mi.To(J).Kr * Wa;
      end loop;
      Restore(L);

      Re := Sum_W / Mi.Two_W;
      Pe := Mi.From(I).Kr * Sum_Wa_K_In / Mi.Two_La;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Sum_W, Sum_Wa, Wa, Ka: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
        Ka := Mi.From(I).Kr + Mi.To(J).Kr;
        if Ka = 0.0 then
          Wa := 0.0;
        else
          Wa := (Mi.From(I).W + Mi.To(J).W) / Ka;
        end if;
        Sum_Wa := Sum_Wa + Wa;
      end loop;
      Restore(L);

      Re := Sum_W / Mi.Two_W;
      Pe := Sum_Wa / Mi.Two_Ula;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Sum_K_In, Sum_W: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Sum_K_In := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_K_In := Sum_K_In + Mi.To(J).Kr;
    end loop;

    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
      Pe := Mi.From(I).Kr * Sum_K_In / (Mi.Two_Mr * Mi.Two_Mr);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);

    Pe := 0.0;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W := Mi.Resistance;
      else
        Sum_W := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
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
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
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
    Nr, N_L, Sum_Eigv, Sum_Trans, Sum_Re: Num;
    Re, Pe: Num;
  begin
    pragma Warnings(Off, El);
    Lol := List_Of_Lists_Of(L);

    Nr := Num(Mi.Size);
    N_L := Num(Number_Of_Elements(L));

    Sum_Eigv := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_Eigv := Sum_Eigv + Mi.Link_Rank_Eigenvec(J);
    end loop;

    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      Sum_Trans := 0.0;
      El := Edges_From(Get_Vertex(Mi.Gr_Trans, I));
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

      if Mi.From(I).W > 0.0 then
        Sum_Re := (1.0 - Mi.Teleportation) * Sum_Trans + Mi.Teleportation * N_L / Nr;
      else
        Sum_Re := N_L / Nr;
      end if;

      Re := Mi.Link_Rank_Eigenvec(I) * Sum_Re;
      Pe := Mi.Link_Rank_Eigenvec(I) * Sum_Eigv;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Link_Rank;

  ------------------------------------------
  -- Update_Weighted_Bipartite_Path_Motif --
  ------------------------------------------

  procedure Update_Weighted_Bipartite_Path_Motif(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_W_In, Sum_W_Path: Num;
    Re, Pe: Num;
  begin
    pragma Warnings(Off, El);
    Lol := List_Of_Lists_Of(L);

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
      if Mi.Resistance /= No_Resistance then
        Sum_W_Path := Mi.Resistance;
      else
        Sum_W_Path := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr_Path, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W_Path := Sum_W_Path + To_Num(Value(E));
        end if;
      end loop;
      Restore(El);

      Re := Sum_W_Path / Mi.Two_W_Path;
      Pe := Mi.From(I).W * Mi.From(I).Path_Null_Factor * Sum_W_In / Mi.Two_W_Path_Null;
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Bipartite_Path_Motif;

  -------------------------------------------
  -- Update_Weighted_Bipartite_Path_Signed --
  -------------------------------------------

  procedure Update_Weighted_Bipartite_Path_Signed(Mi: in Modularity_Info; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    E: Edge;
    I, J: Positive;
    Sum_W_In_Pos, Sum_W_In_Neg, Sum_W_Path: Num;
    Re, Pe: Num;
  begin
    pragma Warnings(Off, El);
    Lol := List_Of_Lists_Of(L);

    Sum_W_In_Pos := 0.0;
    Sum_W_In_Neg := 0.0;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Sum_W_In_Pos := Sum_W_In_Pos + Mi.To(J).W_Pos;
      Sum_W_In_Neg := Sum_W_In_Neg + Mi.To(J).W_Neg;
    end loop;

    Reset(L);
    while Has_Next_Element(L) loop
      I := Index_Of(Next_Element(L));
      if Mi.Resistance /= No_Resistance then
        Sum_W_Path := Mi.Resistance;
      else
        Sum_W_Path := 0.0;
      end if;
      El := Edges_From(Get_Vertex(Mi.Gr_Path, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_W_Path := Sum_W_Path + To_Num(Value(E));
        end if;
      end loop;
      Restore(El);

      Re := Sum_W_Path / (Mi.Two_W_Path_Pos + Mi.Two_W_Path_Neg);
      Pe := 0.0;
      if Mi.Two_W_Path_Null_Pos > 0.0 then
        Pe := Pe + Mi.From(I).W_Pos * Mi.From(I).Path_Null_Factor_PP * Sum_W_In_Pos;
        Pe := Pe + Mi.From(I).W_Neg * Mi.From(I).Path_Null_Factor_NN * Sum_W_In_Neg;
      end if;
      if Mi.Two_W_Path_Null_Neg > 0.0 then
        Pe := Pe - Mi.From(I).W_Pos * Mi.From(I).Path_Null_Factor_PN * Sum_W_In_Neg;
        Pe := Pe - Mi.From(I).W_Neg * Mi.From(I).Path_Null_Factor_NP * Sum_W_In_Pos;
      end if;
      Pe := Pe / (Mi.Two_W_Path_Null_Pos + Mi.Two_W_Path_Null_Neg);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(I).Reward := Re;
      Mi.Q_Node(I).Penalty := Pe;
      Mi.Q_Node(I).Total := Re - Pe;
    end loop;
    Restore(L);
  end Update_Weighted_Bipartite_Path_Signed;

  -----------------------------------------------
  -- Update_Inserted_Element_Unweighted_Newman --
  -----------------------------------------------

  procedure Update_Inserted_Element_Unweighted_Newman(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    I, J: Positive;
    Sum_K_In, Sum_A, Sum_A_Ini: Num;
    Pe_Inc_Fact, Pe_Inc, Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_A_Ini := 0.0;
    if Mi.From(I).Has_Self_Loop then
      Sum_A_Ini := Sum_A_Ini + 1.0;
    end if;
    if Mi.Resistance /= No_Resistance then
      Sum_A_Ini := Sum_A_Ini + Mi.Resistance;
    end if;

    Sum_A := Sum_A_Ini;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      J := Index_Of(Next(El));
      if Belongs_To(Get_Element(Lol, J), L) then
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + 1.0 / Mi.Two_Mr;
        Sum_A := Sum_A + 1.0;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_A := Sum_A_Ini;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(Next(El));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_A := Sum_A + 1.0;
        end if;
      end loop;
      Restore(El);
    end if;

    Sum_K_In := Mi.To(I).Kr;
    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).Kr / (Mi.Two_Mr * Mi.Two_Mr);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).Kr * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_K_In := Sum_K_In + Mi.To(J).Kr;
    end loop;
    Restore(L);

    Re := Sum_A / Mi.Two_Mr;
    Pe := Mi.From(I).Kr * Sum_K_In / (Mi.Two_Mr * Mi.Two_Mr);
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Unweighted_Newman;

  ---------------------------------------------------------
  -- Update_Inserted_Element_Unweighted_Uniform_Nullcase --
  ---------------------------------------------------------

  procedure Update_Inserted_Element_Unweighted_Uniform_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    I, J: Positive;
    Sum_A, Sum_A_Ini: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_A_Ini := 0.0;
    if Mi.From(I).Has_Self_Loop then
      Sum_A_Ini := Sum_A_Ini + 1.0;
    end if;
    if Mi.Resistance /= No_Resistance then
      Sum_A_Ini := Sum_A_Ini + Mi.Resistance;
    end if;

    Pe := Mi.Penalty_Coefficient * Num(1 + Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));

    Sum_A := Sum_A_Ini;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      J := Index_Of(Next(El));
      if Belongs_To(Get_Element(Lol, J), L) then
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + 1.0 / Mi.Two_Mr;
        Sum_A := Sum_A + 1.0;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_A := Sum_A_Ini;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(Next(El));
        if Belongs_To(Get_Element(Lol, J), L) then
          Sum_A := Sum_A + 1.0;
        end if;
      end loop;
      Restore(El);
    end if;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Mi.Q_Node(J).Penalty := Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);

    Re := Sum_A / Mi.Two_Mr;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Unweighted_Uniform_Nullcase;

  ---------------------------------------------
  -- Update_Inserted_Element_Weighted_Newman --
  ---------------------------------------------

  procedure Update_Inserted_Element_Weighted_Newman(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Sum_W, Sum_W_In: Num;
    Pe_Inc_Fact, Pe_Inc, Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / Mi.Two_W;
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Sum_W_In := Mi.To(I).W;
    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).W / (Mi.Two_W * Mi.Two_W);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).W * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_W_In := Sum_W_In + Mi.To(J).W;
    end loop;
    Restore(L);

    Re := Sum_W / Mi.Two_W;
    Pe := Mi.From(I).W * Sum_W_In / (Mi.Two_W * Mi.Two_W);
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Newman;

  ---------------------------------------------
  -- Update_Inserted_Element_Weighted_Signed --
  ---------------------------------------------

  procedure Update_Inserted_Element_Weighted_Signed(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Sum_W, Sum_W_In_Pos, Sum_W_In_Neg: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / (Mi.Two_W_Pos + Mi.Two_W_Neg);
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Sum_W_In_Pos := Mi.To(I).W_Pos;
    Sum_W_In_Neg := Mi.To(I).W_Neg;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe := 0.0;
      if Mi.Two_W_Pos > 0.0 then
        Pe := Pe + Mi.From(J).W_Pos * Mi.To(I).W_Pos / Mi.Two_W_Pos;
      end if;
      if Mi.Two_W_Neg > 0.0 then
        Pe := Pe - Mi.From(J).W_Neg * Mi.To(I).W_Neg / Mi.Two_W_Neg;
      end if;
      Pe := Pe / (Mi.Two_W_Pos + Mi.Two_W_Neg);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_W_In_Pos := Sum_W_In_Pos + Mi.To(J).W_Pos;
      Sum_W_In_Neg := Sum_W_In_Neg + Mi.To(J).W_Neg;
    end loop;
    Restore(L);

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
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Signed;

  -------------------------------------------------------
  -- Update_Inserted_Element_Weighted_Uniform_Nullcase --
  -------------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Uniform_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Sum_W: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Pe := Mi.Penalty_Coefficient * Num(1 + Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / Mi.Two_W;
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Mi.Q_Node(J).Penalty := Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);

    Re := Sum_W / Mi.Two_W;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Uniform_Nullcase;

  ----------------------------------------------------
  -- Update_Inserted_Element_Weighted_Local_Average --
  ----------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Local_Average(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Sum_W, Sum_Wa_K_In, Wa, Ka: Num;
    Pe_Inc_Fact, Pe_Inc, Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / Mi.Two_W;
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).Kr / Mi.Two_La;
    Ka := Mi.From(I).Kr + Mi.To(I).Kr;
    if Ka = 0.0 then
      Wa := 0.0;
    else
      Wa := (Mi.From(I).W + Mi.To(I).W) / Ka;
    end if;
    Sum_Wa_K_In := Mi.To(I).Kr * Wa;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Ka := Mi.From(J).Kr + Mi.To(I).Kr;
      if Ka = 0.0 then
        Wa := 0.0;
      else
        Wa := (Mi.From(J).W + Mi.To(I).W) / Ka;
      end if;
      Pe_Inc := Mi.From(J).Kr * Pe_Inc_Fact * Wa;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Ka := Mi.From(I).Kr + Mi.To(J).Kr;
      if Ka = 0.0 then
        Wa := 0.0;
      else
        Wa := (Mi.From(I).W + Mi.To(J).W) / Ka;
      end if;
      Sum_Wa_K_In := Sum_Wa_K_In + Mi.To(J).Kr * Wa;
    end loop;
    Restore(L);

    Re := Sum_W / Mi.Two_W;
    Pe := Mi.From(I).Kr * Sum_Wa_K_In / Mi.Two_La;
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Local_Average;

  ------------------------------------------------------------
  -- Update_Inserted_Element_Weighted_Uniform_Local_Average --
  ------------------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Uniform_Local_Average(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Sum_W, Sum_Wa, Wa, Ka: Num;
    Pe_Inc_Fact, Pe_Inc, Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / Mi.Two_W;
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Pe_Inc_Fact := Mi.Penalty_Coefficient / Mi.Two_Ula;
    Ka := Mi.From(I).Kr + Mi.To(I).Kr;
    if Ka = 0.0 then
      Wa := 0.0;
    else
      Wa := (Mi.From(I).W + Mi.To(I).W) / Ka;
    end if;
    Sum_Wa := Wa;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Ka := Mi.From(J).Kr + Mi.To(I).Kr;
      if Ka = 0.0 then
        Wa := 0.0;
      else
        Wa := (Mi.From(J).W + Mi.To(I).W) / Ka;
      end if;
      Pe_Inc := Pe_Inc_Fact * Wa;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Ka := Mi.From(I).Kr + Mi.To(J).Kr;
      if Ka = 0.0 then
        Wa := 0.0;
      else
        Wa := (Mi.From(I).W + Mi.To(J).W) / Ka;
      end if;
      Sum_Wa := Sum_Wa + Wa;
    end loop;
    Restore(L);

    Re := Sum_W / Mi.Two_W;
    Pe := Sum_Wa / Mi.Two_Ula;
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Uniform_Local_Average;

  ----------------------------------------------------------------
  -- Update_Inserted_Element_Weighted_Links_Unweighted_Nullcase --
  ----------------------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Links_Unweighted_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Sum_K_In, Wh, Sum_W: Num;
    Pe_Inc_Fact, Pe_Inc, Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / Mi.Two_W;
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Sum_K_In := Mi.To(I).Kr;
    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).Kr / (Mi.Two_Mr * Mi.Two_Mr);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).Kr * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_K_In := Sum_K_In + Mi.To(J).Kr;
    end loop;
    Restore(L);

    Re := Sum_W / Mi.Two_W;
    Pe := Mi.From(I).Kr * Sum_K_In / (Mi.Two_Mr * Mi.Two_Mr);
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Links_Unweighted_Nullcase;

  --------------------------------------------------
  -- Update_Inserted_Element_Weighted_No_Nullcase --
  --------------------------------------------------

  procedure Update_Inserted_Element_Weighted_No_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Sum_W: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Pe := 0.0;

    Sum_W := Mi.From(I).Self_Loop;
    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh / Mi.Two_W;
        Sum_W := Sum_W + Wh;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W := Mi.From(I).Self_Loop;
      El := Edges_From(Get_Vertex(Mi.Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh := To_Num(Eg.Value);
          Sum_W := Sum_W + Wh;
        end if;
      end loop;
      Restore(El);
    end if;

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Mi.Q_Node(J).Penalty := Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);

    Re := Sum_W / Mi.Two_W;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_No_Nullcase;

  ------------------------------------------------
  -- Update_Inserted_Element_Weighted_Link_Rank --
  ------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Link_Rank(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Graphs_Double.Edges_List;
    Eg: Graphs_Double.Edge;
    I, J: Positive;
    Nr, N_L, Wh, Sum_Eigv, Sum_Trans, Sum_Re: Num;
    Re, Pe: Num;
  begin
    pragma Warnings(Off, El);
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Nr := Num(Mi.Size);
    N_L := Num(Number_Of_Elements(L));

    El := Edges_To(Get_Vertex(Mi.Gr_Trans, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := Num(Value(Eg));
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Mi.Link_Rank_Eigenvec(J) * (1.0 - Mi.Teleportation) * Wh;
      end if;
    end loop;
    Restore(El);

    Sum_Trans := 0.0;
    El := Edges_From(Get_Vertex(Mi.Gr_Trans, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(To(Eg));
      if Belongs_To(Get_Element(Lol, J), L) or else J = I then
        Sum_Trans := Sum_Trans + Num(Value(Eg));
      end if;
    end loop;
    Restore(El);

    if Mi.From(I).W > 0.0 then
      Sum_Re := (1.0 - Mi.Teleportation) * Sum_Trans + Mi.Teleportation * (N_L + 1.0) / Nr;
    else
      Sum_Re := (N_L + 1.0) / Nr;
    end if;

    Sum_Eigv := Mi.Link_Rank_Eigenvec(I);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      if Mi.From(J).W > 0.0 then
        Re := Mi.Link_Rank_Eigenvec(J) * Mi.Teleportation / Nr;
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Re;
      else
        Re := Mi.Link_Rank_Eigenvec(J) / Nr;
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Re;
      end if;
      Pe := Mi.Link_Rank_Eigenvec(J) * Mi.Link_Rank_Eigenvec(I);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_Eigv := Sum_Eigv + Mi.Link_Rank_Eigenvec(J);
    end loop;
    Restore(L);

    Re := Mi.Link_Rank_Eigenvec(I) * Sum_Re;
    Pe := Mi.Link_Rank_Eigenvec(I) * Sum_Eigv;
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Link_Rank;

  -----------------------------------------------------------
  -- Update_Inserted_Element_Weighted_Bipartite_Path_Motif --
  -----------------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Bipartite_Path_Motif(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh_Path, Sum_W_Path, Sum_W_In: Num;
    Pe_Inc_Fact, Pe_Inc, Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W_Path := Mi.From(I).Self_Loop_Path;
    El := Edges_To(Get_Vertex(Mi.Gr_Path, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh_Path := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh_Path / Mi.Two_W_Path;
        Sum_W_Path := Sum_W_Path + Wh_Path;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W_Path := Mi.From(I).Self_Loop_Path;
      El := Edges_From(Get_Vertex(Mi.Gr_Path, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh_Path := To_Num(Eg.Value);
          Sum_W_Path := Sum_W_Path + Wh_Path;
        end if;
      end loop;
      Restore(El);
    end if;

    Sum_W_In := Mi.To(I).W;
    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).W * Mi.To(I).Path_Null_Factor / Mi.Two_W_Path_Null;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).W * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_W_In := Sum_W_In + Mi.To(J).W;
    end loop;
    Restore(L);

    Re := Sum_W_Path / Mi.Two_W_Path;
    Pe := Mi.From(I).W * Mi.From(I).Path_Null_Factor * Sum_W_In / Mi.Two_W_Path_Null;
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Bipartite_Path_Motif;

  ------------------------------------------------------------
  -- Update_Inserted_Element_Weighted_Bipartite_Path_Signed --
  ------------------------------------------------------------

  procedure Update_Inserted_Element_Weighted_Bipartite_Path_Signed(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh_Path, Sum_W_Path, Sum_W_In_Pos, Sum_W_In_Neg: Num;
    Re, Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Sum_W_Path := Mi.From(I).Self_Loop_Path;
    El := Edges_To(Get_Vertex(Mi.Gr_Path, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh_Path := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward + Wh_Path / (Mi.Two_W_Path_Pos + Mi.Two_W_Path_Neg);
        Sum_W_Path := Sum_W_Path + Wh_Path;
      end if;
    end loop;
    Restore(El);

    if Mi.Directed then
      Sum_W_Path := Mi.From(I).Self_Loop_Path;
      El := Edges_From(Get_Vertex(Mi.Gr_Path, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        Eg := Next(El);
        J := Index_Of(To(Eg));
        if Belongs_To(Get_Element(Lol, J), L) then
          Wh_Path := To_Num(Eg.Value);
          Sum_W_Path := Sum_W_Path + Wh_Path;
        end if;
      end loop;
      Restore(El);
    end if;

    Sum_W_In_Pos := Mi.To(I).W_Pos;
    Sum_W_In_Neg := Mi.To(I).W_Neg;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe := 0.0;
      if Mi.Two_W_Path_Null_Pos > 0.0 then
        Pe := Pe + Mi.From(J).W_Pos * Mi.From(J).Path_Null_Factor_PP * Mi.To(I).W_Pos;
        Pe := Pe + Mi.From(J).W_Neg * Mi.From(J).Path_Null_Factor_NN * Mi.To(I).W_Neg;
      end if;
      if Mi.Two_W_Path_Null_Neg > 0.0 then
        Pe := Pe - Mi.From(J).W_Pos * Mi.From(J).Path_Null_Factor_PN * Mi.To(I).W_Neg;
        Pe := Pe - Mi.From(J).W_Neg * Mi.From(J).Path_Null_Factor_NP * Mi.To(I).W_Pos;
      end if;
      Pe := Pe / (Mi.Two_W_Path_Null_Pos + Mi.Two_W_Path_Null_Neg);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty + Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
      Sum_W_In_Pos := Sum_W_In_Pos + Mi.To(J).W_Pos;
      Sum_W_In_Neg := Sum_W_In_Neg + Mi.To(J).W_Neg;
    end loop;
    Restore(L);

    Re := Sum_W_Path / (Mi.Two_W_Path_Pos + Mi.Two_W_Path_Neg);
    Pe := 0.0;
    if Mi.Two_W_Path_Null_Pos > 0.0 then
      Pe := Pe + Mi.From(I).W_Pos * Mi.From(I).Path_Null_Factor_PP * Sum_W_In_Pos;
      Pe := Pe + Mi.From(I).W_Neg * Mi.From(I).Path_Null_Factor_NN * Sum_W_In_Neg;
    end if;
    if Mi.Two_W_Path_Null_Neg > 0.0 then
      Pe := Pe - Mi.From(I).W_Pos * Mi.From(I).Path_Null_Factor_PN * Sum_W_In_Neg;
      Pe := Pe - Mi.From(I).W_Neg * Mi.From(I).Path_Null_Factor_NP * Sum_W_In_Pos;
    end if;
    Pe := Pe / (Mi.Two_W_Path_Null_Pos + Mi.Two_W_Path_Null_Neg);
    Pe := Mi.Penalty_Coefficient * Pe;
    Mi.Q_Node(I).Reward := Re;
    Mi.Q_Node(I).Penalty := Pe;
    Mi.Q_Node(I).Total := Re - Pe;
  end Update_Inserted_Element_Weighted_Bipartite_Path_Signed;

  ----------------------------------------------
  -- Update_Removed_Element_Unweighted_Newman --
  ----------------------------------------------

  procedure Update_Removed_Element_Unweighted_Newman(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    I, J: Positive;
    Pe_Inc_Fact, Pe_Inc: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      J := Index_Of(Next(El));
      if Belongs_To(Get_Element(Lol, J), L) then
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - 1.0 / Mi.Two_Mr;
      end if;
    end loop;
    Restore(El);

    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).Kr / (Mi.Two_Mr * Mi.Two_Mr);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).Kr * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Unweighted_Newman;

  --------------------------------------------------------
  -- Update_Removed_Element_Unweighted_Uniform_Nullcase --
  --------------------------------------------------------

  procedure Update_Removed_Element_Unweighted_Uniform_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    I, J: Positive;
    Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Pe := Mi.Penalty_Coefficient * Num(Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      J := Index_Of(Next(El));
      if Belongs_To(Get_Element(Lol, J), L) then
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - 1.0 / Mi.Two_Mr;
      end if;
    end loop;
    Restore(El);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Mi.Q_Node(J).Penalty := Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Unweighted_Uniform_Nullcase;

  --------------------------------------------
  -- Update_Removed_Element_Weighted_Newman --
  --------------------------------------------

  procedure Update_Removed_Element_Weighted_Newman(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh: Num;
    Pe_Inc_Fact, Pe_Inc: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / Mi.Two_W;
      end if;
    end loop;
    Restore(El);

    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).W / (Mi.Two_W * Mi.Two_W);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).W * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Newman;

  --------------------------------------------
  -- Update_Removed_Element_Weighted_Signed --
  --------------------------------------------

  procedure Update_Removed_Element_Weighted_Signed(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh: Num;
    Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / (Mi.Two_W_Pos + Mi.Two_W_Neg);
      end if;
    end loop;
    Restore(El);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe := 0.0;
      if Mi.Two_W_Pos > 0.0 then
        Pe := Pe + Mi.From(J).W_Pos * Mi.To(I).W_Pos / Mi.Two_W_Pos;
      end if;
      if Mi.Two_W_Neg > 0.0 then
        Pe := Pe - Mi.From(J).W_Neg * Mi.To(I).W_Neg / Mi.Two_W_Neg;
      end if;
      Pe := Pe / (Mi.Two_W_Pos + Mi.Two_W_Neg);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Signed;

  ------------------------------------------------------
  -- Update_Removed_Element_Weighted_Uniform_Nullcase --
  ------------------------------------------------------

  procedure Update_Removed_Element_Weighted_Uniform_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh: Num;
    Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Pe := Mi.Penalty_Coefficient * Num(Number_Of_Elements(L)) / (Num(Mi.Size) * Num(Mi.Size));

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / Mi.Two_W;
      end if;
    end loop;
    Restore(El);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Mi.Q_Node(J).Penalty := Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Uniform_Nullcase;

  ---------------------------------------------------
  -- Update_Removed_Element_Weighted_Local_Average --
  ---------------------------------------------------

  procedure Update_Removed_Element_Weighted_Local_Average(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Wa, Ka: Num;
    Pe_Inc_Fact, Pe_Inc: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / Mi.Two_W;
      end if;
    end loop;
    Restore(El);

    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).Kr / Mi.Two_La;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Ka := Mi.From(J).Kr + Mi.To(I).Kr;
      if Ka = 0.0 then
        Wa := 0.0;
      else
        Wa := (Mi.From(J).W + Mi.To(I).W) / Ka;
      end if;
      Pe_Inc := Mi.From(J).Kr * Pe_Inc_Fact * Wa;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Local_Average;

  -----------------------------------------------------------
  -- Update_Removed_Element_Weighted_Uniform_Local_Average --
  -----------------------------------------------------------

  procedure Update_Removed_Element_Weighted_Uniform_Local_Average(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh, Wa, Ka: Num;
    Pe_Inc_Fact, Pe_Inc: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / Mi.Two_W;
      end if;
    end loop;
    Restore(El);

    Pe_Inc_Fact := Mi.Penalty_Coefficient / Mi.Two_Ula;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Ka := Mi.From(J).Kr + Mi.To(I).Kr;
      if Ka = 0.0 then
        Wa := 0.0;
      else
        Wa := (Mi.From(J).W + Mi.To(I).W) / Ka;
      end if;
      Pe_Inc := Pe_Inc_Fact * Wa;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Uniform_Local_Average;

  ---------------------------------------------------------------
  -- Update_Removed_Element_Weighted_Links_Unweighted_Nullcase --
  ---------------------------------------------------------------

  procedure Update_Removed_Element_Weighted_Links_Unweighted_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh: Num;
    Pe_Inc_Fact, Pe_Inc: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / Mi.Two_W;
      end if;
    end loop;
    Restore(El);

    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).Kr / (Mi.Two_Mr * Mi.Two_Mr);
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).Kr * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Links_Unweighted_Nullcase;

  -------------------------------------------------
  -- Update_Removed_Element_Weighted_No_Nullcase --
  -------------------------------------------------

  procedure Update_Removed_Element_Weighted_No_Nullcase(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh: Num;
    Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    Pe := 0.0;

    El := Edges_To(Get_Vertex(Mi.Gr, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh / Mi.Two_W;
      end if;
    end loop;
    Restore(El);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Mi.Q_Node(J).Penalty := Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_No_Nullcase;

  -----------------------------------------------
  -- Update_Removed_Element_Weighted_Link_Rank --
  -----------------------------------------------

  procedure Update_Removed_Element_Weighted_Link_Rank(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Graphs_Double.Edges_List;
    Eg: Graphs_Double.Edge;
    I, J: Positive;
    Nr, Wh: Num;
    Re, Pe: Num;
  begin
    pragma Warnings(Off, El);
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);
    Nr := Num(Mi.Size);

    El := Edges_To(Get_Vertex(Mi.Gr_Trans, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh := Num(Value(Eg));
        Re := Mi.Link_Rank_Eigenvec(J) * (1.0 - Mi.Teleportation) * Wh;
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Re;
      end if;
    end loop;
    Restore(El);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      if Mi.From(J).W > 0.0 then
        Re := Mi.Link_Rank_Eigenvec(J) * Mi.Teleportation / Nr;
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Re;
      else
        Re := Mi.Link_Rank_Eigenvec(J) / Nr;
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Re;
      end if;
      Pe := Mi.Link_Rank_Eigenvec(J) * Mi.Link_Rank_Eigenvec(I);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Link_Rank;

  ----------------------------------------------------------
  -- Update_Removed_Element_Weighted_Bipartite_Path_Motif --
  ----------------------------------------------------------

  procedure Update_Removed_Element_Weighted_Bipartite_Path_Motif(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh_Path: Num;
    Pe_Inc_Fact, Pe_Inc: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr_Path, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh_Path := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh_Path / Mi.Two_W_Path;
      end if;
    end loop;
    Restore(El);

    Pe_Inc_Fact := Mi.Penalty_Coefficient * Mi.To(I).W * Mi.To(I).Path_Null_Factor / Mi.Two_W_Path_Null;
    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe_Inc := Mi.From(J).W * Pe_Inc_Fact;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe_Inc;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Bipartite_Path_Motif;

  -----------------------------------------------------------
  -- Update_Removed_Element_Weighted_Bipartite_Path_Signed --
  -----------------------------------------------------------

  procedure Update_Removed_Element_Weighted_Bipartite_Path_Signed(Mi: in Modularity_Info; E: in Element; L: in List) is
    Lol: List_Of_Lists;
    El: Edges_List;
    Eg: Edge;
    I, J: Positive;
    Wh_Path: Num;
    Pe: Num;
  begin
    Lol := List_Of_Lists_Of(L);
    I := Index_Of(E);

    El := Edges_To(Get_Vertex(Mi.Gr_Path, I));
    Save(El);
    Reset(El);
    while Has_Next(El) loop
      Eg := Next(El);
      J := Index_Of(From(Eg));
      if Belongs_To(Get_Element(Lol, J), L) then
        Wh_Path := To_Num(Eg.Value);
        Mi.Q_Node(J).Reward := Mi.Q_Node(J).Reward - Wh_Path / (Mi.Two_W_Path_Pos + Mi.Two_W_Path_Neg);
      end if;
    end loop;
    Restore(El);

    Save(L);
    Reset(L);
    while Has_Next_Element(L) loop
      J := Index_Of(Next_Element(L));
      Pe := 0.0;
      if Mi.Two_W_Path_Null_Pos > 0.0 then
        Pe := Pe + Mi.From(J).W_Pos * Mi.From(J).Path_Null_Factor_PP * Mi.To(I).W_Pos;
        Pe := Pe + Mi.From(J).W_Neg * Mi.From(J).Path_Null_Factor_NN * Mi.To(I).W_Neg;
      end if;
      if Mi.Two_W_Path_Null_Neg > 0.0 then
        Pe := Pe - Mi.From(J).W_Pos * Mi.From(J).Path_Null_Factor_PN * Mi.To(I).W_Neg;
        Pe := Pe - Mi.From(J).W_Neg * Mi.From(J).Path_Null_Factor_NP * Mi.To(I).W_Pos;
      end if;
      Pe := Pe / (Mi.Two_W_Path_Null_Pos + Mi.Two_W_Path_Null_Neg);
      Pe := Mi.Penalty_Coefficient * Pe;
      Mi.Q_Node(J).Penalty := Mi.Q_Node(J).Penalty - Pe;
      Mi.Q_Node(J).Total := Mi.Q_Node(J).Reward - Mi.Q_Node(J).Penalty;
    end loop;
    Restore(L);
  end Update_Removed_Element_Weighted_Bipartite_Path_Signed;

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
            Pij := To_Num(E.Value) / Wi;
          end if;
          Add_Edge(Vf, Vt, Double(Pij));
        end loop;
        Restore(El);
      end if;
    end loop;
  end Transitions_Graph;

  ----------------------------------------
  -- Link_Rank_Left_Leading_Eigenvector --
  ----------------------------------------

  procedure Link_Rank_Left_Leading_Eigenvector(Mi: in Modularity_Info) is

    function Left_Product(Vec: in PNums; Gr: in Graphs_Double.Graph; N: in Natural; Teleport: in Num) return Nums is
      Prod: Nums(1..N);
      I: Positive;
      Vf, Vt: Graphs_Double.Vertex;
      E: Graphs_Double.Edge;
      El: Graphs_Double.Edges_List;
      Teleport_Term, Nr: Num;
    begin
      pragma Warnings(Off, El);
      -- teleportation term
      Nr := Num(N);
      Teleport_Term := 0.0;
      for I in 1..N loop
        Vf := Get_Vertex(Gr, I);
        if Degree_From(Vf) = 0 then
          Teleport_Term := Teleport_Term + Vec(I) / Nr;
        else
          Teleport_Term := Teleport_Term + Teleport * Vec(I) / Nr;
        end if;
      end loop;
      -- transitions term
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
        Prod(J) := (1.0 - Teleport) * Prod(J) + Teleport_Term;
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
      Convergence_Epsilon: constant Num := Num_Epsilon;
    begin
      for I in Vec1'Range loop
        if abs (Vec1(I) - Vec2(I)) > Convergence_Epsilon then
          return False;
        end if;
      end loop;
      return True;
    end Converged;

    N: Natural;
    Gr: Graphs_Double.Graph;
    Eigv, Eigv_Prev: PNums;

  begin
    if Mi = null then
      raise Uninitialized_Modularity_Info_Error;
    end if;

    N := Mi.Size;
    Gr := Mi.Gr_Trans;

    if not Is_Initialized(Gr) then
      raise Uninitialized_Graph_Error;
    end if;

    Eigv_Prev := Alloc(1, N);
    Eigv      := Alloc(1, N);
    Eigv_Prev.all := (1 => 1.0, others => 0.0);
    Eigv.all      := (others => 1.0 / Num(N));

    while not Converged(Eigv_Prev, Eigv) loop
      Eigv_Prev.all := Eigv.all;
      Eigv.all := Left_Product(Eigv, Gr, N, Mi.Teleportation);
      Normalize(Eigv);
    end loop;
    Free(Eigv_Prev);

    Mi.Link_Rank_Eigenvec := Eigv;
  end Link_Rank_Left_Leading_Eigenvector;

end Graphs.Operations.Modularities;
