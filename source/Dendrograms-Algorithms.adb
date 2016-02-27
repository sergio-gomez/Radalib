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


-- @filename Dendrograms-Algorithms.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/05/2013
-- @revision 19/02/2016
-- @brief Dendrograms Algorithms

with Ada.Unchecked_Deallocation;
with Finite_Disjoint_Lists.Algorithms; use Finite_Disjoint_Lists.Algorithms;

package body Dendrograms.Algorithms is

  -------------------------
  -- Get_Dendrogram_Type --
  -------------------------

  function Get_Dendrogram_Type(Name: in String) return Dendrogram_Type is
  begin
    if    To_Uppercase(Name) = "BD" or To_Lowercase(Name) = "binary_dendrogram" then
      return Binary_Dendrogram;
    elsif To_Uppercase(Name) = "MD" or To_Lowercase(Name) = "multidendrogram"   then
      return Multidendrogram;
    else
      raise Unknown_Dendrogram_Type_Error;
    end if;
  end Get_Dendrogram_Type;

  -------------
  -- To_Name --
  -------------

  function To_Name(Dt: in Dendrogram_Type; Short: in Boolean := False) return String is
  begin
    if Short then
      case Dt is
        when Binary_Dendrogram => return "BD";
        when Multidendrogram   => return "MD";
      end case;
    else
      return Capitalize(Dendrogram_Type'Image(Dt));
    end if;
  end To_Name;

  ------------------------
  -- Get_Proximity_Type --
  ------------------------

  function Get_Proximity_Type(Name: in String) return Proximity_Type is
  begin
    if    To_Uppercase(Name) = "D" or To_Uppercase(Name) = "DIST" or To_Lowercase(Name) = "distance"   then
      return Distance;
    elsif To_Uppercase(Name) = "S" or To_Uppercase(Name) = "SIM"  or To_Lowercase(Name) = "similarity" then
      return Similarity;
    else
      raise Unknown_Proximity_Type_Error;
    end if;
  end Get_Proximity_Type;

  -------------
  -- To_Name --
  -------------

  function To_Name(Pt: in Proximity_Type; Short: in Boolean := False) return String is
  begin
    if Short then
      case Pt is
        when Distance   => return "DIST";
        when Similarity => return "SIM";
      end case;
    else
      return Capitalize(Proximity_Type'Image(Pt));
    end if;
  end To_Name;

  -------------------------
  -- Get_Clustering_Type --
  -------------------------

  function Get_Clustering_Type(Name: in String) return Clustering_Type is
  begin
    if    To_Uppercase(Name) = "SL" or To_Lowercase(Name) = "single_linkage"      then
      return Single_Linkage;
    elsif To_Uppercase(Name) = "CL" or To_Lowercase(Name) = "complete_linkage"    then
      return Complete_Linkage;
    elsif To_Uppercase(Name) = "UA" or To_Lowercase(Name) = "unweighted_average"  or To_Uppercase(Name) = "UPGMA" then
      return Unweighted_Average;
    elsif To_Uppercase(Name) = "WA" or To_Lowercase(Name) = "weighted_average"    or To_Uppercase(Name) = "WPGMA" then
      return Weighted_Average;
    elsif To_Uppercase(Name) = "UC" or To_Lowercase(Name) = "unweighted_centroid" then
      return Unweighted_Centroid;
    elsif To_Uppercase(Name) = "WC" or To_Lowercase(Name) = "weighted_centroid"   then
      return Weighted_Centroid;
    elsif To_Uppercase(Name) = "WD" or To_Lowercase(Name) = "ward"                then
      return Ward;
    else
      raise Unknown_Clustering_Type_Error;
    end if;
  end Get_Clustering_Type;

  -------------
  -- To_Name --
  -------------

  function To_Name(Ct: in Clustering_Type; Short: in Boolean := False) return String is
  begin
    if Short then
      case Ct is
        when Single_Linkage      => return "SL";
        when Complete_Linkage    => return "CL";
        when Unweighted_Average  => return "UA";
        when Weighted_Average    => return "WA";
        when Unweighted_Centroid => return "UC";
        when Weighted_Centroid   => return "WC";
        when Ward                => return "WD";
      end case;
    else
      return Capitalize(Clustering_Type'Image(Ct));
    end if;
  end To_Name;

  ----------------
  -- Round_Data --
  ----------------

  function Round_Data(Data: in PDoubless; Precision: in Natural) return PDoubless is
    Rdata: PDoubless;
  begin
    if Data = null then
      return null;
    end if;

    Rdata := Alloc(Data'First(1), Data'Last(1), Data'First(2), Data'Last(2));

    for I in Data'Range(1) loop
      for J in Data'Range(2) loop
        Rdata(I, J) := Round(Data(I, J), Precision);
      end loop;
    end loop;
    return Rdata;
  end Round_Data;

  ----------------
  -- Round_Data --
  ----------------

  procedure Round_Data(Data: in PDoubless; Precision: in Natural) is
  begin
    for I in Data'Range(1) loop
      for J in Data'Range(2) loop
        Data(I, J) := Round(Data(I, J), Precision);
      end loop;
    end loop;
  end Round_Data;

  -----------------------------
  -- Hierarchical_Clustering --
  -----------------------------

  procedure Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Ct: in Clustering_Type; Precision: in Natural; Md: out Dendrogram) is

    No_Value: constant Double := Double'First;

    N, Nc: Positive;
    Off1, Off2, Offn: Integer;
    Prox: PPsDoubles;
    Clus, Clus_Prev: PClusters;
    Nod_Inf: Node_Info;
    Name: Ustring;
    Total_Clus, Num_Clus: Positive;
    Lol: List_Of_Lists;
    Internal_Names: Boolean;

  begin
    Check_Data(Data, Names);
    N := Data'Length(1);

    -- Initialize proximities between all pairs of clusters
    Off1 := Data'First(1) - 1;
    Off2 := Data'First(2) - 1;

    Prox := Alloc_Upper(1, 2 * N - 1);
    for I in Prox'Range loop
      Prox(I).all := (others => No_Value);
    end loop;
    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        Prox(I)(J) := Data(Off1 + I, Off2 + J);
      end loop;
    end loop;

    -- Initialize clusters
    Total_Clus := N;
    Num_Clus   := N;

    Nc := 2 * N - 1;
    Clus := new Clusters(1..Nc);
    Clus_Prev := new Clusters(1..Nc);

    if Names /= null then
      Offn := Names'First - 1;
    end if;

    for C in 1..N loop
      if Names /= null then
        Name := Names(Offn + C);
      else
        Name := S2U(I2S(C));
      end if;
      Set_Node_Info(Nod_Inf, Name => Name, Position => 0.0, Height => 0.0, Width => 0.0, Length => 0.0, Margin => 0.0, Is_Leaf => True, Num_Leaves => 1, Id => C);
      Clus(C) := (Id => C, St => New_Tree(Nod_Inf), Num_Leaves => 1);
    end loop;
    for C in (N+1)..Nc loop
      Set_Node_Info(Nod_Inf, Name => Null_Ustring, Position => 0.0, Height => 0.0, Width => 0.0, Length => 0.0, Margin => 0.0, Is_Leaf => False, Num_Leaves => 0, Id => C);
      Clus(C) := (Id => C, St => New_Tree(Nod_Inf), Num_Leaves => 0);
    end loop;

    -- Agglomerative Hierarchical Clustering
    Internal_Names := not Equal(Capitalize(Internal_Node_Name_Prefix), No_Internal_Node_Name);
    while Num_Clus > 1 loop
      Clus_Prev.all := Clus.all;

      Find_Joining_Clusters(Clus, Num_Clus, Prox, Pt, Precision, Lol);
      Num_Clus := Number_Of_Lists(Lol);

      Join_Clusters(Lol, Clus, Clus_Prev, Total_Clus, Prox, Pt, Ct, Precision, Internal_Names);
      Free(Lol);
    end loop;

    -- The final MultiDendrogram
    Md := Clus(1).St;
    Set_Lengths_And_Leaves_Heights(Md, Pt, Precision);
    Set_Positions_And_Widths(Md);

    -- Finalize
    Free(Prox);
    Free(Clus);
    Free(Clus_Prev);

  end Hierarchical_Clustering;

  -----------------------------
  -- Hierarchical_Clustering --
  -----------------------------

  procedure Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Ct: in Clustering_Type; Precision: in Natural; Bds: out List_Of_Dendrograms) is

    procedure Save_Dendrogram(T: in Dendrogram) is
    begin
      Add_Last(Clone(T), Bds);
    end Save_Dendrogram;

    procedure Hierarchical_Clust is new Generic_Hierarchical_Clustering(Save_Dendrogram);

  begin
    Initialize(Bds);
    Hierarchical_Clust(Data, Names, Pt, Ct, Precision);
  end Hierarchical_Clustering;

  -------------------------------------
  -- Generic_Hierarchical_Clustering --
  -------------------------------------

  procedure Generic_Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Ct: in Clustering_Type; Precision: in Natural) is

    procedure Recursive_Clustering(Clus: in PClusters; Num_Clus, Total_Clus: in Positive; Prox: in PPsDoubles; Internal_Names: in Boolean) is
      T: Dendrogram;
      Clus_New: PClusters;
      Num_Cl, Total_Cl: Positive;
      Tp: Tied_Pairs;
      Lol: List_Of_Lists;
    begin
      if Num_Clus = 1 then
        T := Clus(1).St;
        Set_Lengths_And_Leaves_Heights(T, Pt, Precision);
        Set_Positions_And_Widths(T);
        Handler(T);
      else
        Find_Tied_Pairs(Clus, Num_Clus, Prox, Pt, Precision, Tp);
        Initialize(Lol, Num_Clus, Isolated_Initialization);

        for Index in 1..Tp.Num_Combi loop
          Build_Pairs_Combination(Tp, Index, Lol);

          Clus_New := new Clusters(Clus'Range);
          Clus_New.all := Clus.all;
          Num_Cl   := Number_Of_Lists(Lol);
          Total_Cl := Total_Clus;
          Join_Clusters(Lol, Clus_New, Clus, Total_Cl, Prox, Pt, Ct, Precision, Internal_Names);

          Recursive_Clustering(Clus_New, Num_Cl, Total_Cl, Prox, Internal_Names);
          Free(Clus_New);
        end loop;

        Free(Lol);
        Free(Tp);
      end if;
    end Recursive_Clustering;

    No_Value: constant Double := Double'First;

    N, Nc: Positive;
    Off1, Off2, Offn: Integer;
    Prox: PPsDoubles;
    Clus: PClusters;
    Nod_Inf: Node_Info;
    Name: Ustring;
    Total_Clus, Num_Clus: Natural;
    Internal_Names: Boolean;

  begin
    Check_Data(Data, Names);
    N := Data'Length(1);

    -- Initialize proximities between all pairs clusters
    Off1 := Data'First(1) - 1;
    Off2 := Data'First(2) - 1;

    Prox := Alloc_Upper(1, 2 * N - 1);
    for I in Prox'Range loop
      Prox(I).all := (others => No_Value);
    end loop;
    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        Prox(I)(J) := Data(Off1 + I, Off2 + J);
      end loop;
    end loop;

    -- Initialize clusters
    Total_Clus := N;
    Num_Clus   := N;

    Nc := 2 * N - 1;
    Clus := new Clusters(1..Nc);

    if Names /= null then
      Offn := Names'First - 1;
    end if;

    for C in 1..N loop
      if Names /= null then
        Name := Names(Offn + C);
      else
        Name := S2U(I2S(C));
      end if;
      Set_Node_Info(Nod_Inf, Name => Name, Position => 0.0, Height => 0.0, Width => 0.0, Length => 0.0, Margin => 0.0, Is_Leaf => True, Num_Leaves => 1, Id => C);
      Clus(C) := (Id => C, St => New_Tree(Nod_Inf), Num_Leaves => 1);
    end loop;
    for C in (N+1)..Nc loop
      Set_Node_Info(Nod_Inf, Name => Null_Ustring, Position => 0.0, Height => 0.0, Width => 0.0, Length => 0.0, Margin => 0.0, Is_Leaf => False, Num_Leaves => 0, Id => C);
      Clus(C) := (Id => C, St => New_Tree(Nod_Inf), Num_Leaves => 0);
    end loop;

    -- Agglomerative Hierarchical Clustering
    Internal_Names := not Equal(Capitalize(Internal_Node_Name_Prefix), No_Internal_Node_Name);
    Recursive_Clustering(Clus, Num_Clus, Total_Clus, Prox, Internal_Names);

    -- Finalize
    Free(Clus(1).St);
    Free(Clus);
    Free(Prox);

  end Generic_Hierarchical_Clustering;

  -----------------------------
  -- Hierarchical_Clustering --
  -----------------------------

  procedure Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Ct: in Clustering_Type; Precision: in Natural; Handler: in Dendrogram_Handler) is
    procedure Hierarchical_Clust is new Generic_Hierarchical_Clustering(Handler.all);
  begin
    Hierarchical_Clust(Data, Names, Pt, Ct, Precision);
  end Hierarchical_Clustering;

  ----------------------------
  -- Get_Ultrametric_Matrix --
  ----------------------------

  function Get_Ultrametric_Matrix(T: in Dendrogram) return PDoubless is

    procedure Update_Ultrametric(Nod: in Node; Um: in PDoubless) is
      Height: Double;
      Children, Lvs1, Lvs2: List_Of_Nodes;
      Child1, Child2, Leaf1, Leaf2: Node;
      Id, Id1, Id2: Integer;
    begin
      if Is_Leaf(Nod) then
        -- Leaves are assigned their Height as auto-ultrametric
        Height := Get_Height(Value(Nod));
        Id := Get_Id(Value(Nod));
        Um(Id, Id) := Height;
      else
        -- Inter-children Ultrametrics
        Height := Get_Height(Value(Nod));
        Children := Get_Children(Nod);
        Save(Children);
        Reset(Children);
        while Has_Next(Children) loop
          Child1 := Next(Children);
          Lvs1 := Get_Leaves(Child1);
          Save(Children);
          while Has_Next(Children) loop
            Child2 := Next(Children);
            Lvs2 := Get_Leaves(Child2);

            Save(Lvs1);
            Reset(Lvs1);
            while Has_Next(Lvs1) loop
              Leaf1 := Next(Lvs1);
              Id1 := Get_Id(Value(Leaf1));
              Save(Lvs2);
              Reset(Lvs2);
              while Has_Next(Lvs2) loop
                Leaf2 := Next(Lvs2);
                Id2 := Get_Id(Value(Leaf2));
                Um(Id1, Id2) := Height;
                Um(Id2, Id1) := Height;
              end loop;
              Restore(Lvs2);
            end loop;
            Restore(Lvs1);

            Free(Lvs2);
          end loop;
          Restore(Children);
          Free(Lvs1);
        end loop;
        Restore(Children);
        -- Depth-First Preorder recursive Update
        Save(Children);
        Reset(Children);
        while Has_Next(Children) loop
          Update_Ultrametric(Next(Children), Um);
        end loop;
        Restore(Children);
      end if;
    end Update_Ultrametric;

    Um: PDoubless;
    N: Natural;
  begin
    N := Number_Of_Leaves(T);
    Um := Alloc(1, N);
    Um.all := (others => (others => 0.0));

    Update_Ultrametric(T, Um);

    return Um;
  end Get_Ultrametric_Matrix;

  ---------------------------
  -- Get_Deviation_Measure --
  ---------------------------

  function Get_Deviation_Measure(Orig, Um: in PDoubless; Dm: in Deviation_Measure) return Double is
  begin
    case Dm is
      when Cophenetic_Correlation         => return Get_Cophenetic_Correlation(Orig, Um);
      when Normalized_Mean_Squared_Error  => return Get_Normalized_Mean_Squared_Error(Orig, Um);
      when Normalized_Mean_Absolute_Error => return Get_Normalized_Mean_Absolute_Error(Orig, Um);
    end case;
  end Get_Deviation_Measure;

  --------------------------------
  -- Get_Cophenetic_Correlation --
  --------------------------------

  function Get_Cophenetic_Correlation(Orig, Um: in PDoubless) return Double is
    Coph: Double;
    POrig, PUm: PDoubles;
  begin
    Check_Proximities(Orig);
    Check_Proximities(Um);
    if Orig'Length(1) /= Um'Length(1) then
      raise Data_Error;
    end if;

    POrig := Get_Proximities_Vector(Orig);
    PUm   := Get_Proximities_Vector(Um);

    Coph := Pearson_Correlation(POrig, PUm);

    Free(POrig);
    Free(PUm);

    return Coph;
  end Get_Cophenetic_Correlation;

  --------------------------------
  -- Get_Cophenetic_Correlation --
  --------------------------------

  procedure Get_Cophenetic_Correlation(Orig, Um: in PDoubless; Coph, Coph_Err: out Double; Cet: in Correlation_Error_Type := Auto) is
    POrig, PUm: PDoubles;
  begin
    Check_Proximities(Orig);
    Check_Proximities(Um);
    if Orig'Length(1) /= Um'Length(1) then
      raise Data_Error;
    end if;

    POrig := Get_Proximities_Vector(Orig);
    PUm   := Get_Proximities_Vector(Um);

    Coph     := Pearson_Correlation(POrig, PUm);
    Coph_Err := Pearson_Correlation_Error(POrig, PUm, Cet);

    Free(POrig);
    Free(PUm);
  end Get_Cophenetic_Correlation;

  ---------------------------------------
  -- Get_Normalized_Mean_Squared_Error --
  ---------------------------------------

  function Get_Normalized_Mean_Squared_Error(Orig, Um: in PDoubless) return Double is
    Nmse, Num, Den, X, Y: Double;
    N: Natural;
    Oos1, Oos2, Uos1, Uos2: Integer;
  begin
    Check_Proximities(Orig);
    Check_Proximities(Um);
    if Orig'Length(1) /= Um'Length(1) then
      raise Data_Error;
    end if;

    N := Orig'Length(1);
    Oos1 := Orig'First(1) - 1;
    Oos2 := Orig'First(2) - 1;
    Uos1 := Um'First(1) - 1;
    Uos2 := Um'First(2) - 1;

    Num := 0.0;
    Den := 0.0;
    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        X := Orig(Oos1 + I, Oos2 + J);
        Y := Um(Uos1 + I, Uos2 + J);
        if X /= Y then
          Num := Num + (X - Y) * (X - Y);
        end if;
        if X /= 0.0 then
          Den := Den + X * X;
        end if;
      end loop;
    end loop;

    Nmse := 0.0;
    if Num = 0.0 then
      Nmse := 0.0;
    elsif Den /= 0.0 then
      Nmse := Num / Den;
    else
      Nmse := Num;
    end if;

    return Nmse;
  end Get_Normalized_Mean_Squared_Error;

  ----------------------------------------
  -- Get_Normalized_Mean_Absolute_Error --
  ----------------------------------------

  function Get_Normalized_Mean_Absolute_Error(Orig, Um: in PDoubless) return Double is
    Nmae, Num, Den, X, Y: Double;
    N: Natural;
    Oos1, Oos2, Uos1, Uos2: Integer;
  begin
    Check_Proximities(Orig);
    Check_Proximities(Um);
    if Orig'Length(1) /= Um'Length(1) then
      raise Data_Error;
    end if;

    N := Orig'Length(1);
    Oos1 := Orig'First(1) - 1;
    Oos2 := Orig'First(2) - 1;
    Uos1 := Um'First(1) - 1;
    Uos2 := Um'First(2) - 1;

    Num := 0.0;
    Den := 0.0;
    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        X := Orig(Oos1 + I, Oos2 + J);
        Y := Um(Uos1 + I, Uos2 + J);
        if X /= Y then
          Num := Num + abs (X - Y);
        end if;
        if X /= 0.0 then
          Den := Den + X;
        end if;
      end loop;
    end loop;

    Nmae := 0.0;
    if Num = 0.0 then
      Nmae := 0.0;
    elsif Den /= 0.0 then
      Nmae := Num / Den;
    else
      Nmae := Num;
    end if;

    return Nmae;
  end Get_Normalized_Mean_Absolute_Error;

  ----------
  -- Free --
  ----------

  procedure Free(Clus: in out PClusters) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Clusters, PClusters);
  begin
    if Clus /= null then
      Dispose(Clus);
      Clus := null;
    end if;
  end Free;

  ----------
  -- Free --
  ----------

  procedure Free(Tp: in out Tied_Pairs) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Tied_Pairs_Rec, Tied_Pairs);
  begin
    if Tp /= null then
      Dispose(Tp);
      Tp := null;
    end if;
  end Free;

  ----------------
  -- Check_Data --
  ----------------

  procedure Check_Data(Data: in PDoubless; Names: in PUstrings) is
    N: Natural;
    Os1, Os2: Integer;
  begin
    if Data = null then
      raise Data_Error;
    end if;
    if Data'Length(1) /= Data'Length(2) then
      raise Data_Error;
    end if;
    if Names /= null and then Names'Length /= Data'Length(1) then
      raise Data_Error;
    end if;

    N := Data'Length(1);
    Os1 := Data'First(1) - 1;
    Os2 := Data'First(2) - 1;

    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        if Data(Os1 + I, Os2 + J) < 0.0 or Data(Os1 + I, Os2 + J) /= Data(Os1 + J, Os2 + I) then
          raise Data_Error;
        end if;
      end loop;
    end loop;
  end Check_Data;

  -----------------------
  -- Check_Proximities --
  -----------------------

  procedure Check_Proximities(M: in PDoubless) is
    N: Natural;
    Os1, Os2: Integer;
  begin
    if M = null then
      raise Data_Error;
    end if;
    if M'Length(1) /= M'Length(2) then
      raise Data_Error;
    end if;

    N := M'Length(1);
    Os1 := M'First(1) - 1;
    Os2 := M'First(2) - 1;

    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        if M(Os1 + I, Os2 + J) < 0.0 or M(Os1 + I, Os2 + J) /= M(Os1 + J, Os2 + I) then
          raise Data_Error;
        end if;
      end loop;
    end loop;
  end Check_Proximities;

  ----------------------------
  -- Get_Proximities_Vector --
  ----------------------------

  function Get_Proximities_Vector(M: in PDoubless) return PDoubles is
    P: PDoubles;
    N, Np, Ind: Natural;
    Os1, Os2: Integer;
  begin
    N := M'Length(1);
    Os1 := M'First(1) - 1;
    Os2 := M'First(2) - 1;
    if N mod 2 = 0 then
      Np := (N / 2) * (N - 1);
    else
      Np := ((N - 1) / 2) * N;
    end if;

    P := Alloc(1, Np);

    Ind := 0;
    for I in 1..(N-1) loop
      for J in (I+1)..N loop
        Ind := Ind + 1;
        P(Ind) := M(Os1 + I, Os2 + J);
      end loop;
    end loop;

    return P;
  end Get_Proximities_Vector;

  -------------------
  -- Get_Proximity --
  -------------------

  function Get_Proximity(Prox: in PPsDoubles; I, J: Positive) return Double is
  begin
    return Prox(Min(I, J))(Max(I, J));
  end Get_Proximity;

  -------------------
  -- Set_Proximity --
  -------------------

  procedure Set_Proximity(Prox: in PPsDoubles; I, J: Positive; Val: in Double) is
  begin
    Prox(Min(I, J))(Max(I, J)) := Val;
  end Set_Proximity;

  ---------------------------
  -- Find_Joining_Clusters --
  ---------------------------

  procedure Find_Joining_Clusters(Clus: in PClusters; Num_Clus: in Positive; Prox: in PPsDoubles; Pt: in Proximity_Type; Precision: in Natural; Lol: out List_Of_Lists) is
    Prox_Join, Px, Epsilon: Double;
    I, J: Positive;
    Ei, Ej: Element;
  begin
    Epsilon := 1.0 / (10.0 ** (Precision + 1));
    -- Find the extreme Proximity;
    case Pt is
      when Distance =>
        Prox_Join := Double'Last;
        for Ci in 1..(Num_Clus-1) loop
          I := Clus(Ci).Id;
          for Cj in (Ci+1)..Num_Clus loop
            J := Clus(Cj).Id;
            Px := Get_Proximity(Prox, I, J);
            if Px < Prox_Join then
              Prox_Join := Px;
            end if;
          end loop;
        end loop;
      when Similarity =>
        Prox_Join := Double'First;
        for Ci in 1..(Num_Clus-1) loop
          I := Clus(Ci).Id;
          for Cj in (Ci+1)..Num_Clus loop
            J := Clus(Cj).Id;
            Px := Get_Proximity(Prox, I, J);
            if Px > Prox_Join then
              Prox_Join := Px;
            end if;
          end loop;
        end loop;
    end case;

    -- Find the Joining Clusters
    Prox_Join := Round(Prox_Join, Precision);
    Initialize(Lol, Num_Clus, Isolated_Initialization);
    for Ci in 1..(Num_Clus-1) loop
      I := Clus(Ci).Id;
      for Cj in (Ci+1)..Num_Clus loop
        J := Clus(Cj).Id;
        Px := Get_Proximity(Prox, I, J);
        Px := Round(Px, Precision);
        if abs (Px - Prox_Join) < Epsilon then
          Ei := Get_Element(Lol, Ci);
          Ej := Get_Element(Lol, Cj);
          Move(List_Of(Ej), List_Of(Ei));
        end if;
      end loop;
    end loop;
    Remove_Empty(Lol);
    Sort_Lists(Lol);
  end Find_Joining_Clusters;

  ---------------------
  -- Find_Tied_Pairs --
  ---------------------

  procedure Find_Tied_Pairs(Clus: in PClusters; Num_Clus: in Positive; Prox: in PPsDoubles; Pt: in Proximity_Type; Precision: in Natural; Tp: out Tied_Pairs) is
    Lol: List_Of_Lists;
    Prox_Join, Px, Epsilon: Double;
    Num_Tied, I, J, Ci, Cj: Positive;
    Num_Cc, Cc, Pind: Natural;
    L: List;
    Ei, Ej: Element;
  begin
    pragma Warnings(Off, L);
    Epsilon := 1.0 / (10.0 ** (Precision + 1));
    Num_Tied := 1;
    -- Find the extreme Proximity;
    case Pt is
      when Distance =>
        Prox_Join := Double'Last;
        for Ci in 1..(Num_Clus-1) loop
          I := Clus(Ci).Id;
          for Cj in (Ci+1)..Num_Clus loop
            J := Clus(Cj).Id;
            Px := Get_Proximity(Prox, I, J);
            if abs (Px - Prox_Join) < Epsilon then
              Prox_Join := Min(Prox_Join, Px);
              Num_Tied := Num_Tied + 1;
            elsif Px < Prox_Join then
              Prox_Join := Px;
              Num_Tied := 1;
            end if;
          end loop;
        end loop;
      when Similarity =>
        Prox_Join := Double'First;
        for Ci in 1..(Num_Clus-1) loop
          I := Clus(Ci).Id;
          for Cj in (Ci+1)..Num_Clus loop
            J := Clus(Cj).Id;
            Px := Get_Proximity(Prox, I, J);
            if abs (Px - Prox_Join) < Epsilon then
              Prox_Join := Max(Prox_Join, Px);
              Num_Tied := Num_Tied + 1;
            elsif Px > Prox_Join then
              Prox_Join := Px;
              Num_Tied := 1;
            end if;
          end loop;
        end loop;
    end case;

    -- Find the Connected Components of Tied Pairs
    Initialize(Lol, Num_Clus, Isolated_Initialization);
    for Ci in 1..(Num_Clus-1) loop
      I := Clus(Ci).Id;
      for Cj in (Ci+1)..Num_Clus loop
        J := Clus(Cj).Id;
        Px := Get_Proximity(Prox, I, J);
        if abs (Px - Prox_Join) < Epsilon then
          Ei := Get_Element(Lol, Ci);
          Ej := Get_Element(Lol, Cj);
          Move(List_Of(Ej), List_Of(Ei));
        end if;
      end loop;
    end loop;
    Remove_Empty(Lol);
    Sort_Lists(Lol);

    -- Initialize Tied Pairs
    Num_Cc := 0;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      if Number_Of_Elements(L) > 1 then
        Num_Cc := Num_Cc + 1;
      end if;
    end loop;
    Restore(Lol);
    Tp := new Tied_Pairs_Rec(Num_Cc, Num_Tied);
    Tp.Num_Combi := 1;
    Tp.Cc_Size   := (others => 0);
    Tp.Cc_Start  := (others => 0);
    Tp.Cc_Acum   := (others => 1);
    Tp.Pair      := (others => (Ci => 0, Cj => 0));

    -- Build the Tied Pairs
    Cc := 0;
    Pind := 0;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      if Number_Of_Elements(L) > 1 then
        Cc := Cc + 1;
        Tp.Cc_Start(Cc) := Pind + 1;
        Save(L);
        Reset(L);
        while Has_Next_Element(L) loop
          Ci := Index_Of(Next_Element(L));
          Save(L);
          while Has_Next_Element(L) loop
            Cj := Index_Of(Next_Element(L));
            Px := Get_Proximity(Prox, Clus(Ci).Id, Clus(Cj).Id);
            if abs (Px - Prox_Join) < Epsilon then
              Pind := Pind + 1;
              Tp.Cc_Size(Cc) := Tp.Cc_Size(Cc) + 1;
              Tp.Pair(Pind) := (Ci => Ci, Cj => Cj);
            end if;
          end loop;
          Restore(L);
        end loop;
        Restore(L);
        Tp.Cc_Acum(Cc) := Tp.Num_Combi;
        Tp.Num_Combi := Tp.Num_Combi * Tp.Cc_Size(Cc);
      end if;
    end loop;
    Restore(Lol);

    Free(Lol);
  exception
    when Constraint_Error =>
      raise Tied_Pairs_Combinations_Overflow;
  end Find_Tied_Pairs;

  -----------------------------
  -- Build_Pairs_Combination --
  -----------------------------

  procedure Build_Pairs_Combination(Tp: in Tied_Pairs; Index: in Positive; Lol: in out List_Of_Lists) is
    Pind: Positive;
    Ei, Ej: Element;
  begin
    Reinitialize(Lol, Isolated_Initialization);
    for Cc in Tp.Cc_Size'Range loop
      Pind := Tp.Cc_Start(Cc) + ((Index - 1) / Tp.Cc_Acum(Cc)) mod Tp.Cc_Size(Cc);
      Ei := Get_Element(Lol, Tp.Pair(Pind).Ci);
      Ej := Get_Element(Lol, Tp.Pair(Pind).Cj);
      Move(List_Of(Ej), List_Of(Ei));
    end loop;
    Remove_Empty(Lol);
  end Build_Pairs_Combination;

  -------------------------------
  -- Set_Cluster_Heterogeneity --
  -------------------------------

  procedure Set_Cluster_Heterogeneity(Prox: in PPsDoubles; L: in List; Index: in Positive; Clus, Clus_Prev: in PClusters; Pt: in Proximity_Type; Precision: in Natural) is
    I1, I2: Positive;
    Dii, Dii_Min, Dii_Max: Double;
    Nod: Node;
    Nod_Inf: Node_Info;
    Height, Margin: Double;
  begin
    -- Calculate Heterogeneity
    if Number_Of_Elements(L) = 1 then
      return;
    elsif Number_Of_Elements(L) = 2 then
      Save(L);
      Reset(L);
      I1 := Index_Of(Next_Element(L));
      I2 := Index_Of(Next_Element(L));
      Restore(L);
      Height := Get_Proximity(Prox, Clus_Prev(I1).Id, Clus_Prev(I2).Id);
      Height := Round(Height, Precision);
      Margin := 0.0;
    else
      Dii_Min := Double'Last;
      Dii_Max := Double'First;
      Save(L);
      Reset(L);
      while Has_Next_Element(L) loop
        I1 := Index_Of(Next_Element(L));
        Save(L);
        while Has_Next_Element(L) loop
          I2 := Index_Of(Next_Element(L));
          Dii := Get_Proximity(Prox, Clus_Prev(I1).Id, Clus_Prev(I2).Id);
          Dii := Round(Dii, Precision);
          if Dii < Dii_Min then
            Dii_Min := Dii;
          end if;
          if Dii > Dii_Max then
            Dii_Max := Dii;
          end if;
        end loop;
        Restore(L);
      end loop;
      Restore(L);
      case Pt is
        when Distance =>
          Height := Dii_Min;
          Margin := Dii_Max - Dii_Min;
        when Similarity =>
          Height := Dii_Max;
          Margin := Dii_Min - Dii_Max;
      end case;
    end if;

    -- Set Height and Margin
    Nod := Clus(Index).St;
    Nod_Inf := Value(Nod);
    Set_Height(Nod_Inf, Height);
    Set_Margin(Nod_Inf, Margin);
    Set_Value(Nod, Nod_Inf);
  end Set_Cluster_Heterogeneity;

  ----------------------------
  -- Get_Clusters_Proximity --
  ----------------------------

  function Get_Clusters_Proximity(Prox: in PPsDoubles; Li, Lj: in List; Index_I, Index_J: in Positive; Clus, Clus_Prev: in PClusters; Pt: in Proximity_Type; Ct: in Clustering_Type) return Double is

    function Alpha_ij(Ct: in Clustering_Type; S_I, S_Xi, S_X_I, S_J, S_Xj, S_X_J: Positive) return Double is
    begin
      case Ct is
        when Single_Linkage      => return 0.0;
        when Complete_Linkage    => return 0.0;
        when Unweighted_Average  => return Double(S_Xi * S_Xj) / Double(S_X_I * S_X_J);
        when Weighted_Average    => return 1.0 / Double(S_I * S_J);
        when Unweighted_Centroid => return Double(S_Xi * S_Xj) / Double(S_X_I * S_X_J);
        when Weighted_Centroid   => return 1.0 / Double(S_I * S_J);
        when Ward                => return Double(S_Xi + S_Xj) / Double(S_X_I + S_X_J);
      end case;
    end Alpha_ij;

    function Beta_ii(Ct: in Clustering_Type; S_I, S_Xi1, S_Xi2, S_X_I, S_X_J: Positive) return Double is
    begin
      case Ct is
        when Single_Linkage      => return 0.0;
        when Complete_Linkage    => return 0.0;
        when Unweighted_Average  => return 0.0;
        when Weighted_Average    => return 0.0;
        when Unweighted_Centroid => return - Double(S_Xi1 * S_Xi2) / Double(S_X_I * S_X_I);
        when Weighted_Centroid   => return -1.0 / Double(S_I * S_I);
        when Ward                => return - (Double(S_X_J) / Double(S_X_I)) * (Double(S_Xi1 + S_Xi2) / Double(S_X_I + S_X_J));
      end case;
    end Beta_ii;

    function Beta_jj(Ct: in Clustering_Type; S_J, S_Xj1, S_Xj2, S_X_J, S_X_I: Positive) return Double renames Beta_ii;

    I, I1, I2, J, J1, J2: Positive;
    S_I, S_Xi, S_Xi1, S_Xi2, S_X_I: Positive;
    S_J, S_Xj, S_Xj1, S_Xj2, S_X_J: Positive;
    Dii, Djj, Dij, Dij_Min, Dij_Max: Double;
    D, D_I_I, D_I_J, D_J_J: Double;
  begin
    S_I := Number_Of_Elements(Li);
    S_J := Number_Of_Elements(Lj);
    S_X_I := Clus(Index_I).Num_Leaves;
    S_X_J := Clus(Index_J).Num_Leaves;

    -- Not new clusters
    if S_I = 1 and S_J = 1 then
      return Get_Proximity(Prox, Clus(Index_I).Id, Clus(Index_J).Id);
    end if;

    -- Inter-cluster contribution
    D_I_J := 0.0;
    Dij_Min := Double'Last;
    Dij_Max := Double'First;
    Save(Li);
    Reset(Li);
    while Has_Next_Element(Li) loop
      I := Index_Of(Next_Element(Li));
      S_Xi := Clus_Prev(I).Num_Leaves;
      Save(Lj);
      Reset(Lj);
      while Has_Next_Element(Lj) loop
        J := Index_Of(Next_Element(Lj));
        S_Xj := Clus_Prev(J).Num_Leaves;
        Dij := Get_Proximity(Prox, Clus_Prev(I).Id, Clus_Prev(J).Id);
        if Ct in Min_Max_Clustering then
          if Dij < Dij_Min then
            Dij_Min := Dij;
          end if;
          if Dij > Dij_Max then
            Dij_Max := Dij;
          end if;
        else
          D_I_J := D_I_J + Alpha_ij(Ct, S_I, S_Xi, S_X_I, S_J, S_Xj, S_X_J) * Dij;
        end if;
      end loop;
      Restore(Lj);
    end loop;
    Restore(Li);

    -- Intra-cluster contribution of First Cluster
    D_I_I := 0.0;
    if S_I > 1 and Ct in Beta_Clustering then
      Save(Li);
      Reset(Li);
      while Has_Next_Element(Li) loop
        I1 := Index_Of(Next_Element(Li));
        S_Xi1 := Clus_Prev(I1).Num_Leaves;
        Save(Li);
        while Has_Next_Element(Li) loop
          I2 := Index_Of(Next_Element(Li));
          S_Xi2 := Clus_Prev(I2).Num_Leaves;
          Dii := Get_Proximity(Prox, Clus_Prev(I1).Id, Clus_Prev(I2).Id);
          D_I_I := D_I_I + Beta_ii(Ct, S_I, S_Xi1, S_Xi2, S_X_I, S_X_J) * Dii;
        end loop;
        Restore(Li);
      end loop;
      Restore(Li);
    end if;

    -- Intra-cluster contribution of Second Cluster
    D_J_J := 0.0;
    if S_J > 1 and Ct in Beta_Clustering then
      Save(Lj);
      Reset(Lj);
      while Has_Next_Element(Lj) loop
        J1 := Index_Of(Next_Element(Lj));
        S_Xj1 := Clus_Prev(J1).Num_Leaves;
        Save(Lj);
        while Has_Next_Element(Lj) loop
          J2 := Index_Of(Next_Element(Lj));
          S_Xj2 := Clus_Prev(J2).Num_Leaves;
          Djj := Get_Proximity(Prox, Clus_Prev(J1).Id, Clus_Prev(J2).Id);
          D_J_J := D_J_J + Beta_jj(Ct, S_J, S_Xj1, S_Xj2, S_X_J, S_X_I) * Djj;
        end loop;
        Restore(Lj);
      end loop;
      Restore(Lj);
    end if;

    -- Total contribution to Clusters Proximity
    if (Pt = Distance and Ct = Single_Linkage) or (Pt = Similarity and Ct = Complete_Linkage) then
      D := Dij_Min;
    elsif (Pt = Distance and Ct = Complete_Linkage) or (Pt = Similarity and Ct = Single_Linkage) then
      D := Dij_Max;
    else
      D := D_I_I + D_I_J + D_J_J;
    end if;
    return D;
  end Get_Clusters_Proximity;

  ------------------------------------
  -- Set_Lengths_And_Leaves_Heights --
  ------------------------------------

  procedure Set_Lengths_And_Leaves_Heights(T: in Dendrogram; Pt: in Proximity_Type; Precision: in Natural) is

    Min_H: Double := Double'Last;
    Max_H: Double := Double'First;
    H_Leaves: Double;

    procedure Update_Internal_Node_Info(Nod: in Node) is
      Parent_Inf, Nod_Inf: Node_Info;
      Hp, Hn, Lnp: Double;
    begin
      -- Set Length of Internal Nodes, and determine Max and Min Heights
      if Is_Root(Nod) then
        Nod_Inf := Value(Nod);
        Hn := Get_Height(Nod_Inf);
        if Min_H > Hn then
          Min_H := Hn;
        end if;
        if Max_H < Hn then
          Max_H := Hn;
        end if;
      elsif Is_Internal(Nod) then
        Parent_Inf := Value(Get_Parent(Nod));
        Nod_Inf := Value(Nod);
        Hp := Get_Height(Parent_Inf);
        Hn := Get_Height(Nod_Inf);
        Lnp := Hp - Hn;
        Set_Length(Nod_Inf, Lnp);
        Set_Value(Nod, Nod_Inf);
        if Min_H > Hn then
          Min_H := Hn;
        end if;
        if Max_H < Hn then
          Max_H := Hn;
        end if;
      end if;
    end Update_Internal_Node_Info;

    procedure Update_Leaf_Info(Nod: in Node) is
      Parent_Inf, Nod_Inf: Node_Info;
      Hp, Lnp: Double;
    begin
      -- Set Height and Length of Leaves
      if Has_Parent(Nod) and Is_Leaf(Nod) then
        Parent_Inf := Value(Get_Parent(Nod));
        Nod_Inf := Value(Nod);
        Hp := Get_Height(Parent_Inf);
        Lnp := Hp - H_Leaves;
        Set_Height(Nod_Inf, H_Leaves);
        Set_Length(Nod_Inf, Lnp);
        Set_Value(Nod, Nod_Inf);
      end if;
    end Update_Leaf_Info;

    procedure Update_Internal_Nodes is new Generic_Depth_First_Preorder_Traversal(Update_Internal_Node_Info);
    procedure Update_Leaves is new Generic_Depth_First_Preorder_Traversal(Update_Leaf_Info);

  begin
    Update_Internal_Nodes(T);

    -- Height of Leaves unknown in Similarities Dendrograms, determined from Max and Min Heights
    case Pt is
      when Distance   => H_Leaves := 0.0;
      when Similarity => H_Leaves := Round(Max_H + 0.05 * (Max_H - Min_H), Precision);
    end case;

    Update_Leaves(T);
  end Set_Lengths_And_Leaves_Heights;

  -------------------
  -- Join_Clusters --
  -------------------

  procedure Join_Clusters(Lol: in List_Of_Lists; Clus, Clus_Prev: in PClusters; Total_Clus: in out Positive; Prox: in PPsDoubles; Pt: in Proximity_Type; Ct: in Clustering_Type; Precision: in Natural; Internal_Names: in Boolean) is
    Nod: Node;
    Nod_Inf: Node_Info;
    C, Ci, Cj: Natural;
    L, Li, Lj: List;
    E: Element;
    D: Double;
    Name: Ustring;
  begin
    pragma Warnings(Off, L);

    -- Prepare new Clusters
    C := 0;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      L := Next_List(Lol);
      C := C + 1;
      if Number_Of_Elements(L) = 1 then
        -- old cluster
        Save(L);
        Reset(L);
        E := Next_Element(L);
        Clus(C) := Clus_Prev(Index_Of(E));
        Restore(L);
      else
        -- new cluster
        Total_Clus := Total_Clus + 1;
        Nod := Clus_Prev(Total_Clus).St;
        Isolate_Node(Nod);
        Clus(C) := (Id => Total_Clus, St => Nod, Num_Leaves => 0);
        Save(L);
        Reset(L);
        while Has_Next_Element(L) loop
          E := Next_Element(L);
          Clus(C).Num_Leaves := Clus(C).Num_Leaves + Clus_Prev(Index_Of(E)).Num_Leaves;
          Add_Tree(Parent => Clus(C).St, Child => Clus_Prev(Index_Of(E)).St);
        end loop;
        Restore(L);
        if Internal_Names then
          Name := Internal_Node_Name_Prefix + I2S(Total_Clus);
        else
          Name := Null_Ustring;
        end if;
        Set_Node_Info(Nod_Inf, Name => Name, Position => 0.0, Height => 0.0, Width => 0.0, Length => 0.0, Margin => 0.0, Is_Leaf => False, Num_Leaves => Clus(C).Num_Leaves, Id => Total_Clus);
        Set_Value(Nod, Nod_Inf);
        Set_Cluster_Heterogeneity(Prox, L, C, Clus, Clus_Prev, Pt, Precision);
      end if;
    end loop;
    Restore(Lol);

    -- Proximities to new Clusters
    Ci := 0;
    Save(Lol);
    Reset(Lol);
    while Has_Next_List(Lol) loop
      Li := Next_List(Lol);
      Ci := Ci + 1;
      Cj := Ci;
      Save(Lol);
      while Has_Next_List(Lol) loop
        Lj := Next_List(Lol);
        Cj := Cj + 1;
        if Number_Of_Elements(Li) > 1 or Number_Of_Elements(Lj) > 1 then
          D := Get_Clusters_Proximity(Prox, Li, Lj, Ci, Cj, Clus, Clus_Prev, Pt, Ct);
          Set_Proximity(Prox, Clus(Ci).Id, Clus(Cj).Id, D);
        end if;
      end loop;
      Restore(Lol);
    end loop;
    Restore(Lol);
  end Join_Clusters;

end Dendrograms.Algorithms;
