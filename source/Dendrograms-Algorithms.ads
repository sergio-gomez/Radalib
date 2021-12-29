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


-- @filename Dendrograms-Algorithms.ads
-- @author Sergio Gomez
-- @author Alberto Fernandez
-- @version 1.0
-- @date 11/05/2013
-- @revision 28/12/2021
-- @brief Dendrograms Algorithms

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Statistics_Double; use Statistics_Double;
with Utils; use Utils;

package Dendrograms.Algorithms is

  No_Internal_Node_Name: constant Ustring := S2U("None");
  Internal_Node_Name_Prefix: Ustring := S2U("Cluster_");

  -- Types
  type Dendrogram_Type is (Binary_Dendrogram,
                           Multidendrogram);

  type Proximity_Type  is (Distance,
                           Similarity);

  type Clustering_Type is (Single_Linkage,
                           Complete_Linkage,
                           Versatile_Linkage,
                           Arithmetic_Linkage,
                           Geometric_Linkage,
                           Harmonic_Linkage,
                           Ward,
                           Centroid,
                           Beta_Flexible);

  type Weighting_Type  is (Weighted,
                           Unweighted);

  type Deviation_Measure is (Cophenetic_Correlation,
                             Normalized_Mean_Squared_Error,
                             Normalized_Mean_Absolute_Error);

  subtype Min_Max_Clustering        is Clustering_Type range Single_Linkage..Complete_Linkage;
  subtype Versatile_Clustering      is Clustering_Type range Versatile_Linkage..Harmonic_Linkage;
  subtype Lance_Williams_Clustering is Clustering_Type range Ward..Beta_Flexible;

  subtype Dendrogram_Handler is Node_Handler;

  Unknown_Dendrogram_Type_Error: exception;
  Unknown_Proximity_Type_Error : exception;
  Unknown_Clustering_Type_Error: exception;
  Unknown_Weighting_Type_Error : exception;
  Stop_Dendrograms_Recursion   : exception;

  Data_Error: exception;
  Tied_Pairs_Combinations_Overflow: exception;


  -- Purpose : Obtain a Dendrogram Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    BD | Binary_Dendrogram
  -- Note    :    MD | Multidendrogram
  --
  -- Name    : The Dendrogram Type Name
  -- return  : The Dendrogram Type
  -- raises  : Unknown_Dendrogram_Type_Error
  function Get_Dendrogram_Type(Name: in String) return Dendrogram_Type;

  -- Purpose : Obtain the Name of a Dendrogram Type
  -- Note    : Short and long names:
  -- Note    :    BD | Binary_Dendrogram
  -- Note    :    MD | Multidendrogram
  --
  -- Dt      : The Dendrogram Type
  -- return  : The Dendrogram Type Name
  function To_Name(Dt: in Dendrogram_Type; Short: in Boolean := False) return String;

  -- Purpose : Obtain a Proximity Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    D | DIST | Distance
  -- Note    :    S | SIM  | Similarity
  --
  -- Name    : The Proximity Type Name
  -- return  : The Proximity Type
  -- raises  : Unknown_Proximity_Type_Error
  function Get_Proximity_Type(Name: in String) return Proximity_Type;

  -- Purpose : Obtain the Name of a Proximity Type
  -- Note    : Short and long names:
  -- Note    :    DIST | Distance
  -- Note    :    SIM  | Similarity
  --
  -- Pt      : The Proximity Type
  -- return  : The Proximity Type Name
  function To_Name(Pt: in Proximity_Type; Short: in Boolean := False) return String;

  -- Purpose : Obtain a Clustering Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    VL | Versatile_Linkage
  -- Note    :    SL | Single_Linkage
  -- Note    :    CL | Complete_Linkage
  -- Note    :    AL | Arithmetic_Linkage
  -- Note    :    GL | Geometric_Linkage
  -- Note    :    HL | Harmonic_Linkage
  -- Note    :    WD | Ward
  -- Note    :    CD | Centroid
  -- Note    :    BF | Beta_Flexible
  --
  -- Name    : The Clustering Type Name
  -- return  : The Clustering Type
  -- raises  : Unknown_Clustering_Type_Error
  function Get_Clustering_Type(Name: in String) return Clustering_Type;

  -- Purpose : Obtain the Name of a Clustering Type
  -- Note    : Short and long names:
  -- Note    :    VL | Versatile_Linkage
  -- Note    :    SL | Single_Linkage
  -- Note    :    CL | Complete_Linkage
  -- Note    :    AL | Arithmetic_Linkage
  -- Note    :    GL | Geometric_Linkage
  -- Note    :    HL | Harmonic_Linkage
  -- Note    :    WD | Ward
  -- Note    :    CD | Centroid
  -- Note    :    BF | Beta_Flexible
  --
  -- Ct      : The Clustering Type
  -- return  : The Clustering Type Name
  function To_Name(Ct: in Clustering_Type; Short: in Boolean := False) return String;

  -- Purpose : Obtain a Weighting Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    W  | Weighted
  -- Note    :    UW | Unweighted
  --
  -- Name    : The Weighting Type Name
  -- return  : The Weighting Type
  -- raises  : Unknown_Weighting_Type_Error
  function Get_Weighting_Type(Name: in String) return Weighting_Type;

  -- Purpose : Obtain the Name of a Weighting Type
  -- Note    : Short and long names:
  -- Note    :    W  | Weighted
  -- Note    :    UW | Unweighted
  --
  -- Wt      : The Weighting Type
  -- return  : The Weighting Type Name
  function To_Name(Wt: in Weighting_Type; Short: in Boolean := False) return String;

  -- Purpose : Round Data to a given Precision
  -- Note    : The original Data remains unchanged
  --
  -- Data    : The Data
  -- Precision:The number of Significant Digits
  -- return  : The Rounded Data
  function Round_Data(Data: in PDoubless; Precision: in Natural) return PDoubless;

  -- Purpose : Round Data to a given Precision
  -- Note    : The original Data is modified
  --
  -- Data    : The Data
  -- Precision:The number of Significant Digits
  procedure Round_Data(Data: in PDoubless; Precision: in Natural);

  -- Purpose : Perform Variable-Group Agglomerative Hierarchical Clustering to obtain the corresponding Multidendrogram
  -- Note    : Data must be a squared symmetric matrix representing Distances or Similarities between a set of Elements
  -- Note    : Diagonal Elements of Data matrix are ignored
  -- Note    : Names may be empty
  --
  -- Data    : The Data
  -- Names   : The Names of the Elements
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Ct      : The Clustering Type
  -- Wt      : The Weighting Type
  -- Cp      : The Clustering Parameter, between -1 and +1, necessary for Versatile Linkage and Beta Flexible clusterings
  -- Md      : The Multidendrogram
  -- raises  : Data_Error
  procedure Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Precision: in Natural; Ct: in Clustering_Type; Wt: in Weighting_Type; Cp: in Double; Md: out Dendrogram);

  -- Purpose : Perform Pair-Group Agglomerative Hierarchical Clustering to obtain all the corresponding Binary Dendrograms
  -- Note    : Data must be a squared symmetric matrix representing Distances or Similarities between a set of Elements
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  -- Note    : Names may be empty
  --
  -- Data    : The Data
  -- Names   : The Names of the Elements
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Ct      : The Clustering Type
  -- Wt      : The Weighting Type
  -- Cp      : The Clustering Parameter, between -1 and +1, necessary for Versatile Linkage and Beta Flexible clusterings
  -- Bds     : The List of Binary Dendrograms
  -- raises  : Data_Error
  procedure Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Precision: in Natural; Ct: in Clustering_Type; Wt: in Weighting_Type; Cp: in Double; Bds: out List_Of_Dendrograms);

  -- Purpose : Perform Pair-Group Agglomerative Hierarchical Clustering with execution of Handler for all the Binary Dendrograms
  -- Note    : The Binary Dendrograms are not stored
  -- Note    : Data must be a squared symmetric matrix representing Distances or Similarities between a set of Elements
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  -- Note    : Names may be empty
  -- Note    : The Handler can raise Stop_Dendrograms_Recursion to stop the recursion
  --
  -- Data    : The Data
  -- Names   : The Names of the Elements
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Ct      : The Clustering Type
  -- Wt      : The Weighting Type
  -- Cp      : The Clustering Parameter, between -1 and +1, necessary for Versatile Linkage and Beta Flexible clusterings
  -- Handler : The Handler
  -- raises  : Data_Error
  generic
    with procedure Handler(Nod: in Node) is <>;
  procedure Generic_Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Precision: in Natural; Ct: in Clustering_Type; Wt: in Weighting_Type; Cp: in Double);

  -- Purpose : Perform Pair-Group Agglomerative Hierarchical Clustering with execution of Handler for all the Binary Dendrograms
  -- Note    : The Binary Dendrograms are not stored
  -- Note    : Data must be a squared symmetric matrix representing Distances or Similarities between a set of Elements
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  -- Note    : Names may be empty
  -- Note    : The Handler can raise Stop_Dendrograms_Recursion to stop the recursion
  --
  -- Data    : The Data
  -- Names   : The Names of the Elements
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Ct      : The Clustering Type
  -- Wt      : The Weighting Type
  -- Cp      : The Clustering Parameter, between -1 and +1, necessary for Versatile Linkage and Beta Flexible clusterings
  -- Handler : The Handler
  -- raises  : Data_Error
  procedure Hierarchical_Clustering(Data: in PDoubless; Names: in PUstrings; Pt: in Proximity_Type; Precision: in Natural; Ct: in Clustering_Type; Wt: in Weighting_Type; Cp: in Double; Handler: in Dendrogram_Handler);

  -- Purpose : Obtain the Ultrametrix Matrix corresponding to a Dendrogram
  -- Note    : Leaves must have distinct Ids in 1..N, where N is the number of Leaves, no check is made
  --
  -- T       : The Dendrogram
  -- return  : The Ultrametric Matrix
  function Get_Ultrametric_Matrix(T: in Dendrogram) return PDoubless;

  -- Purpose : Obtain a Deviation Measure between Original and Ultrametric Proximities
  -- Note    : Original and Ultrametric Proximities should be in the same Precision, no transformation made
  -- Note    : Proximities must be squared symmetric matrices representing Distances or Similarities between a set of Elements
  -- Note    : Original and Ultrametric matrices must have the same size
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  --
  -- Orig    : The Original Proximities
  -- Um      : The Ultrametric Proximities
  -- Dm      : The Deviation Measure type
  -- return  : The Deviation Measure
  -- raises  : Data_Error
  function Get_Deviation_Measure(Orig, Um: in PDoubless; Dm: in Deviation_Measure) return Double;

  -- Purpose : Obtain the Cophenetic Correlation between Original and Ultrametric Proximities
  -- Note    : Original and Ultrametric Proximities should be in the same Precision, no transformation made
  -- Note    : Proximities must be squared symmetric matrices representing Distances or Similarities between a set of Elements
  -- Note    : Original and Ultrametric matrices must have the same size
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  --
  -- Orig    : The Original Proximities
  -- Um      : The Ultrametric Proximities
  -- return  : The Cophenetic Correlation
  -- raises  : Data_Error
  function Get_Cophenetic_Correlation(Orig, Um: in PDoubless) return Double;

  -- Purpose : Obtain the Cophenetic Correlation between Original and Ultrametric Proximities, and an estimation of its Error
  -- Note    : Original and Ultrametric Proximities should be in the same Precision, no transformation made
  -- Note    : Proximities must be squared symmetric matrices representing Distances or Similarities between a set of Elements
  -- Note    : Original and Ultrametric matrices must have the same size
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  --
  -- Orig    : The Original Proximities
  -- Um      : The Ultrametric Proximities
  -- Coph    : The Cophenetic Correlation
  -- Coph_Err: The Cophenetic Correlation Error
  -- Cet     : The Correlation Error type
  -- raises  : Data_Error
  procedure Get_Cophenetic_Correlation(Orig, Um: in PDoubless; Coph, Coph_Err: out Double; Cet: in Correlation_Error_Type := Auto);

  -- Purpose : Obtain the Normalized Mean Squared Error between Original and Ultrametric Proximities
  -- Note    : Original and Ultrametric Proximities should be in the same Precision, no transformation made
  -- Note    : Proximities must be squared symmetric matrices representing Distances or Similarities between a set of Elements
  -- Note    : Original and Ultrametric matrices must have the same size
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  --
  -- Orig    : The Original Proximities
  -- Um      : The Ultrametric Proximities
  -- return  : The Normalized Mean Squared Error
  -- raises  : Data_Error
  function Get_Normalized_Mean_Squared_Error(Orig, Um: in PDoubless) return Double;

  -- Purpose : Obtain the Normalized Mean Absolute Error between Original and Ultrametric Proximities
  -- Note    : Original and Ultrametric Proximities should be in the same Precision, no transformation made
  -- Note    : Proximities must be squared symmetric matrices representing Distances or Similarities between a set of Elements
  -- Note    : Original and Ultrametric matrices must have the same size
  -- Note    : Distances and Similarities must be non-negative
  -- Note    : Diagonal Elements of Data matrix are ignored
  --
  -- Orig    : The Original Proximities
  -- Um      : The Ultrametric Proximities
  -- return  : The Normalized Mean Absolute Error
  -- raises  : Data_Error
  function Get_Normalized_Mean_Absolute_Error(Orig, Um: in PDoubless) return Double;

  -- Purpose : Convert the Clustering Parameter into the Exponent used in Versatile Linkage
  -- Note    : Returns -Infinity (Double'First) or +Infinity (Double'Last) if Cp <= -1 or Cp >= +1
  --
  -- Pt      : The Proximity Type
  -- Ct      : The Clustering Type
  -- Cp      : The Clustering Parameter, between -1 and +1
  -- return  : The Exponent
  function Versatile_Power(Pt: in Proximity_Type; Ct: in Clustering_Type; Cp: in Double) return Double;


private

  -- Clusters
  type Cluster_Rec is record
    Id: Positive;
    St: Subtree;
    Num_Leaves: Natural;
  end record;

  type Clusters is array(Integer range <>) of Cluster_Rec;
  type PClusters is access Clusters;

  -- Tied Pairs
  type Pair_Rec is record
    Ci, Cj: Natural;
  end record;

  type Pair_Recs is array(Integer range <>) of Pair_Rec;

  type Tied_Pairs_Rec(Num_Cc, Num_Tied: Positive) is record
    Num_Combi : Positive;
    Cc_Size   : Integers(1..Num_Cc);
    Cc_Start  : Integers(1..Num_Cc);
    Cc_Acum   : Integers(1..Num_Cc);
    Pair      : Pair_Recs(1..Num_Tied);
  end record;

  type Tied_Pairs is access Tied_Pairs_Rec;

  -- Purpose : Deallocate the space used by an array of Clusters
  -- Note    : Does not deallocate the associated Tree
  --
  -- Clus    : The access to the array of Clusters
  procedure Free(Clus: in out PClusters);

  -- Purpose : Deallocate the space used by Tied Pairs
  --
  -- Tp      : The access to the Tied Pairs
  procedure Free(Tp: in out Tied_Pairs);

  -- Purpose : Check if Data is appropriate
  -- Note    : Data must be squared and symmetric, and Names empty or with the right size
  -- Note    : Distances and Similarities must be non-negative
  --
  -- Data    : The Data
  -- Names   : The Names of the Elements
  -- raises  : Data_Error
  procedure Check_Data(Data: in PDoubless; Names: in PUstrings);

  -- Purpose : Check if Proximities Matrix is appropriate
  -- Note    : Proximities Matrix must be squared and symmetric
  -- Note    : Proximities must be non-negative
  --
  -- M       : The Proximities Matrix
  -- raises  : Data_Error
  procedure Check_Proximities(M: in PDoubless);

  -- Purpose : Transform a Proximities Matrix into a Proximities Vector
  -- Note    : Proximities Matrix must be squared and symmetric, no checks performed
  --
  -- M       : The Proximities Matrix
  -- return  : The Proximities Vector
  function Get_Proximities_Vector(M: in PDoubless) return PDoubles;

  -- Purpose : Get a Proximity value
  -- Note    : No checks performed
  --
  -- Prox    : The Proximities between Clusters
  -- I       : The Index of the First Cluster
  -- J       : The Index of the Second Cluster
  -- return  : The Proximity
  function Get_Proximity(Prox: in PPsDoubles; I, J: Positive) return Double;

  -- Purpose : Get a Proximity value
  -- Note    : No checks performed
  --
  -- Prox    : The Proximities between Clusters
  -- I       : The Index of the First Cluster
  -- J       : The Index of the Second Cluster
  -- return  : The Proximity
  procedure Set_Proximity(Prox: in PPsDoubles; I, J: Positive; Val: in Double);

  -- Purpose : Find the Clusters to be joined
  -- Note    : No empty Lists in the returned List of Lists
  --
  -- Clus    : The Clusters
  -- Num_Clus: The current Number of Clusters
  -- Prox    : The Proximities between Clusters
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Lol     : The Clusters to be joined
  procedure Find_Joining_Clusters(Clus: in PClusters; Num_Clus: in Positive; Prox: in PPsDoubles; Pt: in Proximity_Type; Precision: in Natural; Lol: out List_Of_Lists);

  -- Purpose : Find the Tied Pairs
  -- Note    : Tied Pairs may contain just one Pair if no Tied Proximities are present
  --
  -- Clus    : The Clusters
  -- Num_Clus: The current Number of Clusters
  -- Prox    : The Proximities between Clusters
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Tp      : The Tied Pairs
  procedure Find_Tied_Pairs(Clus: in PClusters; Num_Clus: in Positive; Prox: in PPsDoubles; Pt: in Proximity_Type; Precision: in Natural; Tp: out Tied_Pairs);

  -- Purpose : Build a Pairs Combination
  -- Note    : Index cannot be larger than the number of Combinations, no check is made
  --
  -- Tp      : The Tied Pairs
  -- Index   : The Index of the Pairs Combination
  -- Lol     : The Clusters to be joined
  procedure Build_Pairs_Combination(Tp: in Tied_Pairs; Index: in Positive; Lol: in out List_Of_Lists);

  -- Purpose : Calculate and Set the Cluster Heterogeneity
  -- Note    : Height and Margin are the indicators of this Heterogeneity
  --
  -- Prox    : The Proximities between previous Clusters
  -- L       : The Composition of the Cluster
  -- Index   : The Index of the Cluster
  -- Clus    : The Clusters
  -- Clus_Prev:The Previous Clusters
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  procedure Set_Cluster_Heterogeneity(Prox: in PPsDoubles; L: in List; Index: in Positive; Clus, Clus_Prev: in PClusters; Pt: in Proximity_Type; Precision: in Natural);

  -- Purpose : Find the Proximity between two Clusters
  --
  -- Prox    : The Proximities between previous Clusters
  -- L1      : The Composition of the first Cluster
  -- L2      : The Composition of the second Cluster
  -- Index1  : The Index of the first Cluster
  -- Index2  : The Index of the second Cluster
  -- Clus    : The Clusters
  -- Clus_Prev:The Previous Clusters
  -- Pt      : The Proximity Type
  -- Ct      : The Clustering Type
  -- Wt      : The Weighting Type
  -- Cp      : The Clustering Parameter, between -1 and +1, necessary for Versatile Linkage and Beta Flexible clusterings
  -- return  : The Proximity
  function Get_Clusters_Proximity(Prox: in PPsDoubles; Li, Lj: in List; Index_I, Index_J: in Positive; Clus, Clus_Prev: in PClusters; Pt: in Proximity_Type; Ct: in Clustering_Type; Wt: in Weighting_Type; Cp: in Double) return Double;

  -- Purpose : Set Lengths of Links and Heights of Leaves of a Dendrogram
  --
  -- T       : The Dendrogram
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the Heights
  procedure Set_Lengths_And_Leaves_Heights(T: in Dendrogram; Pt: in Proximity_Type; Precision: in Natural);

  -- Purpose : Join Clusters
  --
  -- Lol     : The Clusters to be joined
  -- Clus    : The Clusters
  -- Clus_Prev : The Previous Clusters
  -- Total_Clus: The Total Number of Clusters
  -- Prox    : The Proximities between Clusters
  -- Pt      : The Proximity Type
  -- Precision:The number of Significant Digits for the calculations
  -- Ct      : The Clustering Type
  -- Wt      : The Weighting Type
  -- Cp      : The Clustering Parameter, between -1 and +1, necessary for Versatile Linkage and Beta Flexible clusterings
  procedure Join_Clusters(Lol: in List_Of_Lists; Clus, Clus_Prev: in PClusters; Total_Clus: in out Positive; Prox: in PPsDoubles; Pt: in Proximity_Type; Precision: in Natural; Ct: in Clustering_Type; Wt: in Weighting_Type; Cp: in Double; Internal_Names: in Boolean);

end Dendrograms.Algorithms;
