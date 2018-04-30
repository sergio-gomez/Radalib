-- Radalib, Copyright (c) 2018 by
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


-- @filename Contingency_Tables.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 1/04/2005
-- @revision 08/04/2018
-- @brief Treatment of Contingency Tables

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Utils; use Utils;

package Contingency_Tables is

  -- Represents a Contingency Table
  type Contingency_Table is private;

  -- Type to choose between different Normalization Schemes
  type Normalization_Method is (Maximum, Arithmetic_Mean, Geometric_Mean, Minimum);

  Incompatible_Lists_Of_Lists_Error: exception;
  Uninitialized_Contingency_Table_Error: exception;
  List_Not_Found_Error: exception;


  -- Purpose : Initialize a Contingency Table from two List of Lists
  -- Note    : The number of Elements of both Lists of Lists have to be equal
  -- Note    : The contingency Table is automatically Updated upon Initialization
  --
  -- Ct      : The Contingency Table
  -- Lol1    : The First List of Lists
  -- Lol2    : The Second List of Lists
  -- raises  : Incompatible_Lists_Of_Lists_Error
  procedure Initialize(Ct: out Contingency_Table; Lol1, Lol2: in List_Of_Lists);

  -- Purpose : To know whether a Contingency Table is Initialized
  --
  -- Ct      : The Contingency_Table
  -- return  : True if Initialized, False otherwise
  function Is_Initialized(Ct: in Contingency_Table) return Boolean;

  -- Purpose : Deallocate all the space used by a Contingency Table
  --
  -- Ct      : The Contingency_Table
  procedure Free(Ct: in out Contingency_Table);

  -- Purpose : Recalculate a Contingency Table
  -- Note    : Needed if Lists of Lists have changed
  --
  -- Ct      : The Contingency Table
  -- raises  : Uninitialized_Contingency_Table_Error
  procedure Update(Ct: in Contingency_Table);

  -- Purpose : Obtain the Lists of Lists of a Contingency Table
  --
  -- Ct      : The Contingency Table
  -- Lol1    : The First List of Lists
  -- Lol2    : The Second List of Lists
  -- raises  : Uninitialized_Contingency_Table_Error
  procedure Get_Lists_Of_Lists(Ct: in Contingency_Table; Lol1, Lol2: out List_Of_Lists);

  -- Purpose : Obtain an element of the Contingency Table
  --
  -- Ct      : The Contingency Table
  -- L1      : A List of the first List of Lists
  -- L2      : A List of the second List of Lists
  -- return  : The number of elements at L1 and L2
  -- raises  : Uninitialized_Contingency_Table_Error
  -- raises  : List_Not_Found_Error
  function Number_Of_Elements(Ct: in Contingency_Table; L1, L2: in List) return Natural;

  -- Purpose : Transpose a Contingency Table
  -- Note    : The Lists of Lists are exchanged
  --
  -- Ct      : The Contingency Table
  -- raises  : Uninitialized_Contingency_Table_Error
  procedure Transpose(Ct: in Contingency_Table);

  -- Purpose : Get the Number of Pairs of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  --
  -- Ct      : The Contingency Table
  -- return  : The Number of Pairs
  -- raises  : Uninitialized_Contingency_Table_Error
  function Number_Of_Pairs(Ct: in Contingency_Table) return Longint;

  -- Purpose : Get the Number of Same Class Agreements of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  --
  -- Ct      : The Contingency Table
  -- return  : The Number of Same Class Agreements
  -- raises  : Uninitialized_Contingency_Table_Error
  function Number_Of_Same_Class_Agreements(Ct: in Contingency_Table) return Longint;

  -- Purpose : Get the Number of Agreements of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  --
  -- Ct      : The Contingency Table
  -- return  : The Number of Agreements
  -- raises  : Uninitialized_Contingency_Table_Error
  function Number_Of_Agreements(Ct: in Contingency_Table) return Longint;

  -- Purpose : Get the Number of Disagreements of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  --
  -- Ct      : The Contingency Table
  -- D1      : The Number of Disagreements (same class in Lol1, different class in Lol2)
  -- D2      : The Number of Disagreements (same class in Lol2, different class in Lol1)
  -- raises  : Uninitialized_Contingency_Table_Error
  procedure Number_Of_Disagreements(Ct: in Contingency_Table; D1, D2: out Longint);

  -- Purpose : Get the Number of Disagreements of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  --
  -- Ct      : The Contingency Table
  -- return  : The Number of Disagreements
  -- raises  : Uninitialized_Contingency_Table_Error
  function Number_Of_Disagreements(Ct: in Contingency_Table) return Longint;

  -- Purpose : Get the Rand Index of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : W.M. Rand: J. American Statistical Association, 66:846-850, 1971
  --
  -- Ct      : The Contingency Table
  -- return  : The Rand Index
  -- raises  : Uninitialized_Contingency_Table_Error
  function Rand_Index(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Adjusted Rand Index of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : L. Hubert, P. Arabie: J. Classification, 2:193-218, 1985
  --
  -- Ct      : The Contingency Table
  -- return  : The Adjusted Rand Index
  -- raises  : Uninitialized_Contingency_Table_Error
  function Adjusted_Rand_Index(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Jaccard Index of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : P. Jaccard: The New Phytologist, 11(2):37-50, 1912
  --
  -- Ct      : The Contingency Table
  -- return  : The Jaccard Index
  -- raises  : Uninitialized_Contingency_Table_Error
  function Jaccard_Index(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Asymmetric Wallace Index of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : D.L. Wallace: J. American Statistical Association, 78(383):569-576, 1983
  --
  -- Ct      : The Contingency Table
  -- Awi1    : The Asymmetric Wallace Index (degree of inclusion of Lol1 clusters in Lol2 clusters)
  -- Awi2    : The Asymmetric Wallace Index (degree of inclusion of Lol2 clusters in Lol1 clusters)
  -- raises  : Uninitialized_Contingency_Table_Error
  procedure Asymmetric_Wallace_Index(Ct: in Contingency_Table; Awi1, Awi2: out Float);

  -- Purpose : Get the Fowlkes-Mallows Index of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : E.B. Fowlkes, C.L. Mallows: J. American Statistical Association, 78(383):553-569, 1983
  --
  -- Ct      : The Contingency Table
  -- return  : The Fowlkes-Mallows Index
  -- raises  : Uninitialized_Contingency_Table_Error
  function Fowlkes_Mallows_Index(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Normalized Mutual Information Index of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : A. Strehl, J. Ghosh: J. Machine Learning Research, 3:583-618, 2002
  -- Variants: N.X. Vinh, J. Epps, J. Bailey: J. Machine Learning Research, 11:2837-2854, 2010
  --
  -- Ct      : The Contingency Table
  -- Mean    : Normalization method
  -- return  : The Normalized Mutual Information index
  -- raises  : Uninitialized_Contingency_Table_Error
  function Normalized_Mutual_Information_Index(Ct: in Contingency_Table; Mean: in Normalization_Method := Geometric_Mean) return Float;

  -- Purpose : Get the Mirkin Metric of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : B.G. Mirkin: Mathematical Classification and Clustering, Kluwer Academic Press, Dordrecht, 1996
  --
  -- Ct      : The Contingency Table
  -- return  : The Mirkin Metric
  -- raises  : Uninitialized_Contingency_Table_Error
  function Mirkin_Metric(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Normalized Mirkin Metric of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : B.G. Mirkin: Mathematical Classification and Clustering, Kluwer Academic Press, Dordrecht, 1996
  --
  -- Ct      : The Contingency Table
  -- return  : The Normalized Mirkin Metric
  -- raises  : Uninitialized_Contingency_Table_Error
  function Normalized_Mirkin_Metric(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the van Dongen Metric of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : S. van Dongen: Technical Report INS-R0012, Centrumm voor Wiskunde en Informatica, 2000
  --
  -- Ct      : The Contingency Table
  -- return  : The van Dongen Metric
  -- raises  : Uninitialized_Contingency_Table_Error
  function Van_Dongen_Metric(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Normalized van Dongen Metric of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : S. van Dongen: Technical Report INS-R0012, Centrumm voor Wiskunde en Informatica, 2000
  --
  -- Ct      : The Contingency Table
  -- return  : The Normalized van Dongen Metric
  -- raises  : Uninitialized_Contingency_Table_Error
  function Normalized_Van_Dongen_Metric(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Variation of Information Metric of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : M. Meila: J. Multivariate Analysis, 98:873-895, 2007
  --
  -- Ct      : The Contingency Table
  -- return  : The Variation of Information Metric
  -- raises  : Uninitialized_Contingency_Table_Error
  function Variation_Of_Information_Metric(Ct: in Contingency_Table) return Float;

  -- Purpose : Get the Normalized Variation of Information Metric of a Contingency Table
  -- Note    : Unassigned Elements are considered as belonging to single element Lists
  -- Defined : M. Meila: J. Multivariate Analysis, 98:873-895, 2007
  --
  -- Ct      : The Contingency Table
  -- return  : The Normalized Variation of Information Metric
  -- raises  : Uninitialized_Contingency_Table_Error
  function Normalized_Variation_Of_Information_Metric(Ct: in Contingency_Table) return Float;


private

  -----------
  -- Lists --
  -----------

  type Lists is array(Natural range <>) of List;
  type PLists is access Lists;

  -----------------------
  -- Contingency_Table --
  -----------------------

  type Contingencies is array(Natural range <>, Natural range<>) of Natural;
  type PContingencies is access Contingencies;

  type Contingency_Rec is record
    Lol1: List_Of_Lists;
    Lol2: List_Of_Lists;
    Ls1: Plists;
    Ls2: Plists;
    Num12: PContingencies;
  end record;

  type Contingency_Table is access Contingency_Rec;


  -- Purpose : Find the Combinatorial Number M over 2
  --
  -- M       : The number of Elements
  -- return  : M over 2
  function Combi2(M: in Natural) return Longint;
  pragma Inline(Combi2);

  -- Purpose : Find the Index of a List in a PLists
  --
  -- Ls      : The PLists
  -- L       : The List
  -- return  : The Index
  -- raises  : List_Not_Found_Error
  function Find_Index(Ls: in PLists; L: in List) return Natural;

end Contingency_Tables;
