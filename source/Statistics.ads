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


-- @filename Statistics.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/11/2007
-- @revision 26/10/2014
-- @brief Statistics of Numerical Arrays

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

generic
  type Num is digits <>;
  type Nums is array(Integer range <>) of Num;
  type PNums is access Nums;
  with function Alloc(First, Last: in Integer) return PNums is <>;
  with procedure Free(P: in out PNums) is <>;
package Statistics is

  type Correlation_Type is (Pearson, Spearman);
  type Correlation_Error_Type is (Jackknife, Bootstrap, Fisher_Transform, Auto);

  Default_Bootstrap_Size: constant Positive := 1000;

  Statistics_Error: exception;
  Negative_Value_Error: exception;

  -- Purpose : Obtain the Size of an Array
  --
  -- V       : The Array
  -- return  : The Size
  -- raises  : Statistics_Error
  function Size(V: in Nums) return Natural;
  function Size(P: in PNums) return Natural;

  -- Purpose : Obtain the Minimum of an Array
  --
  -- V       : The Array
  -- return  : The Minimum
  -- raises  : Statistics_Error
  function Minimum(V: in Nums) return Num;
  function Minimum(P: in PNums) return Num;

  -- Purpose : Obtain the Maximum of an Array
  --
  -- V       : The Array
  -- return  : The Maximum
  -- raises  : Statistics_Error
  function Maximum(V: in Nums) return Num;
  function Maximum(P: in PNums) return Num;

  -- Purpose : To know if an Array has a Negative Value
  --
  -- V       : The Array
  -- return  : True if Has Negative Value
  -- raises  : Statistics_Error
  function Has_Negative(V: in Nums) return Boolean;
  function Has_Negative(P: in PNums) return Boolean;

  -- Purpose : Obtain the (weighted) Sum of (the power of) the elements in an Array
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- Degree  : The Degree
  -- return  : The Sum
  -- raises  : Statistics_Error
  function Sum(V: in Nums; Degree: in Natural := 1) return Num;
  function Sum(P: in PNums; Degree: in Natural := 1) return Num;
  function Sum(V, Wh: in Nums; Degree: in Natural := 1) return Num;
  function Sum(P, Wh: in PNums; Degree: in Natural := 1) return Num;

  -- Purpose : Obtain the (weighted) Arithmetic Mean of an Array
  -- Note    : The value returned is an unbiased estimator of the population Arithmetic Mean
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Arithmetic Mean
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Arithmetic_Mean(V: in Nums) return Num;
  function Arithmetic_Mean(P: in PNums) return Num;
  function Arithmetic_Mean(V, Wh: in Nums) return Num;
  function Arithmetic_Mean(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Geometric Mean of an Array
  -- Note    : All data must be non-negative
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Geometric Mean
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Geometric_Mean(V: in Nums) return Num;
  function Geometric_Mean(P: in PNums) return Num;
  function Geometric_Mean(V, Wh: in Nums) return Num;
  function Geometric_Mean(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Harmonic Mean of an Array
  -- Note    : All data must be non-negative
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Harmonic Mean
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Harmonic_Mean(V: in Nums) return Num;
  function Harmonic_Mean(P: in PNums) return Num;
  function Harmonic_Mean(V, Wh: in Nums) return Num;
  function Harmonic_Mean(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Central Moment of an Array
  -- Note    : The value returned is the sample biased estimator of the Central Moment
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- Degree  : The Degree
  -- return  : The Central Moment
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Central_Moment(V: in Nums; Degree: in Natural) return Num;
  function Central_Moment(P: in PNums; Degree: in Natural) return Num;
  function Central_Moment(V, Wh: in Nums; Degree: in Natural) return Num;
  function Central_Moment(P, Wh: in PNums; Degree: in Natural) return Num;

  -- Purpose : Obtain the (weighted) Variance of an Array
  -- Note    : The value returned is the unbiased estimator of the population Variance
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Variance
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Variance(V: in Nums) return Num;
  function Variance(P: in PNums) return Num;
  function Variance(V, Wh: in Nums) return Num;
  function Variance(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Standard Deviation of an Array
  -- Note    : The value returned is the bias corrected (but not unbiased) estimator of the population Standard Deviation
  -- Note    : It depends on the unbiased estimator of the second cumulant, the Variance
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Standard Deviation
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Standard_Deviation(V: in Nums) return Num;
  function Standard_Deviation(P: in PNums) return Num;
  function Standard_Deviation(V, Wh: in Nums) return Num;
  function Standard_Deviation(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Skewness of an Array
  -- Note    : The value returned is the bias corrected (but not unbiased) estimator of the population Skewness
  -- Note    : It depends on the unbiased estimators of the second and third cumulants
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Skewness
  -- raises  : Statistics_Error
  function Skewness(V: in Nums) return Num;
  function Skewness(P: in PNums) return Num;
  function Skewness(V, Wh: in Nums) return Num;
  function Skewness(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Kurtosis of an Array
  -- Note    : The value returned is the bias corrected (but not unbiased) estimator of the population Kurtosis
  -- Note    : It depends on the unbiased estimators of the second and fourth cumulants
  --
  -- V       : The Array
  -- Wh      : The Weights
  -- return  : The Kurtosis
  -- raises  : Statistics_Error
  function Kurtosis(V: in Nums) return Num;
  function Kurtosis(P: in PNums) return Num;
  function Kurtosis(V, Wh: in Nums) return Num;
  function Kurtosis(P, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Covariance of two Arrays
  -- Note    : The value returned is the unbiased estimator of the population Covariance
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- return  : The Covariance
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Covariance(V1, V2: in Nums) return Num;
  function Covariance(P1, P2: in PNums) return Num;
  function Covariance(V1, V2, Wh: in Nums) return Num;
  function Covariance(P1, P2, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Correlation of two Arrays
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- Ct      : The Correlation type
  -- return  : The Pearson Correlation
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Correlation(V1, V2: in Nums; Ct: in Correlation_Type) return Num;
  function Correlation(P1, P2: in PNums; Ct: in Correlation_Type) return Num;
  function Correlation(V1, V2, Wh: in Nums; Ct: in Correlation_Type) return Num;
  function Correlation(P1, P2, Wh: in PNums; Ct: in Correlation_Type) return Num;

  -- Purpose : Obtain the (weighted) Pearson Correlation of two Arrays
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- return  : The Pearson Correlation
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Pearson_Correlation(V1, V2: in Nums) return Num;
  function Pearson_Correlation(P1, P2: in PNums) return Num;
  function Pearson_Correlation(V1, V2, Wh: in Nums) return Num;
  function Pearson_Correlation(P1, P2, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Spearman Rank Correlation of two Arrays
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- return  : The Spearman Correlation
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Spearman_Correlation(V1, V2: in Nums) return Num;
  function Spearman_Correlation(P1, P2: in PNums) return Num;
  function Spearman_Correlation(V1, V2, Wh: in Nums) return Num;
  function Spearman_Correlation(P1, P2, Wh: in PNums) return Num;

  -- Purpose : Obtain the (weighted) Correlation Error of two Arrays
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- Ct      : The Correlation type
  -- Cet     : The Correlation Error type
  -- return  : The Correlation Error
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Correlation_Error(V1, V2: in Nums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num;
  function Correlation_Error(P1, P2: in PNums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num;
  function Correlation_Error(V1, V2, Wh: in Nums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num;
  function Correlation_Error(P1, P2, Wh: in PNums; Ct: in Correlation_Type; Cet: in Correlation_Error_Type := Auto) return Num;

  -- Purpose : Obtain the (weighted) Pearson Correlation Error of two Arrays
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- Cet     : The Correlation Error type
  -- return  : The Pearson Correlation Error
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Pearson_Correlation_Error(V1, V2: in Nums; Cet: in Correlation_Error_Type := Auto) return Num;
  function Pearson_Correlation_Error(P1, P2: in PNums; Cet: in Correlation_Error_Type := Auto) return Num;
  function Pearson_Correlation_Error(V1, V2, Wh: in Nums; Cet: in Correlation_Error_Type := Auto) return Num;
  function Pearson_Correlation_Error(P1, P2, Wh: in PNums; Cet: in Correlation_Error_Type := Auto) return Num;

  -- Purpose : Obtain the (weighted) Spearman Correlation Error of two Arrays
  --
  -- V1      : The First Array
  -- V2      : The Second Array
  -- Wh      : The Weights
  -- Cet     : The Correlation Error type
  -- return  : The Spearman Correlation Error
  -- raises  : Statistics_Error
  -- raises  : Negative_Value_Error
  function Spearman_Correlation_Error(V1, V2: in Nums; Cet: in Correlation_Error_Type := Auto) return Num;
  function Spearman_Correlation_Error(P1, P2: in PNums; Cet: in Correlation_Error_Type := Auto) return Num;
  function Spearman_Correlation_Error(V1, V2, Wh: in Nums; Cet: in Correlation_Error_Type := Auto) return Num;
  function Spearman_Correlation_Error(P1, P2, Wh: in PNums; Cet: in Correlation_Error_Type := Auto) return Num;

  -- Purpose : Obtain the Simple Linear regression of two Arrays
  -- Note    : Fitting of:  Y = Slope X + Intercept
  -- Note    : If Variance(X) = 0, we return Slope = 0 and Intercept = Average(Y)
  --
  -- X       : The Array of the Input
  -- Y       : The Array of the Output
  -- Slope   : The Slope
  -- Intercept:The Intercept
  -- raises  : Statistics_Error
  procedure Simple_Linear_Regression(X, Y: in Nums; Slope, Intercept: out Num);
  procedure Simple_Linear_Regression(X, Y: in PNums; Slope, Intercept: out Num);

  -- Purpose : Obtain Percentiles
  -- Note    : Matlab algorithm is used
  -- Note    : An array with the 100 percentiles is returned
  --
  -- V       : The Array
  -- return  : The Percentiles
  -- raises  : Statistics_Error
  function Percentiles(V: in Nums) return Nums;
  function Percentiles(P: in PNums) return PNums;

  -- Purpose : Obtain Ranks
  -- Note    : The Ranks correspond to a sorting in ascending order
  --
  -- V       : The Array
  -- return  : The Ranks
  -- raises  : Statistics_Error
  function Ranks(V: in Nums; Mean_Of_Tied: in Boolean := False) return Nums;
  function Ranks(P: in PNums; Mean_Of_Tied: in Boolean := False) return PNums;

  -- Purpose : Obtain the current Bootstrap Size
  --
  -- return  : The Bootstrap Size
  function Get_Bootstrap_Size return Positive;

  -- Purpose : Set the Bootstrap Size
  --
  -- Bs      : The Bootstrap Size
  procedure Set_Bootstrap_Size(Bs: in Positive := Default_Bootstrap_Size);


  function Min(V: in Nums) return Num renames Minimum;
  function Min(P: in PNums) return Num renames Minimum;
  function Max(V: in Nums) return Num renames Maximum;
  function Max(P: in PNums) return Num renames Maximum;
  function Average(V: in Nums) return Num renames Arithmetic_Mean;
  function Average(P: in PNums) return Num renames Arithmetic_Mean;
  function Average(V, Wh: in Nums) return Num renames Arithmetic_Mean;
  function Average(P, Wh: in PNums) return Num renames Arithmetic_Mean;
  function Correlation(V1, V2: in Nums) return Num renames Pearson_Correlation;
  function Correlation(P1, P2: in PNums) return Num renames Pearson_Correlation;
  function Correlation(V1, V2, Wh: in Nums) return Num renames Pearson_Correlation;
  function Correlation(P1, P2, Wh: in PNums) return Num renames Pearson_Correlation;

private

  subtype Correlation_Error_Subtype is Correlation_Error_Type range Correlation_Error_Type'First..Correlation_Error_Type'Pred(Auto);

  Bootstrap_Size: Positive := Default_Bootstrap_Size;
  Bootstrap_G: Generator;


  -- Purpose : Automatic selection of the Correlation Error Subtype
  --
  -- N       : The Size of the sample
  -- return  : The Correlation Error Subtype
  function Select_Correlation_Error_Subtype(N: in Natural) return Correlation_Error_Subtype;

  -- Purpose : Obtain the Fisher Transform estimation of the Correlation Error
  --
  -- Corr    : The Correlation
  -- N       : The Size of the sample
  -- return  : The Fisher Transform Sigma
  function Fisher_Tranform_Sigma(Corr: in Num; N: in Natural) return Num;

end Statistics;
