-- Radalib, Copyright (c) 2015 by
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


-- @filename Statistics_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 08/11/2007
-- @revision 26/10/2014
-- @brief Test of Statistics

with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Statistics_Float; use Statistics_Float;

procedure Statistics_Test is

  procedure Put(V: in Floats; Aft: Natural := 2) is
  begin
    for I in V'Range loop
      Put(" " & F2Ss(V(I), Aft => Aft));
    end loop;
    New_Line;
  end Put;

  procedure Put(P: in PFloats; Aft: Natural := 2) is
  begin
    for I in P'Range loop
      Put(" " & F2Ss(P(I), Aft => Aft));
    end loop;
    New_Line;
  end Put;

  procedure Put_Data(V: in Floats; Aft: Natural := 1) is
  begin
    Put("  Data:");
    Put(V, Aft => Aft);
    New_Line;
  end Put_Data;

  procedure Put_Data(P: in PFloats; Aft: Natural := 1) is
  begin
    Put("  Data:");
    Put(P, Aft => Aft);
    New_Line;
  end Put_Data;

  procedure Put_Data(V1, V2: in Floats; Aft: Natural := 1) is
  begin
    Put("  DataX:");
    Put(V1, Aft => Aft);
    Put("  DataY:");
    Put(V2, Aft => Aft);
    New_Line;
  end Put_Data;

  procedure Put_Data(P1, P2: in PFloats; Aft: Natural := 1) is
  begin
    Put("  DataX:");
    Put(P1, Aft => Aft);
    Put("  DataY:");
    Put(P2, Aft => Aft);
    New_Line;
  end Put_Data;

  procedure Put_Statistics(V: in Floats; Aft: Natural := 4) is
    Pe: Floats(1..100);
    Wh: Floats(V'Range);
  begin
    -- Unweighted
    Put_Line("    Unweighted:");
    Put_Line("      Minimum  : " & F2S(Minimum(V), Aft => Aft, Exp => 0));
    Put_Line("      Maximum  : " & F2S(Maximum(V), Aft => Aft, Exp => 0));
    Put_Line("      Arithmetic Mean : " & F2S(Arithmetic_Mean(V), Aft => Aft, Exp => 0));
    if Minimum(V) >= 0.0 then
      Put_Line("      Geometric Mean  : " & F2S(Geometric_Mean(V), Aft => Aft, Exp => 0));
      Put_Line("      Harmonic Mean   : " & F2S(Harmonic_Mean(V), Aft => Aft, Exp => 0));
    else
      Put_Line("      Geometric Mean  : undefined");
      Put_Line("      Harmonic Mean   : undefined");
    end if;
    Put_Line("      Variance : " & F2S(Variance(V), Aft => Aft, Exp => 0));
    Put_Line("      Std_Dev  : " & F2S(Standard_Deviation(V), Aft => Aft, Exp => 0));
    Put_Line("      Skewness : " & F2S(Skewness(V), Aft => Aft, Exp => 0));
    Put_Line("      Kurtosis : " & F2S(Kurtosis(V), Aft => Aft, Exp => 0));
    for K in 1..5 loop
      Put_Line("      Central Moment Degree " & I2S(K) & " : " & F2S(Central_Moment(V, K), Aft => Aft, Exp => 0));
    end loop;
    Pe := Percentiles(V);
    for K in 1..3 loop
      Put_Line("      Percentile " & I2S(25 * K) & " : " & F2Se0(Pe(25 * K), Aft => Aft));
    end loop;
    Put("      Rank (min  of tied): "); Put(Ranks(V));
    Put("      Rank (mean of tied): "); Put(Ranks(V, Mean_Of_Tied => True));
    -- Weighted (all equal)
    Wh := (others => 2.0);
    Put("    Weighted: "); Put(Wh);
    Put_Line("      Arithmetic Mean : " & F2S(Arithmetic_Mean(V, Wh), Aft => Aft, Exp => 0));
    if Minimum(V) >= 0.0 then
      Put_Line("      Geometric Mean  : " & F2S(Geometric_Mean(V, Wh), Aft => Aft, Exp => 0));
      Put_Line("      Harmonic Mean   : " & F2S(Harmonic_Mean(V, Wh), Aft => Aft, Exp => 0));
    else
      Put_Line("      Geometric Mean  : undefined");
      Put_Line("      Harmonic Mean   : undefined");
    end if;
    Put_Line("      Variance : " & F2S(Variance(V, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Std_Dev  : " & F2S(Standard_Deviation(V, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Skewness : " & F2S(Skewness(V, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Kurtosis : " & F2S(Kurtosis(V, Wh), Aft => Aft, Exp => 0));
    for K in 1..5 loop
      Put_Line("      Central Moment Degree " & I2S(K) & " : " & F2S(Central_Moment(V, Wh, K), Aft => Aft, Exp => 0));
    end loop;
    -- Weighted (not equal)
    Wh := (others => 2.0);
    Wh(Wh'First) := 1.0;
    Put("    Weighted: "); Put(Wh);
    Put_Line("      Arithmetic Mean : " & F2S(Arithmetic_Mean(V, Wh), Aft => Aft, Exp => 0));
    if Minimum(V) >= 0.0 then
      Put_Line("      Geometric Mean  : " & F2S(Geometric_Mean(V, Wh), Aft => Aft, Exp => 0));
      Put_Line("      Harmonic Mean   : " & F2S(Harmonic_Mean(V, Wh), Aft => Aft, Exp => 0));
    else
      Put_Line("      Geometric Mean  : undefined");
      Put_Line("      Harmonic Mean   : undefined");
    end if;
    Put_Line("      Variance : " & F2S(Variance(V, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Std_Dev  : " & F2S(Standard_Deviation(V, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Skewness : " & F2S(Skewness(V, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Kurtosis : " & F2S(Kurtosis(V, Wh), Aft => Aft, Exp => 0));
    for K in 1..5 loop
      Put_Line("      Central Moment Degree " & I2S(K) & " : " & F2S(Central_Moment(V, Wh, K), Aft => Aft, Exp => 0));
    end loop;
  exception
    when Statistics_Error => Put_Line("    Statistics_Error exception raised");
  end Put_Statistics;

  procedure Put_Statistics(P: in PFloats; Aft: Natural := 4) is
    Pe, Wh: PFloats;
  begin
    -- Alloc weights
    Wh := Alloc(P'First, P'Last);
    -- Unweighted
    Put_Line("    Unweighted:");
    Put_Line("      Minimum  : " & F2S(Minimum(P), Aft => Aft, Exp => 0));
    Put_Line("      Maximum  : " & F2S(Maximum(P), Aft => Aft, Exp => 0));
    Put_Line("      Arithmetic Mean : " & F2S(Arithmetic_Mean(P), Aft => Aft, Exp => 0));
    if Minimum(P) >= 0.0 then
      Put_Line("      Geometric Mean  : " & F2S(Geometric_Mean(P), Aft => Aft, Exp => 0));
      Put_Line("      Harmonic Mean   : " & F2S(Harmonic_Mean(P), Aft => Aft, Exp => 0));
    else
      Put_Line("      Geometric Mean  : undefined");
      Put_Line("      Harmonic Mean   : undefined");
    end if;
    Put_Line("      Variance : " & F2S(Variance(P), Aft => Aft, Exp => 0));
    Put_Line("      Std_Dev  : " & F2S(Standard_Deviation(P), Aft => Aft, Exp => 0));
    Put_Line("      Skewness : " & F2S(Skewness(P), Aft => Aft, Exp => 0));
    Put_Line("      Kurtosis : " & F2S(Kurtosis(P), Aft => Aft, Exp => 0));
    for K in 1..5 loop
      Put_Line("      Central Moment Degree " & I2S(K) & " : " & F2S(Central_Moment(P, K), Aft => Aft, Exp => 0));
    end loop;
    Pe := Percentiles(P);
    for K in 1..3 loop
      Put_Line("      Percentile " & I2S(25 * K) & " : " & F2Se0(Pe(25 * K), Aft => Aft));
    end loop;
    Free(Pe);
    Pe := Ranks(P);
    Put("      Ranks (min  of tied) : "); Put(Pe.all);
    Free(Pe);
    Pe := Ranks(P, Mean_Of_Tied => True);
    Put("      Ranks (mean of tied) : "); Put(Pe.all);
    Free(Pe);
    -- Weighted (all equal)
    Wh.all := (others => 2.0);
    Put("    Weighted: "); Put(Wh.all);
    Put_Line("      Arithmetic Mean : " & F2S(Arithmetic_Mean(P, Wh), Aft => Aft, Exp => 0));
    if Minimum(P) >= 0.0 then
      Put_Line("      Geometric Mean  : " & F2S(Geometric_Mean(P, Wh), Aft => Aft, Exp => 0));
      Put_Line("      Harmonic Mean   : " & F2S(Harmonic_Mean(P, Wh), Aft => Aft, Exp => 0));
    else
      Put_Line("      Geometric Mean  : undefined");
      Put_Line("      Harmonic Mean   : undefined");
    end if;
    Put_Line("      Variance : " & F2S(Variance(P, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Std_Dev  : " & F2S(Standard_Deviation(P, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Skewness : " & F2S(Skewness(P, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Kurtosis : " & F2S(Kurtosis(P, Wh), Aft => Aft, Exp => 0));
    for K in 1..5 loop
      Put_Line("      Central Moment Degree " & I2S(K) & " : " & F2S(Central_Moment(P, Wh, K), Aft => Aft, Exp => 0));
    end loop;
    -- Weighted (not equal)
    Wh.all := (others => 2.0);
    Wh(Wh'First) := 1.0;
    Put("    Weighted: "); Put(Wh.all);
    Put_Line("      Arithmetic Mean : " & F2S(Arithmetic_Mean(P, Wh), Aft => Aft, Exp => 0));
    if Minimum(P) >= 0.0 then
      Put_Line("      Geometric Mean  : " & F2S(Geometric_Mean(P, Wh), Aft => Aft, Exp => 0));
      Put_Line("      Harmonic Mean   : " & F2S(Harmonic_Mean(P, Wh), Aft => Aft, Exp => 0));
    else
      Put_Line("      Geometric Mean  : undefined");
      Put_Line("      Harmonic Mean   : undefined");
    end if;
    Put_Line("      Variance : " & F2S(Variance(P, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Std_Dev  : " & F2S(Standard_Deviation(P, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Skewness : " & F2S(Skewness(P, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Kurtosis : " & F2S(Kurtosis(P, Wh), Aft => Aft, Exp => 0));
    for K in 1..5 loop
      Put_Line("      Central Moment Degree " & I2S(K) & " : " & F2S(Central_Moment(P, Wh, K), Aft => Aft, Exp => 0));
    end loop;
    -- Free weights
    Free(Wh);
  exception
    when Statistics_Error => Put_Line("    Statistics_Error exception raised");
  end Put_Statistics;

  procedure Put_Statistics(V1, V2: in Floats; Aft: Natural := 4) is
    A, B: Float;
    Wh: Floats(V1'Range);
  begin
    -- Unweighted
    Put_Line("    Unweighted:");
    Put_Line("      Covariance : " & F2S(Covariance(V1, V2), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Pearson)  : " & F2S(Correlation(V1, V2), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Spearman) : " & F2S(Spearman_Correlation(V1, V2), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Jackknife)        : " & F2S(Pearson_Correlation_Error(V1, V2, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Bootstrap)        : " & F2S(Pearson_Correlation_Error(V1, V2, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Fisher_Transform) : " & F2S(Pearson_Correlation_Error(V1, V2, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Jackknife)        : " & F2S(Spearman_Correlation_Error(V1, V2, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Bootstrap)        : " & F2S(Spearman_Correlation_Error(V1, V2, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Fisher_Transform) : " & F2S(Spearman_Correlation_Error(V1, V2, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Simple_Linear_Regression(V1, V2, A, B);
    Put_Line("      Simple Linear Regression Y = a X + b :  a = " & F2S(A, Aft => Aft, Exp => 0) & "   b = " & F2S(B, Aft => Aft, Exp => 0));
    Simple_Linear_Regression(V2, V1, A, B);
    Put_Line("      Simple Linear Regression X = a Y + b :  a = " & F2S(A, Aft => Aft, Exp => 0) & "   b = " & F2S(B, Aft => Aft, Exp => 0));
    -- Weighted (all equal)
    Wh := (others => 2.0);
    Put("    Weighted: "); Put(Wh);
    Put_Line("      Covariance : " & F2S(Covariance(V1, V2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Pearson)  : " & F2S(Correlation(V1, V2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Spearman) : " & F2S(Spearman_Correlation(V1, V2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Jackknife)        : " & F2S(Pearson_Correlation_Error(V1, V2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Bootstrap)        : " & F2S(Pearson_Correlation_Error(V1, V2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Fisher_Transform) : " & F2S(Pearson_Correlation_Error(V1, V2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Jackknife)        : " & F2S(Spearman_Correlation_Error(V1, V2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Bootstrap)        : " & F2S(Spearman_Correlation_Error(V1, V2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Fisher_Transform) : " & F2S(Spearman_Correlation_Error(V1, V2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    -- Weighted (not equal)
    Wh := (others => 2.0);
    Wh(Wh'First) := 1.0;
    Put("    Weighted: "); Put(Wh);
    Put_Line("      Covariance : " & F2S(Covariance(V1, V2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Pearson)  : " & F2S(Correlation(V1, V2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Spearman) : " & F2S(Spearman_Correlation(V1, V2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Jackknife)        : " & F2S(Pearson_Correlation_Error(V1, V2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Bootstrap)        : " & F2S(Pearson_Correlation_Error(V1, V2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Fisher_Transform) : " & F2S(Pearson_Correlation_Error(V1, V2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Jackknife)        : " & F2S(Spearman_Correlation_Error(V1, V2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Bootstrap)        : " & F2S(Spearman_Correlation_Error(V1, V2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Fisher_Transform) : " & F2S(Spearman_Correlation_Error(V1, V2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
  exception
    when Statistics_Error => Put_Line("    Statistics_Error exception raised");
  end Put_Statistics;

  procedure Put_Statistics(P1, P2: in PFloats; Aft: Natural := 4) is
    Wh: PFloats;
    A, B: Float;
  begin
    -- Alloc weights
    Wh := Alloc(P1'First, P1'Last);
    -- Unweighted
    Put_Line("    Unweighted:");
    Put_Line("      Covariance : " & F2S(Covariance(P1, P2), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Pearson)  : " & F2S(Correlation(P1, P2), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Spearman) : " & F2S(Spearman_Correlation(P1, P2), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Jackknife)        : " & F2S(Pearson_Correlation_Error(P1, P2, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Bootstrap)        : " & F2S(Pearson_Correlation_Error(P1, P2, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Fisher_Transform) : " & F2S(Pearson_Correlation_Error(P1, P2, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Jackknife)        : " & F2S(Spearman_Correlation_Error(P1, P2, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Bootstrap)        : " & F2S(Spearman_Correlation_Error(P1, P2, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Fisher_Transform) : " & F2S(Spearman_Correlation_Error(P1, P2, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Simple_Linear_Regression(P1, P2, A, B);
    Put_Line("      Simple Linear Regression Y = a X + b :  a = " & F2S(A, Aft => Aft, Exp => 0) & "   b = " & F2S(B, Aft => Aft, Exp => 0));
    Simple_Linear_Regression(P2, P1, A, B);
    Put_Line("      Simple Linear Regression X = a Y + b :  a = " & F2S(A, Aft => Aft, Exp => 0) & "   b = " & F2S(B, Aft => Aft, Exp => 0));
    -- Weighted (all equal)
    Wh.all := (others => 2.0);
    Put("    Weighted: "); Put(Wh.All);
    Put_Line("      Covariance : " & F2S(Covariance(P1, P2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Pearson)  : " & F2S(Correlation(P1, P2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Spearman) : " & F2S(Spearman_Correlation(P1, P2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Jackknife)        : " & F2S(Pearson_Correlation_Error(P1, P2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Bootstrap)        : " & F2S(Pearson_Correlation_Error(P1, P2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Fisher_Transform) : " & F2S(Pearson_Correlation_Error(P1, P2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Jackknife)        : " & F2S(Spearman_Correlation_Error(P1, P2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Bootstrap)        : " & F2S(Spearman_Correlation_Error(P1, P2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Fisher_Transform) : " & F2S(Spearman_Correlation_Error(P1, P2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    -- Weighted (not equal)
    Wh.all := (others => 2.0);
    Wh(Wh'First) := 1.0;
    Put("    Weighted: "); Put(Wh.All);
    Put_Line("      Covariance : " & F2S(Covariance(P1, P2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Pearson)  : " & F2S(Correlation(P1, P2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation (Spearman) : " & F2S(Spearman_Correlation(P1, P2, Wh), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Jackknife)        : " & F2S(Pearson_Correlation_Error(P1, P2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Bootstrap)        : " & F2S(Pearson_Correlation_Error(P1, P2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Pearson , Fisher_Transform) : " & F2S(Pearson_Correlation_Error(P1, P2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Jackknife)        : " & F2S(Spearman_Correlation_Error(P1, P2, Wh, Cet => Jackknife), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Bootstrap)        : " & F2S(Spearman_Correlation_Error(P1, P2, Wh, Cet => Bootstrap), Aft => Aft, Exp => 0));
    Put_Line("      Correlation Error (Spearman, Fisher_Transform) : " & F2S(Spearman_Correlation_Error(P1, P2, Wh, Cet => Fisher_Transform), Aft => Aft, Exp => 0));
    -- Free weights
    Free(Wh);
  exception
    when Statistics_Error => Put_Line("    Statistics_Error exception raised");
  end Put_Statistics;

  P, P1, P2: PFloats;
  V1: Floats(1..7);
  V2: Floats(5..11);

begin

  Put_Line("Tiny data sets"); New_Line;

  P := Alloc(1, 1);
  P(1) := 1.0;
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(0, 1);
  P.all := (0.0, 2.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(-1, 1);
  P.all := (-5.0, 3.0, -2.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(-1, 1);
  P.all := (1.0, 2.0, 1.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(1, 3);
  P.all := (1.0, 3.0, 2.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  Put_Line("Check Skewness"); New_Line;

  P := Alloc(1, 5);
  P.all := (1.0, 2.0, 3.0, 5.0, 9.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(1, 5);
  P.all := (1.0, 5.0, 7.0, 8.0, 9.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  Put_Line("Check Kurtosis"); New_Line;

  P := Alloc(1, 7);
  P.all := (1.0, 1.0, 1.0, 5.0, 9.0, 9.0, 9.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(1, 7);
  P.all := (1.0, 1.0, 5.0, 5.0, 5.0, 9.0, 9.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(1, 7);
  P.all := (1.0, 3.0, 4.0, 5.0, 6.0, 7.0, 9.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(1, 7);
  P.all := (1.0, 5.0, 5.0, 5.0, 5.0, 5.0, 9.0);
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  Put_Line("Check Percentiles"); New_Line;

  P := Alloc(1, 4);
  for I in P'Range loop
    P(I) := Float(I);
  end loop;
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  P := Alloc(-5, 5);
  for I in P'Range loop
    P(I) := Float(I);
  end loop;
  Put_Data(P);
  Put_Statistics(P);
  Free(P);
  New_Line;

  Put_Line("Tiny data pairs"); New_Line;

  P1 := Alloc(1, 3);
  P2 := Alloc(1, 3);
  P1.all := (1.0, 2.0, 3.0);
  P2.all := (1.0, 3.0, 2.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  P1 := Alloc(1, 3);
  P2 := Alloc(1, 3);
  P1.all := (1.0, 2.0, 3.0);
  P2.all := (1.0, 3.0, 5.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  P1 := Alloc(1, 3);
  P2 := Alloc(1, 3);
  P1.all := (1.0, 2.0, 3.0);
  P2.all := (7.0, 4.0, 1.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  P1 := Alloc(1, 3);
  P2 := Alloc(1, 3);
  P1.all := (1.0, 2.0, 3.0);
  P2.all := (1.0, 2.0, 1.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  P1 := Alloc(1, 3);
  P2 := Alloc(1, 3);
  P1.all := (1.0, 1.0, 1.0);
  P2.all := (1.0, 2.0, 1.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  P1 := Alloc(1, 3);
  P2 := Alloc(1, 3);
  P1.all := (1.0, 2.0, 3.0);
  P2.all := (1.0, 2.0, 3.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  P1 := Alloc(1, 7);
  P2 := Alloc(1, 7);
  P1.all := (1.0, 2.0, 3.0, 6.0, 9.0, 10.0, 11.0);
  P2.all := (-20.0, -3.0, -1.0, 0.0, 1.0, 4.0, 15.0);
  Put_Data(P1, P2);
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  Put_Line("Check Floats versus PFloats"); New_Line;

  V1 := (1.0, 1.0, 3.0, 5.0, 2.0, 5.0, 4.0);
  P1 := Alloc(V1'First, V1'Last);
  P1.all := V1;
  Put_Data(P1);
  Put_Line("  Floats:");
  Put_Statistics(V1);
  Put_Line("  PFloats:");
  Put_Statistics(P1);
  Free(P1);
  New_Line;

  V1 := (1.0, -1.0, -3.0, 5.0, 2.0, 5.0, 4.0);
  P1 := Alloc(V1'First, V1'Last);
  P1.all := V1;
  Put_Data(P1);
  Put_Line("  Floats:");
  Put_Statistics(V1);
  Put_Line("  PFloats:");
  Put_Statistics(P1);
  Free(P1);
  New_Line;

  V1 := (1.0, -1.0, -3.0, 5.0, 2.0, 5.0, 4.0);
  V2 := (0.0, -1.0, -3.0, 7.0, 2.0, 5.0, 4.0);
  P1 := Alloc(V1'First, V1'Last);
  P2 := Alloc(V2'First, V2'Last);
  P1.all := V1;
  P2.all := V2;
  Put_Data(P1, P2);
  Put_Line("  Floats:");
  Put_Statistics(V1, V2);
  Put_Line("  PFloats:");
  Put_Statistics(P1, P2);
  Free(P1);
  Free(P2);
  New_Line;

  Put_Line("Real data: LSAT vs GPA"); New_Line;
  P1 := Alloc(1, 15);
  P2 := Alloc(1, 15);
  P1.all := (576.0, 635.0, 558.0, 578.0, 666.0, 580.0, 555.0, 661.0, 651.0, 605.0, 653.0, 575.0, 545.0, 572.0, 594.0);
  P2.all := (3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96);
  Put("  LSAT: ");
  Put(P1, Aft => 1); New_Line;
  Put_Statistics(P1, 6);
  New_Line;
  Put("  GPA: ");
  Put(P2, Aft => 2); New_Line;
  Put_Statistics(P2, 6);
  New_Line;
  Put_Line(" LSAT vs GPA:");
  Put_Statistics(P1, P2, 6);
  Free(P1);
  Free(P2);
  New_Line;

end Statistics_Test;
