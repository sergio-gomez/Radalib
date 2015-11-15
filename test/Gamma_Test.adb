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


-- @filename Gamma_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 2/12/2004
-- @revision 26/10/2014
-- @brief Test of the Gamma Functions

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Gamma_Functions; use Gamma_Functions;

procedure Gamma_Test is

  function Exponential_Integral(P, X: in Float) return Float is
    Itmax: constant := 100;
    Eps: constant := 1.0e-10;
    Gam: Float;
    Sum: Float;
    Del: Float;
    Idx: Float;
  begin
    if P <= 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument P of Exponential_Integral must be greater to 0.0");
    end if;
    if X <= 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Exponential_Integral must be greater to 0.0");
    else
      Idx := 1.0 - P;
      Gam := Gamma(Idx);
      Sum := 1.0 / (Idx * Gam);
      Del := 1.0;
      for N in 1..Itmax loop
        Idx := Idx + 1.0;
        Del := Del * X / Idx;
        Sum := Sum + Del;
        if (abs Del) < Eps * (abs Sum) then
--          Put("("); Put(N, Width => 0); Put(")");
          return Gam * (X ** (P - 1.0) - Exp(-X) * Sum);
        end if;
      end loop;
      Raise_Exception(Gamma_Error'Identity, "Argument P of Exponential_Integral too large, Itmax too small");
    end if;
  end Exponential_Integral;

  function Inc_Gamma(A, X: in Float) return Float is
  begin
    if A >= 0.0 then
      return Incomplete_Uppercase_Gamma(A, X);
    else
      return (X ** A) * Exponential_Integral(1.0 - A, X);
    end if;
  end Inc_Gamma;

  X: Float;
  A: Float;

begin
  A := -0.2;
  Put("A = "); Put(A);
  Put("  G = "); Put(Gamma(A));
--  Put("  LG = "); Put(Ln_Gamma(A));
  New_Line;
  X := 1.0;
  Put("  X = "); Put(X); New_Line;
  Put("    IEI = "); Put(Inc_Gamma(A, X)); New_Line;
  Put("    IGU = "); Put(Incomplete_Uppercase_Gamma(A, X)); New_Line;
  Put("    IGL = "); Put(Incomplete_Lowercase_Gamma(A, X)); New_Line;
  Put("    P   = "); Put(Incomplete_Gamma_Ratio_P(A, X)); New_Line;
  Put("    Q   = "); Put(Incomplete_Gamma_Ratio_Q(A, X)); New_Line;
  X := 0.5;
  Put("  X = "); Put(X); New_Line;
  Put("    IEI = "); Put(Inc_Gamma(A, X)); New_Line;
  Put("    IGU = "); Put(Incomplete_Uppercase_Gamma(A, X)); New_Line;
  Put("    IGL = "); Put(Incomplete_Lowercase_Gamma(A, X)); New_Line;
  Put("    P   = "); Put(Incomplete_Gamma_Ratio_P(A, X)); New_Line;
  Put("    Q   = "); Put(Incomplete_Gamma_Ratio_Q(A, X)); New_Line;
  X := 1.0e-1;
  Put("  X = "); Put(X); New_Line;
  Put("    IEI = "); Put(Inc_Gamma(A, X)); New_Line;
  Put("    IGU = "); Put(Incomplete_Uppercase_Gamma(A, X)); New_Line;
  Put("    IGL = "); Put(Incomplete_Lowercase_Gamma(A, X)); New_Line;
  Put("    P   = "); Put(Incomplete_Gamma_Ratio_P(A, X)); New_Line;
  Put("    Q   = "); Put(Incomplete_Gamma_Ratio_Q(A, X)); New_Line;
  X := 1.0e-2;
  Put("  X = "); Put(X); New_Line;
  Put("    IEI = "); Put(Inc_Gamma(A, X)); New_Line;
  Put("    IGU = "); Put(Incomplete_Uppercase_Gamma(A, X)); New_Line;
  Put("    IGL = "); Put(Incomplete_Lowercase_Gamma(A, X)); New_Line;
  Put("    P   = "); Put(Incomplete_Gamma_Ratio_P(A, X)); New_Line;
  Put("    Q   = "); Put(Incomplete_Gamma_Ratio_Q(A, X)); New_Line;
  New_Line;
  X := 1.0e-4;
  Put("  X = "); Put(X); New_Line;
  Put("    IEI = "); Put(Inc_Gamma(A, X)); New_Line;
  Put("    IGU = "); Put(Incomplete_Uppercase_Gamma(A, X)); New_Line;
  Put("    IGL = "); Put(Incomplete_Lowercase_Gamma(A, X)); New_Line;
  Put("    P   = "); Put(Incomplete_Gamma_Ratio_P(A, X)); New_Line;
  Put("    Q   = "); Put(Incomplete_Gamma_Ratio_Q(A, X)); New_Line;
  New_Line;
  X := 1.0e-8;
  Put("  X = "); Put(X); New_Line;
  Put("    IEI = "); Put(Inc_Gamma(A, X)); New_Line;
  Put("    IGU = "); Put(Incomplete_Uppercase_Gamma(A, X)); New_Line;
  Put("    IGL = "); Put(Incomplete_Lowercase_Gamma(A, X)); New_Line;
  Put("    P   = "); Put(Incomplete_Gamma_Ratio_P(A, X)); New_Line;
  Put("    Q   = "); Put(Incomplete_Gamma_Ratio_Q(A, X)); New_Line;
  New_Line;
end Gamma_Test;
