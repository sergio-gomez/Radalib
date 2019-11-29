-- Radalib, Copyright (c) 2019 by
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


-- @filename Gamma_Functions.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 29/11/2004
-- @revision 5/12/2004
-- @brief Calculation of Gamma Functions

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Gamma_Functions is

  -----------
  -- Gamma --
  -----------

  function Gamma(A: in Float) return Float is
  begin
    if A > 0.0 then
      return Exp(Ln_Gamma(A));
    else
      return Pi / (Sin(Pi * A) * Exp(Ln_Gamma(1.0 - A)));
    end if;
  exception
    when others =>
      Raise_Exception(Gamma_Error'Identity, "Invalid argument for Gamma");
  end Gamma;

  --------------
  -- Ln_Gamma --
  --------------

  function Ln_Gamma(A: in Float) return Float is
    Coef: constant array(0..6) of Float := (2.5066282746310005,
                                            76.18009172947146, -86.50532032941677,
                                            24.01409824083091, -1.231739572450155,
                                            0.1208650973866179e-2, -0.5395239384953e-5);
    X: constant Float := A;
    Y: Float := A;
    Ser: Float := 1.000000000190015;
    Tmp: Float;
  begin
    if A <= 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument of Ln_Gamma must be greater to 0.0");
    end if;
    Tmp := X + 5.5;
    Tmp := (X + 0.5) * Log(Tmp) - Tmp;
    for J in 1..6 loop
      Y := Y + 1.0;
      Ser := Ser + Coef(J) / Y;
    end loop;
    return Tmp + Log(Coef(0) * Ser / X);
  exception
    when others =>
      Raise_Exception(Gamma_Error'Identity, "Invalid argument for Ln_Gamma");
  end Ln_Gamma;

  --------------------------------
  -- Incomplete_Uppercase_Gamma --
  --------------------------------

  function Incomplete_Uppercase_Gamma(A: in Float; X: in Float) return Float is
    Xmax: constant := 95.0;
    Gam_Ln: Float;
    Gam_P: Float;
    Gam_Q: Float;
    Gam: Float;
    Atmp: Float;
    Steps: Integer;
  begin
    if X < 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Incomplete_Uppercase_Gamma must be greater or equal to 0.0");
    elsif X = 0.0 then
      return Float'Last;
    elsif X >= Xmax then
      return 0.0;
    else
      if A < 0.0 then
        Steps:= -Integer(Float'Floor(A));
        Atmp := A + Float(Steps);
        Gam := Incomplete_Uppercase_Gamma(Atmp, X);
        for I in reverse 0..Steps-1 loop
          Atmp := A + Float(I);
          Gam := (Gam - (X ** Atmp) * Exp(-X)) / Atmp;
        end loop;
        return Gam;
      elsif X < A + 1.0 then
        Gamma_P_Series(A, X, Gam_P, Gam_Ln);
        return (1.0 - Gam_P) * Exp(Gam_Ln);
      else
        Gamma_Q_Continued_Fraction(A, X, Gam_Q, Gam_Ln);
        return Gam_Q * Exp(Gam_Ln);
      end if;
    end if;
  exception
    when others =>
      Raise_Exception(Gamma_Error'Identity, "Invalid argument A for Incomplete_Uppercase_Gamma");
  end Incomplete_Uppercase_Gamma;

  --------------------------------
  -- Incomplete_Lowercase_Gamma --
  --------------------------------

  function Incomplete_Lowercase_Gamma(A: in Float; X: in Float) return Float is
    Xmax: constant := 95.0;
    Gam_Ln: Float;
    Gam_P: Float;
    Gam_Q: Float;
    Gam: Float;
    Atmp: Float;
    Steps: Integer;
  begin
    if X < 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Incomplete_Lowercase_Gamma must be greater or equal to 0.0");
    elsif X = 0.0 then
      return 0.0;
    elsif X >= Xmax then
      return Gamma(A);
    else
      if A < 0.0 then
        Steps:= -Integer(Float'Floor(A));
        Atmp := A + Float(Steps);
        Gam := Incomplete_Lowercase_Gamma(Atmp, X);
        for I in reverse 0..Steps-1 loop
          Atmp := A + Float(I);
          Gam := (Gam + (X ** Atmp) * Exp(-X)) / Atmp;
        end loop;
        return Gam;
      elsif X < A + 1.0 then
        Gamma_P_Series(A, X, Gam_P, Gam_Ln);
        return Gam_P * Exp(Gam_Ln);
      else
        Gamma_Q_Continued_Fraction(A, X, Gam_Q, Gam_Ln);
        return (1.0 - Gam_Q) * Exp(Gam_Ln);
      end if;
    end if;
  exception
    when others =>
      Raise_Exception(Gamma_Error'Identity, "Invalid argument A for Incomplete_Lowercase_Gamma");
  end Incomplete_Lowercase_Gamma;

  ------------------------------
  -- Incomplete_Gamma_Ratio_Q --
  ------------------------------

  function Incomplete_Gamma_Ratio_Q(A: in Float; X: in Float) return Float is
    Xmax: constant := 95.0;
    Gam_Ln: Float;
    Gam_P: Float;
    Gam_Q: Float;
  begin
    if X < 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Incomplete_Gamma_Ratio_Q must be greater or equal to 0.0");
    elsif X = 0.0 then
      return 1.0;
    elsif X >= Xmax then
      return 0.0;
    else
      if A < 0.0 then
        return Incomplete_Uppercase_Gamma(A, X) / Gamma(A);
      elsif X < A + 1.0 then
        Gamma_P_Series(A, X, Gam_P, Gam_Ln);
        return 1.0 - Gam_P;
      else
        Gamma_Q_Continued_Fraction(A, X, Gam_Q, Gam_Ln);
        return Gam_Q;
      end if;
    end if;
  exception
    when others =>
      Raise_Exception(Gamma_Error'Identity, "Invalid argument A for Incomplete_Gamma_Ratio_Q");
  end Incomplete_Gamma_Ratio_Q;

  ------------------------------
  -- Incomplete_Gamma_Ratio_P --
  ------------------------------

  function Incomplete_Gamma_Ratio_P(A: in Float; X: in Float) return Float is
    Xmax: constant := 95.0;
    Gam_Ln: Float;
    Gam_P: Float;
    Gam_Q: Float;
  begin
    if X < 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Incomplete_Gamma_Ratio_P must be greater or equal to 0.0");
    elsif X = 0.0 then
      return 0.0;
    elsif X >= Xmax then
      return 1.0;
    else
      if A < 0.0 then
        return Incomplete_Lowercase_Gamma(A, X) / Gamma(A);
      elsif X < A + 1.0 then
        Gamma_P_Series(A, X, Gam_P, Gam_Ln);
        return Gam_P;
      else
        Gamma_Q_Continued_Fraction(A, X, Gam_Q, Gam_Ln);
        return 1.0 - Gam_Q;
      end if;
    end if;
  exception
    when others =>
      Raise_Exception(Gamma_Error'Identity, "Invalid argument A for Incomplete_Gamma_Ratio_P");
  end Incomplete_Gamma_Ratio_P;

  --------------------
  -- Gamma_P_Series --
  --------------------

  procedure Gamma_P_Series(A, X: in Float; Gam_P, Gam_Ln: out Float) is
    Itmax: constant := 100;
    Eps: constant := 3.0e-7;
    Xmax: constant := 95.0;
    Ap: Float;
    Del: Float;
    Sum: Float;
  begin
    if A <= 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument A of Gamma_P_Series must be greater to 0.0");
    end if;
    Gam_Ln := Ln_Gamma(A);
    Gam_P := 0.0;
    if X < 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Gamma_P_Series must be greater or equal to 0.0");
    elsif X = 0.0 then
      Gam_P := 0.0;
      return;
    elsif X >= Xmax then
      Gam_P := 1.0;
      return;
    else
      Ap := A;
      Sum := 1.0 / A;
      Del := Sum;
      for N in 1..Itmax loop
        Ap := Ap + 1.0;
        Del := Del * X / Ap;
        Sum := Sum + Del;
        if (abs Del) < Eps * (abs Sum) then
          Gam_P := Sum * Exp(-X + A * Log(X) - Gam_Ln);
          return;
        end if;
      end loop;
      Raise_Exception(Gamma_Error'Identity, "Argument A of Gamma_P_Series too large, Itmax too small");
    end if;
  end Gamma_P_Series;

  --------------------------------
  -- Gamma_Q_Continued_Fraction --
  --------------------------------

  procedure Gamma_Q_Continued_Fraction(A, X: in Float; Gam_Q, Gam_Ln: out Float) is
    Itmax: constant := 100;
    Eps: constant := 3.0e-7;
    Fpmin: constant := 1.0e-30;
    Xmax: constant := 95.0;
    An: Float;
    B: Float;
    C: Float;
    D: Float;
    Del: Float;
    H: Float;
  begin
    if A <= 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument A of Gamma_Q_Continued_Fraction must be greater to 0.0");
    end if;
    Gam_Ln := Ln_Gamma(A);
    Gam_Q := 1.0;
    if X < 0.0 then
      Raise_Exception(Gamma_Error'Identity, "Argument X of Gamma_Q_Continued_Fraction must be greater or equal to 0.0");
    elsif X = 0.0 then
      Gam_Q := 1.0;
      return;
    elsif X >= Xmax then
      Gam_Q := 0.0;
      return;
    else
      B := X + 1.0 - A;
      C := 1.0 / Fpmin;
      D := 1.0 / B;
      H := D;
      for I in 1..Itmax loop
        An := -Float(I) * (Float(I) - A);
        B := B + 2.0;
        D := An * D + B;
        if (abs D) < Fpmin then
          D := Fpmin;
        end if;
        C := B + An / C;
        if (abs C) < Fpmin then
          C := Fpmin;
        end if;
        D := 1.0 / D;
        Del := D * C;
        H := H * Del;
        if (abs (Del - 1.0)) < Eps then
          Gam_Q := Exp(-X + A * Log(X) - Gam_Ln) * H;
          return;
        end if;
      end loop;
      Raise_Exception(Gamma_Error'Identity, "Argument A of Gamma_Q_Continued_Fraction too large, Itmax too small");
    end if;
  end Gamma_Q_Continued_Fraction;

end Gamma_Functions;

